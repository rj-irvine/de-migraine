###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2026-07-30
#
# Program Inputs       : "data/rx_obs" (from 08_rx.R)
# Program Outputs      : "data/cov5"                (treatment-pattern table),
#                        "data/rx_daysupply_diag"   (days-supply diagnostics),
#                        "data/rx_episodes"         (constructed episodes),
#                        "data/rx_lot"              (lines of therapy),
#                        "data/rx_adherence"        (per-patient MPR/PDC),
#                        "data/rx_adherence_fixed_window" (fixed-window PDC),
#                        "data/atc_duration_lookup" (per-ATC median duration),
#                        "data/rx_daysupply_source" (observed vs imputed split)
#
# Description          : N02 treatment patterns for the matched cohort (both
#                        arms), over each patient's follow-up window:
#                          (A) Treatment episodes & persistence
#                          (B) Lines of therapy (by molecule)
#                          (C) Adherence over each patient's own treatment
#                              span. Computed and saved as working, but NOT
#                              reported: it is not comparable between arms.
#                          (C2) Adherence over a fixed 365-day window from
#                              index. This is the reported version.
#
#                        Runs off data/rx_obs, so no Snowflake needed.
#
#                        Days supply comes from the `duration` field (days).
#                        Where it is missing the line takes the median observed
#                        duration for its own ATC code rather than a flat
#                        constant; see the Days supply block below for why.
#                        Grace period is 30 days. Both live in the constants
#                        below, so change them in one place if needed.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-07-30  Ryan Irvine             Treatment episodes, LoT, adherence
# 0.2       2026-08-12  Ryan Irvine             Fixed-window PDC (cov5_8/cov5_9)
# 0.3       2026-08-13  Ryan Irvine             Keep zero-coverage patients in the
#                                               fixed-window denominator
# 0.4       2026-08-13  Ryan Irvine             union_covered_days() moved to
#                                               functions/ (shared with 10)
# 0.5       2026-08-13  Ryan Irvine             Days supply imputed per ATC code
#                                               instead of a flat 30 days
# 0.6       2026-08-13  Ryan Irvine             Drop the span-based MPR/PDC rows
#                                               from the table (not comparable)
# 0.7       2026-08-14  Ryan Irvine             Fix cov5_4 ordering: control-only
#                                               molecules no longer sort first
# 1.0
################################################################################

# Step 1. Setup ----
# This program works entirely from data/rx_obs, so it does not need Snowflake.
# DE_OFFLINE tells 00_global.R to skip the connection and the codelist rebuild.
# Removed straight afterwards so a later program in the same session (runAll.R)
# still gets a live connection.
DE_OFFLINE <- TRUE
source("00_global.R")
rm(DE_OFFLINE)

GRACE_DAYS <- 30 # a gap > coverage + GRACE ends an episode / line
MIN_ATC_OBS <- 50 # observed durations needed before an ATC median is trusted
DEFAULT_DAYS_SUPPLY <- 30 # last resort only: no ATC median and no global median

rx_obs <- readRDS("data/rx_obs")

# ---------------------------------------------------------------------------
# Days supply ----
# `duration` is in days (confirmed: frequency_code "J" is a unit marker meaning
# day), but only ~24% of N02 lines carry one, and whether a line carries one is
# driven almost entirely by the drug rather than the patient - see
# 11_duration_missingness.R. Recording rates by class are near-identical across
# arms (opioids 35%/36%, everyday analgesics 28%/31%, migraine-specific
# 3.3%/5.5%); cases only look worse overall because they receive far more
# migraine-specific drugs, which are taken as needed and so rarely carry a
# duration at all.
#
# A single flat fallback therefore flattened away exactly the variation that
# distinguishes the arms, and manufactured roughly 60% of the case/control
# coverage gap out of drug mix. Each missing line instead takes the median
# observed duration for its own ATC code, so the differing drug mix is carried
# through. Codes with fewer than MIN_ATC_OBS observed durations fall back to
# the global median, and DEFAULT_DAYS_SUPPLY is a last resort that should not
# normally be reached.
#
# Caveats that survive this and belong on any output built from it: about
# three quarters of case lines are still imputed, just better; for the
# migraine-specific drugs the median rests on the ~3% of lines that happened to
# carry a duration, which may not be typical; and `duration` counts days of
# administration, not days of coverage, so it understates depot products such
# as the monthly CGRP antibodies.
# ---------------------------------------------------------------------------
rx_dur <- rx_obs |>
  mutate(
    duration_num = suppressWarnings(as.numeric(duration)),
    duration_observed = !is.na(duration_num) & duration_num > 0
  )

atc_duration_lookup <- rx_dur |>
  filter(duration_observed) |>
  group_by(product_atc_code) |>
  summarise(n_observed = n(), atc_median = median(duration_num), .groups = "drop") |>
  filter(n_observed >= MIN_ATC_OBS)
saveRDS(atc_duration_lookup, "data/atc_duration_lookup")

global_median_duration <- median(rx_dur$duration_num[rx_dur$duration_observed])

# Order lines within patient (the constructs below depend on this order).
rx <- rx_dur |>
  left_join(atc_duration_lookup, by = "product_atc_code") |>
  mutate(
    event_date = as.Date(event_date),
    day_supply = case_when(
      duration_observed ~ duration_num,
      !is.na(atc_median) ~ atc_median,
      !is.na(global_median_duration) ~ global_median_duration,
      TRUE ~ DEFAULT_DAYS_SUPPLY
    ),
    cov_end = event_date + day_supply
  ) |>
  arrange(cohort, person_id, event_date, num_sequence, product_atc_code)

# Provenance of the days supply, so the imputed share can be quoted directly.
rx_daysupply_source <- rx |>
  mutate(source = case_when(
    duration_observed ~ "observed",
    !is.na(atc_median) ~ "ATC median",
    TRUE ~ "global median"
  )) |>
  count(cohort, source, name = "n_lines") |>
  group_by(cohort) |>
  mutate(pct = round(100 * n_lines / sum(n_lines), 1)) |>
  ungroup()
saveRDS(rx_daysupply_source, "data/rx_daysupply_source")

# ===========================================================================
# Step 2. Days-supply diagnostics ----
# Distribution of the raw duration/quantity/frequency fields.
# ===========================================================================
rx_daysupply_diag <- rx_obs |>
  summarise(
    n_lines = n(),
    duration_missing = sum(is.na(duration) | suppressWarnings(as.numeric(duration)) <= 0),
    duration_min = suppressWarnings(min(as.numeric(duration), na.rm = TRUE)),
    duration_p25 = suppressWarnings(quantile(as.numeric(duration), 0.25, na.rm = TRUE)),
    duration_median = suppressWarnings(median(as.numeric(duration), na.rm = TRUE)),
    duration_p75 = suppressWarnings(quantile(as.numeric(duration), 0.75, na.rm = TRUE)),
    duration_max = suppressWarnings(max(as.numeric(duration), na.rm = TRUE)),
    quantity_median = suppressWarnings(median(as.numeric(quantity), na.rm = TRUE)),
    quantity_max = suppressWarnings(max(as.numeric(quantity), na.rm = TRUE))
  )
freq_dist <- rx_obs |>
  count(frequency_code, name = "n_lines") |>
  arrange(desc(n_lines))
saveRDS(rx_daysupply_diag, "data/rx_daysupply_diag")
saveRDS(freq_dist, "data/rx_frequency_dist")

# ===========================================================================
# (A) TREATMENT EPISODES & PERSISTENCE ----
# An episode is a run of prescriptions each starting within (previous coverage
# end + GRACE_DAYS). A bigger gap starts a new episode.
# ===========================================================================
episodes_lines <- rx |>
  group_by(cohort, person_id) |>
  mutate(
    prev_cov_end = lag(cummax(as.numeric(cov_end))),
    prev_cov_end = as.Date(prev_cov_end, origin = "1970-01-01"),
    new_episode = is.na(prev_cov_end) | event_date > (prev_cov_end + GRACE_DAYS),
    episode_id = cumsum(new_episode)
  ) |>
  ungroup()

rx_episodes <- episodes_lines |>
  group_by(cohort, person_id, episode_id) |>
  summarise(
    episode_start = min(event_date),
    episode_cov_end = max(cov_end),
    n_rx = n(),
    episode_duration_days = as.numeric(max(cov_end) - min(event_date)),
    .groups = "drop"
  )
saveRDS(rx_episodes, "data/rx_episodes")

# Per-patient episode summary
episodes_per_patient <- rx_episodes |>
  group_by(cohort, person_id) |>
  summarise(n_episodes = n(), .groups = "drop")

# Persistence: duration of the FIRST episode per patient
first_episode <- rx_episodes |>
  group_by(cohort, person_id) |>
  arrange(episode_start, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

# cov5_1. Number of N02 treatment episodes per patient
cov5_1 <- summarize_var(episodes_per_patient, x = "n_episodes", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1, "Number of N02 treatment episodes per patient", name)
  ) |>
  select(-`NA`)

# cov5_2. Duration of first N02 treatment episode (days) = persistence proxy
cov5_2 <- summarize_var(first_episode, x = "episode_duration_days", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  "Persistence: duration of first N02 treatment episode (days)", name)
  ) |>
  select(-`NA`)

# ===========================================================================
# (B) LINES OF THERAPY ----
# A line is a run on the same molecule. A molecule switch or a gap beyond grace
# starts the next line.
# ===========================================================================
lot_lines <- rx |>
  filter(!is.na(product_molecule_code)) |>
  group_by(cohort, person_id) |>
  mutate(
    prev_cov_end = lag(cummax(as.numeric(cov_end))),
    prev_cov_end = as.Date(prev_cov_end, origin = "1970-01-01"),
    prev_mol = lag(product_molecule_code),
    gap_break = is.na(prev_cov_end) | event_date > (prev_cov_end + GRACE_DAYS),
    mol_break = is.na(prev_mol) | product_molecule_code != prev_mol,
    new_line = gap_break | mol_break,
    line_no = cumsum(new_line)
  ) |>
  ungroup()

rx_lot <- lot_lines |>
  group_by(cohort, person_id, line_no) |>
  summarise(
    molecule = first(product_molecule_code),
    line_start = min(event_date),
    n_rx = n(),
    .groups = "drop"
  )
saveRDS(rx_lot, "data/rx_lot")

lines_per_patient <- rx_lot |>
  group_by(cohort, person_id) |>
  summarise(
    n_lines = n_distinct(line_no),
    n_lines_cat = NA_character_,
    .groups = "drop"
  ) |>
  mutate(
    n_lines_cat = case_when(
      n_lines == 1 ~ "1 line",
      n_lines == 2 ~ "2 lines",
      n_lines == 3 ~ "3 lines",
      n_lines >= 4 ~ ">= 4 lines"
    )
  )

# cov5_3. Number of lines of therapy per patient, n (%)
cov5_3 <- summarize_var(lines_per_patient, x = "n_lines_cat", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1, "Number of N02 lines of therapy per patient, n (%)", name))

# cov5_4. Molecule at first line of therapy (top 6 + Other), n (%)
# Ranked by how many CASES started on the molecule, since the objective is to
# characterise treatment of headache patients.
#
# The header row and a control-only molecule both have an empty case cell, so
# a single "missing means put it first" rule cannot tell them apart. It used to
# treat both as 99999, which sorted molecules that no headache patient ever
# started on to the very top of the block - ahead of metamizole - where they
# also consumed one of the limited molecule slots and pushed a real molecule
# into "Other molecule". The header is now pinned explicitly, and a molecule
# with no case patients ranks 0 so it falls into "Other molecule".
N_TOP_MOLECULES <- 6

lot1 <- rx_lot |>
  group_by(cohort, person_id) |>
  arrange(line_no, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

cov5_4_full <- summarize_var(lot1, x = "molecule", group_var = "cohort") |>
  mutate(
    is_header = is.na(name),
    order = case |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |> as.numeric(),
    name = ifelse(is_header, "Molecule at first line of therapy, n (%)", name),
    order = ifelse(is_header, Inf, ifelse(is.na(order), 0, order))
  ) |>
  arrange(desc(order))

# Everything past the header plus the top N is pooled into "Other molecule".
cov5_4_other <- cov5_4_full |>
  filter(row_number() >= N_TOP_MOLECULES + 2) |>
  mutate(name = "     Other molecule") |>
  mutate(
    case_num = case |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |> as.numeric(),
    control_num = control |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |> as.numeric()
  ) |>
  group_by(name) |>
  summarise(
    case = prettyNum(sum(case_num, na.rm = TRUE), big.mark = ","),
    control = prettyNum(sum(control_num, na.rm = TRUE), big.mark = ","),
    .groups = "drop"
  )

cov5_4 <- cov5_4_full |>
  filter(between(row_number(), 1, N_TOP_MOLECULES + 1)) |> # +1 for the header
  select(name, case, control) |>
  union_all(cov5_4_other)

# ===========================================================================
# (C) ADHERENCE OVER EACH PATIENT'S OWN SPAN ----
# Over each patient's span (first Rx to last coverage end):
#   MPR = total days-supply / span days      (capped at 1)
#   PDC = distinct covered days / span days
# Adherent = PDC >= 0.80.
#
# NOT REPORTED. These are computed and saved to data/rx_adherence as working,
# but deliberately kept out of the cov5 table, because they are not comparable
# between arms: a patient with a single prescription has a span equal to that
# one supply and so scores 1.00 by construction. That is why controls appear
# far more adherent than cases on this measure (MPR 0.73 vs 0.52, 61.9% vs
# 39.0% adherent) while the fixed-window measure in (C2) - which judges every
# patient over the same amount of time - shows no difference at all. Printing
# both invited the reader to take the wrong one. Use (C2) for anything
# reported; keep these only for methods discussion.
# ===========================================================================
# union_covered_days() now lives in functions/union_covered_days.R (sourced by
# 00_global.R) so 10_daysupply_check.R can reuse the identical logic.

# Distinct covered-day count per patient via the interval union above.
covered_days <- rx |>
  group_by(cohort, person_id) |>
  summarise(covered = union_covered_days(event_date, cov_end), .groups = "drop")

adherence <- rx |>
  group_by(cohort, person_id) |>
  summarise(
    span_start = min(event_date),
    span_end = max(cov_end),
    total_supply = sum(day_supply, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(covered_days, by = c("cohort", "person_id")) |>
  mutate(
    span_days = pmax(as.numeric(span_end - span_start), 1),
    mpr = pmin(total_supply / span_days, 1),
    pdc = pmin(covered / span_days, 1),
    adherent = ifelse(pdc >= 0.80, "Yes", "No")
  )
saveRDS(adherence, "data/rx_adherence")

# No cov5_5/6/7. The span-based summaries that used to sit here were dropped
# from the table for the reason given above; data/rx_adherence still holds the
# per-patient values if they are ever needed. Numbering of the blocks below is
# left unchanged so it keeps matching the header and the revision history.

# ===========================================================================
# (C2) ADHERENCE OVER A FIXED WINDOW ----
# The span-based measures above are not comparable between arms, which is why
# they are computed but not reported. Here the denominator is instead a fixed
# FIXED_WINDOW_DAYS window from index, so
# every patient is judged over the same amount of time. Coverage is clipped to
# the window at both ends. Still among treated patients only (rx_obs holds no
# rows for patients with zero N02 lines).
#
# Patients whose prescriptions all fall after the window are kept, not filtered:
# their coverage clips to nothing and they score PDC 0, which is the true value.
# Dropping them would bias the arms differently (they are a much larger share of
# controls than of cases) and so recreate the very imbalance this block fixes.
# ===========================================================================
FIXED_WINDOW_DAYS <- 365 # all patients have >= 1 year of follow-up by design

adherence_fw <- rx |>
  mutate(
    win_start = as.Date(index_date),
    win_end = pmin(as.Date(censor_date), as.Date(index_date) + FIXED_WINDOW_DAYS)
  ) |>
  group_by(cohort, person_id) |>
  summarise(
    win_start = first(win_start),
    win_end = first(win_end),
    covered_fw = union_covered_days(event_date, cov_end, first(win_start), first(win_end)),
    supply_fw = sum(pmax(
      pmin(as.numeric(cov_end), as.numeric(first(win_end))) -
        pmax(as.numeric(event_date), as.numeric(first(win_start))), 0
    )),
    .groups = "drop"
  ) |>
  mutate(
    window_days = pmax(as.numeric(win_end - win_start), 1),
    pdc_fw = pmin(covered_fw / window_days, 1),
    mpr_fw = pmin(supply_fw / window_days, 1),
    adherent_fw = ifelse(pdc_fw >= 0.80, "Yes", "No")
  )
saveRDS(adherence_fw, "data/rx_adherence_fixed_window")

# cov5_8. PDC over the fixed window
cov5_8 <- summarize_var(adherence_fw, x = "pdc_fw", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  paste0("Proportion of days covered (PDC), fixed ",
                         FIXED_WINDOW_DAYS, "-day window from index"), name)
  ) |>
  select(-`NA`)

# cov5_9. Adherent over the fixed window, n (%)
cov5_9 <- summarize_var(adherence_fw, x = "adherent_fw", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       paste0("Adherent to N02 therapy (PDC >= 0.80), fixed ",
                              FIXED_WINDOW_DAYS, "-day window, n (%)"), name))

# ===========================================================================
# Assemble treatment-pattern table ----
# ===========================================================================
cov5 <- data.frame(
  name = "To characterise N02 treatment patterns (episodes, lines of therapy, adherence) of headache disorder patients",
  case = NA, control = NA
) |>
  union_all(cov5_1) |>
  union_all(cov5_2) |>
  union_all(cov5_3) |>
  union_all(cov5_4) |>
  union_all(cov5_8) |>
  union_all(cov5_9)

saveRDS(cov5, "data/cov5")
print("cov5 (N02 treatment patterns) has been created and saved to data directory.")

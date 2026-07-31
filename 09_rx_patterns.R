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
#                        "data/rx_adherence"        (per-patient MPR/PDC)
#
# Description          : N02 treatment patterns for the matched cohort (both
#                        arms), over each patient's follow-up window:
#                          (A) Treatment episodes & persistence
#                          (B) Lines of therapy (by molecule)
#                          (C) Adherence: MPR and PDC
#
#                        Runs off data/rx_obs, so no Snowflake needed.
#
#                        Days-supply comes from the `duration` field (days),
#                        with a 30-day fallback and a 30-day grace period. Both
#                        live in days_supply() / the constants below, so change
#                        them in one place if needed.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-07-30  Ryan Irvine             Treatment episodes, LoT, adherence
# 0.2
# 1.0
################################################################################

# Step 1. Setup ----
source("00_global.R")

DEFAULT_DAYS_SUPPLY <- 30 # used when duration is missing / <= 0
GRACE_DAYS <- 30 # a gap > coverage + GRACE ends an episode / line

# Days-supply from the duration field, with a fallback.
days_supply <- function(duration) {
  d <- suppressWarnings(as.numeric(duration))
  ifelse(is.na(d) | d <= 0, DEFAULT_DAYS_SUPPLY, d)
}

rx_obs <- readRDS("data/rx_obs")

# Order lines within patient (the constructs below depend on this order).
rx <- rx_obs |>
  mutate(
    event_date = as.Date(event_date),
    day_supply = days_supply(duration),
    cov_end = event_date + day_supply
  ) |>
  arrange(cohort, person_id, event_date, num_sequence, product_atc_code)

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
lot1 <- rx_lot |>
  group_by(cohort, person_id) |>
  arrange(line_no, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

cov5_4_full <- summarize_var(lot1, x = "molecule", group_var = "cohort") |>
  mutate(
    order = case |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |> as.numeric(),
    name = ifelse(is.na(name), "Molecule at first line of therapy, n (%)", name),
    order = ifelse(is.na(order), 99999, order)
  ) |>
  arrange(desc(order))

cov5_4_other <- cov5_4_full |>
  filter(row_number() >= 7) |>
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
  filter(between(row_number(), 1, 6)) |>
  select(name, case, control) |>
  union_all(cov5_4_other)

# ===========================================================================
# (C) ADHERENCE: MPR and PDC ----
# Over each patient's span (first Rx to last coverage end):
#   MPR = total days-supply / span days      (capped at 1)
#   PDC = distinct covered days / span days
# Adherent = PDC >= 0.80.
# ===========================================================================
# Covered days for one patient: merge overlapping [start, end) intervals and sum.
union_covered_days <- function(start, end) {
  ord <- order(start)
  s <- as.numeric(start[ord])
  e <- as.numeric(end[ord])
  cur_s <- s[1]
  cur_e <- e[1]
  total <- 0
  if (length(s) > 1) {
    for (i in 2:length(s)) {
      if (s[i] <= cur_e) {
        if (e[i] > cur_e) cur_e <- e[i]
      } else {
        total <- total + (cur_e - cur_s)
        cur_s <- s[i]
        cur_e <- e[i]
      }
    }
  }
  total + (cur_e - cur_s)
}

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

# cov5_5. Medication possession ratio (MPR), capped at 1
cov5_5 <- summarize_var(adherence, x = "mpr", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1, "Medication possession ratio (MPR), capped at 1", name)
  ) |>
  select(-`NA`)

# cov5_6. Proportion of days covered (PDC)
cov5_6 <- summarize_var(adherence, x = "pdc", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1, "Proportion of days covered (PDC)", name)
  ) |>
  select(-`NA`)

# cov5_7. Adherent (PDC >= 0.80), n (%)
cov5_7 <- summarize_var(adherence, x = "adherent", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1, "Adherent to N02 therapy (PDC >= 0.80), n (%)", name))

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
  union_all(cov5_5) |>
  union_all(cov5_6) |>
  union_all(cov5_7)

saveRDS(cov5, "data/cov5")
print("cov5 (N02 treatment patterns) has been created and saved to data directory.")

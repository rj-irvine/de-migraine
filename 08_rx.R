###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2026-07-17
#
# Program Inputs       : "data/patpop_matched", "data/rx_codelist"
# Program Outputs      : "data/cov4",
#                        "data/rx_lines_raw"   (durable line-level N02 extract),
#                        "data/rx_obs"         (in-window, cohort-tagged N02 lines),
#                        "data/rx_patient_tx"  (per-patient treatment-pattern summary),
#                        "data/time_to_first_rx",
#                        "data/rx_detail_raw"  (durable prescription_detail extract,
#                                               if the view is available)
#
# Description          : DE-specific objective (not performed in the UK study).
#                        Prescription patterns for ATC group N02 (analgesics),
#                        with particular interest in N02C (antimigraine), over
#                        the matched cohort, both arms, within each person's
#                        follow-up window (index_date < event_date <= censor_date).
#
#                        Analyses (cov4_1..cov4_11, by cohort):
#                          cov4_1  Number of N02 Rx per patient
#                          cov4_2  N02 Rx lines by ATC code
#                          cov4_3  Index (first-line) N02 subgroup, n (%)
#                          cov4_4  Time from headache index to first N02 Rx (days)
#                          cov4_5  Annualized N02 Rx per patient
#                          cov4_6  Total quantity dispensed per patient
#                          cov4_7  Possible acute-medication overuse (>=10 N02
#                                  Rx/year), n (%)  [MOH risk proxy]
#                        Treatment patterns:
#                          cov4_8  Distinct N02 subgroups per patient (breadth)
#                          cov4_9  Switched between N02 subgroups, n (%)
#                          cov4_10 Combination use among N02C users, n (%)
#                          cov4_11 Most common N02 subgroup pathways, n (%)
#
#                        LICENCE NOTE: data access ends after the current pull,
#                        so this program also saves durable line-level extracts
#                        (rx_lines_raw, rx_detail_raw) to data/ so any further
#                        prescription analysis can be re-derived OFFLINE without
#                        re-querying Snowflake.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-07-17  Ryan Irvine             New DE prescription objective
# 0.2       2026-07-30  Ryan Irvine             Treatment patterns (first-line,
#                                               persistence/intensity) + durable
#                                               raw extracts before licence ends
# 1.0
################################################################################

# Step 1. Run global program ----
source("00_global.R")

# Step 2. Load inputs ----
patpop_matched <- readRDS("data/patpop_matched")
rx_codelist <- readRDS("data/rx_codelist")

# N02 product ids, pushed into Snowflake for the prescription filter.
n02_product_ids <- as.character(rx_codelist$product_id)

# Matched-cohort follow-up windows (both arms, case-defined window).
match_windows <- bind_rows(
  patpop_matched |>
    transmute(person_id = person_id_case, cohort = "case",
              index_date, censor_date, followup_days),
  patpop_matched |>
    transmute(person_id = person_id_control, cohort = "control",
              index_date, censor_date, followup_days)
)

# ---------------------------------------------------------------------------
# Step 3. Durable line-level N02 extract ----
# Pull EVERY available field for N02 prescription lines (filter to N02 products
# in-DB, then collect). This is the licence-safety artefact: once saved, all
# downstream analysis can run offline. contact_prescriptions carries quantity,
# frequency_code, duration, box, dci_flag, num_sequence, diagnostic_code; the
# date comes from the parent contact.
# ---------------------------------------------------------------------------
rx_lines <- contact_prescriptions |>
  select(
    contact_id, product_id, diagnostic_code, num_sequence,
    quantity, frequency_code, duration, box, dci_flag
  ) |>
  filter(product_id %in% local(n02_product_ids)) |>
  inner_join(
    contact |>
      select(contact_id, person_id, provider_id, event_date = start_date),
    by = "contact_id"
  ) |>
  filter(!is.na(person_id) & !is.na(event_date) & event_date >= StartDate) |>
  # Attach product attributes (molecule, generic/brand) from the product master,
  # which is lazy here — keep the join lazy (both sides lazy) then collect once.
  left_join(
    product |>
      select(product_id, product_atc_code, product_molecule_code,
             short_name, long_name, brand_name, is_generic),
    by = "product_id"
  ) |>
  collect()

# Attach cohort/window locally and keep only in-window lines. Also derive the
# N02 subgroup (4th-level ATC) and antimigraine flag from the codelist.
rx_obs <- rx_lines |>
  inner_join(match_windows, by = "person_id", relationship = "many-to-many") |>
  filter(event_date > index_date & event_date <= censor_date) |>
  mutate(
    product_atc_code = toupper(product_atc_code),
    atc_subgroup = ifelse(is.na(product_atc_code), NA_character_,
                          substr(product_atc_code, 1, 4)),
    is_antimigraine = ifelse(!is.na(atc_subgroup) & atc_subgroup == "N02C", 1L, 0L)
  )

# Save the durable extracts (licence safety).
saveRDS(rx_lines, "data/rx_lines_raw")
saveRDS(rx_obs, "data/rx_obs")
print("Durable N02 line-level extract saved to data/rx_lines_raw and data/rx_obs.")

# ---------------------------------------------------------------------------
# Step 3b. Durable prescription_detail extract (richer fields) ----
# The 43-column prescription_detail table adds treatment_code, duration_min/max,
# renewal, prevention_flag, dose/frequency, specialist_code. Its DE view name is
# unconfirmed, so this is wrapped so a wrong/missing name does not abort the run.
# ---------------------------------------------------------------------------
rx_detail_raw <- tryCatch(
  {
    detail_tbl <- tbl(con, I(prescription_detail_view)) |>
      rename_all(tolower)
    detail_tbl |>
      select(
        contact_id, product_id, diagnostic_code, num_sequence, treatment_code,
        dose_1, dose_2, product_dose_unit_code, frequency_type_code,
        frequency_1, frequency_2, frequency_code, duration_min, duration_max,
        renewal, prescription_flag, dci_flag, prevention_flag, specialist_code,
        product_init_date
      ) |>
      filter(product_id %in% local(n02_product_ids)) |>
      inner_join(
        contact |> select(contact_id, person_id, event_date = start_date),
        by = "contact_id"
      ) |>
      filter(!is.na(person_id) & event_date >= StartDate) |>
      collect()
  },
  error = function(e) {
    message("prescription_detail extract skipped (view unavailable or renamed): ",
            conditionMessage(e))
    NULL
  }
)
if (!is.null(rx_detail_raw)) {
  saveRDS(rx_detail_raw, "data/rx_detail_raw")
  print("Durable prescription_detail extract saved to data/rx_detail_raw.")
}

# ===========================================================================
# ANALYSES (all by cohort, over the follow-up window)
# ===========================================================================

# Per-person window info (one row per patient in the matched set, incl. those
# with zero N02 lines) so intensity/overuse denominators use the full cohort.
# 1:1 matching means each id appears once per arm, but collapse defensively in
# case an id recurs, taking the max follow-up.
person_window <- match_windows |>
  group_by(person_id, cohort) |>
  summarise(followup_days = max(followup_days, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------------------------
# cov4_1. Number of N02 prescriptions per patient (among patients with >=1) ----
# ---------------------------------------------------------------------------
n02_per_person <- rx_obs |>
  group_by(cohort, person_id) |>
  summarise(n_rx = n(), total_qty = sum(quantity, na.rm = TRUE), .groups = "drop")

cov4_1 <- summarize_var(n02_per_person, x = "n_rx", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  "Number of N02 prescriptions per patient (among patients with >=1)",
                  name)
  ) |>
  select(-`NA`)

# ---------------------------------------------------------------------------
# cov4_2. N02 prescription lines by full ATC code ----
# ---------------------------------------------------------------------------
rx_by_atc <- rx_obs |>
  mutate(product_atc_code = ifelse(is.na(product_atc_code),
                                   "N02 (unclassified)", product_atc_code))

cov4_2 <- summarize_var(rx_by_atc, x = "product_atc_code", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1, "N02 prescription lines by ATC code", name))

atc_order <- rx_by_atc |>
  distinct(product_atc_code, atc_subgroup, is_antimigraine) |>
  mutate(sort_key = paste0("     ", product_atc_code)) |>
  arrange(desc(is_antimigraine), product_atc_code)

cov4_2 <- cov4_2 |>
  mutate(row_id = row_number()) |>
  left_join(atc_order |> mutate(rank = row_number()) |> select(sort_key, rank),
            by = c("name" = "sort_key")) |>
  arrange(row_id != 1, rank, row_id) |>
  select(-row_id, -rank)

# ---------------------------------------------------------------------------
# cov4_3. Index (first-line) N02 subgroup, n (%) ----
# The N02 subgroup of each patient's FIRST N02 prescription in follow-up.
# Ties on date broken by lowest num_sequence then ATC to be deterministic.
# ---------------------------------------------------------------------------
first_rx <- rx_obs |>
  filter(!is.na(atc_subgroup)) |>
  group_by(cohort, person_id) |>
  arrange(event_date, num_sequence, product_atc_code, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

cov4_3 <- first_rx |>
  summarize_var(x = "atc_subgroup", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       "Index (first-line) N02 subgroup, n (%)", name))

# ---------------------------------------------------------------------------
# cov4_4. Time from headache index date to first N02 prescription (days) ----
# ---------------------------------------------------------------------------
time_to_first_rx <- first_rx |>
  mutate(days_to_first_rx = as.numeric(event_date - index_date))
saveRDS(time_to_first_rx, "data/time_to_first_rx")

cov4_4 <- time_to_first_rx |>
  summarize_var(x = "days_to_first_rx", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  "Time from headache index to first N02 prescription (days)", name)
  ) |>
  select(-`NA`)

# ---------------------------------------------------------------------------
# cov4_5. Annualized N02 prescriptions per patient ----
# (n_rx / followup_days) * 365.25, among patients with >=1 N02 Rx.
# ---------------------------------------------------------------------------
n02_annual <- n02_per_person |>
  left_join(person_window, by = c("person_id", "cohort")) |>
  mutate(n_rx_annual = (n_rx / as.numeric(followup_days)) * 365.25)

cov4_5 <- summarize_var(n02_annual, x = "n_rx_annual", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  "Number of N02 prescriptions per patient, annualized", name)
  ) |>
  select(-`NA`)

# ---------------------------------------------------------------------------
# cov4_6. Total N02 quantity dispensed per patient (among patients with >=1) ----
# ---------------------------------------------------------------------------
cov4_6 <- summarize_var(n02_per_person, x = "total_qty", group_var = "cohort") |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(row_number() == 1,
                  "Total N02 quantity dispensed per patient (among patients with >=1)",
                  name)
  ) |>
  select(-`NA`)

# ---------------------------------------------------------------------------
# cov4_7. Possible acute-medication overuse, n (%) ----
# Proxy for medication-overuse-headache (MOH) risk: patients averaging >=10 N02
# prescriptions per year of follow-up. Denominator is the FULL matched arm
# (patients with zero N02 Rx count as "No"), so this is a population rate.
# ---------------------------------------------------------------------------
overuse <- person_window |>
  left_join(n02_annual |> select(person_id, cohort, n_rx_annual),
            by = c("person_id", "cohort")) |>
  mutate(
    n_rx_annual = ifelse(is.na(n_rx_annual), 0, n_rx_annual),
    overuse = ifelse(n_rx_annual >= 10, "Yes", "No")
  )

cov4_7 <- summarize_var(overuse, x = "overuse", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       "Possible acute-medication overuse (>=10 N02 Rx/yr), n (%)",
                       name))

# ===========================================================================
# TREATMENT PATTERNS
# The following analyses characterise how N02 therapy evolves over a patient's
# follow-up: regimen breadth, switching between N02 subgroups, combination /
# add-on use, and the ordered subgroup pathway (lines of therapy).
# All operate on rx_obs (in-window N02 lines, cohort-tagged). "Subgroup" is the
# 4th-level ATC (N02A opioids, N02B other analgesics, N02C antimigraine).
# ===========================================================================

# Per-patient subgroup summary: the ordered distinct-subgroup sequence, the
# count of distinct subgroups (regimen breadth / lines of therapy), the count of
# distinct molecules, and flags for each subgroup's presence.
patient_tx <- rx_obs |>
  filter(!is.na(atc_subgroup)) |>
  arrange(cohort, person_id, event_date, num_sequence, product_atc_code) |>
  group_by(cohort, person_id) |>
  summarise(
    n_subgroups = n_distinct(atc_subgroup),
    n_molecules = n_distinct(product_molecule_code[!is.na(product_molecule_code)]),
    has_n02a = as.integer(any(atc_subgroup == "N02A")),
    has_n02b = as.integer(any(atc_subgroup == "N02B")),
    has_n02c = as.integer(any(atc_subgroup == "N02C")),
    # Ordered pathway of DISTINCT subgroups in first-appearance order.
    subgroup_path = paste(unique(atc_subgroup), collapse = " -> "),
    .groups = "drop"
  ) |>
  mutate(
    switched = ifelse(n_subgroups >= 2, "Yes", "No"),
    n_subgroups_cat = case_when(
      n_subgroups == 1 ~ "1 subgroup",
      n_subgroups == 2 ~ "2 subgroups",
      n_subgroups >= 3 ~ ">= 3 subgroups"
    ),
    # Combination / add-on among N02C (antimigraine) users.
    n02c_combo = case_when(
      has_n02c == 1 & (has_n02a == 1 | has_n02b == 1) ~ "N02C + other N02",
      has_n02c == 1 ~ "N02C only",
      TRUE ~ NA_character_
    )
  )
saveRDS(patient_tx, "data/rx_patient_tx")

# ---------------------------------------------------------------------------
# cov4_8. Number of distinct N02 subgroups per patient, n (%) ----
# Regimen breadth / lines of therapy (among patients with >=1 N02 Rx).
# ---------------------------------------------------------------------------
cov4_8 <- patient_tx |>
  summarize_var(x = "n_subgroups_cat", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       "Number of distinct N02 subgroups per patient, n (%)", name))

# ---------------------------------------------------------------------------
# cov4_9. Switched between N02 subgroups during follow-up, n (%) ----
# "Yes" = received >=2 distinct N02 subgroups over follow-up.
# ---------------------------------------------------------------------------
cov4_9 <- patient_tx |>
  summarize_var(x = "switched", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       "Switched between N02 subgroups during follow-up, n (%)", name))

# ---------------------------------------------------------------------------
# cov4_10. Combination / add-on among N02C users, n (%) ----
# Of patients ever prescribed N02C, the share also prescribed N02A or N02B.
# Denominator here is N02C users (patients with no N02C are NA and drop out).
# ---------------------------------------------------------------------------
cov4_10 <- patient_tx |>
  filter(!is.na(n02c_combo)) |>
  summarize_var(x = "n02c_combo", group_var = "cohort") |>
  mutate(name = ifelse(row_number() == 1,
                       "Combination use among N02C (antimigraine) users, n (%)", name))

# ---------------------------------------------------------------------------
# cov4_11. Most common N02 subgroup treatment pathways, n (%) ----
# The ordered distinct-subgroup sequence (e.g. "N02B -> N02C"); top 6 shown, the
# remainder collapsed to "Other pathway" via the same re-parse pattern used in
# cov2_3/cov3_1.
# ---------------------------------------------------------------------------
cov4_11_full <- patient_tx |>
  summarize_var(x = "subgroup_path", group_var = "cohort") |>
  mutate(
    order = case |>
      str_extract("^[0-9,]+") |>
      str_replace_all(",", "") |>
      as.numeric(),
    name = ifelse(is.na(name),
                  "Most common N02 subgroup treatment pathways, n (%)", name),
    order = ifelse(is.na(order), 99999, order)
  ) |>
  arrange(desc(order))

cov4_11_other <- cov4_11_full |>
  filter(row_number() >= 7) |>
  mutate(name = "     Other pathway") |>
  mutate(
    case_num = case |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |>
      as.numeric(),
    control_num = control |> str_extract("^[0-9,]+") |> str_replace_all(",", "") |>
      as.numeric()
  ) |>
  group_by(name) |>
  summarise(
    case = prettyNum(sum(case_num, na.rm = TRUE), big.mark = ","),
    control = prettyNum(sum(control_num, na.rm = TRUE), big.mark = ","),
    .groups = "drop"
  )

cov4_11 <- cov4_11_full |>
  filter(between(row_number(), 1, 6)) |>
  select(name, case, control) |>
  union_all(cov4_11_other)

# ===========================================================================
# Step 7. Combine into a single outcomes block and save ----
# ===========================================================================
cov4 <- data.frame(
  name = "To assess N02 (analgesic) prescription patterns, incl. N02C antimigraine, of headache disorder patients",
  case = NA, control = NA
) |>
  union_all(cov4_1) |>
  union_all(cov4_2) |>
  union_all(cov4_3) |>
  union_all(cov4_4) |>
  union_all(cov4_5) |>
  union_all(cov4_6) |>
  union_all(cov4_7) |>
  union_all(cov4_8) |>
  union_all(cov4_9) |>
  union_all(cov4_10) |>
  union_all(cov4_11)

saveRDS(cov4, file = "data/cov4")
print("cov4 (N02 prescription patterns) has been created and saved to data directory.")

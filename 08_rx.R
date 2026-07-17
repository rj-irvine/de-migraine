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
# Program Outputs      : "data/cov4"
#
# Description          : DE-specific objective (not performed in the UK study).
#                        Counts migraine-related prescriptions in ATC group N02
#                        (analgesics), with particular interest in N02C
#                        (antimigraine preparations), over the matched cohort.
#                        The unit is the prescription line (one row of
#                        contact_prescriptions) within each person's follow-up
#                        window (index_date < event_date <= censor_date).
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-07-17  Ryan Irvine             New DE prescription objective
# 0.2
# 1.0
################################################################################

# Step 1. Run global program ----
source("00_global.R")

# Step 2. Load inputs ----
patpop_matched <- readRDS("../data/patpop_matched")
rx_codelist <- readRDS("../data/rx_codelist")

# N02 product ids, pushed into Snowflake for the prescription filter.
n02_product_ids <- as.character(rx_codelist$product_id)

# Step 3. All N02 prescription lines with their contact date ----
# Prescription lines live on contact_prescriptions (contact_id, product_id);
# the prescribing date comes from the parent contact (start_date). Restrict to
# N02 products up front so only the relevant lines are carried forward.
rx_lines <- contact_prescriptions |>
  select(contact_id, product_id) |>
  filter(product_id %in% local(n02_product_ids)) |>
  inner_join(
    contact |>
      select(contact_id, person_id, event_date = start_date),
    by = "contact_id"
  ) |>
  filter(!is.na(person_id) & !is.na(event_date) & event_date >= StartDate)

# Step 4. Attach each arm's follow-up window, keep in-window lines ----
# Both arms use the case-defined window (index_date, censor_date), matching how
# cov1/cov2/cov3 compare the matched pair over the same interval. patpop_matched
# is a local data frame; build one long table of (person_id, cohort, window) and
# push it into Snowflake (copy = TRUE) so the join to rx_lines runs in-database.
match_windows <- bind_rows(
  patpop_matched |>
    transmute(person_id = person_id_case, cohort = "case", index_date, censor_date),
  patpop_matched |>
    transmute(person_id = person_id_control, cohort = "control", index_date, censor_date)
)

rx_obs <- rx_lines |>
  inner_join(match_windows, by = "person_id", copy = TRUE) |>
  filter(event_date > index_date & event_date <= censor_date) |>
  collect() |>
  # Attach ATC classification from the (local) product master.
  left_join(
    rx_codelist |>
      select(product_id, product_atc_code, atc_subgroup, is_antimigraine),
    by = "product_id"
  )

# Step 5. cov4_1. Total N02 prescription lines per person ----
# One count per person over the follow-up window (numeric summary: mean/med/rng).
# Patients with zero N02 lines drop out of the inner joins above, so this
# describes the count among patients with at least one N02 prescription.
n02_per_person <- rx_obs |>
  group_by(cohort, person_id) |>
  summarise(n_rx = n(), .groups = "drop")

cov4_1 <- summarize_var(
  data = n02_per_person,
  x = "n_rx",
  group_var = "cohort"
) |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      row_number() == 1,
      "Number of N02 prescriptions per patient (among patients with >=1)",
      name
    )
  ) |>
  select(-`NA`)

# Step 6. cov4_2. Prescription lines by full ATC code (N02) ----
# Categorical breakdown: for each distinct ATC code under N02, the number of
# prescription lines and its share of all N02 lines in the arm. N02C
# (antimigraine) codes are surfaced first via the ordering below.
rx_by_atc <- rx_obs |>
  mutate(
    product_atc_code = ifelse(
      is.na(product_atc_code),
      "N02 (unclassified)",
      product_atc_code
    )
  )

cov4_2 <- summarize_var(
  data = rx_by_atc,
  x = "product_atc_code",
  group_var = "cohort"
) |>
  mutate(
    name = ifelse(
      row_number() == 1,
      "N02 prescription lines by ATC code",
      name
    )
  )

# Order the ATC rows so N02C (antimigraine) codes appear first, then the rest
# alphabetically; keep the title row (row 1) pinned to the top.
atc_order <- rx_by_atc |>
  distinct(product_atc_code, atc_subgroup, is_antimigraine) |>
  mutate(sort_key = paste0("     ", product_atc_code)) |>
  arrange(desc(is_antimigraine), product_atc_code)

cov4_2 <- cov4_2 |>
  mutate(row_id = row_number()) |>
  left_join(
    atc_order |>
      mutate(rank = row_number()) |>
      select(sort_key, rank),
    by = c("name" = "sort_key")
  ) |>
  arrange(row_id != 1, rank, row_id) |>
  select(-row_id, -rank)

# Step 7. Combine into a single outcomes block and save ----
cov4 <- data.frame(
  name = "To assess N02 (analgesic) prescription patterns, incl. N02C antimigraine, of headache disorder patients",
  case = NA,
  control = NA
) |>
  union_all(cov4_1) |>
  union_all(cov4_2)

saveRDS(cov4, file = "../data/cov4")
print("cov4 (N02 prescription counts) has been created and saved to data directory.")

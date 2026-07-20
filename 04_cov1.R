###############################################################################
# Study Name           : UK Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-20
#
# Program Inputs       : "data/patpop_matched"
# Program Outputs      : "data/cov1"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-20  Ryan Irvine             Conversion from SAS to R
# 0.2
# 1.0
################################################################################

# Step 1. Run global program ----
source("00_global.R")

# Step 2. Load patpop_matched
patpop_matched <- readRDS("data/patpop_matched")

# Step 3. Get all observations for population of interest ----
temp <- contact |>
  right_join(
    contact_diagnostics |> select(contact_id, diagnostic_code),
    by = "contact_id"
  ) |>
  select(
    person_id,
    event_date = start_date,
    observation_code = diagnostic_code,
    contact_id,
    contact_type_code
  ) |>
  filter(
    event_date >= StartDate &
      !contact_type_code == "R"
  )

obs_case <- patpop_matched_obs(id_var = "person_id_case") |>
  mutate(cohort = "case") |>
  left_join(
    (patpop_matched |> select(person_id_case, censor_date, followup_days)),
    by = join_by(person_id == person_id_case)
  )
obs_control <- patpop_matched_obs(id_var = "person_id_control") |>
  mutate(cohort = "control") |>
  left_join(
    (patpop_matched |> select(person_id_control, censor_date, followup_days)),
    by = join_by(person_id == person_id_control)
  )

patpop_matched_obs_full <- obs_case |>
  union_all(obs_control) |>
  mutate(
    headache = ifelse(
      observation_code %in% (diagnosis_codelist |> pull(code)),
      1,
      0
    )
  )

rm(obs_case, obs_control)

# cov1_1. Number of GP Visits (headache disorder related) (after first headache disorder diagnosis) ----
gp_visit_headache <- patpop_matched_obs_full |>
  filter(
    headache == 1 & event_date >= index_date & event_date <= censor_date
  ) |>
  distinct(person_id, event_date, cohort) |>
  group_by(person_id) |>
  mutate(n_visit = as.numeric(row_number())) |>
  filter(max(n_visit) == n_visit) |>
  ungroup()

cov1_1 <- summarize_var(
  data = gp_visit_headache,
  x = "n_visit",
  group_var = "cohort"
) |>
  pivot_wider(names_from = cohort) |>
  mutate(
    control = NA,
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      row_number() == 1,
      "Number of GP Visits (headache disorder related) (after first headache disorder diagnosis)",
      name
    )
  ) |>
  select(-`NA`)

# cov1_2. Number of GP Visits (all-cause) (after first headache disorder diagnosis) ----
gp_visit_all <- patpop_matched_obs_full |>
  filter(event_date >= index_date & event_date <= censor_date) |>
  distinct(person_id, event_date, cohort, followup_days) |>
  group_by(person_id) |>
  mutate(n_visit = as.numeric(row_number())) |>
  filter(max(n_visit) == n_visit) |>
  ungroup()

cov1_2 <- summarize_var(
  data = gp_visit_all,
  x = "n_visit",
  group_var = "cohort"
) |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      row_number() == 1,
      "Number of GP Visits (all cause) (after first headache disorder diagnosis)",
      name
    )
  ) |>
  select(-`NA`)

# cov1_3. Annualized GP Visits (all-cause) (after first headache disorder diagnosis) ----
gp_visit_annual <- gp_visit_all |>
  mutate(n_visit_annual = (n_visit / as.numeric(followup_days)) * 365.25)

saveRDS(gp_visit_annual, "data/gp_visit_annual")

cov1_3 <- summarize_var(
  data = gp_visit_annual,
  x = "n_visit_annual",
  group_var = "cohort"
) |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      row_number() == 1,
      "Number of GP Visits, annualized (all cause) (after first headache disorder diagnosis)",
      name
    )
  ) |>
  select(-`NA`)

# Combine into single table and save ----
cov1 <- data.frame(
  name = "To assess the number of GP visits of headache disorder patients",
  case = NA,
  control = NA
) |>
  union_all(cov1_1) |>
  union_all(cov1_2) |>
  union_all(cov1_3)

saveRDS(cov1, file = "data/cov1")

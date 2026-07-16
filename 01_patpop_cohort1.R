###############################################################################
# Study Name           : UK Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-14
#
# Program Inputs       : "data/diagnosis_codelist"
# Program Outputs      : "data/patpop_cohort1", "data/table1_1"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-14  Ryan Irvine             Conversion from SAS to R
# 0.2
# 1.0
################################################################################

# Step 1. Run global program ----
source("00_global.R")

# Step 2. All observable patients in the UK after [StartDate] ----
all_patients <- person |>
  filter(last_contact_date >= StartDate) |>
  mutate(
    first_obs = ifelse(
      first_contact_date < StartDate,
      StartDate,
      first_contact_date
    )
  ) |>
  select(person_id, first_obs, last_obs = last_contact_date)

N_all_patients <- all_patients |>
  distinct(person_id) |>
  summarise(N = n()) |>
  collect() |>
  pull(N) # N = 5,103,238

# Step 3. M2Q Criterion ----
## 3-1. Gather all headache disorder observations and leave as lazy table ----
dx <- as.character(diagnosis_codelist$code)
headache_obs <- contact_diagnostics |>
  right_join(
    contact |>
      select(person_id, contact_id, event_date = start_date, provider_id),
    by = "contact_id"
  ) |>
  filter(diagnostic_code %in% local(dx) & !is.na(event_date)) |>
  select(
    person_id,
    contact_id,
    diagnostic_code,
    event_date,
    provider_id
  ) |>
  filter(
    !is.na(person_id) &
      !is.na(event_date) &
      !is.na(diagnostic_code) &
      event_date >= StartDate
  ) |>
  inner_join(
    (all_patients),
    by = "person_id"
  ) |>
  window_order(person_id, event_date) |>
  mutate(event_quarter = paste0(year(event_date), " Q", quarter(event_date))) |>
  left_join(
    person |>
      select(
        person_id,
        year_of_birth,
        gender_code,
        care_site_id
      ),
    by = "person_id"
  ) |>
  filter(gender_code %in% c("M", "F") & !is.na(year_of_birth)) #|>
#collect() |>
#left_join(diagnosis_codelist, by = join_by(x$diagnostic_code == y$code))

## 3-2. >= 2 headache disorder diagnoses in 2 different quarters ----
diff_Q <- headache_obs |>
  group_by(person_id) |>
  filter(n_distinct(event_quarter) >= 2) |>
  ungroup()

## 3-3. >= 2 diagnoses in the same quarter made by different providers ----
diff_P <- headache_obs |>
  group_by(person_id, event_quarter) |>
  filter(n_distinct(provider_id) >= 2) |>
  ungroup()

## 3-4. Combine into single list, pull all observation records for population ----
obs_cohort1 <- diff_Q |>
  union_all(diff_P) |>
  select(person_id) |>
  inner_join(
    headache_obs,
    by = join_by(person_id),
    relationship = "many-to-many"
  )

N_m2q_criteria <- obs_cohort1 |>
  distinct(person_id) |>
  summarise(N = n()) |>
  collect() |>
  pull(N) # N = 57,955

# Step 4. Age >= 18 in year of first diagnosis
## 4-1. Identify first diagnosis
first_diagnosis_ge18 <- obs_cohort1 |>
  window_order(person_id, event_date) |>
  group_by(person_id) |>
  mutate(n_diagnosis = row_number()) |>
  filter(n_diagnosis == 1 & (year(event_date) - year_of_birth >= 18)) |>
  ungroup() |>
  select(
    person_id,
    index_date = event_date,
    index_diagnosis = diagnostic_code,
    gender_code,
    year_of_birth,
    care_site_id,
    first_obs,
    last_obs
  )

N_18 <- first_diagnosis_ge18 |>
  distinct(person_id) |>
  summarise(N = n()) |>
  collect() |>
  pull(N) # N = 51,134

patpop_cohort1 <- first_diagnosis_ge18

# Step 5. All patients must have at least 1 year of follow-up ----
patpop_cohort1 <- first_diagnosis_ge18 |>
  filter(as.numeric(difftime(index_date, last_obs, units = "days")) >= 365)
N_1yr <- patpop_cohort1 |>
  distinct(person_id) |>
  summarise(N = n()) |>
  collect() |>
  pull(N)

# Step 6. Construct part 1 of attrition table ----
## NOTE: Only first 4 rows are created here, final row will be generated in
## 02_patpop_cohort2.R and appended
table1_1 <- data.frame(
  label = c(
    paste0(
      "1. All observable patients in THIN UK from [",
      format(StartDate, "%b %d %Y"),
      "] onwards."
    ),
    "2. Patients with at least two diagnostic codes for a headache disorder in two different quarters OR by two different physicians in the same quarter.",
    paste0(
      "3. All patients who are at least 18 years of age when receiving their first headache disorder diagnosis during the identification period ([",
      format(StartDate, "%b %d %Y"),
      "] onwards)."
    ),
    "4. All patients with at least 1 year of follow-up after index date."
  ),
  value = c(
    N_all_patients,
    N_m2q_criteria,
    N_18,
    N_1yr
  )
)

saveRDS(table1_1, "../data/table1_1")

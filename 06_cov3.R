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
# Program Outputs      : "data/cov3"
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

# Step 3. Get required demo info for population ----
## Index age, gender
demo_case <- patpop_matched |>
  mutate(index_age = year(index_date) - year_of_birth_case, cohort = "case") |>
  select(
    person_id = person_id_case,
    index_date,
    index_age,
    gender_code,
    cohort,
    year_of_birth = year_of_birth_case
  )

demo_control <- patpop_matched |>
  mutate(
    index_age = year(index_date) - year_of_birth_control,
    cohort = "control"
  ) |>
  select(
    person_id = person_id_control,
    index_date,
    index_age,
    gender_code,
    cohort,
    year_of_birth = year_of_birth_control
  )

patpop_demo1 <- demo_case |> union_all(demo_control)

rm(demo_case, demo_control)

## First diagnosis label in identification period
headache_codes <- as.vector(diagnosis_codelist$code)
patpop_headache_obs <- contact |>
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
  ) |>
  filter(
    observation_code %in%
      headache_codes &
      !is.na(event_date)
  ) |>
  collect() |>
  right_join(patpop_demo1, by = join_by(person_id)) |>
  left_join(
    diagnosis_codelist |> select(code, label_fmt),
    by = join_by(observation_code == code)
  ) |>
  arrange(cohort, person_id, contact_id)

first_hist_diagnosis_date <- patpop_headache_obs |>
  group_by(person_id) |>
  mutate(
    first_hist_diagnosis_date = as.Date(ifelse(
      row_number() == 1,
      event_date,
      NA
    ))
  ) |>
  ungroup() |>
  filter(cohort == "case" & !is.na(first_hist_diagnosis_date)) |>
  select(person_id, first_hist_diagnosis_date)

index_diagnosis_label <- patpop_headache_obs |>
  filter(event_date >= StartDate) |>
  group_by(person_id) |>
  mutate(
    index_diagnosis_label = ifelse(
      row_number() == 1,
      label_fmt,
      NA
    )
  ) |>
  ungroup() |>
  filter(cohort == "case" & !is.na(index_diagnosis_label)) |>
  select(person_id, index_diagnosis_label)

patpop_demo <- patpop_demo1 |>
  left_join(first_hist_diagnosis_date, by = "person_id") |>
  left_join(index_diagnosis_label, by = "person_id")

# cov3_1 First Headache Disorder Diagnosis ----
temp <- patpop_demo |>
  summarize_var(x = "index_diagnosis_label", group_var = "cohort") |>
  head(30)

cov3_1_full <- temp |>
  mutate(
    order = case |>
      str_extract("^[0-9,]+") |> # extract digits (and commas) at start
      str_replace_all(",", "") |> # remove commas
      as.numeric()
  ) |> # convert to numeric
  mutate(
    name = ifelse(
      is.na(name),
      "Index Headache Disorder Diagnosis (ICD-10), n (%)",
      name
    ),
    order = ifelse(is.na(order) == 1, 99999, order)
  ) |>
  arrange(desc(order))

cov3_1_other <- cov3_1_full |>
  filter(row_number() >= 7) |>
  mutate(name = "     Other headache disorder diagnosis") |>
  mutate(
    case_num = case |>
      str_extract("^[0-9,]+") |>
      str_replace_all(",", "") |>
      as.numeric()
  ) |>
  group_by(name) |>
  summarise(
    case = paste0(
      prettyNum(sum(case_num), big.mark = ","),
      " (",
      round(
        sum(case_num) /
          nrow(
            patpop_demo |>
              filter(cohort == "case")
          ) *
          100,
        2
      ),
      "%)"
    ),
    control = NA
  )

cov3_1 <- cov3_1_full |>
  filter(between(row_number(), 1, 6)) |>
  select(name, case, control) |>
  union_all(cov3_1_other)

# cov3_2 Year of Diagnosis in Identification Period ----
cov3_2 <- patpop_demo |>
  filter(cohort == "case") |>
  mutate(year = as.character(year(index_date))) |>
  summarize_var(x = "year", group_var = "cohort") |>
  mutate(
    name = ifelse(
      is.na(name),
      "Year of Diagnosis in Identification Period",
      name
    ),
    control = NA
  )

# cov3_3 Age at First Diagnosis in Identification Period (years) (continuous) ----
cov3_3 <- patpop_demo |>
  summarize_var(x = "index_age", group_var = "cohort") |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      is.na(name),
      "Age at First Diagnosis in Identification Period (years) (ontinuous)",
      name
    )
  ) |>
  pivot_wider(names_from = "cohort") |>
  select(-`NA`)

# cov3_4 Age at First Diagnosis in Identification Period (years) (categorical) ----
cov3_4 <- patpop_demo |>
  mutate(
    index_age_cat = case_when(
      between(index_age, 18, 29) ~ "1",
      between(index_age, 30, 39) ~ "2",
      between(index_age, 40, 49) ~ "3",
      between(index_age, 50, 59) ~ "4",
      between(index_age, 60, 69) ~ "5",
      index_age >= 70 ~ "6"
    )
  ) |>
  summarize_var(x = "index_age_cat", group_var = "cohort") |>
  mutate(
    name = ifelse(
      is.na(name),
      "Age at First Diagnosis in Identification Period (years) (categorical)",
      name
    )
  )

# cov3_5 Age at First Historical Diagnosis (years) (continuous) ----
cov3_5 <- patpop_demo |>
  mutate(historical_age = year(first_hist_diagnosis_date) - year_of_birth) |>
  filter(cohort == "case") |>
  summarize_var(x = "historical_age", group_var = "cohort") |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      is.na(name),
      "Age at First Historical Diagnosis (years) (continuous)",
      name
    )
  ) |>
  pivot_wider(names_from = "cohort") |>
  select(-`NA`) |>
  mutate(control = NA)

# cov3_6 Age at Historical Diagnosis (years) (categorical) ----
cov3_6 <- patpop_demo |>
  mutate(historical_age = year(first_hist_diagnosis_date) - year_of_birth) |>
  filter(cohort == "case") |>
  mutate(
    historical_age_cat = case_when(
      historical_age < 18 ~ "0",
      between(historical_age, 18, 29) ~ "1",
      between(historical_age, 30, 39) ~ "2",
      between(historical_age, 40, 49) ~ "3",
      between(historical_age, 50, 59) ~ "4",
      between(historical_age, 60, 69) ~ "5",
      historical_age >= 70 ~ "6"
    )
  ) |>
  summarize_var(x = "historical_age_cat", group_var = "cohort") |>
  mutate(
    name = ifelse(
      is.na(name),
      "Age at First Historical Diagnosis (years) (categorical)",
      name
    ),
    control = NA
  )

# cov3_7 Gender -----
cov3_7 <- patpop_demo |>
  transmute(
    person_id,
    cohort,
    gender = case_when(gender_code == "M" ~ "1", gender_code == "F" ~ "2")
  ) |>
  summarize_var(x = "gender", group_var = "cohort") |>
  mutate(
    name = ifelse(
      is.na(name),
      "Gender",
      name
    )
  )

# cov3 output
cov3 <- data.frame(
  name = "To describe demograhpic and clinical characteristics of headache disorder patients",
  case = NA,
  control = NA
) |>
  union_all(cov3_1) |>
  union_all(cov3_2) |>
  union_all(cov3_3) |>
  union_all(cov3_4) |>
  union_all(cov3_5) |>
  union_all(cov3_6) |>
  union_all(cov3_7)

saveRDS(cov3, "data/cov3")

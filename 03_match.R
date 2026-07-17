###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       : "data/table1_1" (via 01), plus lazy cohorts from 01/02
# Program Outputs      : "data/patpop_matched", "data/table1"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-17  Ryan Irvine             Conversion from SAS to R
# 0.2
# 1.0
################################################################################

# Step 1. Build cohorts ----
# Source 01 and 02 (each of which sources 00_global.R) so that patpop_cohort1
# and patpop_cohort2 exist as live lazy Snowflake tbls and the matching join is
# pushed down to the database.
source("01_patpop_cohort1.R")
source("02_patpop_cohort2.R")

# Step 2. Load cohort data and assign to study (headache disorder) and ----
# control (no headache disorder). Create range variables as needed.
study <- patpop_cohort1
control <- patpop_cohort2

# Step 3. Identify all possible matches between headache and no headache patients ----
## Matching on:
##     year_of_birth +/- 2 years
##     gender_code
##     care_site_id

set.seed(123)
patpop_matched <-
  study %>%
  # Precompute bounds for join_by()
  mutate(
    yob_lower = year_of_birth - 2L,
    yob_upper = year_of_birth + 2L
  ) %>%
  inner_join(
    control,
    by = join_by(
      gender_code,
      care_site_id,
      between(y$year_of_birth, x$yob_lower, x$yob_upper)
    ),
    relationship = "many-to-many",
    suffix = c("_case", "_control")
  ) %>%
  # Count how many cases each control can match (scarcity)
  add_count(person_id_control, name = "n_cases") %>%
  # One DB-side random tie breaker
  mutate(rand = sql("RANDOM()")) %>%
  # Assign each control to at most one case (scarcity-first)
  group_by(person_id_control) %>%
  window_order(n_cases, rand) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Assign at most one control per case
  group_by(person_id_case) %>%
  window_order(n_cases, rand) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Finalize follow-up
  mutate(
    censor_date = last_obs,
    followup_days = as.numeric(censor_date - index_date)
  ) %>%
  select(
    person_id_case,
    index_date,
    gender_code,
    person_id_control,
    censor_date,
    followup_days,
    year_of_birth_case,
    year_of_birth_control
  ) |>
  collect()

saveRDS(patpop_matched, "../data/patpop_matched")

# Step 6. Determine count for last row of attrition table ----
table1 <- readRDS("../data/table1_1") |>
  union_all(
    data.frame(
      label = c(
        "5. Patients in step 4 who are matched with a patient (1:1) having no history of headache disorder diagnoses (matched on year of birth, gender, site_id)."
      ),
      value = c(
        patpop_matched |>
          #distinct(person_id_case) |>
          summarise(N = n()) |>
          collect() |>
          pull(N)
      )
    )
  )


saveRDS(table1, "../data/table1")

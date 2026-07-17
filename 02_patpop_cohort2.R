###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       : "data/diagnosis_codelist"
# Program Outputs      : None (patpop_cohort2 stays a lazy tbl; see 2-3 note)
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

# Step 1. Run global program ----
source("00_global.R")

# Step 2. Identify patients with no history of headache disorder in the ----
# identification period
## 2-1. Look for patients with no headache records ----
dx <- as.character(diagnosis_codelist$code)
no_headache_ID <- contact_diagnostics |>
  right_join(
    contact |>
      select(contact_id, person_id, provider_id, event_date = start_date),
    by = "contact_id"
  ) |>
  filter(
    !is.na(person_id) &
      !is.na(event_date) &
      !is.na(diagnostic_code) &
      event_date >= StartDate
  ) |>
  mutate(temp = ifelse((diagnostic_code %in% local(dx)), 1, 0)) |>
  group_by(person_id) |>
  summarise(headache_indicator = sum(temp, na.rm = TRUE)) |> # No NA values, but include to suppress aggregation warning
  filter(headache_indicator == 0) |>
  select(person_id, headache_indicator)

## 2-2. All controls must have at least one year of follow-up ----
# Follow-up for controls runs from StartDate to last observation, so last_obs
# must be >= StartDate + 365 days. (The UK program had the difftime arguments
# reversed and then built the cohort from the unfiltered pool, so this filter
# never took effect.)
no_headache_ID1 <- no_headache_ID |>
  inner_join(
    all_patients |> select(person_id, first_obs, last_obs),
    by = "person_id"
  ) |>
  filter(
    as.numeric(difftime(last_obs, StartDate, units = "days")) >= 365
  )

## 2-3. Pull their demographic info from person table
patpop_cohort2 <- no_headache_ID1 |>
  left_join(
    (person |>
      select(
        person_id,
        year_of_birth,
        gender_code,
        care_site_id
      )),
    by = join_by(person_id)
  ) |>
  filter(
    sql(
      paste0(
        'DATEDIFF(year, DATE_FROM_PARTS("year_of_birth", 1, 1), ',
        StartDate_sql,
        ') >= 18'
      )
    )
  ) |>
  select(person_id, year_of_birth, gender_code, care_site_id, first_obs, last_obs) |>
  distinct()

# NOTE: patpop_cohort2 is a lazy Snowflake query, not a local data frame. It is
# intentionally NOT saved to disk (the control pool is large and a lazy tbl does
# not survive a session restart). 03_match.R rebuilds it by sourcing this
# program so the matching join is pushed down to Snowflake.

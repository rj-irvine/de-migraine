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
# Program Outputs      : "data/time_to_first_referral", "data/cov2"
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
patpop_matched <- readRDS("../data/patpop_matched")

# Step 3. Get all referrals for population ----
ref_case <- patpop_matched |>
  select(person_id = person_id_case, index_date, censor_date, followup_days) |>
  inner_join(
    (referral |>
      select(person_id, referral_id, event_date, diagnostic_code) |>
      collect()),
    by = join_by(person_id)
  ) |>
  filter(
    index_date <= event_date &
      event_date <= censor_date &
      diagnostic_code %in%
        (referral_codelist |>
          pull(code))
  ) |>
  arrange(person_id, referral_id) |>
  group_by(person_id) |>
  mutate(n_referral = row_number()) |>
  distinct() |>
  ungroup()

ref_control <- patpop_matched |>
  select(
    person_id = person_id_control,
    index_date,
    censor_date,
    followup_days
  ) |>
  inner_join(
    (referral |>
      select(person_id, referral_id, event_date, diagnostic_code) |>
      collect()),
    by = join_by(person_id)
  ) |>
  filter(
    index_date <= event_date &
      event_date <= censor_date &
      diagnostic_code %in%
        (referral_codelist |>
          pull(code))
  ) |>
  arrange(person_id, referral_id) |>
  group_by(person_id) |>
  mutate(n_referral = row_number()) |>
  distinct() |>
  ungroup()


temp <- (patpop_matched |>
  select(person_id = person_id_case) |>
  mutate(cohort = "case")) |>
  union_all(
    (patpop_matched |>
      select(person_id = person_id_control) |>
      mutate(cohort = "control"))
  )

patpop_referral <- temp |>
  left_join(
    ref_case |>
      union_all(ref_control) |>
      mutate(
        referred_to_care = 1,
        referred_to_neuro = ifelse(
          diagnostic_code %in%
            (referral_codelist |>
              filter(specialty == "Neurology") |>
              pull(code)),
          1,
          2
        )
      ) |>
      left_join(
        referral_codelist |> select(code, label, specialty),
        by = join_by(diagnostic_code == code)
      ),
    by = join_by(person_id)
  ) |>
  mutate(
    referred_to_care = ifelse(is.na(referred_to_care), 2, referred_to_care),
    referred_to_neuro = ifelse(is.na(referred_to_neuro), 2, referred_to_neuro)
  )

rm(ref_case, ref_control)

# cov2_1. Referral to any specialized care, n (%) ----
cov2_1 <- patpop_referral |>
  distinct(person_id, cohort, referred_to_care) |>
  mutate(referred_to_care = (as.character(referred_to_care))) |>
  summarize_var(x = "referred_to_care", group_var = "cohort") |>
  mutate(
    name = ifelse(is.na(name), "Referral to any specialized care, n (%)", name)
  )

# cov2_2. Referral to neurological specialized care, n (%) ----
cov2_2 <- patpop_referral |>
  group_by(person_id) |>
  mutate(
    referred_to_neuro1 = (as.character(ifelse(
      any(referred_to_neuro == 1),
      1,
      2
    )))
  ) |>
  ungroup() |>
  distinct(person_id, cohort, referred_to_neuro1) |>
  summarize_var(x = "referred_to_neuro1", group_var = "cohort") |>
  mutate(
    name = ifelse(
      is.na(name),
      "Referral to neurological specialized care, n (%)",
      name
    )
  )

# cov2_3. Specialty of first referral after first headache disorder diagnosis, n (%) ----
# REWRITING .....
temp <- patpop_referral |>
  filter(n_referral == 1) |>
  summarize_var(x = "specialty", group_var = "cohort")

cov2_3_full <- temp |>
  mutate(
    order = case |>
      str_extract("^[0-9,]+") |> # extract digits (and commas) at start
      str_replace_all(",", "") |> # remove commas
      as.numeric()
  ) |> # convert to numeric)
  mutate(
    name = ifelse(
      is.na(name),
      "Specialty of first referral after headache disorder diagnosis, n (%)",
      name
    ),
    order = ifelse(is.na(order) == 1, 9999, order),
    order = ifelse(name == "     Other / General", 0, order)
  ) |>
  arrange(desc(order))

saveRDS(cov2_3_full, "../data/cov2_3_full")

cov2_3_other <- cov2_3_full |>
  filter(row_number() >= 7) |>
  mutate(name = "     Other / General") |>
  mutate(
    case_num = case |>
      str_extract("^[0-9,]+") |>
      str_replace_all(",", "") |>
      as.numeric(),
    control_num = control |>
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
            patpop_referral |>
              filter(n_referral == 1 & cohort == "case")
          ) *
          100,
        2
      ),
      "%)"
    ),
    control = NA
  )

cov2_3 <- cov2_3_full |>
  filter(between(row_number(), 1, 6)) |>
  select(name, case, control) |>
  union_all(cov2_3_other)

# cov2_4. Time to first referral (in days) (from first headache disorder diagnosis) ----
temp <- patpop_referral |>
  filter(n_referral == 1) |>
  mutate(time_to_first_referral = as.numeric(event_date - index_date))
saveRDS(temp, "../data/time_to_first_referral") # Save to make Figure 1 - distrubution

cov2_4 <- temp |>
  summarize_var(x = "time_to_first_referral", group_var = "cohort") |>
  pivot_wider(names_from = "cohort") |>
  select(-`NA`) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      is.na(name),
      "Time to first referral (in days) (from first headache disorder diagnosis)",
      name
    )
  )

# cov2_5. Number of Specialist Referrals ----
cov2_5 <- patpop_referral |>
  group_by(person_id) |>
  filter(max(n_referral) == n_referral) |>
  ungroup() |>
  summarize_var(x = "n_referral", group_var = "cohort") |>
  pivot_wider(names_from = "cohort") |>
  select(-`NA`) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      is.na(name),
      "Number of Specialist Referrals",
      name
    )
  )


# cov2_6. Number of Specialist Referrals, annualized ----
cov2_6 <- patpop_referral |>
  group_by(person_id) |>
  filter(max(n_referral) == n_referral) |>
  ungroup() |>
  mutate(
    n_referral_annual = (n_referral / as.numeric(followup_days)) * 365.25
  ) |>
  summarize_var(
    x = "n_referral_annual",
    group_var = "cohort"
  ) |>
  pivot_wider(names_from = cohort) |>
  mutate(
    name = ifelse(!is.na(name), paste0("     ", name), name),
    name = ifelse(
      row_number() == 1,
      "Number of Specialist Referrals, annualized",
      name
    )
  ) |>
  select(-`NA`)

cov2 <- data.frame(
  name = "To assess referral patterns of headache disorder patients in primary care treatment",
  case = NA,
  control = NA
) |>
  union_all(cov2_1) |>
  union_all(cov2_2) |>
  union_all(cov2_3) |>
  union_all(cov2_4) |>
  union_all(cov2_5) |>
  union_all(cov2_6)

saveRDS(cov2, "../data/cov2")

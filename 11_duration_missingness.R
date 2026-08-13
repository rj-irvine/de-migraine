###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2026-08-13
#
# Program Inputs       : "data/rx_obs" (from 08_rx.R)
# Program Outputs      : "rawresults/duration_missingness.txt" (report)
#                        "data/duration_missingness"           (tables, RDS)
#                        "data/atc_duration_lookup"            (imputation table)
#
# Description          : Follow-up to 10_daysupply_check.R, which established
#                        that `duration` is genuinely in days (confirmed: the
#                        frequency_code "J" is a unit marker meaning day) but
#                        that only ~24% of N02 lines carry one. The flat 30-day
#                        substitution therefore drives most reported coverage,
#                        and because it is missing more often for cases (78%)
#                        than controls (68%) it also manufactures part of the
#                        case/control coverage gap.
#
#                        Since "J" is only a unit, it says nothing about WHICH
#                        prescriptions get a duration. That has to be measured:
#
#                          A. Can the missing values be derived instead of
#                             guessed? (is `quantity` usable where duration
#                             is not?)
#                          B. What predicts a duration being recorded - drug,
#                             cohort, calendar time, how much the patient
#                             fills?
#                          C. Is it a practice-level recording habit? If
#                             providers split into always-records and
#                             never-records, the observed quarter is a sample
#                             of practices rather than of prescriptions.
#                          D. Within patients who have both kinds of line, do
#                             the recorded ones look like the unrecorded ones?
#                             This is the cleanest test, since the patient is
#                             held constant.
#                          E. Does a drug-specific substitution beat the flat
#                             30 days? Imputing each missing line with the
#                             median observed duration FOR THAT ATC CODE uses
#                             the differing drug mix between arms instead of
#                             flattening it away.
#
#                        Runs offline from saved extracts. Aggregate output
#                        only, nothing patient-level.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-08-13  Ryan Irvine             Initial
# 1.0
################################################################################

# Step 1. Setup ----
DE_OFFLINE <- TRUE
source("00_global.R")
rm(DE_OFFLINE)

GRACE_DAYS <- 30         # must match 09_rx_patterns.R
FIXED_WINDOW_DAYS <- 365 # must match 09_rx_patterns.R
DEFAULT_DAYS_SUPPLY <- 30
MIN_ATC_OBS <- 50        # observed lines needed before trusting an ATC median

options(width = 120)

rx_obs <- readRDS("data/rx_obs")
out <- list()

show <- function(x, title) {
  cat("\n", title, "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

report <- file.path(rawresults_path, "duration_missingness.txt")
sink(report, split = TRUE)
cat("Which N02 prescriptions carry a duration, and does it matter?\n")
cat("Run:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 78), "\n")

rx <- rx_obs |>
  mutate(
    event_date = as.Date(event_date),
    duration_num = suppressWarnings(as.numeric(duration)),
    quantity_num = suppressWarnings(as.numeric(quantity)),
    observed = !is.na(duration_num) & duration_num > 0,
    atc_subgroup = substr(toupper(product_atc_code), 1, 4),
    year = as.integer(format(event_date, "%Y"))
  )

# ===========================================================================
# A. Can the missing values be derived from quantity instead? ----
# If quantity is sensibly populated exactly where duration is not, then a
# daily dose would let us compute the days supply properly. If quantity is
# junk in both halves, this route is closed and substitution is all there is.
# ===========================================================================
cat("\n\nA. IS `quantity` USABLE WHERE `duration` IS MISSING?\n")
cat(strrep("-", 78), "\n")

out$quantity_by_observed <- rx |>
  group_by(duration_recorded = observed) |>
  summarise(
    n_lines = n(),
    pct_quantity_missing = round(100 * mean(is.na(quantity_num)), 1),
    pct_quantity_zero = round(100 * mean(!is.na(quantity_num) & quantity_num == 0), 1),
    pct_quantity_usable = round(100 * mean(!is.na(quantity_num) & quantity_num > 0), 1),
    q_p25 = suppressWarnings(quantile(quantity_num[!is.na(quantity_num) & quantity_num > 0], 0.25)),
    q_median = suppressWarnings(median(quantity_num[!is.na(quantity_num) & quantity_num > 0])),
    q_p75 = suppressWarnings(quantile(quantity_num[!is.na(quantity_num) & quantity_num > 0], 0.75)),
    q_max = suppressWarnings(max(quantity_num, na.rm = TRUE)),
    .groups = "drop"
  )
show(out$quantity_by_observed, "Quantity, split by whether a duration was recorded")
cat("\nIf the FALSE row shows a usable quantity, a dose assumption could derive\n")
cat("the missing days supply. If it is mostly zero/missing, this route is shut.\n")

# ===========================================================================
# B. What predicts a duration being recorded? ----
# ===========================================================================
cat("\n\nB. WHAT PREDICTS A DURATION BEING RECORDED?\n")
cat(strrep("-", 78), "\n")

out$observed_by_cohort <- rx |>
  group_by(cohort) |>
  summarise(n_lines = n(), pct_observed = round(100 * mean(observed), 1),
            .groups = "drop")
show(out$observed_by_cohort, "By cohort")

out$observed_by_subgroup <- rx |>
  group_by(cohort, atc_subgroup) |>
  summarise(n_lines = n(), pct_observed = round(100 * mean(observed), 1),
            median_observed_duration = suppressWarnings(
              median(duration_num[observed])),
            .groups = "drop") |>
  arrange(atc_subgroup, cohort)
show(out$observed_by_subgroup, "By ATC subgroup and cohort")
cat("\nIf recording rates differ sharply by drug class, the case/control gap in\n")
cat("missingness is drug mix rather than anything about the patients.\n")

out$observed_by_atc <- rx |>
  group_by(product_atc_code) |>
  summarise(n_lines = n(), pct_observed = round(100 * mean(observed), 1),
            median_observed_duration = suppressWarnings(
              median(duration_num[observed])),
            .groups = "drop") |>
  arrange(desc(n_lines)) |>
  head(15)
show(out$observed_by_atc, "15 most prescribed ATC codes")

out$observed_by_year <- rx |>
  group_by(year) |>
  summarise(n_lines = n(), pct_observed = round(100 * mean(observed), 1),
            .groups = "drop") |>
  arrange(year)
show(out$observed_by_year, "By calendar year")

fills <- rx |>
  group_by(cohort, person_id) |>
  summarise(n_fills = n(), .groups = "drop") |>
  mutate(fill_band = cut(n_fills, c(0, 1, 2, 4, 9, Inf),
                         labels = c("1", "2", "3-4", "5-9", "10+")))
out$observed_by_fills <- rx |>
  left_join(fills, by = c("cohort", "person_id")) |>
  group_by(fill_band) |>
  summarise(n_lines = n(), pct_observed = round(100 * mean(observed), 1),
            .groups = "drop")
show(out$observed_by_fills, "By how many N02 lines the patient has")

# ===========================================================================
# C. Is recording a practice-level habit? ----
# A U-shaped distribution (providers piled at 0% and at 100%) means whole
# practices either record durations or never do, so the observed quarter is
# effectively a sample of practices.
# ===========================================================================
cat("\n\nC. IS RECORDING A PRACTICE-LEVEL HABIT?\n")
cat(strrep("-", 78), "\n")

if ("provider_id" %in% names(rx)) {
  prov <- rx |>
    filter(!is.na(provider_id)) |>
    group_by(provider_id) |>
    summarise(n_lines = n(), pct_observed = 100 * mean(observed), .groups = "drop") |>
    filter(n_lines >= 10)

  out$provider_profile <- prov |>
    mutate(band = cut(pct_observed, c(-0.01, 0.01, 10, 25, 50, 75, 90, 99.99, 100.01),
                      labels = c("0% (never)", "0-10%", "10-25%", "25-50%",
                                 "50-75%", "75-90%", "90-100%", "100% (always)"))) |>
    count(band, name = "n_providers") |>
    mutate(pct_of_providers = round(100 * n_providers / sum(n_providers), 1))
  show(out$provider_profile,
       paste0("Providers with 10+ lines (n = ", nrow(prov), "), by share recorded"))
  cat("\nWeight piled at the two ends means it is a recording habit, not a\n")
  cat("property of individual prescriptions.\n")
} else {
  cat("provider_id not present in rx_obs - cannot test.\n")
}

# ===========================================================================
# D. Within-patient comparison ----
# Patients holding both kinds of line let us compare like with like: the
# patient, their illness and their practice are held constant, so any
# difference is about the prescription itself.
# ===========================================================================
cat("\n\nD. WITHIN PATIENTS WHO HAVE BOTH KINDS OF LINE\n")
cat(strrep("-", 78), "\n")

mixed <- rx |>
  group_by(cohort, person_id) |>
  filter(any(observed) & any(!observed)) |>
  ungroup()

out$mixed_patients <- mixed |>
  distinct(cohort, person_id) |>
  count(cohort, name = "n_patients")
show(out$mixed_patients, "Patients with both recorded and unrecorded lines")

if (nrow(mixed)) {
  out$within_patient_subgroup <- mixed |>
    group_by(observed, atc_subgroup) |>
    summarise(n_lines = n(), .groups = "drop") |>
    group_by(observed) |>
    mutate(pct = round(100 * n_lines / sum(n_lines), 1)) |>
    ungroup() |>
    arrange(atc_subgroup, observed)
  show(out$within_patient_subgroup, "Drug mix, recorded vs unrecorded lines")
  cat("\nSimilar percentages down each drug class means recording is close to\n")
  cat("arbitrary within a patient, which is the best case for generalising.\n")
}

# ===========================================================================
# E. Does a drug-specific substitution beat the flat 30 days? ----
# Each missing line takes the median observed duration for its own ATC code
# (where enough observations exist), so the differing drug mix between arms
# is carried through instead of being flattened to one number.
# ===========================================================================
cat("\n\nE. DRUG-SPECIFIC SUBSTITUTION vs THE FLAT 30 DAYS\n")
cat(strrep("-", 78), "\n")

atc_lookup <- rx |>
  filter(observed) |>
  group_by(product_atc_code) |>
  summarise(n_observed = n(), atc_median = median(duration_num), .groups = "drop") |>
  filter(n_observed >= MIN_ATC_OBS)
global_median <- median(rx$duration_num[rx$observed])
saveRDS(atc_lookup, "data/atc_duration_lookup")

cat("ATC codes with >=", MIN_ATC_OBS, "observed durations:", nrow(atc_lookup), "\n")
cat("Global median observed duration:", global_median, "days\n")

rx_imp <- rx |>
  left_join(atc_lookup, by = "product_atc_code") |>
  mutate(
    source = case_when(observed ~ "observed",
                       !is.na(atc_median) ~ "ATC median",
                       TRUE ~ "global median"),
    ds_atc = case_when(observed ~ duration_num,
                       !is.na(atc_median) ~ atc_median,
                       TRUE ~ global_median)
  )

out$imputation_source <- rx_imp |>
  group_by(cohort, source) |>
  summarise(n_lines = n(), .groups = "drop") |>
  group_by(cohort) |>
  mutate(pct = round(100 * n_lines / sum(n_lines), 1)) |>
  ungroup()
show(out$imputation_source, "Where each line's days supply comes from")

# Headline measures for a given days-supply column, reusing 09's logic.
headline <- function(d, label) {
  d <- d |>
    mutate(cov_end = event_date + day_supply) |>
    arrange(cohort, person_id, event_date, num_sequence, product_atc_code)

  eps <- d |>
    group_by(cohort, person_id) |>
    mutate(prev_cov_end = lag(cummax(as.numeric(cov_end))),
           prev_cov_end = as.Date(prev_cov_end, origin = "1970-01-01"),
           new_episode = is.na(prev_cov_end) | event_date > (prev_cov_end + GRACE_DAYS),
           episode_id = cumsum(new_episode)) |>
    ungroup() |>
    group_by(cohort, person_id, episode_id) |>
    summarise(episode_start = min(event_date),
              episode_duration_days = as.numeric(max(cov_end) - min(event_date)),
              .groups = "drop") |>
    group_by(cohort, person_id) |>
    summarise(n_episodes = n(),
              first_duration = episode_duration_days[which.min(episode_start)],
              .groups = "drop")

  fw <- d |>
    mutate(win_start = as.Date(index_date),
           win_end = pmin(as.Date(censor_date),
                          as.Date(index_date) + FIXED_WINDOW_DAYS)) |>
    group_by(cohort, person_id) |>
    summarise(win_start = first(win_start), win_end = first(win_end),
              covered_fw = union_covered_days(event_date, cov_end,
                                              first(win_start), first(win_end)),
              .groups = "drop") |>
    mutate(window_days = pmax(as.numeric(win_end - win_start), 1),
           pdc_fw = pmin(covered_fw / window_days, 1))

  eps |>
    left_join(fw, by = c("cohort", "person_id")) |>
    group_by(cohort) |>
    summarise(scenario = label,
              n_patients = n(),
              median_first_duration = median(first_duration),
              iqr_first_duration = IQR(first_duration),
              mean_pdc = round(mean(pdc_fw, na.rm = TRUE), 3),
              pct_adherent = round(100 * mean(pdc_fw >= 0.80, na.rm = TRUE), 2),
              .groups = "drop")
}

message("  scenario: flat 30-day substitution")
flat <- headline(rx_imp |> mutate(day_supply = ifelse(observed, duration_num,
                                                      DEFAULT_DAYS_SUPPLY)),
                 "flat 30 days (reported)")
message("  scenario: ATC-specific substitution")
atc <- headline(rx_imp |> mutate(day_supply = ds_atc), "ATC-specific median")

out$imputation_comparison <- bind_rows(flat, atc) |>
  select(scenario, cohort, n_patients, median_first_duration,
         iqr_first_duration, mean_pdc, pct_adherent)
show(out$imputation_comparison, "Headline measures under each substitution")
cat("\nIf the case/control coverage gap narrows sharply under the ATC-specific\n")
cat("version, the gap in the reported numbers was drug mix, not treatment.\n")

cat("\n", strrep("=", 78), "\n")
cat("End of report.\n")
sink()

saveRDS(out, "data/duration_missingness")
print(paste("Missingness report written to", report))

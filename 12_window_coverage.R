###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2026-08-13
#
# Program Inputs       : "data/rx_lines_raw", "data/rx_obs",
#                        "data/patpop_matched"
# Program Outputs      : "rawresults/window_coverage.txt" (report)
#                        "data/window_coverage"           (tables, RDS)
#
# Description          : 11_duration_missingness.R tabulated N02 prescription
#                        lines by calendar year and found NONE before 2021,
#                        although the identification period opens 2016-12-01
#                        and patients need only one year of follow-up. Either
#                        the prescription source begins around 2021, or every
#                        index date is recent.
#
#                        It matters because cov5_8/cov5_9 measure coverage over
#                        a fixed 365 days from index. If prescribing data does
#                        not exist for that stretch of calendar time, those
#                        patients score zero coverage for a reason that has
#                        nothing to do with their treatment, and they are
#                        counted as non-adherent. That would depress the
#                        reported 1.5% adherence and the mean PDC by an
#                        arbitrary amount.
#
#                        rx_lines_raw settles which explanation holds: it is
#                        filtered only by StartDate, not by anybody's window.
#
#                          A. When does N02 prescribing data actually start?
#                          B. How are index dates distributed?
#                          C. How many patients have a fixed window that ends
#                             before prescribing data begins?
#                          D. Fixed-window PDC restricted to patients whose
#                             whole window sits inside the usable period.
#
#                        Runs offline. Aggregate output only.
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

GRACE_DAYS <- 30
FIXED_WINDOW_DAYS <- 365
DEFAULT_DAYS_SUPPLY <- 30

options(width = 120)
out <- list()

show <- function(x, title) {
  cat("\n", title, "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

report <- file.path(rawresults_path, "window_coverage.txt")
sink(report, split = TRUE)
cat("Does the fixed adherence window sit inside the prescribing data?\n")
cat("Run:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 78), "\n")

# ===========================================================================
# A. When does N02 prescribing data start? ----
# rx_lines_raw is every N02 line at or after StartDate, with no window filter,
# so it shows the true extent of the source.
# ===========================================================================
cat("\n\nA. WHEN DOES N02 PRESCRIBING DATA START?\n")
cat(strrep("-", 78), "\n")
cat("Study StartDate:", format(StartDate), "\n")

rx_lines_raw <- readRDS("data/rx_lines_raw")
rx_lines_raw <- rx_lines_raw |> mutate(event_date = as.Date(event_date))

out$raw_by_year <- rx_lines_raw |>
  mutate(year = as.integer(format(event_date, "%Y"))) |>
  count(year, name = "n_lines") |>
  arrange(year)
show(out$raw_by_year, "All N02 lines (no window filter) by year")

rx_data_start <- min(rx_lines_raw$event_date, na.rm = TRUE)
cat("\nEarliest N02 prescription anywhere in the extract:", format(rx_data_start), "\n")
cat("Gap between study start and first prescription:",
    as.integer(rx_data_start - StartDate), "days\n")
cat("\nA gap of years means the prescribing source starts late, and every\n")
cat("window closing before that date is empty for reasons of data coverage.\n")

# ===========================================================================
# B. Index date distribution ----
# ===========================================================================
cat("\n\nB. INDEX DATE DISTRIBUTION\n")
cat(strrep("-", 78), "\n")

patpop_matched <- readRDS("data/patpop_matched")
match_windows <- bind_rows(
  patpop_matched |>
    transmute(person_id = person_id_case, cohort = "case",
              index_date, censor_date, followup_days),
  patpop_matched |>
    transmute(person_id = person_id_control, cohort = "control",
              index_date, censor_date, followup_days)
) |>
  mutate(index_date = as.Date(index_date),
         censor_date = as.Date(censor_date),
         win_end = pmin(censor_date, index_date + FIXED_WINDOW_DAYS),
         index_year = as.integer(format(index_date, "%Y")))

out$index_by_year <- match_windows |>
  count(cohort, index_year, name = "n_patients") |>
  arrange(index_year, cohort)
show(out$index_by_year, "Matched patients by index year")

# ===========================================================================
# C. How many fixed windows close before prescribing data begins? ----
# ===========================================================================
cat("\n\nC. WINDOWS THAT CLOSE BEFORE THE DATA STARTS\n")
cat(strrep("-", 78), "\n")

match_windows <- match_windows |>
  mutate(
    window_status = case_when(
      win_end <= rx_data_start ~ "entirely before data",
      index_date < rx_data_start ~ "partly before data",
      TRUE ~ "fully inside data"
    )
  )

out$window_status <- match_windows |>
  count(cohort, window_status, name = "n_patients") |>
  group_by(cohort) |>
  mutate(pct = round(100 * n_patients / sum(n_patients), 1)) |>
  ungroup()
show(out$window_status, "Fixed 365-day window vs prescribing data coverage")

out$window_status_year <- match_windows |>
  filter(cohort == "case") |>
  count(index_year, window_status, name = "n_patients") |>
  group_by(index_year) |>
  mutate(pct = round(100 * n_patients / sum(n_patients), 1)) |>
  ungroup() |>
  arrange(index_year, window_status)
show(out$window_status_year, "Same, by index year (cases)")

# Treated patients only, the population cov5_8/cov5_9 actually report on.
rx_obs <- readRDS("data/rx_obs")
treated <- rx_obs |> distinct(cohort, person_id)

out$treated_window_status <- match_windows |>
  inner_join(treated, by = c("cohort", "person_id")) |>
  count(cohort, window_status, name = "n_patients") |>
  group_by(cohort) |>
  mutate(pct = round(100 * n_patients / sum(n_patients), 1)) |>
  ungroup()
show(out$treated_window_status,
     "Same, among treated patients (the cov5_8/cov5_9 denominator)")

# ===========================================================================
# D. Fixed-window PDC restricted to usable windows ----
# ===========================================================================
cat("\n\nD. ADHERENCE ON USABLE WINDOWS ONLY\n")
cat(strrep("-", 78), "\n")

rx <- rx_obs |>
  mutate(
    event_date = as.Date(event_date),
    duration_num = suppressWarnings(as.numeric(duration)),
    observed = !is.na(duration_num) & duration_num > 0,
    day_supply = ifelse(observed, duration_num, DEFAULT_DAYS_SUPPLY),
    cov_end = event_date + day_supply
  )

pdc_all <- rx |>
  mutate(win_start = as.Date(index_date),
         win_end = pmin(as.Date(censor_date),
                        as.Date(index_date) + FIXED_WINDOW_DAYS)) |>
  group_by(cohort, person_id) |>
  summarise(win_start = first(win_start), win_end = first(win_end),
            covered_fw = union_covered_days(event_date, cov_end,
                                            first(win_start), first(win_end)),
            .groups = "drop") |>
  mutate(window_days = pmax(as.numeric(win_end - win_start), 1),
         pdc_fw = pmin(covered_fw / window_days, 1)) |>
  left_join(match_windows |> select(cohort, person_id, window_status),
            by = c("cohort", "person_id"))

summarise_pdc <- function(d, label) {
  d |>
    group_by(cohort) |>
    summarise(scenario = label,
              n_patients = n(),
              mean_pdc = round(mean(pdc_fw), 3),
              median_pdc = round(median(pdc_fw), 3),
              pct_zero = round(100 * mean(pdc_fw == 0), 1),
              pct_adherent = round(100 * mean(pdc_fw >= 0.80), 2),
              .groups = "drop")
}

out$pdc_by_window <- bind_rows(
  summarise_pdc(pdc_all, "all treated (reported)"),
  summarise_pdc(pdc_all |> filter(window_status == "fully inside data"),
                "usable windows only")
) |>
  select(scenario, cohort, n_patients, mean_pdc, median_pdc, pct_zero, pct_adherent)
show(out$pdc_by_window, "Fixed-window PDC before and after restriction")

out$pdc_by_status <- pdc_all |>
  group_by(cohort, window_status) |>
  summarise(n_patients = n(), mean_pdc = round(mean(pdc_fw), 3),
            pct_zero = round(100 * mean(pdc_fw == 0), 1), .groups = "drop") |>
  arrange(window_status, cohort)
show(out$pdc_by_status, "Coverage by window status")
cat("\nIf 'entirely before data' patients are overwhelmingly at zero coverage,\n")
cat("the reported adherence figure is measuring data availability.\n")

cat("\n", strrep("=", 78), "\n")
cat("End of report.\n")
sink()

saveRDS(out, "data/window_coverage")
print(paste("Window coverage report written to", report))

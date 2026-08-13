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
#                        "data/rx_detail_raw" (optional, if the view existed)
# Program Outputs      : "rawresults/daysupply_check.txt"  (readable report)
#                        "data/daysupply_check"            (same tables, RDS)
#
# Description          : Validation of the days-supply assumption that
#                        09_rx_patterns.R rests on. 09 treats the `duration`
#                        field as a count of days and substitutes 30 days when
#                        it is missing or non-positive. Nothing in the data
#                        dictionary confirms either choice, and the reported
#                        persistence figure (median first-episode duration of
#                        exactly 30 days, with an interquartile range of zero)
#                        looks like it is being produced by the substitution
#                        rather than by the data.
#
#                        Four questions, in order:
#                          A. Is `duration` even expressed in days?
#                          B. How much of the coverage is substituted?
#                          C. Why is first-episode duration flat at 30 days?
#                             (substitution, or single-prescription episodes,
#                             which are two different problems)
#                          D. How far do the headline numbers move if the
#                             substituted value changes?
#                          E. Does rx_detail_raw carry a better field?
#
#                        Runs off saved extracts, so no Snowflake needed.
#                        Reads nothing and writes nothing patient-level.
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

GRACE_DAYS <- 30        # must match 09_rx_patterns.R
FIXED_WINDOW_DAYS <- 365 # must match 09_rx_patterns.R
DEFAULT_DAYS_SUPPLY <- 30 # the value under test
FALLBACKS <- c(7, 14, 28, 30, 60) # alternatives for the sensitivity run

options(width = 120) # keep the wider tables from wrapping in the report

rx_obs <- readRDS("data/rx_obs")

out <- list() # every table also goes to data/daysupply_check for later use

# Quantiles as a one-row data frame, so tables print tidily.
q_row <- function(x, label) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(data.frame(measure = label, n = 0))
  }
  data.frame(
    measure = label,
    n = length(x),
    min = min(x),
    p05 = unname(quantile(x, 0.05)),
    p25 = unname(quantile(x, 0.25)),
    median = median(x),
    p75 = unname(quantile(x, 0.75)),
    p95 = unname(quantile(x, 0.95)),
    max = max(x)
  )
}

show <- function(x, title) {
  cat("\n", title, "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
}

report <- file.path(rawresults_path, "daysupply_check.txt")
sink(report, split = TRUE)
cat("Days-supply validation for 09_rx_patterns.R\n")
cat("Run:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(strrep("=", 78), "\n")

# ===========================================================================
# A. Is `duration` expressed in days? ----
# If it is, the common values should look like pack sizes and course lengths
# (7, 10, 14, 20, 28, 30, 50, 100). If instead the top values are 1, 2 and 3,
# the field is far more likely to be a count of packs or of months, in which
# case every duration in 09 is out by roughly a factor of 30 and the whole
# treatment-pattern section needs rebuilding rather than recalibrating.
# ===========================================================================
cat("\n\nA. IS `duration` IN DAYS?\n")
cat(strrep("-", 78), "\n")

dur <- suppressWarnings(as.numeric(rx_obs$duration))

out$duration_quantiles <- q_row(dur[!is.na(dur) & dur > 0], "duration (valid only)")
show(out$duration_quantiles, "Distribution of usable duration values")

out$duration_top_values <- rx_obs |>
  mutate(duration_num = suppressWarnings(as.numeric(duration))) |>
  filter(!is.na(duration_num) & duration_num > 0) |>
  count(duration_num, name = "n_lines") |>
  arrange(desc(n_lines)) |>
  mutate(pct_of_valid = round(100 * n_lines / sum(n_lines), 2),
         cumulative_pct = round(cumsum(100 * n_lines / sum(n_lines)), 2)) |>
  head(25)
show(out$duration_top_values, "25 most common duration values")

# Pack count is the obvious confounder: if duration really means packs, it
# should track `box` almost exactly.
out$duration_by_box <- rx_obs |>
  mutate(duration_num = suppressWarnings(as.numeric(duration)),
         box_num = suppressWarnings(as.numeric(box))) |>
  filter(!is.na(duration_num) & duration_num > 0 & !is.na(box_num)) |>
  group_by(box_num) |>
  summarise(n_lines = n(),
            median_duration = median(duration_num),
            p25 = unname(quantile(duration_num, 0.25)),
            p75 = unname(quantile(duration_num, 0.75)),
            .groups = "drop") |>
  arrange(box_num) |>
  head(12)
show(out$duration_by_box, "Median duration by number of boxes dispensed")

out$duration_by_frequency <- rx_obs |>
  mutate(duration_num = suppressWarnings(as.numeric(duration))) |>
  group_by(frequency_code) |>
  summarise(n_lines = n(),
            pct_missing_duration = round(
              100 * mean(is.na(duration_num) | duration_num <= 0), 1),
            median_duration = suppressWarnings(
              median(duration_num[!is.na(duration_num) & duration_num > 0])),
            .groups = "drop") |>
  arrange(desc(n_lines)) |>
  head(15)
show(out$duration_by_frequency, "Duration by frequency_code (most common codes)")

# ===========================================================================
# B. How much of the coverage is substituted rather than observed? ----
# ===========================================================================
cat("\n\nB. HOW MUCH IS SUBSTITUTED?\n")
cat(strrep("-", 78), "\n")

rx <- rx_obs |>
  mutate(
    event_date = as.Date(event_date),
    duration_num = suppressWarnings(as.numeric(duration)),
    imputed = is.na(duration_num) | duration_num <= 0,
    day_supply = ifelse(imputed, DEFAULT_DAYS_SUPPLY, duration_num),
    cov_end = event_date + day_supply
  ) |>
  arrange(cohort, person_id, event_date, num_sequence, product_atc_code)

out$imputed_lines <- rx |>
  group_by(cohort) |>
  summarise(n_lines = n(),
            n_imputed = sum(imputed),
            pct_imputed = round(100 * mean(imputed), 1),
            .groups = "drop")
show(out$imputed_lines, "Prescription lines with no usable duration")

out$imputed_patients <- rx |>
  group_by(cohort, person_id) |>
  summarise(all_imputed = all(imputed), any_imputed = any(imputed),
            .groups = "drop") |>
  group_by(cohort) |>
  summarise(n_patients = n(),
            pct_any_imputed = round(100 * mean(any_imputed), 1),
            pct_all_imputed = round(100 * mean(all_imputed), 1),
            .groups = "drop")
show(out$imputed_patients, "Patients affected by substitution")

# ===========================================================================
# C. Why is first-episode duration flat at 30 days? ----
# Two candidate explanations, with very different consequences:
#   (1) the 30-day substitution is doing the work, or
#   (2) most first episodes are a single prescription, in which case the
#       episode length IS the days supply by definition and no days-supply
#       field, however accurate, would produce spread.
# If (2) dominates, persistence is not broken so much as inapplicable to
# single-fill patients, and it can still be reported for the rest.
# ===========================================================================
cat("\n\nC. WHY IS FIRST-EPISODE DURATION FLAT?\n")
cat(strrep("-", 78), "\n")

episodes_lines <- rx |>
  group_by(cohort, person_id) |>
  mutate(
    prev_cov_end = lag(cummax(as.numeric(cov_end))),
    prev_cov_end = as.Date(prev_cov_end, origin = "1970-01-01"),
    new_episode = is.na(prev_cov_end) | event_date > (prev_cov_end + GRACE_DAYS),
    episode_id = cumsum(new_episode)
  ) |>
  ungroup()

first_ep <- episodes_lines |>
  group_by(cohort, person_id, episode_id) |>
  summarise(episode_start = min(event_date),
            episode_duration_days = as.numeric(max(cov_end) - min(event_date)),
            n_rx = n(),
            all_imputed = all(imputed),
            .groups = "drop") |>
  group_by(cohort, person_id) |>
  arrange(episode_start, .by_group = TRUE) |>
  slice(1) |>
  ungroup()

out$first_episode_size <- first_ep |>
  mutate(fills = ifelse(n_rx >= 4, "4+", as.character(n_rx))) |>
  count(cohort, fills, name = "n_patients") |>
  group_by(cohort) |>
  mutate(pct = round(100 * n_patients / sum(n_patients), 1)) |>
  ungroup()
show(out$first_episode_size, "Prescriptions in the first episode")

out$first_episode_duration <- bind_rows(
  first_ep |> group_by(cohort) |>
    group_modify(~ q_row(.x$episode_duration_days, "all patients")),
  first_ep |> filter(n_rx == 1) |> group_by(cohort) |>
    group_modify(~ q_row(.x$episode_duration_days, "single-fill episodes")),
  first_ep |> filter(n_rx >= 2) |> group_by(cohort) |>
    group_modify(~ q_row(.x$episode_duration_days, "2+ fill episodes")),
  first_ep |> filter(!all_imputed) |> group_by(cohort) |>
    group_modify(~ q_row(.x$episode_duration_days, "any observed duration")),
  first_ep |> filter(n_rx >= 2 & !all_imputed) |> group_by(cohort) |>
    group_modify(~ q_row(.x$episode_duration_days, "2+ fills, observed"))
) |>
  mutate(measure = factor(measure, levels = c(
    "all patients", "single-fill episodes", "2+ fill episodes",
    "any observed duration", "2+ fills, observed"))) |>
  arrange(measure, cohort)
show(out$first_episode_duration, "First-episode duration by subgroup")

# ===========================================================================
# D. How far do the headline numbers move with the substituted value? ----
# The question slide 9 and slide 11 actually need answered: is the assumption
# load-bearing? If mean coverage barely shifts between a 7-day and a 60-day
# substitution then the caveat can be softened. If it swings, it cannot.
# "observed only" drops substituted lines entirely rather than guessing.
# ===========================================================================
cat("\n\nD. SENSITIVITY TO THE SUBSTITUTED VALUE\n")
cat(strrep("-", 78), "\n")
cat("Each row re-runs episode construction and fixed-window PDC end to end.\n")

run_scenario <- function(fallback, label, drop_imputed = FALSE) {
  d <- rx
  if (drop_imputed) {
    d <- d |> filter(!imputed)
  } else {
    d <- d |> mutate(day_supply = ifelse(imputed, fallback, duration_num))
  }
  if (!nrow(d)) return(NULL)

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
              .groups = "drop")

  per_patient <- eps |>
    group_by(cohort, person_id) |>
    summarise(n_episodes = n(),
              first_duration = episode_duration_days[which.min(episode_start)],
              .groups = "drop")

  fw <- d |>
    mutate(win_start = as.Date(index_date),
           win_end = pmin(as.Date(censor_date),
                          as.Date(index_date) + FIXED_WINDOW_DAYS)) |>
    group_by(cohort, person_id) |>
    summarise(win_start = first(win_start),
              win_end = first(win_end),
              covered_fw = union_covered_days(event_date, cov_end,
                                              first(win_start), first(win_end)),
              .groups = "drop") |>
    mutate(window_days = pmax(as.numeric(win_end - win_start), 1),
           pdc_fw = pmin(covered_fw / window_days, 1))

  per_patient |>
    left_join(fw, by = c("cohort", "person_id")) |>
    group_by(cohort) |>
    summarise(scenario = label,
              n_patients = n(),
              median_episodes = median(n_episodes),
              median_first_duration = median(first_duration),
              iqr_first_duration = IQR(first_duration),
              mean_pdc = round(mean(pdc_fw, na.rm = TRUE), 3),
              pct_adherent = round(100 * mean(pdc_fw >= 0.80, na.rm = TRUE), 2),
              .groups = "drop")
}

scenarios <- lapply(FALLBACKS, function(f) {
  message("  scenario: ", f, "-day substitution")
  run_scenario(f, paste0(f, "-day substitution"))
})
message("  scenario: observed durations only")
scenarios[[length(scenarios) + 1]] <- run_scenario(NA, "observed only",
                                                   drop_imputed = TRUE)

out$sensitivity <- bind_rows(scenarios) |>
  select(scenario, cohort, n_patients, median_episodes,
         median_first_duration, iqr_first_duration, mean_pdc, pct_adherent)
show(out$sensitivity, "Headline measures under each assumption")
cat("\nThe 30-day substitution row is what the workbook and the deck report.\n")

# ===========================================================================
# E. Is there a better field in rx_detail_raw? ----
# 08_rx.R pulled prescription_detail under tryCatch, so it may not exist.
# ===========================================================================
cat("\n\nE. ALTERNATIVE SOURCE (rx_detail_raw)\n")
cat(strrep("-", 78), "\n")

if (file.exists("data/rx_detail_raw")) {
  detail <- readRDS("data/rx_detail_raw")
  cat("rx_detail_raw found:", nrow(detail), "rows\n")
  cat("Columns:", paste(names(detail), collapse = ", "), "\n")
  out$detail_completeness <- data.frame(
    column = names(detail),
    pct_populated = round(100 * sapply(detail, function(x) mean(!is.na(x))), 1)
  )
  show(out$detail_completeness, "Completeness of each column")
} else {
  cat("data/rx_detail_raw not present - the view was unavailable at pull time.\n")
  cat("No alternative days-supply field to fall back on.\n")
}

cat("\n", strrep("=", 78), "\n")
cat("End of report.\n")
sink()

saveRDS(out, "data/daysupply_check")
print(paste("Days-supply check written to", report))

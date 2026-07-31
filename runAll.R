###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2026-07-30
#
# Program Inputs       : None (drives the full pipeline)
# Program Outputs      : All data/, results/ artefacts of the DE pipeline
#
# Description          : Runs the complete DE Migraine pipeline end to end,
#                        EXCLUDING the referral program (05_cov2.R), which has
#                        no DE data source and is not part of the DE study.
#
#                        Run order and dependencies:
#                          03_match.R        sources 01 + 02 (each sources 00),
#                                            so this builds codelists, both
#                                            cohorts, the 1:1 match, and the
#                                            attrition table -> data/patpop_matched
#                          04_cov1.R         GP visits            -> data/cov1, gp_visit_annual
#                          06_cov3.R         demographics         -> data/cov3
#                          07_figure1.R      figures              -> results/figure1*.png
#                          08_rx.R           N02 Rx counts + extracts
#                          09_rx_patterns.R  N02 treatment patterns (episodes, LoT, adherence)
#                          99_table_output.R presentation workbook -> results/*.xlsx
#                          99_euroboard_appendix.R  codelist appendix workbook
#
#                        05_cov2.R (referrals) is intentionally omitted.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2026-07-30  Ryan Irvine             Full-pipeline driver (no referrals)
# 0.2
# 1.0
################################################################################

# Programs to run, in dependency order. 05_cov2.R is deliberately excluded
# (referral objective has no DE data source). 03 pulls in 00/01/02 internally.
pipeline <- c(
  "03_match.R",
  "04_cov1.R",
  "06_cov3.R",
  "07_figure1.R",
  "08_rx.R",
  "09_rx_patterns.R",
  "99_table_output.R",
  "99_euroboard_appendix.R"
)

run_started <- Sys.time()
message("=====================================================================")
message("DE Migraine pipeline (referral program 05 excluded)")
message("Start: ", format(run_started, "%Y-%m-%d %H:%M:%S"))
message("=====================================================================")

results <- data.frame(
  program = character(0),
  status = character(0),
  seconds = numeric(0),
  stringsAsFactors = FALSE
)

for (prog in pipeline) {
  if (!file.exists(prog)) {
    stop("Pipeline program not found: ", prog,
         " (run runAll.R from the project root).")
  }
  message("\n--- Running ", prog, " ---")
  t0 <- Sys.time()
  ok <- tryCatch(
    {
      # Run in the global env, same as running each program by hand.
      source(prog, local = FALSE)
      TRUE
    },
    error = function(e) {
      message("!! ERROR in ", prog, ": ", conditionMessage(e))
      FALSE
    }
  )
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  results <- rbind(results, data.frame(
    program = prog,
    status = ifelse(ok, "OK", "FAILED"),
    seconds = round(secs, 1),
    stringsAsFactors = FALSE
  ))
  if (!ok) {
    message("\nPipeline halted at ", prog, " after an error. ",
            "Fix the issue and re-run (earlier saved outputs are reusable).")
    break
  }
}

message("\n=====================================================================")
message("Pipeline summary")
message("=====================================================================")
print(results, row.names = FALSE)
message("Total elapsed: ",
        round(as.numeric(difftime(Sys.time(), run_started, units = "mins")), 1),
        " min")

if (all(results$status == "OK") && nrow(results) == length(pipeline)) {
  message("\nAll programs completed. Deliverables written to results/.")
} else {
  message("\nPipeline did NOT complete cleanly — see the summary above.")
}
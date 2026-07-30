###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       : "data/diagnosis_codelist", "data/rx_codelist"
# Program Outputs      : "results/euroboard_migraine_appendix.xlsx"
#
# Description          : Euroboard codelist appendix. The referral codelist is
#                        dropped for DE (no referral data source).
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-17  Ryan Irvine             Conversion from SAS to R
# 0.2       2026-07-20  Ryan Irvine             DE port: drop referral codelist,
#                                               add N02 ATC codelist
# 1.0
################################################################################

# Global ----
source("00_global.R")

# Open output destination ----
wb <- createWorkbook()

# Appendix 1. Diagnosis Codelist ----
addWorksheet(wb, "A1. Diagnosis Codelist")
writeData(wb, "A1. Diagnosis Codelist", readRDS("data/diagnosis_codelist"))

# Appendix 2. N02 Prescription (ATC) Codelist ----
addWorksheet(wb, "A2. N02 ATC Codelist")
writeData(wb, "A2. N02 ATC Codelist", readRDS("data/rx_codelist"))

# Close output destination ----
saveWorkbook(wb, "results/euroboard_migraine_appendix.xlsx", overwrite = TRUE)

###############################################################################
# Study Name           : UK Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       :
# Program Outputs      : "results/raw_results.xlsx"
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

# Global ----
source("R/00_global.R")

# Open output distination ----
wb <- createWorkbook()

# Appendix 1. Diagnosis Codelist ----
addWorksheet(wb, "A1. Diagnosis Codelist")
writeData(wb, "A1. Diagnosis Codelist", readRDS("data/diagnosis_codelist"))

# Appendix 2. Referral Codelist ----
addWorksheet(wb, "A2. Referral Codelist")
writeData(wb, "A2. Referral Codelist", readRDS("data/referral_codelist"))

# Close output destination ----
saveWorkbook(wb, "output/euroboard_migraine_appendix.xlsx", overwrite = TRUE)

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
source("00_global.R")

# Open output distination ----
wb <- createWorkbook()

# Table 1. Patient Attrition Flow ----
addWorksheet(wb, "Table 1. Attrition")
## Format ----
table1_fmt <- readRDS("../data/table1") |>
  mutate(value_fmt = prettyNum(value, big.mark = ",")) |>
  select(-value)
## Output ----
writeData(wb, "Table 1. Attrition", table1_fmt)


# Table 2. Outcomes ----
addWorksheet(wb, "Table 2. Outcomes")
## Format ----
table2 <- readRDS("../data/cov1") |>
  union_all(readRDS("../data/cov2")) |>
  union_all(readRDS("../data/cov3"))
## Output ----
writeData(wb, "Table 2. Outcomes", table2)


# Appendix 1. Diagnosis Codelist ----
addWorksheet(wb, "A1. Diagnosis Codelist")
writeData(wb, "A1. Diagnosis Codelist", readRDS("../data/diagnosis_codelist"))


# Appendix 2. Referral Codelist ----
addWorksheet(wb, "A2. Referral Codelist")
writeData(wb, "A2. Referral Codelist", readRDS("../data/referral_codelist"))

# Appendix 3. Full First Specialist List
addWorksheet(wb, "A3. Full Specialist Listing")
writeData(wb, "A3. Full Specialist Listing", readRDS("../data/cov2_3_full"))


# Close output destination ----
saveWorkbook(wb, "../output/raw_results.xlsx", overwrite = TRUE)

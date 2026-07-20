###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-14
#
# Program Inputs       : None
# Program Outputs      : "data/diagnosis_codelist", "data/rx_codelist"
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-14  Ryan Irvine             Conversion from SAS to R
# 0.2
# 1.0
################################################################################

# Load packages required for study ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(dbplyr)
  library(janitor)
  library(glue)
  library(openxlsx)
  library(DBI)
  library(MatchIt)
  library(gt)
  library(openxlsx)
  library(labelled)
  library(writexl)
})

# Establish Snowflake Connection ----
if (!exists("con")) {
  readRenviron(
    "/organon/projects/or_analytics/irvinery/snowflake_passkey.Renviron"
  )
  con <- dbConnect(
    odbc::odbc(),
    Driver = "snowflake",
    UID = Sys.getenv("SNOWFLAKE_USER"),
    PWD = Sys.getenv("SNOWFLAKE_TOKEN"),
    Database = "ORD_IDTM",
    Server = Sys.getenv("SNOWFLAKE_SERVER"),
    Warehouse = "ORD_IDMT_WH"
  )
  print("Snowflake connection has been established.")
} else {
  print("Snowflake connect already established.")
}

# Global variables ----
StartDate <- as.Date("2016-12-01")
StartDate_sql <- "TO_DATE('2016-12-01')"
data_path <- "data"
rawresults_path <- "rawresults"
results_path <- "results"

# Helpter functions ----
source_all <- function(folder_path, pattern = "\\.R$") {
  # List all files in the folder that match the pattern (default: .R files)
  files <- list.files(folder_path, pattern = pattern, full.names = TRUE)

  # Loop through and source each file
  for (f in files) {
    message("Sourcing: ", f)
    source(f)
  }
}

source_all("functions")

# Load data from Snowflake ----
# NOTE (DE port): view names carry the DE country token (V_DE_*) in the same
# schema as the UK study. Verify each name against information_schema on the
# analysis machine before the first run.
codelist <- tbl(con, I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_CODELIST")) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
codelist_translate <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_CODELIST_TRANSLATE")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  ) |> 
  filter(language_code == "en")
person <- tbl(con, I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_PERSON")) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
contact_diagnostics <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_CONTACT_DIAGNOSTICS")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
contact <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_CONTACT")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
# Prescription source for the DE-specific N02 objective (08_rx.R).
contact_prescriptions <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_CONTACT_PRESCRIPTIONS")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
product <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_DE_PRODUCT")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )


# Create study codelist(s) ----
## Diagnosis Codelist ----
# DE port: codes are ICD-10 (CIM-10) natively, so the UK Read-code hardcoding
# (INUK.* -> "Headache"/"R51") is dropped. `code` holds the ICD-10 code and
# `code_group` its chapter grouping. The G43/G44/R51 code_group logic and the
# label exclusions carry over from the UK program.
# NOTE: confirm the DE list_code value for the ICD-10 group. The DE data
# dictionary describes list_code values including "cim10_code"; verify against
# the codelist on the analysis machine (UK used "diagnostic_code").
diagnosis_codelist <- codelist_translate |>
  mutate(label = tolower(label)) |>
  left_join(codelist |> select(code, code_group), by = "code") |> 
  filter(
    list_code == "diagnostic_code",
    (
      # keep NA code_group if label matches
      str_detect(coalesce(code_group, ""), "G43") |
        str_detect(coalesce(code_group, ""), "G44") |
        str_detect(coalesce(code_group, ""), "R51") |
        str_detect(label, "migraine") |
        str_detect(label, "headache") |
        str_detect(label, "kopfschmerz") |
        str_detect(label, "migräne")
    ),
    !str_detect(label, "history"),
    !str_detect(label, "h/o"),
    !str_detect(label, "family"),
    !str_detect(label, "refer"),
    !str_detect(label, "no"),
    !str_detect(label, "fh"),
    !str_detect(label, "seen"),
    !str_detect(label, "dna"),
    !str_detect(label, "contraceptive"),
    !str_detect(label, "abdominal"),
    !str_detect(label, "viral"),
    !str_detect(label, "prophylaxis"),
    !str_detect(coalesce(code_group, ""), "O"),
    !str_detect(coalesce(code_group, ""), "G97"),
    !str_detect(coalesce(code_group, ""), "F"),
    !str_detect(coalesce(code_group, ""), "N"),
    !str_detect(coalesce(code_group, ""), "T")
  ) %>%
  select(code, label, code_group) |>
  rename(
    icd10_label = label,
    icd10_code = code_group
  ) |>
  mutate(label_fmt = paste0(icd10_code, ": ", icd10_label)) |>
  select(-icd10_label, -icd10_code) |>
  collect()
saveRDS(diagnosis_codelist, file = "data/diagnosis_codelist")
print("diagnosis_codelist has been created and saved to data directory.")

## Prescription (ATC) codelist ----
# DE-specific objective: count prescriptions in ATC group N02 (analgesics),
# with particular interest in N02C (antimigraine preparations). The list is
# built from the product master (product_atc_code), one row per distinct ATC
# code under N02, so 08_rx.R can label counts with the drug name.
rx_codelist <- product |>
  mutate(product_atc_code = toupper(product_atc_code)) |>
  filter(str_detect(coalesce(product_atc_code, ""), "^N02")) |>
  mutate(
    atc_subgroup = substr(product_atc_code, 1, 4),
    is_antimigraine = ifelse(atc_subgroup == "N02C", 1L, 0L)
  ) |>
  select(
    product_id,
    product_atc_code,
    atc_subgroup,
    is_antimigraine,
    short_name,
    long_name,
    product_molecule_code
  ) |>
  collect()
saveRDS(rx_codelist, "data/rx_codelist")
print("rx_codelist has been created and saved to data directory.")

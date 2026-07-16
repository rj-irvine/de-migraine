###############################################################################
# Study Name           : UK Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-14
#
# Program Inputs       : None
# Program Outputs      : "data/diagnosis_codelist", "data/referral_codelist"
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
data_path <- "../data"
output_path <- "../output"
results_path <- "../results"

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
codelist <- tbl(con, I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_CODELIST")) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
codelist_translate <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_CODELIST")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
person <- tbl(con, I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_PERSON")) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
contact_diagnostics <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_CONTACT_DIAGNOSTICS")
) |>
  rename_all(tolower) |>
  select(
    -job_id,
    -created_date,
    -updated_date,
    -status_code
  )
referral <- tbl(
  con,
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_UK_REFERRAL")
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
  I("ORD_IDMT.ORD_CEGEDIM_PUB.V_UK_CONTACT")
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
diagnosis_codelist <- codelist |>
  mutate(label = tolower(label)) |>
  filter(
    list_code == "diagnostic_code",
    (
      # keep NA code_group if label matches
      str_detect(coalesce(code_group, ""), "G43") |
        str_detect(coalesce(code_group, ""), "G44") |
        str_detect(coalesce(code_group, ""), "R51") |
        str_detect(label, "migraine") |
        str_detect(label, "headache")
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
  mutate(
    icd10_label = ifelse(
      code %in%
        c(
          "INUK.1B1G.00",
          "INUK.1B1G.11",
          "INUK.1BA..00",
          "INUK.1BB..00",
          "INUK.1BA9.00",
          "INUK.1B1G000",
          "INUK.1BA2.00",
          "INUK.1BA3.00",
          "INUK.1BA4.00",
          "INUK.1BA5.00",
          "INUK.1BA6.00",
          "INUK.1BA7.00",
          "INUK.1BA8.00",
          "INUK.1BB1.00",
          "INUK.1BB2.00",
          "INUK.1BB3.00",
          "INUK.1BB4.00",
          "INUK.E278200",
          "INUK.E278111",
          "INUK.E278100",
          "INUK.F261100",
          "INUK.R040z11",
          "INUK.R040011",
          "INUK.R040000",
          "INUK.R040.00"
        ),
      "Headache",
      icd10_label
    ),
    icd10_code = ifelse(
      code %in%
        c(
          "INUK.1B1G.00",
          "INUK.1B1G.11",
          "INUK.1BA..00",
          "INUK.1BB..00",
          "INUK.1BA9.00",
          "INUK.1B1G000",
          "INUK.1BA2.00",
          "INUK.1BA3.00",
          "INUK.1BA4.00",
          "INUK.1BA5.00",
          "INUK.1BA6.00",
          "INUK.1BA7.00",
          "INUK.1BA8.00",
          "INUK.1BB1.00",
          "INUK.1BB2.00",
          "INUK.1BB3.00",
          "INUK.1BB4.00",
          "INUK.E278200",
          "INUK.E278111",
          "INUK.E278100",
          "INUK.F261100",
          "INUK.R040z11",
          "INUK.R040011",
          "INUK.R040000",
          "INUK.R040.00"
        ),
      "R51",
      icd10_code
    )
  ) |>
  mutate(label_fmt = paste0(icd10_code, ": ", icd10_label)) |>
  select(-icd10_label, -icd10_code) |>
  collect()
saveRDS(diagnosis_codelist, file = "../data/diagnosis_codelist")
print("diagnosis_codelist has been created and saved to data directory.")

## Referral codelist ----
referral_codelist <- codelist |>
  filter(list_code == "diagnostic_code") |>
  select(code, label) |>
  filter(
    str_detect(tolower(label), "refer") &
      !str_detect(tolower(label), "child") &
      !str_detect(tolower(label), "decline") &
      !str_detect(tolower(label), "defer") &
      !str_detect(tolower(label), "prefer") &
      !str_detect(tolower(label), "non-refer") &
      !str_detect(tolower(label), "exam.") &
      !str_detect(tolower(label), "reference") &
      !str_detect(tolower(label), "fh")
  ) |>
  mutate(
    specialty = case_when(
      # Neurology
      grepl(
        "neurolog|\\bneuro\\b|neurosurg|parkinson|epileps|headache|clinical neurophysiolog",
        label,
        TRUE
      ) ~ "Neurology",

      # Cardiology
      grepl(
        "cardiolog|cardiac|heart|atrial fibrillation|device service|echocardiogram|electrocardiogram|angina|cardiothoracic",
        label,
        TRUE
      ) ~ "Cardiology",

      # Oncology
      grepl(
        "oncolog|cancer|sarcoma|radiotherap|chemotherap|haematology malignancy",
        label,
        TRUE
      ) ~ "Oncology",

      # Psychiatry / Mental Health
      grepl(
        "psychiatr|psycholog|mental|psychosis|cognitive behavioural|iapt|psychotherapist|psychosexual|forensic psychiatrist|psychogeriatric",
        label,
        TRUE
      ) ~ "Psychiatry / Mental Health",

      # Surgery
      grepl(
        "surgeon|surgical|surgery|maxillofacial|colorectal|vascular|thoracic|hand surgeon|bariatric|orthopaedic triage",
        label,
        TRUE
      ) ~ "Surgery",

      # Nursing
      grepl("nurse|midwife|matron|stoma", label, TRUE) ~ "Nursing",

      # Paediatrics
      grepl(
        "paediatr|neonatolog|school nurse|child",
        label,
        TRUE
      ) ~ "Paediatrics",

      # Obstetrics & Gynaecology
      grepl(
        "gynaecolog|obstetric|u\\s*gynaecolog|family planning|antenatal|postnatal|fertilit|female sterilisation|hysteroscopy",
        label,
        TRUE
      ) ~ "Obstetrics & Gynaecology",

      # Dermatology
      grepl(
        "dermatolog|eczema|skin|teledermatology|patch skin test",
        label,
        TRUE
      ) ~ "Dermatology",

      # Radiology
      grepl(
        "radiolog|radiograph|imaging|interventional radiology|radiographer|dxa",
        label,
        TRUE
      ) ~ "Radiology",

      # Respiratory
      grepl(
        "respiratory|chest|lung|spirom|bronchoscopy|pulmonary|tb|copd|sleep clinic",
        label,
        TRUE
      ) ~ "Respiratory",

      # Endocrinology
      grepl(
        "endocrinolog|diabet|thyroid|lipid|hypercholesterolaemia|glucose",
        label,
        TRUE
      ) ~ "Endocrinology",

      # Gastroenterology
      grepl(
        "gastroenterolog|dyspeps|colonoscopy|endosc|sigmoidoscopy|rectal|\\bgi\\b",
        label,
        TRUE
      ) ~ "Gastroenterology",

      # Hepatology
      grepl("hepat|liver", label, TRUE) ~ "Hepatology",

      # Urology
      grepl("urolog|haematuria", label, TRUE) ~ "Urology",

      # Orthopaedics
      grepl(
        "orthopaed|musculoskeletal|bone|fracture|osteoporosis|varicose vein|falls service|bone density",
        label,
        TRUE
      ) ~ "Orthopaedics",

      # Genetics
      grepl(
        "genetic|geneticist|cytogenetic|molecular genetic",
        label,
        TRUE
      ) ~ "Genetics",

      # Haematology
      grepl(
        "haematolog|blood pressure monitoring",
        label,
        TRUE
      ) ~ "Haematology",

      # Nephrology
      grepl("renal|nephrolog|kidney", label, TRUE) ~ "Nephrology",

      # Ophthalmology
      grepl(
        "ophthalmolog|eye|glaucoma|diabetic eye|optometrist|ophthalmologist|orthoptist",
        label,
        TRUE
      ) ~ "Ophthalmology",

      # ENT
      grepl("\\bent\\b|ear|nose|throat", label, TRUE) ~ "ENT",

      # Audiology
      grepl(
        "audiolog|hearing aid|hearing therapist",
        label,
        TRUE
      ) ~ "Audiology",

      # Dentistry
      grepl(
        "dent(al)?|orthodont|oral (dentist|surgery)|prosthodont|periodont",
        label,
        TRUE
      ) ~ "Dentistry",

      # Dietetics / Nutrition
      grepl(
        "dietiti|nutrition|obesity|weight management|dietician",
        label,
        TRUE
      ) ~ "Dietetics / Nutrition",

      # Therapy & Rehab (Physio / OT / SLT)
      grepl(
        "physiotherap|rehabilitation|occupational therapist|orthotist|speech and language|macmillan physiotherapist|wheelchair",
        label,
        TRUE
      ) ~ "Physiotherapy / Rehabilitation",

      # Social Care / Counselling
      grepl(
        "social services|social worker|safeguarding|carer|housing|domestic violence|social prescribing|community navigator|family nurse partnership|age uk|alzheimer",
        label,
        TRUE
      ) ~ "Social Care / Counselling",

      TRUE ~ "Other / General"
    ),
    neuro_ref = as.character(ifelse(
      str_detect(tolower(label), "neuro"),
      1,
      2
    ))
  ) |>
  collect()

saveRDS(referral_codelist, "../data/referral_codelist")
print("referral_codelise has been created and saved to data directory.")

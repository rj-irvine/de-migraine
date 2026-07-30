###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       : "data/table1", "data/patpop_matched", "data/cov1",
#                        "data/cov3", "data/cov4", "data/diagnosis_codelist",
#                        "data/rx_codelist",
#                        "results/figure1a_gp_visits_hist.png" (optional)
# Program Outputs      : "results/DE_Migraine_Headache_Results.xlsx"
#
# Description          : Presentation-ready workbook, styled to match the UK
#                        deliverable (V2_UK_Migraine_Headache_Results.xlsx):
#                        a Table of Contents, content offset to B2, cohort-count
#                        column headers, indented sub-rows, and footnotes.
#                        DE differences from UK: referral tables/appendices are
#                        removed (no referral data source), and the N02
#                        prescription objective is added.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-17  Ryan Irvine             Conversion from SAS to R
# 0.2       2026-07-17  Ryan Irvine             DE port: attrition + N02 Rx
# 0.3       2026-07-30  Ryan Irvine             Restyle to match UK deliverable
# 1.0
################################################################################

# Global ----
source("00_global.R")

# ---------------------------------------------------------------------------
# Layout constants (match the UK deliverable) ----
# ---------------------------------------------------------------------------
COL0 <- 2 # content starts in column B; column A is a left margin
ROW0 <- 2 # content starts in row 2; row 1 is a top margin

# Palette
NAVY <- "#1F3864" # title bar
BLUE <- "#2E5496" # header row
BAND <- "#EAF0F8" # zebra banding
RULE <- "#BFBFBF" # borders
GREY <- "#F2F2F2" # TOC header

# ---------------------------------------------------------------------------
# Shared styles ----
# ---------------------------------------------------------------------------
st_title <- createStyle(
  fontSize = 13, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = NAVY,
  halign = "left", valign = "center"
)
st_header <- createStyle(
  textDecoration = "bold", fontColour = "#FFFFFF", fgFill = BLUE,
  halign = "center", valign = "center", wrapText = TRUE,
  border = "TopBottomLeftRight", borderColour = NAVY
)
st_header_left <- createStyle(
  textDecoration = "bold", fontColour = "#FFFFFF", fgFill = BLUE,
  halign = "left", valign = "center", wrapText = TRUE,
  border = "TopBottomLeftRight", borderColour = NAVY
)
st_body <- createStyle(
  valign = "top", wrapText = TRUE,
  border = "bottom", borderColour = RULE
)
st_body_band <- createStyle(
  valign = "top", wrapText = TRUE, fgFill = BAND,
  border = "bottom", borderColour = RULE
)
st_num <- createStyle(
  numFmt = "#,##0", halign = "right", valign = "top",
  border = "bottom", borderColour = RULE
)
st_num_band <- createStyle(
  numFmt = "#,##0", halign = "right", valign = "top", fgFill = BAND,
  border = "bottom", borderColour = RULE
)
st_center <- createStyle(halign = "center", valign = "top")
st_objective <- createStyle(textDecoration = "bold", valign = "center")
st_footnote <- createStyle(fontSize = 9, textDecoration = "italic",
                           fontColour = "#595959", valign = "top",
                           wrapText = TRUE)

# ---------------------------------------------------------------------------
# Helper: write one titled, styled table onto a worksheet ----
#   headers      : character vector of column header labels (length = ncol(df))
#   num_cols     : names(df) to render as right-aligned thousands integers
#   center_cols  : names(df) to centre (e.g. code columns, value columns)
#   objective_col: name of the column whose section-title rows (rows where the
#                  other value columns are NA) should render bold, un-indented
#   col_widths   : numeric widths for each column
#   returns the row index of the last written data row (for footnotes)
# ---------------------------------------------------------------------------
write_styled_table <- function(wb, sheet, title, df,
                               headers = names(df),
                               num_cols = NULL, center_cols = NULL,
                               col_widths = NULL, header_height = 18) {
  addWorksheet(wb, sheet, gridLines = FALSE)
  showGridLines(wb, sheet, showGridLines = FALSE)

  n_col <- ncol(df)
  c0 <- COL0
  c1 <- COL0 + n_col - 1

  # Title bar
  title_row <- ROW0
  writeData(wb, sheet, title, startRow = title_row, startCol = c0)
  mergeCells(wb, sheet, cols = c0:c1, rows = title_row)
  addStyle(wb, sheet, st_title, rows = title_row, cols = c0:c1, gridExpand = TRUE)
  setRowHeights(wb, sheet, rows = title_row, heights = 24)

  # Header row (one blank spacer row under the title)
  header_row <- title_row + 2
  for (j in seq_len(n_col)) {
    writeData(wb, sheet, headers[j], startRow = header_row, startCol = c0 + j - 1)
  }
  # First column header left-aligned, the rest centred.
  addStyle(wb, sheet, st_header_left, rows = header_row, cols = c0, gridExpand = TRUE)
  if (n_col > 1) {
    addStyle(wb, sheet, st_header, rows = header_row,
             cols = (c0 + 1):c1, gridExpand = TRUE)
  }
  setRowHeights(wb, sheet, rows = header_row, heights = header_height)

  # Body
  first_data <- header_row + 1
  writeData(wb, sheet, df, startRow = first_data, startCol = c0,
            colNames = FALSE, borders = "none")
  data_rows <- first_data:(first_data + nrow(df) - 1)

  for (k in seq_along(data_rows)) {
    r <- data_rows[k]
    banded <- (k %% 2 == 0)
    addStyle(wb, sheet, if (banded) st_body_band else st_body,
             rows = r, cols = c0:c1, gridExpand = TRUE, stack = TRUE)
  }

  # Numeric columns
  if (!is.null(num_cols)) {
    for (nm in intersect(num_cols, names(df))) {
      ci <- c0 + match(nm, names(df)) - 1
      for (k in seq_along(data_rows)) {
        addStyle(wb, sheet, if (k %% 2 == 0) st_num_band else st_num,
                 rows = data_rows[k], cols = ci, stack = TRUE)
      }
    }
  }
  # Centred columns
  if (!is.null(center_cols)) {
    for (nm in intersect(center_cols, names(df))) {
      ci <- c0 + match(nm, names(df)) - 1
      addStyle(wb, sheet, st_center, rows = data_rows, cols = ci,
               gridExpand = TRUE, stack = TRUE)
    }
  }

  # Widths
  if (!is.null(col_widths)) {
    setColWidths(wb, sheet, cols = c0:c1, widths = col_widths)
  } else {
    setColWidths(wb, sheet, cols = c0:c1, widths = "auto")
  }
  # Column A margin
  setColWidths(wb, sheet, cols = 1, widths = 2.5)

  freezePane(wb, sheet, firstActiveRow = first_data)
  invisible(max(data_rows))
}

# Bold the objective/section-title rows of an outcomes table: rows whose value
# columns are all NA are section headers (e.g. "To assess ..."), which we render
# bold and un-banded so the table reads as grouped sections like the UK file.
style_objective_rows <- function(wb, sheet, df, value_cols, first_data_row) {
  is_obj <- apply(df[value_cols], 1, function(x) all(is.na(x)))
  obj_rows <- which(is_obj)
  for (i in obj_rows) {
    r <- first_data_row + i - 1
    addStyle(wb, sheet, st_objective, rows = r,
             cols = COL0:(COL0 + ncol(df) - 1), gridExpand = TRUE, stack = TRUE)
  }
}

# ---------------------------------------------------------------------------
# Load data + derive cohort headers ----
# ---------------------------------------------------------------------------
patpop_matched <- readRDS("data/patpop_matched")
n_case <- length(unique(patpop_matched$person_id_case))
n_control <- length(unique(patpop_matched$person_id_control))

hdr_case <- paste0("Cohort 1\nHeadache Disorder\nN = ",
                   prettyNum(n_case, big.mark = ","))
hdr_control <- paste0("Cohort 2\nNo Headache Disorder\nN = ",
                      prettyNum(n_control, big.mark = ","))

# ---------------------------------------------------------------------------
# Open workbook ----
# ---------------------------------------------------------------------------
wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)

# ---------------------------------------------------------------------------
# Table of Contents ----
# ---------------------------------------------------------------------------
toc <- data.frame(
  Item = c(
    "Table 1. Attrition Flow",
    "Table 2. Outcome Variables",
    "Table 3. N02 Prescriptions",
    "Figure 1. Annualized GP Visits",
    "Appendix 1. Diagnosis Codelist",
    "Appendix 2. N02 ATC Codelist"
  ),
  Tab = c(
    "T1. Attrition Flow",
    "T2. Outcome Variables",
    "T3. N02 Prescriptions",
    "F1. Annualized GP Visits",
    "A1. Diagnosis Codelist",
    "A2. N02 ATC Codelist"
  ),
  Description = c(
    "Patient selection / attrition flow",
    "Continuous and discrete outcome measures (GP visits, demographics)",
    "N02 (analgesic) prescription counts, incl. N02C antimigraine",
    "Distribution of annualized all-cause GP visits by cohort",
    "Codelist used to identify headache disorder patients (ICD-10)",
    "N02 (analgesic) products used for the prescription objective (ATC)"
  ),
  Notes = c("", "", "DE-specific objective (not in UK study)", "", "", ""),
  stringsAsFactors = FALSE
)

write_styled_table(
  wb, "Table of Contents",
  title = "DE Migraine / Headache Study (25P01) — Results",
  df = toc,
  headers = c("Item", "Tab", "Description", "Notes"),
  col_widths = c(34, 26, 62, 40),
  header_height = 18
)

# ---------------------------------------------------------------------------
# Table 1. Attrition Flow ----
# ---------------------------------------------------------------------------
table1_fmt <- readRDS("data/table1") |>
  mutate(value = prettyNum(value, big.mark = ",")) |>
  rename(Criteria = label, N = value)

last1 <- write_styled_table(
  wb, "T1. Attrition Flow",
  title = "Table 1. Headache Disorder Patient Attrition",
  df = table1_fmt,
  headers = c("Criteria", "N"),
  center_cols = "N",
  col_widths = c(88, 16)
)

# Matching footnote (as in the UK deliverable)
foot_row <- last1 + 2
writeData(
  wb, "T1. Attrition Flow",
  paste0("Headache disorder and non-headache disorder patients are matched ",
         "on: gender, care site, and year of birth (± 2 years)."),
  startRow = foot_row, startCol = COL0
)
mergeCells(wb, "T1. Attrition Flow", cols = COL0:(COL0 + 1), rows = foot_row)
addStyle(wb, "T1. Attrition Flow", st_footnote, rows = foot_row,
         cols = COL0:(COL0 + 1), gridExpand = TRUE)
setRowHeights(wb, "T1. Attrition Flow", rows = foot_row, heights = 28)

# ---------------------------------------------------------------------------
# Table 2. Outcome Variables (GP visits + demographics) ----
# Referral outcomes (cov2) excluded for DE.
# ---------------------------------------------------------------------------
table2 <- readRDS("data/cov1") |>
  union_all(readRDS("data/cov3")) |>
  rename(`Outcome Variable` = name, case = case, control = control)

# First data row: title at ROW0, blank spacer, header at ROW0+2, data at ROW0+3.
fd2 <- ROW0 + 3
write_styled_table(
  wb, "T2. Outcome Variables",
  title = "Table 2. Outcome Variables for the Evaluation of Research Objectives",
  df = table2,
  headers = c("Outcome Variable", hdr_case, hdr_control),
  center_cols = c("case", "control"),
  col_widths = c(62, 26, 26),
  header_height = 46
)
style_objective_rows(wb, "T2. Outcome Variables", table2,
                     value_cols = c("case", "control"), first_data_row = fd2)

# ---------------------------------------------------------------------------
# Table 3. N02 Prescription Counts (DE-specific) ----
# ---------------------------------------------------------------------------
cov4 <- readRDS("data/cov4") |>
  rename(`Outcome Variable` = name, case = case, control = control)

write_styled_table(
  wb, "T3. N02 Prescriptions",
  title = "Table 3. N02 (Analgesic) Prescription & Treatment Patterns, incl. N02C Antimigraine",
  df = cov4,
  headers = c("Outcome Variable", hdr_case, hdr_control),
  center_cols = c("case", "control"),
  col_widths = c(58, 26, 26),
  header_height = 46
)
style_objective_rows(wb, "T3. N02 Prescriptions", cov4,
                     value_cols = c("case", "control"), first_data_row = fd2)

# ---------------------------------------------------------------------------
# Figure 1. Annualized GP Visits ----
# Embeds the PNG saved by 07_figure1.R if present.
# ---------------------------------------------------------------------------
addWorksheet(wb, "F1. Annualized GP Visits", gridLines = FALSE)
showGridLines(wb, "F1. Annualized GP Visits", showGridLines = FALSE)
writeData(wb, "F1. Annualized GP Visits",
          "Figure 1. Distribution of Annualized GP Visits by Cohort",
          startRow = ROW0, startCol = COL0)
mergeCells(wb, "F1. Annualized GP Visits", cols = COL0:(COL0 + 6), rows = ROW0)
addStyle(wb, "F1. Annualized GP Visits", st_title, rows = ROW0,
         cols = COL0:(COL0 + 6), gridExpand = TRUE)
setRowHeights(wb, "F1. Annualized GP Visits", rows = ROW0, heights = 24)
setColWidths(wb, "F1. Annualized GP Visits", cols = 1, widths = 2.5)

# Embed both panels saved by 07_figure1.R (histogram, then jitter), stacking
# them vertically. Missing PNGs fall back to a note so the sheet is never blank.
figs <- c(
  "results/figure1a_gp_visits_hist.png",
  "results/figure1b_gp_visits_jitter.png"
)
img_row <- ROW0 + 2
for (fp in figs) {
  if (file.exists(fp)) {
    insertImage(wb, "F1. Annualized GP Visits", fp,
                startRow = img_row, startCol = COL0,
                width = 8, height = 5, units = "in")
    img_row <- img_row + 26 # advance ~one 5in image height in rows
  } else {
    writeData(wb, "F1. Annualized GP Visits",
              paste0("[Figure not found: run 07_figure1.R to generate ", fp, "]"),
              startRow = img_row, startCol = COL0)
    img_row <- img_row + 2
  }
}

# ---------------------------------------------------------------------------
# Appendix 1. Diagnosis Codelist ----
# ---------------------------------------------------------------------------
diag_cl <- readRDS("data/diagnosis_codelist")
last_a1 <- write_styled_table(
  wb, "A1. Diagnosis Codelist",
  title = "Appendix 1. Headache Disorder Diagnosis Codelist (ICD-10)",
  df = diag_cl,
  center_cols = intersect(c("code"), names(diag_cl)),
  col_widths = NULL
)
addFilter(wb, "A1. Diagnosis Codelist",
          rows = ROW0 + 2, cols = COL0:(COL0 + ncol(diag_cl) - 1))

# ---------------------------------------------------------------------------
# Appendix 2. N02 Prescription (ATC) Codelist ----
# ---------------------------------------------------------------------------
rx_cl <- readRDS("data/rx_codelist")
write_styled_table(
  wb, "A2. N02 ATC Codelist",
  title = "Appendix 2. N02 (Analgesic) Prescription Products (ATC)",
  df = rx_cl,
  center_cols = intersect(c("product_id", "product_atc_code", "atc_subgroup",
                            "is_antimigraine"), names(rx_cl)),
  col_widths = NULL
)
addFilter(wb, "A2. N02 ATC Codelist",
          rows = ROW0 + 2, cols = COL0:(COL0 + ncol(rx_cl) - 1))

# ---------------------------------------------------------------------------
# Save ----
# ---------------------------------------------------------------------------
saveWorkbook(wb, "results/DE_Migraine_Headache_Results.xlsx", overwrite = TRUE)
print("DE_Migraine_Headache_Results.xlsx has been written to the results directory.")

###############################################################################
# Study Name           : DE Migraine
# Study ID             : 25P01
# Study Folder Path    : /organon/projects/or_analytics/irvinery/01_projects/
#                          25P01_THIN_Migraine_Headache/
# Lead Investigator    : Paula Chu, OR
# Lead Programmer      : Ryan Irvine, CDS
# Date of Creation     : 2025-11-17
#
# Program Inputs       : "data/table1", "data/cov4", "data/diagnosis_codelist",
#                        "data/rx_codelist"
# Program Outputs      : "results/de_migraine_tables.xlsx"
#
# Description          : Assembles the presentation-ready workbook. This DE
#                        deliverable covers the attrition table and the new N02
#                        prescription objective; the UK covariate/referral
#                        sheets are added back as those programs are ported.
#
###############################################################################
#                          REVISION / VERSION HISTORY                         #
###############################################################################
# Version   Date        Author                  Description
# -------   ----------  ---------------------   ------------------------------
# 0.1       2025-11-17  Ryan Irvine             Conversion from SAS to R
# 0.2       2026-07-17  Ryan Irvine             DE port: attrition + N02 Rx,
#                                               styled output to results/
# 1.0
################################################################################

# Global ----
source("00_global.R")

# Shared styles ----
title_style <- createStyle(
  fontSize = 14,
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#1F4E79",
  halign = "left",
  valign = "center"
)
header_style <- createStyle(
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#2E75B6",
  halign = "left",
  valign = "center",
  border = "TopBottom",
  borderColour = "#1F4E79"
)
body_style <- createStyle(
  valign = "center",
  border = "bottom",
  borderColour = "#D9D9D9"
)
num_style <- createStyle(
  numFmt = "#,##0",
  halign = "right",
  valign = "center",
  border = "bottom",
  borderColour = "#D9D9D9"
)

# Helper: write one titled, styled table onto a worksheet ----
# Lays out a merged title row, a styled header row, banded body rows, sized
# columns, a frozen header, and an autofilter. `num_cols` are right-aligned with
# a thousands separator.
write_styled_table <- function(wb, sheet, title, df, num_cols = NULL,
                               col_widths = NULL) {
  addWorksheet(wb, sheet, gridLines = FALSE)

  n_col <- ncol(df)
  n_row <- nrow(df)

  # Title row (row 1), merged across the table width.
  writeData(wb, sheet, title, startRow = 1, startCol = 1)
  mergeCells(wb, sheet, cols = 1:n_col, rows = 1)
  addStyle(wb, sheet, title_style, rows = 1, cols = 1:n_col, gridExpand = TRUE)
  setRowHeights(wb, sheet, rows = 1, heights = 22)

  # Header + data starting row 3 (row 2 left blank as a spacer).
  header_row <- 3
  writeData(
    wb, sheet, df,
    startRow = header_row, startCol = 1,
    headerStyle = header_style,
    borders = "none"
  )

  data_rows <- (header_row + 1):(header_row + n_row)

  # Banded rows for readability.
  addStyle(wb, sheet, body_style, rows = data_rows, cols = 1:n_col,
           gridExpand = TRUE, stack = TRUE)
  band_style <- createStyle(fgFill = "#F2F6FB")
  band_rows <- data_rows[seq_along(data_rows) %% 2 == 0]
  if (length(band_rows) > 0) {
    addStyle(wb, sheet, band_style, rows = band_rows, cols = 1:n_col,
             gridExpand = TRUE, stack = TRUE)
  }

  # Numeric columns: thousands separator, right aligned.
  if (!is.null(num_cols)) {
    num_idx <- match(num_cols, names(df))
    num_idx <- num_idx[!is.na(num_idx)]
    for (ci in num_idx) {
      addStyle(wb, sheet, num_style, rows = data_rows, cols = ci,
               gridExpand = TRUE, stack = TRUE)
    }
  }

  # Column widths: caller-supplied, else auto-fit with a sensible cap so a long
  # free-text column (e.g. a code label) does not blow out the sheet.
  if (is.null(col_widths)) {
    setColWidths(wb, sheet, cols = 1:n_col, widths = "auto")
  } else {
    setColWidths(wb, sheet, cols = seq_along(col_widths), widths = col_widths)
  }

  # Freeze the header and enable filtering on the data block.
  freezePane(wb, sheet, firstActiveRow = header_row + 1)
  addFilter(wb, sheet, rows = header_row, cols = 1:n_col)
}

# Open workbook ----
wb <- createWorkbook()
modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)

# Table 1. Patient Attrition Flow ----
table1_fmt <- readRDS("data/table1") |>
  rename(Step = label, N = value)

write_styled_table(
  wb,
  sheet = "Table 1. Attrition",
  title = "Table 1. Patient Attrition Flow — DE Migraine (Study 25P01)",
  df = table1_fmt,
  num_cols = "N",
  col_widths = c(95, 14)
)

# Table 2. N02 Prescription Counts (DE-specific objective) ----
cov4 <- readRDS("data/cov4") |>
  rename(Measure = name, Case = case, Control = control)

write_styled_table(
  wb,
  sheet = "Table 2. N02 Prescriptions",
  title = "Table 2. N02 (Analgesic) Prescription Counts, incl. N02C Antimigraine",
  df = cov4,
  col_widths = c(58, 30, 30)
)

# Appendix 1. Diagnosis Codelist ----
diag_cl <- readRDS("data/diagnosis_codelist")
write_styled_table(
  wb,
  sheet = "A1. Diagnosis Codelist",
  title = "Appendix 1. Headache Disorder Diagnosis Codelist (ICD-10)",
  df = diag_cl
)

# Appendix 2. N02 Prescription (ATC) Codelist ----
rx_cl <- readRDS("data/rx_codelist")
write_styled_table(
  wb,
  sheet = "A2. N02 ATC Codelist",
  title = "Appendix 2. N02 Prescription Products (ATC)",
  df = rx_cl
)

# Save workbook ----
saveWorkbook(wb, "results/de_migraine_tables.xlsx", overwrite = TRUE)
print("de_migraine_tables.xlsx has been written to the results directory.")

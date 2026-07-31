# DE Migraine / Headache Study (25P01)

A retrospective, 1:1 matched cohort study of headache-disorder patients versus
matched controls in the German (DE) Cegedim/THIN primary-care EHR data. It is a
port of a completed UK study, with two differences:

- The **referral** objective is dropped (DE has no referral data source).
- A **prescription** objective (ATC group **N02**, incl. N02C antimigraine) is
  added, with treatment-pattern analyses.

## Study design

- **Cases:** patients with a headache disorder, identified by the M2Q rule —
  at least two diagnosis codes in two different quarters, or by two different
  physicians in the same quarter — aged ≥18 at first diagnosis, with ≥1 year of
  follow-up.
- **Controls:** patients with no headache disorder, 1:1 matched to cases on
  year of birth (±2 years), gender, and care site.
- **Follow-up window:** index date to the patient's last observation. Both arms
  use the case-defined window.

## Requirements

- R with: tidyverse, dbplyr, DBI, odbc, janitor, glue, openxlsx, labelled,
  ggplot2 (see the library block in `00_global.R`).
- A Snowflake connection. Credentials load from an external `.Renviron`
  (`SNOWFLAKE_USER` / `SNOWFLAKE_TOKEN` / `SNOWFLAKE_SERVER`) — never hardcoded.

## Running

Run from the project root. To run the whole pipeline:

```r
source("runAll.R")
```

`runAll.R` runs the programs in order, times each step, and stops on the first
error. It excludes the referral program (`05_cov2.R`).

To run programs individually, use this order (each sources `00_global.R`):

```
03_match.R        # sources 01 + 02; builds codelists, cohorts, match, attrition
04_cov1.R         # GP visits
06_cov3.R         # demographics
07_figure1.R      # figures
08_rx.R           # N02 prescription counts + extracts
09_rx_patterns.R  # N02 treatment patterns (runs off saved extracts)
99_table_output.R # results workbook
99_euroboard_appendix.R  # codelist appendix workbook
```

## Programs

| File | Purpose | Key outputs |
|------|---------|-------------|
| `00_global.R` | Packages, Snowflake connection, view handles, diagnosis + ATC codelists | `data/diagnosis_codelist`, `data/rx_codelist` |
| `01_patpop_cohort1.R` | Cases (M2Q, age ≥18, ≥1yr follow-up) + attrition rows 1–4 | `data/table1_1` |
| `02_patpop_cohort2.R` | Controls (no headache) | — |
| `03_match.R` | 1:1 matching + final attrition row | `data/patpop_matched`, `data/table1` |
| `04_cov1.R` | GP visits (headache-related, all-cause, annualized) | `data/cov1`, `data/gp_visit_annual` |
| `05_cov2.R` | Referrals — **not used in DE** (guarded) | — |
| `06_cov3.R` | Demographics & clinical characteristics | `data/cov3` |
| `07_figure1.R` | Annualized GP-visit figures | `results/figure1*.png` |
| `08_rx.R` | N02 prescription counts, first-line, intensity, overuse, subgroup patterns | `data/cov4`, `data/rx_*` extracts |
| `09_rx_patterns.R` | N02 treatment episodes, lines of therapy, adherence (MPR/PDC) | `data/cov5`, `data/rx_episodes`, `data/rx_lot`, `data/rx_adherence` |
| `99_table_output.R` | Presentation workbook | `results/DE_Migraine_Headache_Results.xlsx` |
| `99_euroboard_appendix.R` | Codelist appendix workbook | `results/euroboard_migraine_appendix.xlsx` |

### functions/

- `source_all.R` — source every `.R` in a folder.
- `summarize_var.R` — summary engine (mean/SD, median/IQR, range for numerics;
  n/% with Clopper-Pearson CIs for categoricals), by cohort.
- `patpop_matched_obs.R` — join the matched cohort to a contact stream.

## Outputs

- **`data/`** — intermediate tables (RDS).
- **`rawresults/`** — raw results.
- **`results/`** — final formatted outputs (Excel workbook, figures).

The main deliverable, `results/DE_Migraine_Headache_Results.xlsx`, has:

- Table of Contents
- Table 1 — attrition flow
- Table 2 — outcomes (GP visits + demographics)
- Table 3 — N02 prescription counts & patterns
- Table 4 — N02 treatment patterns (episodes, lines of therapy, adherence)
- Figure 1 — annualized GP visits
- Appendices — diagnosis and N02 ATC codelists

## Prescription data (N02)

`08_rx.R` also saves line-level extracts (`data/rx_lines_raw`, `data/rx_obs`,
`data/rx_patient_tx`, `data/rx_detail_raw`). `09_rx_patterns.R` reads
`data/rx_obs`, so treatment-pattern analyses run without a Snowflake connection.

Days-supply for episodes / lines of therapy / adherence comes from the
prescription `duration` field (30-day fallback), with a 30-day grace period —
both set in `09_rx_patterns.R`.

## Notes

- The full pipeline runs on a separate machine with the Snowflake connection;
  this repo moves the programs between machines.
- Paths are written without a `../` prefix (working directory is the project
  root).
- `documents/` holds the UK study protocol, UK results, the DE data dictionary,
  and `PORTING-NOTES.md` (details on adapting the study to another country).

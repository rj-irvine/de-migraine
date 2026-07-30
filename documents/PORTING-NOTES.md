# Porting Notes — Migraine/Headache Study (25P01)

How to adapt this analysis from one country to the next. Written during the
UK → DE port (2026-07) so the reasoning survives into the next country.

The study is a retrospective 1:1 matched cohort of headache-disorder patients
vs. no-headache controls in Cegedim/THIN primary-care EHR data. The UK version
was completed first; DE was the first port and added a prescription objective.

---

## 1. Operating constraints (all countries)

- **The analysis runs on a separate machine** that holds the Snowflake
  connection. This repo is the transfer mechanism between machines. Code here
  cannot be executed or verified locally — changes must be reasoned through by
  reading, not by running.
- **No Snowflake write access.** You cannot create temp tables. This rules out
  dbplyr's `copy = TRUE` on any local→lazy join. Every join must be
  **local↔local or lazy↔lazy** — dbplyr errors with *"x and y must both be lazy
  or both be data.frames"* otherwise. Rules of thumb:
  - Pushing a local **vector** into a lazy filter (`filter(x %in% local(v))`)
    becomes an SQL `IN (...)` list. Fine for **small** sets only — Snowflake
    caps a literal list at **200,000** expressions. The matched cohort (~485k
    persons) blows past this, so do NOT filter the contact stream by matched ids
    this way. Instead collect the DB side after cheap in-DB predicates (date,
    type) and drop to the cohort with a **local** join (see `04_cov1.R` Step 3 +
    `patpop_matched_obs()`). If a large-set filter is unavoidable, batch the
    vector into <200k chunks and row-bind locally.
  - Pushing a local **table** into a lazy join is what you cannot do. To join a
    local frame (e.g. `readRDS("data/patpop_matched")`) to a database table,
    first restrict the database side (ideally with a `local()` vector filter as
    above), `collect()` it, then finish the join in R with both sides local.
  - See `08_rx.R` and `04_cov1.R` for the pattern; `06_cov3.R` collects its
    lazy chain (line ~85) before any local join for the same reason. §5 has the
    history.
- **Paths** use no `../` prefix: intermediate tables → `data/`, raw results →
  `rawresults/`, final formatted outputs → `results/`. The working directory on
  the analysis machine is already the project root.
- **Credentials** load from an external `.Renviron`
  (`SNOWFLAKE_USER` / `SNOWFLAKE_TOKEN` / `SNOWFLAKE_SERVER`). Never hardcode or
  commit them. No patient-level data belongs in this repo.

---

## 2. Program flow

```
00_global.R          setup, Snowflake views, codelists
  ├── 01_patpop_cohort1.R    cases (M2Q criterion) + attrition rows 1-4
  ├── 02_patpop_cohort2.R    controls (no headache)
  └── 03_match.R             1:1 matching + attrition row 5  → data/patpop_matched
        ├── 04_cov1.R        GP visits            (ported)
        ├── 05_cov2.R        referrals            (NOT ported — guarded stop(); see §4)
        ├── 06_cov3.R        demographics         (ported)
        ├── 07_figure1.R     figures → results/   (ported)
        └── 08_rx.R          N02 prescriptions    (DE addition — see §6)
              └── 99_table_output.R   styled Excel workbook → results/

DE status: attrition + GP visits + demographics + N02 prescriptions + figures
are all ported. Only the referral objective (05) is excluded, and it is guarded
with a stop() so it cannot run by accident. 99_table_output.R assembles Table 1
(attrition), Table 2 (cov1 GP visits + cov3 demographics), Table 3 (N02 Rx), and
the codelist appendices. 99_euroboard_appendix.R writes the diagnosis + N02 ATC
codelists.
```

`03_match.R` sources `01` and `02` directly (each of which sources `00`) so the
cohorts exist as **live lazy Snowflake tbls** and the matching join is pushed
down to the database. The cohorts are deliberately *not* saved to disk — a lazy
tbl does not survive a session restart, and collecting the control pool would be
enormous.

**But `patpop_matched` MUST be `collect()`ed before `saveRDS`** (end of the 03
pipeline). It is the small, final matched set that 04/06/08 read back with
`readRDS` and access via `$` / `pull()`. If it is saved lazy, the RDS holds a
dead DB pointer and downstream fails with *"use pull instead of \$"* (on `$`) or
*"external pointer is not valid"* (on `pull()`). This `collect()` has been lost
in edits once — keep it.

---

## 3. What is country-specific (the actual porting checklist)

Everything below silently produces **zero rows** if wrong, rather than erroring.
Verify each against the target country's Snowflake instance *before* a full run.

| Item | Where | UK value | DE value | How to verify |
|---|---|---|---|---|
| View names | `00_global.R` | `V_UK_*` | `V_DE_*` | `information_schema.views`. Some deployments use shared multi-country views filtered on `country_code` instead. |
| `list_code` for the diagnosis codelist | `00_global.R` | `diagnostic_code` | `diagnostic_code` | `SELECT DISTINCT list_code FROM <codelist>`. The DE data dictionary describes a `cim10_code` value, which looked right but was not what the data used — check, don't infer. |
| `language_code` for labels | `00_global.R` | n/a (labels already English) | `"en"` | `SELECT DISTINCT language_code FROM <codelist_translate>`. Could be `EN`, `eng`, or numeric. |
| Diagnosis coding scheme | `00_global.R` | Read codes (`INUK.*`) | ICD-10 / CIM-10 | Determines whether the label→code back-mapping is needed at all. See §4. |
| `contact_type_code` exclusion | `04_cov1.R`, `06_cov3.R` | `"R"` (referral) | verify | `SELECT DISTINCT contact_type_code FROM <contact>` + the codelist labels. |
| Referral source | `05_cov2.R` | `V_UK_UK_REFERRAL` | **does not exist** | See §4. |
| Label language for regex filters | `00_global.R` | English | English (via translation table) | If the target country has no English translation, every label regex must be localized. |

**Sanity check after `00_global.R` runs:** `nrow(diagnosis_codelist)` should be
in the dozens. If it is 0, the `list_code` or `language_code` value is wrong —
do not proceed, everything downstream will be empty but will not error.

---

## 4. The two structural differences found so far

### Coding scheme (UK Read codes vs. ICD-10)

The UK codelist filters on `list_code == "diagnostic_code"` and then hardcodes
26 Read codes (`INUK.1B1G.00`, `INUK.R040.00`, …) to force
`icd10_label = "Headache"` / `icd10_code = "R51"`, because Read codes do not
carry ICD-10 groupings natively.

**In DE this entire block was deleted.** DE codes are ICD-10 natively:
`code` holds the ICD-10 code and `code_group` its chapter grouping, so the
G43/G44/R51 `code_group` logic works directly and no back-mapping is needed.

For a new country: check whether the coding scheme is ICD-10-native. If yes,
delete the hardcode block (as in DE). If it is a national scheme (Read, CIM,
ICPC…), you will need an equivalent mapping and should expect to build it with
clinical input, not by regex.

### Referrals (UK-only)

`05_cov2.R` — the entire referral objective (`cov2_1`–`cov2_6`, time-to-first-
referral, the specialist appendix) — depends on `V_UK_UK_REFERRAL`
(person_id, referral_id, event_date, diagnostic_code). The doubled `UK_UK_`
in that view name marks it as a **country-specific extension, not part of the
standard Cegedim model**.

**No DE equivalent exists.** The DE data dictionary contains no table with
`referral_id`. This objective is currently unported and `05_cov2.R` will fail
if run against DE.

Candidate proxies if the objective must be reproduced without a referral table:
`provider.specialty_code` / `specialist_code`, `care_site.care_site_type_code`,
`contact.provider_id` + `contact_type_code`. None is equivalent; all would need
a documented methods deviation.

Also note `05_cov2.R` classifies specialty with ~28 **English-language regexes**
over the code label (`neurolog|cardiolog|…`). That classifier is not portable to
a country whose labels are not English unless an English translation exists.

---

## 5. Bugs fixed during the DE port (verify these did not regress)

These were latent in the UK code. All are fixed in the current tree.

- **Inverted follow-up filters.** `01` step 5 and `02` step 2-2 computed
  `difftime(index_date, last_obs)` — arguments reversed, which excludes exactly
  the patients who *do* have follow-up. Correct form: `last_obs - index_date >= 365`.
- **Control follow-up filter discarded.** `02` computed `no_headache_ID1` (the
  filtered pool) and then built `patpop_cohort2` from the *unfiltered*
  `no_headache_ID`, so the control 1-year requirement never applied.
- **`03` could not run standalone** — it referenced `patpop_cohort1`/`_cohort2`
  as in-session globals. Now sources `01` and `02` explicitly.
- **Suffix collision in the matching join.** After fixing the bug above,
  `patpop_cohort2` began carrying `last_obs`, so the join's
  `suffix = c("_case","_control")` turned it into `last_obs_case` /
  `last_obs_control` and the bare `mutate(censor_date = last_obs)` failed with
  *object 'last_obs' not found*. Fixed to `last_obs_case` — the case defines the
  follow-up window. **This is the trap to watch when changing what columns the
  cohorts carry:** any column present in *both* cohorts gets suffixed, and join
  keys do not.
- **Attrition row mislabelled** — two rows numbered "4."; the matching row is now "5.".
- **Output path** — `99_table_output.R` wrote to a nonexistent `output/`; now `results/`.

### Still open (not blocking, but real)

- `set.seed(123)` in `03` does **not** control `sql("RANDOM()")` — the
  randomness happens in Snowflake, so matching is **not reproducible**. If
  reproducibility is required, replace the tie-breaker with a deterministic
  hash of the person ids.
- `07_figure1.R` sources `"R/00_global.R"` (no such directory) and saves neither plot.
- `functions/summarize_var.R`: `class(x) %in% c("character","factor")` warns on
  factors (`class()` returns two elements) and returns `NULL` silently for Date
  or logical inputs.
- `99_euroboard_appendix.R` still references the referral codelist.

---

## 6. The DE prescription objective (`08_rx.R`) — template for new objectives

Counts prescriptions in ATC group **N02** (analgesics), with particular interest
in **N02C** (antimigraine preparations). Not part of the UK study.

Design decisions, so they can be re-confirmed rather than re-guessed:

- **Population:** the matched cohort, **both arms**, so cases and controls are
  comparable.
- **Window:** each person's follow-up, `index_date < event_date <= censor_date`.
  Both arms use the **case-defined** window, matching how `04`/`05`/`06` treat
  the matched pair.
- **Unit:** the prescription *line* (one row of `contact_prescriptions`).
- **Breakdown:** one row per distinct full `product_atc_code` under N02, ordered
  so N02C codes appear first.
- **Denominator caveat:** patients with zero N02 lines drop out of the joins, so
  the per-patient summary describes patients **with ≥1 prescription**. If a
  full-cohort mean (counting zeros) is wanted, left-join the counts back onto
  the matched cohort and fill NA with 0.

**Data model note:** prescription lines live on `contact_prescriptions`
(`contact_id`, `product_id`, `quantity`, `frequency_code`, `duration`, `box`,
`dci_flag`, `num_sequence`, `diagnostic_code`) and carry **no date** — the
prescribing date comes from the parent `contact.start_date`. ATC codes and
molecule come from `product` (`product_atc_code`, `product_molecule_code`,
`is_generic`, `brand_name`). A richer 43-column `prescription_detail` table adds
`treatment_code`, `duration_min/max`, `renewal`, `prevention_flag`, dose &
frequency, `specialist_code` — its DE view name is **unconfirmed**
(`prescription_detail_view` in `00_global.R`, guessed `V_DE_PRESCRIPTION_DETAIL`),
so `08_rx.R` pulls it inside a `tryCatch` that degrades to skipping if wrong.

**Treatment-pattern analyses (08_rx.R, cov4_1..cov4_7):** Rx count/patient, lines
by ATC code, index (first-line) N02 subgroup, time to first N02 Rx, annualized
Rx, total quantity dispensed, and a possible acute-medication-overuse flag
(>=10 N02 Rx/yr, an MOH proxy; denominator = full matched arm, so zeros count).

**Licence-safety extracts:** because DE data access ended after the final pull
(2026-07), `08_rx.R` saves durable line-level RDS extracts — `data/rx_lines_raw`
(all N02 lines + product attributes), `data/rx_obs` (in-window, cohort-tagged),
and `data/rx_detail_raw` (prescription_detail, if the view resolved). Any further
Rx analysis should be re-derived OFFLINE from these, NOT re-queried.

**IN-list cap caveat:** the N02 `product_id` filter uses `%in% local(...)`, which
is safe only because the N02 product list is small (<<200k). Never do this with
a large key set (see §1) — the 485k patient list hit Snowflake's 200k cap.

**The no-`copy = TRUE` pattern** (see §1) is load-bearing here:

```r
rx_lines <- contact_prescriptions |>
  filter(product_id %in% local(n02_product_ids)) |>   # filter hard in-DB
  inner_join(contact |> select(...), by = "contact_id") |>
  collect()                                            # then come local

rx_obs <- rx_lines |>
  inner_join(match_windows, by = "person_id", relationship = "many-to-many") |>
  filter(event_date > index_date & event_date <= censor_date)
```

---

## 7. Output formatting

`99_table_output.R` must produce a **presentation-ready** workbook, not a data
dump. It is styled to match the UK deliverable
(`documents/V2_UK_Migraine_Headache_Results.xlsx`), whose conventions are:

- A **Table of Contents** sheet (Item / Tab / Description / Notes) listing every
  sheet.
- Content offset to **B2** (column A and row 1 are margins).
- **Cohort-count column headers**: `Cohort 1\nHeadache Disorder\nN = <n_case>` /
  `Cohort 2\nNo Headache Disorder\nN = <n_control>`, derived at runtime from
  `patpop_matched` (do NOT hardcode counts — the UK file did, and they went
  stale across reruns).
- Objective/section rows (the "To assess ..." rows where both value columns are
  NA) rendered **bold** so the outcomes table reads as grouped sections.
- Indented sub-rows (5 leading spaces, produced upstream by `summarize_var`).
- A **matching footnote** on the attrition sheet.
- Navy title bar, blue header, zebra banding, frozen header, autofilter on
  appendices; figures embedded from the `results/figure1*.png` PNGs (07).

Add sheets through the `write_styled_table()` helper so the look stays uniform.

**DE vs. UK deliverable:** all referral sheets/appendices are removed (T2b Ref
Specialty, A2 Referral Codelist, A3 Full Specialty). DE adds **T3. N02
Prescriptions** and **A2. N02 ATC Codelist**. Output file:
`results/DE_Migraine_Headache_Results.xlsx`.

---

## 8. Suggested next step for a multi-country repo

This document is the stopgap. The structural fix is to parameterize:

```r
country <- "DE"
view <- function(x) I(glue("ORD_IDMT.ORD_CEGEDIM_PUB.V_{country}_{x}"))
```

plus a small per-country config block for the `list_code`, `language_code`, and
`contact_type_code` values in §3. That would let one repo serve all countries
and reduce porting to editing a config block and resolving §4-style structural
gaps.

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
  dbplyr's `copy = TRUE` on any local→lazy join. When a local data frame must
  meet a database table, filter the database side as hard as possible, then
  `collect()` and finish the join in R. See `08_rx.R` for the pattern, and
  §5 for why this bit us once already.
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
        ├── 04_cov1.R        GP visits
        ├── 05_cov2.R        referrals            (UK only — see §4)
        ├── 06_cov3.R        demographics
        ├── 07_figure1.R     figures
        └── 08_rx.R          N02 prescriptions    (DE addition — see §6)
              └── 99_table_output.R   styled Excel workbook → results/
```

`03_match.R` sources `01` and `02` directly (each of which sources `00`) so the
cohorts exist as **live lazy Snowflake tbls** and the matching join is pushed
down to the database. The cohorts are deliberately *not* saved to disk — a lazy
tbl does not survive a session restart, and collecting the control pool would be
enormous.

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
(`contact_id`, `product_id`) and carry **no date** — the prescribing date comes
from the parent `contact.start_date`. ATC codes come from `product.product_atc_code`.

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
dump. It defines a reusable `write_styled_table()` helper giving each sheet a
merged title bar, styled header row, banded rows, thousands separators on
counts, a frozen header, an autofilter, and sized columns. Add new sheets
through that helper rather than calling `writeData()` directly, so the workbook
stays visually consistent.

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

# Substance names for the N02 (analgesic) ATC codes used in this study, so the
# output tables read as drug names rather than codes.
#
# Why this is a static table: the DE product master (data/rx_codelist) carries
# only brand names with strengths - "Tramabeta long 100 mg", "Instanyl 50
# Mikrogramm/Dosis" - so a substance name cannot be recovered from it reliably
# ("Instanyl" is fentanyl). The DE data licence has ended, so the codelist
# translation table cannot be re-queried either. ATC is an international
# standard and these names do not change, so they are recorded here.
#
# Cross-checked against figures already in the deck: N02CC01 sumatriptan
# (130,067 lines), N02CC03 zolmitriptan (36,592), N02CC04 rizatriptan (55,782),
# N02CD01 erenumab (14,846), N02CD02 galcanezumab (1,848), N02CD03 fremanezumab
# (5,258), N02CD05 eptinezumab (289), N02BB02 metamizole.
#
# Codes with no entry here keep showing the bare code rather than being guessed.
atc_n02_labels <- data.frame(
  code = c(
    # Group-level codes, used where a product carries only a partial code
    "N02A", "N02AC", "N02B", "N02C", "N02CC", "N02CX",
    # N02A opioids
    "N02AA01", "N02AA03", "N02AA05", "N02AA08", "N02AA55", "N02AA59",
    "N02AB02", "N02AB03", "N02AC03", "N02AE01", "N02AF02",
    "N02AJ06", "N02AJ07", "N02AJ09", "N02AJ13",
    "N02AX01", "N02AX02", "N02AX05", "N02AX06", "N02AX51",
    # N02B other analgesics and antipyretics
    "N02BA01", "N02BA11", "N02BA51", "N02BB01", "N02BB02", "N02BB04",
    "N02BE01", "N02BE51", "N02BF01", "N02BF02", "N02BG08", "N02BG10",
    # N02C antimigraine
    "N02CA02", "N02CC01", "N02CC02", "N02CC03", "N02CC04", "N02CC05",
    "N02CC06", "N02CC07", "N02CC08",
    "N02CD01", "N02CD02", "N02CD03", "N02CD05", "N02CD06", "N02CD07"
  ),
  label = c(
    "Opioids", "Diphenylpropylamine derivatives",
    "Other analgesics and antipyretics", "Antimigraine preparations",
    "Triptans", "Other antimigraine preparations",
    "Morphine", "Hydromorphone", "Oxycodone", "Dihydrocodeine",
    "Oxycodone, combinations", "Codeine, combinations",
    "Pethidine", "Fentanyl", "Piritramide", "Buprenorphine", "Nalbuphine",
    "Codeine and paracetamol", "Codeine and acetylsalicylic acid",
    "Codeine and other non-opioid analgesics", "Tramadol and paracetamol",
    "Tilidine", "Tramadol", "Meptazinol", "Tapentadol",
    "Tilidine, combinations",
    "Acetylsalicylic acid", "Diflunisal",
    "Acetylsalicylic acid, combinations", "Phenazone", "Metamizole sodium",
    "Propyphenazone", "Paracetamol", "Paracetamol, combinations",
    "Gabapentin", "Pregabalin", "Ziconotide", "Cannabinoids",
    "Ergotamine", "Sumatriptan", "Naratriptan", "Zolmitriptan", "Rizatriptan",
    "Almotriptan", "Eletriptan", "Frovatriptan", "Lasmiditan",
    "Erenumab", "Galcanezumab", "Fremanezumab", "Eptinezumab", "Rimegepant",
    "Atogepant"
  ),
  stringsAsFactors = FALSE
)

# Short plain names for the three drug families, used in the pathway rows where
# a full ATC group name would be unreadable ("N02B -> N02A").
atc_family_names <- c(N02A = "Opioids", N02B = "Everyday analgesics",
                      N02C = "Migraine-specific")

# Make an ATC row label readable.
#
# Two cases, kept deliberately narrow so prose is never touched:
#   1. the row is nothing but a code       "N02CC01" -> "Sumatriptan (N02CC01)"
#   2. the row is a pathway of families    "N02B -> N02A" ->
#                                          "Everyday analgesics -> Opioids"
# Anything else - "Combination use among N02C (antimigraine) users, n (%)",
# "N02 prescription lines by ATC code" - is returned untouched. Leading
# whitespace, which carries the table's indent, is preserved.
label_atc_code <- function(x) {
  lookup <- setNames(atc_n02_labels$label, atc_n02_labels$code)
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    indent <- regmatches(s, regexpr("^\\s*", s))
    body <- trimws(s)

    # Case 1: the whole row is a single ATC code.
    if (body %in% names(lookup)) {
      return(paste0(indent, lookup[[body]], " (", body, ")"))
    }

    # Case 2: a pathway built only from family codes and joining tokens.
    if (grepl("^N02[ABC]( (\\+|->|only|other) ?N?0?2?[ABC]?)*$", body) ||
        grepl("^N02[ABC] (only|\\+ other N02)$", body) ||
        grepl("^N02[ABC] -> N02[ABC]( -> N02[ABC])*$", body)) {
      for (fam in names(atc_family_names)) {
        body <- gsub(paste0("\\b", fam, "\\b"), atc_family_names[[fam]], body)
      }
      body <- sub("other N02\\b", "other painkillers", body)
      return(paste0(indent, body))
    }

    s
  }, character(1), USE.NAMES = FALSE)
}

# Substance name for each Cegedim molecule code, derived from the product
# master: every product carries both a molecule code and an ATC code, so the
# ATC code seen most often for a molecule identifies the substance, which the
# table above then names. Returns a named character vector, molecule -> label.
molecule_labels <- function(rx_codelist) {
  d <- rx_codelist[!is.na(rx_codelist$product_molecule_code) &
                     !is.na(rx_codelist$product_atc_code), ]
  if (!nrow(d)) return(character(0))
  lookup <- setNames(atc_n02_labels$label, atc_n02_labels$code)
  split_atc <- split(toupper(d$product_atc_code), d$product_molecule_code)
  out <- vapply(split_atc, function(codes) {
    modal <- names(sort(table(codes), decreasing = TRUE))[1]
    if (modal %in% names(lookup)) lookup[[modal]] else NA_character_
  }, character(1))
  out[!is.na(out)]
}

# Apply molecule_labels() to a column of row names: "CGDE.04356" ->
# "Metamizole sodium (CGDE.04356)". Codes with no match are left alone, as is
# any row that is not purely a molecule code (e.g. "Other molecule").
label_molecule_code <- function(x, mol_lookup) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    indent <- regmatches(s, regexpr("^\\s*", s))
    body <- trimws(s)
    if (body %in% names(mol_lookup)) {
      return(paste0(indent, mol_lookup[[body]], " (", body, ")"))
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

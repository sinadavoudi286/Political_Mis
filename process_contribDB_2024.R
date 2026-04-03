## ============================================================
## Process ContribDB_2024.csv → one cycle of panel data
## ============================================================

library(data.table)

cat("=== Step 1: Load employer lookup ===\n")
load("/Users/myoffice/Desktop/employer_lookup_panel.rdata")
# employer_lookup_clean: employer_clean → gvkey, conm (614 rows)
setDT(employer_lookup_clean)
setkey(employer_lookup_clean, employer_clean)
cat("  Lookup has", nrow(employer_lookup_clean), "employer-gvkey mappings\n")

cat("\n=== Step 2: Read ContribDB_2024.csv (select columns only) ===\n")
# Only read the 6 columns we need — keeps RAM ~2-4GB instead of 27GB
dt <- fread(
  "/Users/myoffice/Desktop/contribDB_2024.csv",
  select = c("cycle", "bonica.cid", "contributor.name", "contributor.type",
             "contributor.occupation", "contributor.employer",
             "contributor.cfscore"),
  showProgress = TRUE
)
cat("  Rows read:", format(nrow(dt), big.mark = ","), "\n")

cat("\n=== Step 3: Filter to individuals with non-missing cfscore ===\n")
dt <- dt[contributor.type == "I"]
cat("  After individual filter:", format(nrow(dt), big.mark = ","), "\n")
dt <- dt[!is.na(contributor.cfscore)]
cat("  After cfscore filter:", format(nrow(dt), big.mark = ","), "\n")

cat("\n=== Step 4: Standardize employer names ===\n")
# Uppercase
dt[, employer_clean := toupper(trimws(contributor.employer))]

# Remove common suffixes
suffixes <- c(
  "\\s*,?\\s+(INCORPORATED|INCORPORATE|INC\\.?|CORPORATION|CORP\\.?|COMPANY|CO\\.?)\\s*$",
  "\\s*,?\\s+(LIMITED|LTD\\.?|LLC|L\\.?L\\.?C\\.?|LLP|L\\.?L\\.?P\\.?)\\s*$",
  "\\s*,?\\s+(GROUP|HOLDINGS?|ENTERPRISES?|INTERNATIONAL|INTL\\.?)\\s*$",
  "\\s*,?\\s+(THE|& CO\\.?|AND COMPANY)\\s*$",
  "\\s*[,.]\\s*$"
)
for (s in suffixes) {
  dt[, employer_clean := gsub(s, "", employer_clean, perl = TRUE)]
}
dt[, employer_clean := trimws(employer_clean)]

# Remove junk employers
junk_patterns <- c(
  "^$", "^N/?A$", "^NONE$", "^NOT EMPLOYED", "^SELF[- ]?EMPLOYED",
  "^SELF$", "^RETIRED", "^HOMEMAKER", "^STUDENT$", "^UNEMPLOYED",
  "^NOT APPLICABLE", "^INFORMATION REQUESTED", "^REFUSED",
  "^REQUESTED$", "^DISABLED$"
)
junk_regex <- paste(junk_patterns, collapse = "|")
dt <- dt[!grepl(junk_regex, employer_clean, perl = TRUE)]
dt <- dt[employer_clean != ""]
cat("  After removing junk employers:", format(nrow(dt), big.mark = ","), "\n")

cat("\n=== Step 5: Match employers to gvkey lookup ===\n")
dt_matched <- merge(dt, employer_lookup_clean, by = "employer_clean", all = FALSE)
cat("  Matched rows:", format(nrow(dt_matched), big.mark = ","), "\n")
cat("  Unique gvkeys:", uniqueN(dt_matched$gvkey), "\n")

cat("\n=== Step 6: Flag CEOs ===\n")
ceo_pattern <- "\\b(CEO|CHIEF EXECUTIVE|C\\.?E\\.?O\\.?)\\b"
dt_matched[, is_ceo := grepl(ceo_pattern, toupper(contributor.occupation), perl = TRUE)]
cat("  CEO-flagged rows:", sum(dt_matched$is_ceo), "\n")
cat("  Firms with ≥1 CEO row:", uniqueN(dt_matched[is_ceo == TRUE]$gvkey), "\n")

cat("\n=== Step 7: Compute per-firm, per-cycle stats ===\n")
# Employee stats (non-CEO donors)
emp_stats <- dt_matched[is_ceo == FALSE, .(
  emp_mean_cfscore  = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore    = sd(contributor.cfscore, na.rm = TRUE),
  n_emp_donors      = .N,
  n_emp_unique      = uniqueN(bonica.cid)
), by = .(gvkey, cycle)]

# CEO stats (pick the CEO with most donations in this cycle as the "primary" CEO)
ceo_stats <- dt_matched[is_ceo == TRUE, .(n_donations = .N,
                                            ceo_cfscore = first(contributor.cfscore),
                                            ceo_name = first(contributor.name),
                                            employer_clean = first(employer_clean)),
                         by = .(gvkey, cycle, bonica.cid)]
ceo_stats <- ceo_stats[order(gvkey, cycle, -n_donations)]
ceo_stats <- ceo_stats[, .SD[1], by = .(gvkey, cycle)]  # keep top CEO per firm-cycle
ceo_stats[, c("bonica.cid", "n_donations") := NULL]

# Merge CEO + employee stats
panel_2024 <- merge(ceo_stats, emp_stats, by = c("gvkey", "cycle"), all = FALSE)

# Compute misalignment
panel_2024[, abs_misalignment := abs(ceo_cfscore - emp_mean_cfscore)]
panel_2024[, signed_misalignment := ceo_cfscore - emp_mean_cfscore]

# Add company name (use unique gvkey-conm to avoid duplicates)
conm_lookup <- unique(employer_lookup_clean[, .(gvkey, conm)])[, .SD[1], by = gvkey]
panel_2024 <- merge(panel_2024, conm_lookup, by = "gvkey", all.x = TRUE)

cat("  Panel rows (firm-cycles):", nrow(panel_2024), "\n")
cat("  Unique firms:", uniqueN(panel_2024$gvkey), "\n")

cat("\n=== Step 8: Summary ===\n")
cat("  Mean abs misalignment:", round(mean(panel_2024$abs_misalignment), 3), "\n")
cat("  Mean signed misalignment:", round(mean(panel_2024$signed_misalignment), 3), "\n")
cat("  Mean employee donors per firm:", round(mean(panel_2024$n_emp_donors), 1), "\n")
cat("\n  Top 10 firms by employee donor count:\n")
print(panel_2024[order(-n_emp_donors), .(conm, gvkey, n_emp_donors, ceo_cfscore,
                                          emp_mean_cfscore, abs_misalignment)][1:10])

cat("\n=== Step 9: Save ===\n")
save(panel_2024, file = "/Users/myoffice/Desktop/panel_2024.rdata")
cat("  Saved panel_2024.rdata to Desktop\n")
cat("  Done!\n")

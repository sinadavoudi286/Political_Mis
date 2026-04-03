## ============================================================
## Process contribDB_2020.csv — pre-filter approach
## Only apply expensive gsub to unmatched rows
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

cat("=== Loading employer lookup ===\n")
load("employer_lookup_for_panel.rdata")  # all_exact
setDT(all_exact)
lookup_names <- unique(all_exact$employer_clean)
cat("Lookup has", length(lookup_names), "unique employer names\n")

## ---- Junk employers to exclude ----
junk <- c("SELF", "SELF-EMPLOYED", "SELF EMPLOYED", "SELFEMPLOYED",
          "RETIRED", "NONE", "N/A", "NA", "NOT EMPLOYED",
          "HOMEMAKER", "HOME MAKER", "STUDENT", "UNEMPLOYED",
          "NOT APPLICABLE", "REFUSED", "INFORMATION REQUESTED",
          "INFORMATION REQUESTED PER BEST EFFORTS")

## ---- Suffix patterns (only applied to unmatched subset) ----
suffixes <- c("\\s+INC\\.?$", "\\s+INCORPORATED$", "\\s+CORP\\.?$",
              "\\s+CORPORATION$", "\\s+LLC$", "\\s+LLP$", "\\s+LP$",
              "\\s+LTD\\.?$", "\\s+LIMITED$", "\\s+CO\\.?$",
              "\\s+COMPANY$", "\\s+GROUP$", "\\s+HOLDINGS?$",
              "\\s+ENTERPRISES?$", "\\s+INTERNATIONAL$",
              "\\s+& CO\\.?$", "\\s+AND CO\\.?$",
              ",\\s*INC\\.?$", ",\\s*LLC$", ",\\s*LTD\\.?$",
              ",\\s*CORP\\.?$")

## ---- Read file ----
filename <- "contribDB_2020.csv"
cols_needed <- c("cycle", "bonica.cid", "contributor.type",
                 "contributor.employer", "contributor.cfscore")

cat("\n=== Reading", filename, "===\n")
t0 <- proc.time()
dt <- fread(filename, select = cols_needed, showProgress = TRUE)
cat("Read", format(nrow(dt), big.mark = ","), "rows in",
    round((proc.time() - t0)[3], 1), "seconds\n")

## ---- Filter to individuals with valid cfscore and employer ----
cat("\n=== Filtering ===\n")
dt <- dt[contributor.type == "I"]
cat("Individuals:", format(nrow(dt), big.mark = ","), "\n")

dt[, contributor.cfscore := as.numeric(contributor.cfscore)]
dt <- dt[!is.na(contributor.cfscore)]
cat("With cfscore:", format(nrow(dt), big.mark = ","), "\n")

dt <- dt[!is.na(contributor.employer) & contributor.employer != ""]
cat("With employer:", format(nrow(dt), big.mark = ","), "\n")

# Drop contributor.type — no longer needed
dt[, contributor.type := NULL]
gc()

## ---- Step 4: Fast pre-match (toupper + trimws only, NO gsub suffix removal) ----
cat("\n=== Fast pre-match ===\n")
t0 <- proc.time()
dt[, emp_quick := toupper(trimws(contributor.employer))]
cat("toupper+trimws done in", round((proc.time() - t0)[3], 1), "seconds\n")

# Remove junk
dt <- dt[!emp_quick %in% junk]
cat("After junk removal:", format(nrow(dt), big.mark = ","), "\n")

# Direct match against lookup
dt[, direct_match := emp_quick %chin% lookup_names]
cat("Direct match rows:", format(sum(dt$direct_match), big.mark = ","),
    "(", round(100 * mean(dt$direct_match), 1), "%)\n")

## ---- Step 5: Assign gvkey to direct matches ----
matched_direct <- merge(dt[direct_match == TRUE, .(cycle, bonica.cid, contributor.cfscore,
                                                    employer_clean = emp_quick)],
                        all_exact[, .(employer_clean, gvkey)],
                        by = "employer_clean")
cat("Direct matched rows with gvkey:", format(nrow(matched_direct), big.mark = ","), "\n")

## ---- Step 6: Suffix cleaning on unmatched subset only ----
cat("\n=== Suffix cleaning on unmatched subset ===\n")
unmatched <- dt[direct_match == FALSE, .(cycle, bonica.cid, contributor.cfscore, emp_quick)]
cat("Unmatched rows to clean:", format(nrow(unmatched), big.mark = ","), "\n")

t0 <- proc.time()
unmatched[, employer_clean := emp_quick]
for (suf in suffixes) {
  unmatched[, employer_clean := gsub(suf, "", employer_clean)]
}
unmatched[, employer_clean := gsub("\\s+", " ", trimws(employer_clean))]
cat("Suffix cleaning done in", round((proc.time() - t0)[3], 1), "seconds\n")

# Re-match
matched_suffix <- merge(unmatched[, .(cycle, bonica.cid, contributor.cfscore, employer_clean)],
                        all_exact[, .(employer_clean, gvkey)],
                        by = "employer_clean")
cat("Suffix-cleaned matches:", format(nrow(matched_suffix), big.mark = ","), "\n")

rm(dt, unmatched); gc()

## ---- Step 7: Combine ----
cat("\n=== Combining ===\n")
all_matched <- rbindlist(list(
  matched_direct[, .(cycle, bonica.cid, contributor.cfscore, gvkey)],
  matched_suffix[, .(cycle, bonica.cid, contributor.cfscore, gvkey)]
))
cat("Total matched rows:", format(nrow(all_matched), big.mark = ","), "\n")
cat("Unique gvkeys:", uniqueN(all_matched$gvkey), "\n")
cat("Unique donors:", format(uniqueN(all_matched$bonica.cid), big.mark = ","), "\n")

rm(matched_direct, matched_suffix); gc()

## ---- Step 8: Aggregate per gvkey ----
cat("\n=== Aggregating ===\n")
emp_panel <- all_matched[, .(
  emp_mean_cfscore   = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore     = sd(contributor.cfscore, na.rm = TRUE),
  n_emp_donors       = uniqueN(bonica.cid),
  n_contributions    = .N
), by = .(gvkey, cycle)]

rm(all_matched); gc()

## ---- Step 9: Save ----
cycle_val <- emp_panel[1, cycle]
outfile <- paste0("employee_panel_", cycle_val, ".rdata")
save(emp_panel, file = outfile)

cat("\n=== Summary for cycle", cycle_val, "===\n")
cat("Firms (gvkey):", nrow(emp_panel), "\n")
cat("\nEmployee mean cfscore:\n"); print(summary(emp_panel$emp_mean_cfscore))
cat("\nDonor count:\n"); print(summary(emp_panel$n_emp_donors))
cat("\nFirms with >= 20 donors:", sum(emp_panel$n_emp_donors >= 20), "\n")
cat("Firms with >= 50 donors:", sum(emp_panel$n_emp_donors >= 50), "\n")
cat("Firms with >= 100 donors:", sum(emp_panel$n_emp_donors >= 100), "\n")
cat("\nSaved:", outfile, "\n")
cat("Done!\n")

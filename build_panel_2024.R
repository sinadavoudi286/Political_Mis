## ============================================================
## Build employee-side panel from ContribDB_2024.csv,
## merge with CEO identity + ideology, compute misalignment
## ============================================================

library(data.table)

setwd("/Users/myoffice/Desktop")

cat("=== STEP 1: Load employer lookup and ExecuComp CEOs ===\n")

load("employer_lookup_for_panel.rdata")   # all_exact
load("execucomp_ceos.rdata")              # execucomp_ceos
christensen <- fread("POLITICALSCORES.csv")

cat("Employer lookup:", nrow(all_exact), "rows,",
    uniqueN(all_exact$gvkey), "unique gvkeys\n")
cat("ExecuComp CEOs:", nrow(execucomp_ceos), "rows\n")
cat("Christensen scores:", nrow(christensen), "rows,",
    sum(!is.na(christensen$Politics)), "with non-missing Politics\n\n")

# Check column names
cat("Employer lookup cols:", names(all_exact), "\n")
cat("ExecuComp cols:", names(execucomp_ceos), "\n")
cat("Christensen cols:", names(christensen), "\n\n")

## ============================================================
## STEP 2: Read ContribDB_2024.csv — select only needed columns
## ============================================================
cat("=== STEP 2: Reading ContribDB_2024.csv (select columns only) ===\n")

cols_needed <- c("cycle", "bonica.cid", "contributor.type",
                 "contributor.employer", "contributor.cfscore",
                 "contributor.name")

dt <- fread("contribDB_2024.csv", select = cols_needed, showProgress = TRUE)
cat("Raw rows:", format(nrow(dt), big.mark=","), "\n")
cat("Columns:", names(dt), "\n\n")

## ============================================================
## STEP 3: Filter to individuals with non-missing employer/cfscore
## ============================================================
cat("=== STEP 3: Filter ===\n")

dt <- dt[contributor.type == "I"]
cat("After individual filter:", format(nrow(dt), big.mark=","), "\n")

dt <- dt[!is.na(contributor.cfscore) & contributor.cfscore != ""]
cat("After cfscore filter:", format(nrow(dt), big.mark=","), "\n")

# Ensure cfscore is numeric
dt[, contributor.cfscore := as.numeric(contributor.cfscore)]

dt <- dt[!is.na(contributor.employer) & contributor.employer != ""]
cat("After employer filter:", format(nrow(dt), big.mark=","), "\n\n")

## ============================================================
## STEP 4: Clean employer names (match lookup cleaning)
## ============================================================
cat("=== STEP 4: Clean employer names ===\n")

# Uppercase, trim whitespace
dt[, employer_clean := toupper(trimws(contributor.employer))]

# Remove common suffixes
suffixes <- c("\\s+INC\\.?$", "\\s+INCORPORATED$", "\\s+CORP\\.?$",
              "\\s+CORPORATION$", "\\s+LLC$", "\\s+LLP$", "\\s+LP$",
              "\\s+LTD\\.?$", "\\s+LIMITED$", "\\s+CO\\.?$",
              "\\s+COMPANY$", "\\s+GROUP$", "\\s+HOLDINGS?$",
              "\\s+ENTERPRISES?$", "\\s+INTERNATIONAL$",
              "\\s+& CO\\.?$", "\\s+AND CO\\.?$",
              ",\\s*INC\\.?$", ",\\s*LLC$", ",\\s*LTD\\.?$",
              ",\\s*CORP\\.?$")
for (suf in suffixes) {
  dt[, employer_clean := gsub(suf, "", employer_clean)]
}

# Collapse multiple spaces
dt[, employer_clean := gsub("\\s+", " ", employer_clean)]
dt[, employer_clean := trimws(employer_clean)]

# Remove junk employers
junk <- c("SELF", "SELF-EMPLOYED", "SELF EMPLOYED", "SELFEMPLOYED",
          "RETIRED", "NONE", "N/A", "NA", "NOT EMPLOYED",
          "HOMEMAKER", "HOME MAKER", "STUDENT", "UNEMPLOYED",
          "NOT APPLICABLE", "REFUSED", "INFORMATION REQUESTED",
          "INFORMATION REQUESTED PER BEST EFFORTS")
dt <- dt[!employer_clean %in% junk]
cat("After removing junk employers:", format(nrow(dt), big.mark=","), "\n\n")

## ============================================================
## STEP 5: Match to gvkey via employer lookup
## ============================================================
cat("=== STEP 5: Match employer to gvkey ===\n")

# Merge on employer_clean
dt_matched <- merge(dt, all_exact[, .(employer_clean, gvkey)],
                    by = "employer_clean", allow.cartesian = FALSE)
cat("Matched rows:", format(nrow(dt_matched), big.mark=","), "\n")
cat("Unique gvkeys:", uniqueN(dt_matched$gvkey), "\n")
cat("Unique donors:", uniqueN(dt_matched$bonica.cid), "\n\n")

## ============================================================
## STEP 6: Compute employee ideology stats per gvkey × cycle
## ============================================================
cat("=== STEP 6: Aggregate employee ideology by gvkey ===\n")

emp_panel <- dt_matched[, .(
  emp_mean_cfscore   = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore     = sd(contributor.cfscore, na.rm = TRUE),
  n_emp_donors       = uniqueN(bonica.cid),
  n_contributions    = .N
), by = .(gvkey, cycle)]

cat("Employee panel: ", nrow(emp_panel), "gvkey-cycle obs\n")
cat("Unique gvkeys:", uniqueN(emp_panel$gvkey), "\n\n")

# Free memory
rm(dt, dt_matched); gc()

## ============================================================
## STEP 7: Merge with ExecuComp CEO identity
## ============================================================
cat("=== STEP 7: Merge with ExecuComp CEOs ===\n")

# Map cycle to year (cycle 2024 → year 2024)
emp_panel[, year := cycle]

# Merge CEO identity
panel <- merge(emp_panel,
               execucomp_ceos[, .(gvkey, year, execid, exec_fullname)],
               by = c("gvkey", "year"), all.x = TRUE)

cat("After CEO merge:", nrow(panel), "rows\n")
cat("With CEO identified:", sum(!is.na(panel$execid)),
    "(", round(100*mean(!is.na(panel$execid)),1), "%)\n\n")

## ============================================================
## STEP 8: Merge with Christensen CEO ideology scores
## ============================================================
cat("=== STEP 8: Merge with Christensen Politics scores ===\n")

# Ensure execid formats align (both character)
panel[, execid := as.character(execid)]
christensen[, EXECID := as.character(EXECID)]

# Check format examples
cat("Panel execid examples:", head(panel[!is.na(execid)]$execid, 5), "\n")
cat("Christensen EXECID examples:", head(christensen$EXECID, 5), "\n")

panel <- merge(panel, christensen[, .(EXECID, Politics)],
               by.x = "execid", by.y = "EXECID", all.x = TRUE)

cat("With Politics score:", sum(!is.na(panel$Politics)),
    "(", round(100*mean(!is.na(panel$Politics)),1), "%)\n\n")

## ============================================================
## STEP 9: Compute misalignment
## ============================================================
cat("=== STEP 9: Compute misalignment ===\n")

# Rescale Christensen [-1,+1] to DIME scale [-2,+2]
panel[, ceo_cfscore_rescaled := Politics * 2]

# Absolute and signed misalignment
panel[, misalign_abs := abs(ceo_cfscore_rescaled - emp_mean_cfscore)]
panel[, misalign_signed := ceo_cfscore_rescaled - emp_mean_cfscore]

# Z-score robustness
panel[, ceo_z := scale(ceo_cfscore_rescaled), by = cycle]
panel[, emp_z := scale(emp_mean_cfscore), by = cycle]
panel[, misalign_z := abs(ceo_z - emp_z)]

## ============================================================
## STEP 10: Summary statistics
## ============================================================
cat("=== STEP 10: Summary Statistics ===\n\n")

cat("--- FULL PANEL ---\n")
cat("Total gvkey-cycle obs:", nrow(panel), "\n")
cat("Unique firms (gvkey):", uniqueN(panel$gvkey), "\n")
cat("Unique CEOs:", uniqueN(panel[!is.na(execid)]$execid), "\n\n")

cat("--- EMPLOYEE IDEOLOGY ---\n")
cat("Employee mean cfscore:\n")
print(summary(panel$emp_mean_cfscore))
cat("\nEmployee donor count:\n")
print(summary(panel$n_emp_donors))

cat("\n--- CEO COVERAGE ---\n")
cat("Has ExecuComp CEO:", sum(!is.na(panel$execid)), "/", nrow(panel), "\n")
cat("Has Christensen score:", sum(!is.na(panel$Politics)), "/", nrow(panel), "\n")

# Subset with misalignment computable
mis_panel <- panel[!is.na(misalign_abs)]
cat("\n--- MISALIGNMENT (firms with both CEO and employee scores) ---\n")
cat("N obs:", nrow(mis_panel), "\n")
cat("N unique firms:", uniqueN(mis_panel$gvkey), "\n")
cat("\nAbsolute misalignment:\n")
print(summary(mis_panel$misalign_abs))
cat("\nSigned misalignment (CEO - employees):\n")
print(summary(mis_panel$misalign_signed))
cat("\nCEO more conservative than employees:",
    sum(mis_panel$misalign_signed > 0), "/", nrow(mis_panel),
    "(", round(100*mean(mis_panel$misalign_signed > 0),1), "%)\n")

# Quality filter: >= 20 employee donors
mis_panel_q <- mis_panel[n_emp_donors >= 20]
cat("\n--- QUALITY-FILTERED (>= 20 employee donors) ---\n")
cat("N obs:", nrow(mis_panel_q), "\n")
cat("N unique firms:", uniqueN(mis_panel_q$gvkey), "\n")
cat("\nAbsolute misalignment:\n")
print(summary(mis_panel_q$misalign_abs))
cat("\nSigned misalignment:\n")
print(summary(mis_panel_q$misalign_signed))

cat("\n--- TOP 10 FIRMS BY # EMPLOYEE DONORS ---\n")
top10 <- mis_panel_q[order(-n_emp_donors)][1:min(10, nrow(mis_panel_q))]
print(top10[, .(gvkey, exec_fullname, n_emp_donors,
                emp_mean_cfscore, ceo_cfscore_rescaled,
                misalign_abs, misalign_signed)])

## ============================================================
## STEP 11: Save
## ============================================================
cat("\n=== Saving panel_2024_full.rdata ===\n")
save(panel, file = "panel_2024_full.rdata")
cat("Done! Saved", nrow(panel), "rows.\n")

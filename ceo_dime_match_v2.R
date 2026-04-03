## ============================================================
## CEO-DIME name matching — vectorized, no for-loops
## Following user's exact specification
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

## ============================================================
## Step 1: Clean name function
## ============================================================
clean_name <- function(x) {
  x <- toupper(x)
  x <- gsub('[.,]', ' ', x)  # periods and commas to spaces
  x <- gsub('\\b(JR|SR|II|III|IV|V|MD|PHD|MBA|CFA|CPA|ESQ|BSC|DDS|DO)\\b', '', x)
  x <- gsub('\\s+', ' ', trimws(x))
  x
}

## ============================================================
## Step 2: Build DIME lookup table
## ============================================================
cat("=== Step 2: Build DIME lookup ===\n")
load("dime_contribs_slim.rdata")  # contribs
setDT(contribs)

# Keep only needed columns, drop the rest
dime_names <- contribs[, .(bonica.cid,
                           name = most.recent.contributor.name,
                           cfscore = contributor.cfscore,
                           employer = most.recent.contributor.employer,
                           num_distinct = num.distinct)]
rm(contribs); gc()

cat("DIME rows:", format(nrow(dime_names), big.mark = ","), "\n")

# Clean names and create join key: "LASTNAME FIRSTNAME"
# DIME names are "lastname, firstname" — after clean_name, comma becomes space
# So "smith, james e" → "SMITH JAMES E" → remove comma already done → key = "SMITH JAMES E"
# But we want "LASTNAME FIRSTNAME" only (drop middle), so:
cat("Cleaning DIME names...\n")
t0 <- proc.time()
dime_names[, name_clean := clean_name(name)]
dime_names[, name_key := gsub(',', '', name_clean)]
dime_names[, name_key := gsub('\\s+', ' ', trimws(name_key))]

# DIME format after cleaning: "LASTNAME FIRSTNAME [MIDDLE...]"
# Extract just first two tokens: LASTNAME FIRSTNAME
dime_names[, name_key := sub('^(\\S+\\s+\\S+).*', '\\1', name_key)]

cat("Done in", round((proc.time() - t0)[3], 1), "seconds\n")
cat("Sample DIME keys:\n")
print(dime_names[sample(.N, 10), .(name, name_key, num_distinct)])

## ============================================================
## Step 3: Clean ExecuComp names into same format
## ============================================================
cat("\n=== Step 3: Clean ExecuComp names ===\n")
load("execucomp_ceos.rdata")  # execucomp_ceos
setDT(execucomp_ceos)
execucomp_ceos[, execid := as.character(execid)]

execucomp_ceos[, name_clean := clean_name(exec_fullname)]

# ExecuComp is "FIRSTNAME [MIDDLE] LASTNAME" — reverse to "LASTNAME FIRSTNAME"
# Use sapply on strsplit (only 50K rows, fine)
name_parts <- strsplit(execucomp_ceos$name_clean, ' ')
execucomp_ceos[, name_key := sapply(name_parts, function(p) {
  if (length(p) >= 2) paste(p[length(p)], p[1]) else p[1]
})]

cat("Sample ExecuComp keys:\n")
# Spot-check tricky names
tricky_idx <- execucomp_ceos$exec_fullname %in% c(
  "A. Ryals McMullian, Jr.", "Leonard S. Schleifer, M.D., Ph.D.",
  "J. Frank Harrison, III", "James Dimon", "Warren E. Buffett",
  "Kevin J. McNamara", "Elon R. Musk", "Thomas F. Frist Jr.",
  "Christopher Lloyd Harris, B.Sc., C.F.A., M.B.A.")
print(execucomp_ceos[tricky_idx, .(exec_fullname, name_clean, name_key)])

## ============================================================
## Step 4: Merge — single data.table join
## ============================================================
cat("\n=== Step 4: Merge ExecuComp × DIME ===\n")

# Get unique CEO list (one row per execid) for matching
exec_unique <- unique(execucomp_ceos[, .(execid, name_key, exec_fullname)])

t0 <- proc.time()
matches <- merge(execucomp_ceos[, .(execid, gvkey, year, name_key, exec_fullname)],
                 dime_names[, .(bonica.cid, name_key, cfscore, employer, num_distinct)],
                 by = 'name_key', allow.cartesian = TRUE)
cat("Raw matches:", format(nrow(matches), big.mark = ","), "in",
    round((proc.time() - t0)[3], 1), "seconds\n")
cat("Unique execids matched:", uniqueN(matches$execid), "out of",
    uniqueN(exec_unique$execid), "\n")

## ============================================================
## Step 5: Disambiguate duplicates
## ============================================================
cat("\n=== Step 5: Disambiguate ===\n")

# Load employer lookup to get firm names per gvkey
load("employer_lookup_for_panel.rdata")  # all_exact

# Flag if DIME employer matches Compustat firm
matches <- merge(matches, all_exact[, .(gvkey, employer_clean)],
                 by = 'gvkey', allow.cartesian = TRUE)

# Clean DIME employer for comparison
matches[, dime_employer_clean := toupper(trimws(employer))]

matches[, employer_match := !is.na(dime_employer_clean) & !is.na(employer_clean) &
          (grepl(employer_clean, dime_employer_clean, fixed = TRUE) |
           grepl(dime_employer_clean, employer_clean, fixed = TRUE))]

cat("Employer matches:", sum(matches$employer_match), "\n")

# Pick best match per execid: employer match first, then highest num_distinct
matches[, priority := ifelse(employer_match, 1L, 2L)]
setorder(matches, execid, priority, -num_distinct)
ceo_lookup <- unique(matches, by = 'execid')

cat("Final CEO lookup:", nrow(ceo_lookup), "unique execids\n")
cat("  With employer match:", sum(ceo_lookup$employer_match), "\n")
cat("  Without (name only):", sum(!ceo_lookup$employer_match), "\n")

# Match quality stats
cat("\nnum_distinct distribution:\n")
print(summary(ceo_lookup$num_distinct))
cat("\ncfscore distribution:\n")
print(summary(ceo_lookup$cfscore))

# Show top matches
cat("\nTop 20 by num_distinct:\n")
print(head(ceo_lookup[order(-num_distinct),
                      .(exec_fullname, name_key, cfscore, num_distinct,
                        employer_match, employer, employer_clean)], 20))

## ============================================================
## Step 6: Save lookup and rebuild panel
## ============================================================
cat("\n=== Step 6: Save and rebuild panel ===\n")

# Slim lookup for saving
ceo_dime_lookup <- ceo_lookup[, .(execid, exec_fullname, bonica.cid,
                                   ceo_cfscore = cfscore, num_distinct,
                                   name_key, employer_match,
                                   dime_employer = employer)]
save(ceo_dime_lookup, file = "ceo_dime_lookup.rdata")
cat("Saved: ceo_dime_lookup.rdata (", nrow(ceo_dime_lookup), "rows)\n")

# Free memory
rm(dime_names, matches, ceo_lookup, all_exact); gc()

# Rebuild 2024 panel
load("panel_2024_full.rdata")  # panel

# Start from employee-side columns
panel_new <- panel[, .(gvkey, cycle, emp_mean_cfscore, emp_median_cfscore,
                       emp_sd_cfscore, n_emp_donors, n_contributions)]
panel_new[, year := cycle]

# CEO identity from ExecuComp
panel_new <- merge(panel_new,
                   execucomp_ceos[, .(gvkey, year, execid, exec_fullname)],
                   by = c("gvkey", "year"), all.x = TRUE)

cat("Has ExecuComp CEO:", sum(!is.na(panel_new$execid)), "/", nrow(panel_new), "\n")

# CEO ideology from DIME lookup
panel_new <- merge(panel_new,
                   ceo_dime_lookup[, .(execid, ceo_cfscore, bonica.cid,
                                       num_distinct, employer_match)],
                   by = "execid", all.x = TRUE)

cat("Has DIME CEO cfscore:", sum(!is.na(panel_new$ceo_cfscore)), "/", nrow(panel_new), "\n")

# Misalignment (same DIME CFscore scale)
panel_new[, misalign_abs := abs(ceo_cfscore - emp_mean_cfscore)]
panel_new[, misalign_signed := ceo_cfscore - emp_mean_cfscore]

## ============================================================
## Summary statistics
## ============================================================
cat("\n========================================\n")
cat("=== SUMMARY STATISTICS ===\n")
cat("========================================\n\n")

cat("--- PANEL OVERVIEW ---\n")
cat("Total firm-cycle obs:", nrow(panel_new), "\n")
cat("Unique firms:", uniqueN(panel_new$gvkey), "\n")
cat("Has ExecuComp CEO:", sum(!is.na(panel_new$execid)), "\n")
cat("Has DIME CEO cfscore:", sum(!is.na(panel_new$ceo_cfscore)), "\n")
cat("Has misalignment:", sum(!is.na(panel_new$misalign_abs)), "\n\n")

cat("--- EMPLOYEE IDEOLOGY ---\n")
print(summary(panel_new$emp_mean_cfscore))
cat("\nDonor count:\n")
print(summary(panel_new$n_emp_donors))

mis <- panel_new[!is.na(misalign_abs)]
cat("\n--- MISALIGNMENT (same-scale DIME CFscores) ---\n")
cat("N:", nrow(mis), "firms\n")

cat("\nCEO cfscore:\n"); print(summary(mis$ceo_cfscore))
cat("\nEmployee mean cfscore:\n"); print(summary(mis$emp_mean_cfscore))
cat("\nAbsolute misalignment:\n"); print(summary(mis$misalign_abs))
cat("\nSigned misalignment (CEO − employees):\n"); print(summary(mis$misalign_signed))
cat("\nCEO more conservative:", sum(mis$misalign_signed > 0), "/",
    nrow(mis), "(", round(100 * mean(mis$misalign_signed > 0), 1), "%)\n")

# By employer match quality
cat("\nBy match quality:\n")
print(mis[, .(n = .N,
              mean_abs = round(mean(misalign_abs), 3),
              median_abs = round(median(misalign_abs), 3),
              mean_signed = round(mean(misalign_signed), 3)),
          by = employer_match])

# Quality filtered
misq <- mis[n_emp_donors >= 20]
cat("\n--- QUALITY-FILTERED (≥ 20 employee donors) ---\n")
cat("N:", nrow(misq), "firms\n")
cat("\nAbsolute misalignment:\n"); print(summary(misq$misalign_abs))
cat("\nSigned misalignment:\n"); print(summary(misq$misalign_signed))
cat("\nCEO more conservative:", sum(misq$misalign_signed > 0), "/",
    nrow(misq), "(", round(100 * mean(misq$misalign_signed > 0), 1), "%)\n")

cat("\nTop 20 firms by employee donors:\n")
print(head(misq[order(-n_emp_donors),
                .(gvkey, exec_fullname, n_emp_donors,
                  emp_mean = round(emp_mean_cfscore, 3),
                  ceo = round(ceo_cfscore, 3),
                  abs_mis = round(misalign_abs, 3),
                  signed_mis = round(misalign_signed, 3),
                  emp_match = employer_match)], 20))

# Save
panel <- panel_new
save(panel, file = "panel_2024_full.rdata")
cat("\nSaved: panel_2024_full.rdata (", nrow(panel), "rows)\n")
cat("Done!\n")

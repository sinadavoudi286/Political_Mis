## ============================================================
## Option 1: Match ALL ExecuComp CEOs to DIME by name
## Use DIME CFscores for both CEO and employee ideology
## VECTORIZED version — no for-loops over 21M rows
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

cat("=== Loading data ===\n")
load("execucomp_ceos.rdata")        # execucomp_ceos
load("dime_contribs_slim.rdata")    # contribs
load("panel_2024_full.rdata")       # panel

## ============================================================
## Vectorized name standardization
## ============================================================

## Remove whole-word suffixes from end of string (vectorized)
remove_suffixes_vec <- function(x) {
  # Each pattern is a whole-word suffix at end of string,
  # optionally preceded by comma/space
  pats <- c(
    "[,\\s]+Jr\\.?\\s*$",
    "[,\\s]+Sr\\.?\\s*$",
    "[,\\s]+III\\s*$",
    "[,\\s]+II\\s*$",
    "[,\\s]+IV\\s*$",
    "[,\\s]+Ph\\.?\\s*D\\.?\\s*$",
    "[,\\s]+M\\.?\\s*D\\.?\\s*$",
    "[,\\s]+J\\.?\\s*D\\.?\\s*$",
    "[,\\s]+Esq\\.?\\s*$",
    "[,\\s]+CPA\\s*$",
    "[,\\s]+CFA\\s*$",
    "[,\\s]+C\\.?\\s*F\\.?\\s*A\\.?\\s*$",
    "[,\\s]+M\\.?\\s*B\\.?\\s*A\\.?\\s*$",
    "[,\\s]+B\\.?\\s*Sc\\.?\\s*$",
    "[,\\s]+D\\.?\\s*B\\.?\\s*A\\.?\\s*$"
  )
  # Two passes to handle stacked suffixes (e.g. "M.D., Ph.D.")
  for (pass in 1:3) {
    for (p in pats) {
      x <- gsub(p, "", x, ignore.case = TRUE)
    }
  }
  x
}

## ExecuComp: "Firstname [M.] Lastname" → "LASTNAME FIRSTNAME"
standardize_exec_vec <- function(names_vec) {
  x <- toupper(trimws(names_vec))
  x <- remove_suffixes_vec(x)
  x <- gsub("[.]", "", x)              # remove periods
  x <- gsub(",", "", x)               # remove commas
  x <- gsub("\\s+", " ", trimws(x))   # collapse whitespace

  # Extract first and last token
  # Last space separates "everything before" from "surname"
  last_space <- regexpr("\\s\\S+$", x)
  surname <- trimws(substr(x, last_space, nchar(x)))
  # First token = given name (before first space)
  first_space <- regexpr("\\s", x)
  given <- ifelse(first_space > 0,
                  substr(x, 1, first_space - 1),
                  x)
  result <- paste(surname, given)
  # Handle single-word names
  result[first_space < 0] <- x[first_space < 0]
  result[is.na(names_vec) | names_vec == ""] <- NA_character_
  result
}

## DIME: "lastname, firstname [middle]" → "LASTNAME FIRSTNAME"
standardize_dime_vec <- function(names_vec) {
  x <- toupper(trimws(names_vec))
  x <- remove_suffixes_vec(x)
  x <- gsub("[.]", "", x)
  x <- gsub("\\s+", " ", trimws(x))

  # Split on first comma
  has_comma <- grepl(",", x, fixed = TRUE)

  # For names WITH comma: "SURNAME, GIVEN [MIDDLE ...]"
  comma_pos <- regexpr(",", x, fixed = TRUE)
  surname <- ifelse(has_comma,
                    trimws(substr(x, 1, pmax(comma_pos - 1, 1))),
                    NA_character_)
  after_comma <- ifelse(has_comma,
                        trimws(substr(x, comma_pos + 1, nchar(x))),
                        NA_character_)
  # First token after comma = given name
  first_space_after <- regexpr("\\s", after_comma)
  given_comma <- ifelse(first_space_after > 0,
                        substr(after_comma, 1, first_space_after - 1),
                        after_comma)

  result_comma <- paste(surname, given_comma)

  # For names WITHOUT comma: "GIVEN SURNAME" → "SURNAME GIVEN"
  first_space <- regexpr("\\s", x)
  given_no_comma <- ifelse(first_space > 0,
                           substr(x, 1, first_space - 1),
                           x)
  last_space <- regexpr("\\s\\S+$", x)
  surname_no_comma <- ifelse(last_space > 0,
                             trimws(substr(x, last_space, nchar(x))),
                             x)
  result_no_comma <- ifelse(first_space > 0,
                            paste(surname_no_comma, given_no_comma),
                            x)

  result <- ifelse(has_comma, result_comma, result_no_comma)
  result[is.na(names_vec) | names_vec == ""] <- NA_character_
  result
}

## ============================================================
## Step 1: Prepare ExecuComp CEO names
## ============================================================
cat("\n=== Step 1: Prepare ExecuComp CEO names ===\n")

exec_unique <- unique(execucomp_ceos[, .(execid, exec_fullname)])
exec_unique[, execid := as.character(execid)]
cat("Unique ExecuComp CEOs:", nrow(exec_unique), "\n")

exec_unique[, match_name := standardize_exec_vec(exec_fullname)]

# Spot-check
tricky <- data.table(exec_fullname = c(
  "A. Ryals McMullian, Jr.",
  "Leonard S. Schleifer, M.D., Ph.D.",
  "J. Frank Harrison, III",
  "James Dimon",
  "Warren E. Buffett",
  "Kevin J. McNamara",
  "Elon R. Musk",
  "Christopher Lloyd Harris, B.Sc., C.F.A., M.B.A.",
  "Thomas F. Frist Jr.",
  "R. Michael Rouleau"
))
tricky[, match_name := standardize_exec_vec(exec_fullname)]
cat("\nSpot-check:\n")
print(tricky)

## ============================================================
## Step 2: Prepare DIME contributor names (VECTORIZED)
## ============================================================
cat("\n=== Step 2: Prepare DIME contributor names (vectorized) ===\n")

dime <- contribs[contributor.type == "I" & !is.na(contributor.cfscore) & num.distinct >= 2]
cat("DIME individuals (num.distinct >= 2):", format(nrow(dime), big.mark=","), "\n")

cat("Parsing DIME names (vectorized)...\n")
t0 <- proc.time()
dime[, match_name := standardize_dime_vec(most.recent.contributor.name)]
cat("Done in", round((proc.time() - t0)[3], 1), "seconds\n")

cat("Sample:\n")
print(dime[sample(.N, 10), .(most.recent.contributor.name, match_name)])

# Remove NA/empty
dime <- dime[!is.na(match_name) & match_name != "" & match_name != "NA NA"]

# Keep best per match_name (highest num.distinct)
dime_best <- dime[order(-num.distinct)][!duplicated(match_name)]
cat("Unique DIME match names:", format(nrow(dime_best), big.mark=","), "\n")

## ============================================================
## Step 3: Exact match
## ============================================================
cat("\n=== Step 3: Exact name match ===\n")

matched <- merge(exec_unique[, .(execid, exec_fullname, match_name)],
                 dime_best[, .(match_name, bonica.cid, contributor.cfscore,
                               num.distinct, most.recent.contributor.name,
                               most.recent.contributor.occupation)],
                 by = "match_name")

cat("Exact matches:", nrow(matched), "out of", nrow(exec_unique), "CEOs\n")
cat("Match rate:", round(100 * nrow(matched) / nrow(exec_unique), 1), "%\n")

cat("\nMatched CEO num.distinct distribution:\n")
print(summary(matched$num.distinct))

cat("\nMatched CEO cfscore distribution:\n")
print(summary(matched$contributor.cfscore))

cat("\nTop 30 matches by num.distinct:\n")
print(head(matched[order(-num.distinct),
                   .(exec_fullname, most.recent.contributor.name,
                     contributor.cfscore, num.distinct,
                     most.recent.contributor.occupation)], 30))

## ============================================================
## Step 4: False-positive filter
## ============================================================
cat("\n=== Step 4: False-positive filter ===\n")

# Count how many DIME people share each match_name
name_freq <- dime[, .N, by = match_name]
matched <- merge(matched, name_freq, by = "match_name", all.x = TRUE)
setnames(matched, "N", "dime_name_freq")

cat("Match name frequency distribution:\n")
print(quantile(matched$dime_name_freq, probs = c(0, .25, .5, .75, .9, .95, .99, 1)))

# Conservative filter: for common names (>20 people), require num.distinct >= 4
matched_safe <- matched[!(dime_name_freq > 20 & num.distinct < 4)]
cat("\nAfter filtering risky common-name matches:\n")
cat("  Kept:", nrow(matched_safe), "\n")
cat("  Dropped:", nrow(matched) - nrow(matched_safe), "\n")

# Show some dropped ones
dropped <- matched[dime_name_freq > 20 & num.distinct < 4]
if (nrow(dropped) > 0) {
  cat("\nSample dropped (common name, low reliability):\n")
  print(head(dropped[, .(exec_fullname, match_name, dime_name_freq,
                          num.distinct)], 15))
}

## ============================================================
## Step 5: Build and save CEO lookup
## ============================================================
cat("\n=== Step 5: Build CEO ideology lookup ===\n")

ceo_ideology_lookup <- matched_safe[, .(
  execid,
  exec_fullname,
  bonica.cid,
  dime_contributor_name = most.recent.contributor.name,
  ceo_cfscore = contributor.cfscore,
  num.distinct,
  match_name
)]

cat("CEO ideology lookup:", nrow(ceo_ideology_lookup), "CEOs with DIME CFscores\n")
cat("Unique execids:", uniqueN(ceo_ideology_lookup$execid), "\n")

save(ceo_ideology_lookup, file = "ceo_ideology_lookup.rdata")
cat("Saved: ceo_ideology_lookup.rdata\n")

## ============================================================
## Step 6: Rebuild 2024 panel
## ============================================================
cat("\n=== Step 6: Rebuild 2024 panel ===\n")

# Start clean from employee-side columns
panel_new <- panel[, .(gvkey, cycle, emp_mean_cfscore, emp_median_cfscore,
                       emp_sd_cfscore, n_emp_donors, n_contributions)]
panel_new[, year := cycle]

# CEO identity
panel_new <- merge(panel_new,
                   execucomp_ceos[, .(gvkey, year, execid, exec_fullname)],
                   by = c("gvkey", "year"), all.x = TRUE)
panel_new[, execid := as.character(execid)]
cat("Has ExecuComp CEO:", sum(!is.na(panel_new$execid)), "/", nrow(panel_new), "\n")

# CEO ideology from DIME
panel_new <- merge(panel_new,
                   ceo_ideology_lookup[, .(execid, ceo_cfscore, bonica.cid, num.distinct)],
                   by = "execid", all.x = TRUE)
cat("Has DIME CEO cfscore:", sum(!is.na(panel_new$ceo_cfscore)), "/", nrow(panel_new), "\n")

# Misalignment (same DIME scale!)
panel_new[, misalign_abs := abs(ceo_cfscore - emp_mean_cfscore)]
panel_new[, misalign_signed := ceo_cfscore - emp_mean_cfscore]

## ============================================================
## Step 7: Summary statistics
## ============================================================
cat("\n=== Step 7: Summary Statistics ===\n\n")

cat("--- FULL PANEL ---\n")
cat("Total gvkey-cycle obs:", nrow(panel_new), "\n")
cat("Unique firms:", uniqueN(panel_new$gvkey), "\n")
cat("Has ExecuComp CEO:", sum(!is.na(panel_new$execid)), "\n")
cat("Has DIME CEO cfscore:", sum(!is.na(panel_new$ceo_cfscore)), "\n\n")

cat("--- EMPLOYEE IDEOLOGY ---\n")
print(summary(panel_new$emp_mean_cfscore))
cat("\nDonor count:\n")
print(summary(panel_new$n_emp_donors))

mis <- panel_new[!is.na(misalign_abs)]
cat("\n--- MISALIGNMENT (same-scale DIME CFscores) ---\n")
cat("N obs:", nrow(mis), "\n")
cat("Unique firms:", uniqueN(mis$gvkey), "\n")

cat("\nCEO cfscore:\n"); print(summary(mis$ceo_cfscore))
cat("\nEmployee mean cfscore:\n"); print(summary(mis$emp_mean_cfscore))
cat("\nAbsolute misalignment:\n"); print(summary(mis$misalign_abs))
cat("\nSigned misalignment (CEO - employees):\n"); print(summary(mis$misalign_signed))
cat("\nCEO more conservative:", sum(mis$misalign_signed > 0), "/",
    nrow(mis), "(", round(100 * mean(mis$misalign_signed > 0), 1), "%)\n")

# Quality filtered
misq <- mis[n_emp_donors >= 20]
cat("\n--- QUALITY-FILTERED (>= 20 employee donors) ---\n")
cat("N obs:", nrow(misq), "\n")
cat("Unique firms:", uniqueN(misq$gvkey), "\n")
cat("\nAbsolute misalignment:\n"); print(summary(misq$misalign_abs))
cat("\nSigned misalignment:\n"); print(summary(misq$misalign_signed))
cat("\nCEO more conservative:", sum(misq$misalign_signed > 0), "/",
    nrow(misq), "(", round(100 * mean(misq$misalign_signed > 0), 1), "%)\n")

cat("\nTop 15 firms by employee donors:\n")
print(head(misq[order(-n_emp_donors),
                .(gvkey, exec_fullname, n_emp_donors,
                  emp_mean_cfscore, ceo_cfscore,
                  misalign_abs, misalign_signed)], 15))

## Save
panel <- panel_new
save(panel, file = "panel_2024_full.rdata")
cat("\nSaved: panel_2024_full.rdata (", nrow(panel), "rows)\n")
cat("Done!\n")

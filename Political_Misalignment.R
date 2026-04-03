load("~/Library/Mobile Documents/com~apple~CloudDocs/My Research/Data/PACs (DIME)/dime_recipients_1979_2024.rdata")load("~/Library/Mobile Documents/com~apple~CloudDocs/My Research/Data/PACs (DIME)/dime_recipients_1979_2024.rdata")
load("~/Library/Mobile Documents/com~apple~CloudDocs/My Research/Data/PACs (DIME)/dime_contributors_1979_2024.rdata")



# What's the object called?
ls()

# Once you see the name (probably something like "contribs" or "dime_contributors"), run:
colnames(contribs)
nrow(contribs)
install.packages("data.table")
library(data.table)
setDT(contribs)

# -------------------------------------------------------------------
# STEP 1: Keep only columns we need, then drop the rest from memory
# -------------------------------------------------------------------
keep_cols <- c(
  "bonica.cid",
  "contributor.type",
  "most.recent.contributor.name",
  "most.recent.contributor.occupation",
  "most.recent.contributor.employer",
  "most.recent.contributor.state",
  "contributor.cfscore",
  "num.distinct",
  "contributor.gender",
  "first_cycle_active",
  "last_cycle_active"
)

contribs <- contribs[, ..keep_cols]
gc()
cat("Slimmed to", ncol(contribs), "columns,", nrow(contribs), "rows\n")

# -------------------------------------------------------------------
# STEP 2: Filter
# -------------------------------------------------------------------
# Individuals only
contribs <- contribs[contributor.type == "I"]
cat("Individuals only:", nrow(contribs), "\n")

# Active in 2000 or later
contribs <- contribs[last_cycle_active >= 2000]
cat("Last active >= 2000:", nrow(contribs), "\n")

# Valid CFscore
contribs <- contribs[!is.na(contributor.cfscore)]
cat("Non-missing CFscore:", nrow(contribs), "\n")

# At least 2 distinct recipients (minimally reliable score)
contribs <- contribs[num.distinct >= 2]
cat("num.distinct >= 2:", nrow(contribs), "\n")

# -------------------------------------------------------------------
# STEP 3: Check occupation field
# -------------------------------------------------------------------
cat("\n--- Top 30 occupations ---\n")
print(contribs[, .N, by = most.recent.contributor.occupation][order(-N)][1:30])

# -------------------------------------------------------------------
# STEP 4: Flag CEOs with broader pattern
# -------------------------------------------------------------------
ceo_pattern <- paste(c(
  "\\bceo\\b", "\\bchief executive officer\\b", "\\bchief executive\\b",
  "\\bc\\.e\\.o\\b", "president/ceo", "chairman/ceo", "ceo/pres",
  "ceo/chairman", "president & ceo", "chairman & ceo",
  "president and ceo", "chairman and ceo", "ceo & president",
  "ceo and president", "ceo & chairman", "ceo and chairman",
  "ceo/founder", "founder/ceo", "founder & ceo"
), collapse = "|")

contribs[, is_ceo := grepl(ceo_pattern, most.recent.contributor.occupation, ignore.case = TRUE)]
cat("CEO-flagged donors:", sum(contribs$is_ceo), "\n")

# How about "president" alone? (many are CEO-equivalent, but noisy)
cat("'president' exact:", contribs[tolower(most.recent.contributor.occupation) == "president", .N], "\n")

# -------------------------------------------------------------------
# STEP 5: Look at CEO employers — how many have employees too?
# -------------------------------------------------------------------
ceos <- contribs[is_ceo == TRUE]
cat("\nCEO donors:", nrow(ceos), "\n")
cat("Unique CEO employers:", uniqueN(ceos$most.recent.contributor.employer), "\n")

# Top CEO employers
cat("\n--- Top 30 employers among CEO donors ---\n")
print(ceos[, .N, by = most.recent.contributor.employer][order(-N)][1:30])

# -------------------------------------------------------------------
# STEP 6: For those CEO employers, how many non-CEO employees exist?
# -------------------------------------------------------------------
ceo_employers <- unique(ceos$most.recent.contributor.employer)
employees_at_ceo_firms <- contribs[is_ceo == FALSE & 
                                     most.recent.contributor.employer %in% ceo_employers]
cat("\nNon-CEO donors at CEO employers:", nrow(employees_at_ceo_firms), "\n")

emp_counts <- employees_at_ceo_firms[, .N, by = most.recent.contributor.employer]
cat("Employers with >= 5 non-CEO donors:", emp_counts[N >= 5, .N], "\n")
cat("Employers with >= 10 non-CEO donors:", emp_counts[N >= 10, .N], "\n")
cat("Employers with >= 20 non-CEO donors:", emp_counts[N >= 20, .N], "\n")




# -------------------------------------------------------------------
# STEP 7: Remove junk employers from BOTH ceos and contribs
# -------------------------------------------------------------------
junk_employers <- c(
  NA, "", "self", "self employed", "self-employed", "selfemployed",
  "retired", "not employed", "none", "n/a", "na", "homemaker",
  "housewife", "ceo", "president", "myself", "me", "my own",
  "own business", "private", "private practice", "various",
  "information requested", "information requested per best efforts",
  "refused", "decline to state", "not applicable", "unemployed",
  "student", "volunteer", "business owner", "entrepreneur",
  "nonprofit", "non profit", "non-profit", "consultant",
  "chief executive officer"
)

# Save the slimmed contribs (11 columns, already filtered) BEFORE the new column
# Drop the emp_lower column if it partially created
if ("emp_lower" %in% colnames(contribs)) contribs[, emp_lower := NULL]

save(contribs, file = "dime_contribs_slim.rdata")
cat("Saved. Rows:", nrow(contribs), "\n")


#--------Restart R--------


library(data.table)
load("dime_contribs_slim.rdata")
gc()
cat("Loaded. Rows:", nrow(contribs), "| RAM after load:\n")
print(gc())

# Now we have headroom — proceed with cleaning
junk_employers <- c(
  "", "self", "self employed", "self-employed", "selfemployed",
  "retired", "not employed", "none", "n/a", "na", "homemaker",
  "housewife", "ceo", "president", "myself", "me", "my own",
  "own business", "private", "private practice", "various",
  "information requested", "information requested per best efforts",
  "refused", "decline to state", "not applicable", "unemployed",
  "student", "volunteer", "business owner", "entrepreneur",
  "nonprofit", "non profit", "non-profit", "consultant",
  "chief executive officer"
)

# Do it in-place to avoid copies
contribs[, emp_lower := tolower(trimws(most.recent.contributor.employer))]
contribs <- contribs[!is.na(emp_lower) & !emp_lower %in% junk_employers]
contribs[, emp_lower := NULL]
gc()
cat("After junk removal:", nrow(contribs), "\n")

# -------------------------------------------------------------------
# STEP 8: Employer name standardization
# -------------------------------------------------------------------
contribs[, employer_clean := toupper(trimws(most.recent.contributor.employer))]

suffixes <- c(
  "\\bINC\\.?\\b", "\\bCORP\\.?\\b", "\\bCORPORATION\\b",
  "\\bCO\\.?\\b", "\\bLLC\\b", "\\bLTD\\.?\\b", "\\bLIMITED\\b",
  "\\bLP\\b", "\\bL\\.P\\.\\b", "\\bPLC\\b", "\\bNA\\b",
  "\\bTHE\\b", ",", "\\.", "\\/"
)
for (p in suffixes) {
  contribs[, employer_clean := gsub(p, " ", employer_clean)]
}
contribs[, employer_clean := gsub("\\s+", " ", trimws(employer_clean))]
cat("Employer names standardized\n")

# -------------------------------------------------------------------
# STEP 9: Rebuild CEO flag (was lost after restart)
# -------------------------------------------------------------------
ceo_pattern <- paste(c(
  "\\bceo\\b", "\\bchief executive officer\\b", "\\bchief executive\\b",
  "\\bc\\.e\\.o\\b", "president/ceo", "chairman/ceo", "ceo/pres",
  "ceo/chairman", "president & ceo", "chairman & ceo",
  "president and ceo", "chairman and ceo", "ceo & president",
  "ceo and president", "ceo & chairman", "ceo and chairman",
  "ceo/founder", "founder/ceo", "founder & ceo"
), collapse = "|")

contribs[, is_ceo := grepl(ceo_pattern, most.recent.contributor.occupation, ignore.case = TRUE)]
cat("CEOs:", sum(contribs$is_ceo), "\n")

# -------------------------------------------------------------------
# STEP 10: Build firm-level CEO and employee aggregates
# -------------------------------------------------------------------
ceos_clean <- contribs[is_ceo == TRUE, .(
  ceo_name         = most.recent.contributor.name[which.max(num.distinct)],
  ceo_cfscore      = contributor.cfscore[which.max(num.distinct)],
  ceo_bonica_cid   = bonica.cid[which.max(num.distinct)],
  ceo_num_distinct = num.distinct[which.max(num.distinct)],
  n_ceo_donors     = .N
), by = employer_clean]

cat("Unique firms with CEO donor:", nrow(ceos_clean), "\n")

emp_clean <- contribs[is_ceo == FALSE, .(
  emp_mean_cfscore   = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore     = sd(contributor.cfscore, na.rm = TRUE),
  n_employees        = .N
), by = employer_clean]

# -------------------------------------------------------------------
# STEP 11: Merge and compute misalignment
# -------------------------------------------------------------------
firm <- merge(ceos_clean, emp_clean, by = "employer_clean", all = FALSE)

firm[, `:=`(
  misalign_abs    = abs(ceo_cfscore - emp_mean_cfscore),
  misalign_signed = ceo_cfscore - emp_mean_cfscore,
  misalign_median = abs(ceo_cfscore - emp_median_cfscore)
)]

cat("Firms with both CEO + employees:", nrow(firm), "\n")

# Quality tiers
cat("\n--- Sample by threshold ---\n")
cat("emp >= 5,  CEO distinct >= 4:", firm[n_employees >= 5 & ceo_num_distinct >= 4, .N], "\n")
cat("emp >= 10, CEO distinct >= 4:", firm[n_employees >= 10 & ceo_num_distinct >= 4, .N], "\n")
cat("emp >= 20, CEO distinct >= 4:", firm[n_employees >= 20 & ceo_num_distinct >= 4, .N], "\n")
cat("emp >= 50, CEO distinct >= 4:", firm[n_employees >= 50 & ceo_num_distinct >= 4, .N], "\n")

# -------------------------------------------------------------------
# STEP 12: Summary stats at >= 20 threshold
# -------------------------------------------------------------------
firm_final <- firm[n_employees >= 20 & ceo_num_distinct >= 4]
cat("\n*** FINAL SAMPLE:", nrow(firm_final), "firms ***\n")

vars <- c("misalign_abs", "misalign_signed", "misalign_median",
          "ceo_cfscore", "emp_mean_cfscore", "emp_sd_cfscore", "n_employees")

sumstats <- firm_final[, lapply(.SD, function(x) {
  c(N      = sum(!is.na(x)),
    Mean   = round(mean(x, na.rm = TRUE), 3),
    SD     = round(sd(x, na.rm = TRUE), 3),
    P25    = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75    = round(quantile(x, 0.75, na.rm = TRUE), 3),
    Min    = round(min(x, na.rm = TRUE), 3),
    Max    = round(max(x, na.rm = TRUE), 3))
}), .SDcols = vars]

sumstats_t <- data.table(
  Statistic = c("N", "Mean", "SD", "P25", "Median", "P75", "Min", "Max"),
  sumstats
)

cat("\n===============================================\n")
cat("SUMMARY STATISTICS: CEO-Employee Misalignment\n")
cat("===============================================\n")
print(sumstats_t)

# Top/bottom
cat("\n--- Top 15 Most Misaligned ---\n")
print(firm_final[order(-misalign_abs)][1:15,
                                       .(employer_clean, ceo_name, ceo_cfscore, emp_mean_cfscore, misalign_abs, n_employees)])

cat("\n--- Top 15 Most Aligned ---\n")
print(firm_final[order(misalign_abs)][1:15,
                                      .(employer_clean, ceo_name, ceo_cfscore, emp_mean_cfscore, misalign_abs, n_employees)])

# Save
save(firm_final, firm, file = "misalignment_data.rdata")
cat("\nSaved misalignment_data.rdata\n")

# -------------------------------------------------------------------
# Quick cleanup: remove generic/junk employer names
# -------------------------------------------------------------------
junk_firms <- c(
  "SCHOOL", "STUDIO", "FISH", "CARES", "MKTG", "SOCIAL SERVICE",
  "ME MYSELF AND I", "CURRENT EMPLOYEE", "PUBLIC LIBRARY",
  "HOSPITAL", "CHURCH", "UNIVERSITY", "COLLEGE", "CITY",
  "COUNTY", "STATE", "FEDERAL", "US GOVERNMENT", "GOVERNMENT",
  "ARMY", "NAVY", "AIR FORCE", "MARINES", "MILITARY",
  "SCHOOL DISTRICT", "POLICE", "FIRE DEPARTMENT",
  "POST OFFICE", "USPS", "IRS"
)

# Also remove very short names (1-3 chars) — almost always junk
firm_final <- firm_final[!employer_clean %in% junk_firms]
firm_final <- firm_final[nchar(employer_clean) >= 4]
cat("After removing junk firm names:", nrow(firm_final), "\n")

# Check the new top misaligned
cat("\n--- Top 15 Most Misaligned (cleaned) ---\n")
print(firm_final[order(-misalign_abs)][1:15,
                                       .(employer_clean, ceo_name, ceo_cfscore, emp_mean_cfscore, 
                                         misalign_abs, n_employees)])

# -------------------------------------------------------------------
# Regenerate summary stats
# -------------------------------------------------------------------
vars <- c("misalign_abs", "misalign_signed", "misalign_median",
          "ceo_cfscore", "emp_mean_cfscore", "emp_sd_cfscore", "n_employees")

sumstats <- firm_final[, lapply(.SD, function(x) {
  c(N      = sum(!is.na(x)),
    Mean   = round(mean(x, na.rm = TRUE), 3),
    SD     = round(sd(x, na.rm = TRUE), 3),
    P25    = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75    = round(quantile(x, 0.75, na.rm = TRUE), 3))
}), .SDcols = vars]

sumstats_t <- data.table(
  Statistic = c("N", "Mean", "SD", "P25", "Median", "P75"),
  sumstats
)

cat("\n===============================================\n")
cat("SUMMARY STATISTICS (cleaned)\n")
cat("===============================================\n")
print(sumstats_t)

# -------------------------------------------------------------------
# Recognizable firm names — spot check
# -------------------------------------------------------------------
cat("\n--- Sample of recognizable firms ---\n")
big_firms <- firm_final[n_employees >= 500][order(-n_employees)]
print(big_firms[1:30,
                .(employer_clean, ceo_name, ceo_cfscore, emp_mean_cfscore,
                  misalign_abs, n_employees)])

# Save updated
save(firm_final, firm, file = "misalignment_data.rdata")


# -------------------------------------------------------------------
# Remove remaining non-corporate junk
# -------------------------------------------------------------------
more_junk <- c(
  "STATE OF CALIFORNIA", "ATTORNEY", "SELF", "FARMER", "DENTIST",
  "OWNER", "REAL ESTATE", "TEACHER", "DOCTOR", "PHYSICIAN",
  "LAWYER", "NURSE", "ENGINEER", "SALES", "MANAGER",
  "KAISER", "KAISER PERMANENTE", "MAYO CLINIC",
  "YMCA", "YWCA", "UNITED WAY", "PLANNED PARENTHOOD",
  "RED CROSS", "SALVATION ARMY", "GOODWILL",
  "US ARMY", "US NAVY", "US AIR FORCE",
  "USDA", "EPA", "FDA", "NASA", "USPS",
  "PUBLIC SCHOOLS", "SCHOOL SYSTEM"
)

# Also remove anything starting with "STATE OF" or "CITY OF" or "COUNTY OF"
firm_final <- firm_final[!employer_clean %in% more_junk]
firm_final <- firm_final[!grepl("^STATE OF |^CITY OF |^COUNTY OF |^TOWN OF |^DEPARTMENT OF ", 
                                employer_clean)]
cat("After additional cleaning:", nrow(firm_final), "\n")

# -------------------------------------------------------------------
# Quick Compustat merge test
# You need a Compustat extract with company names. If you have one:
# compustat <- fread("your_compustat_file.csv")
# For now, let's just prep the firm names for matching
# -------------------------------------------------------------------

# Save the cleaned firm names for Compustat matching
firm_names_for_matching <- firm_final[, .(
  employer_clean, ceo_name, ceo_cfscore, 
  emp_mean_cfscore, misalign_abs, n_employees
)]

save(firm_final, firm_names_for_matching, file = "misalignment_data.rdata")
cat("Saved updated misalignment_data.rdata\n")

# -------------------------------------------------------------------
# How many look like real public companies? Quick heuristic:
# Large employee donor count + recognizable name
# -------------------------------------------------------------------
cat("\n--- Firms with 100+ employee donors (likely public/large) ---\n")
cat("Count:", firm_final[n_employees >= 100, .N], "\n")
cat("\n--- Firms with 500+ employee donors ---\n")
cat("Count:", firm_final[n_employees >= 500, .N], "\n")



# -------------------------------------------------------------------
# Connect to WRDS and pull Compustat company names
# -------------------------------------------------------------------
install.packages("RPostgres")  # if needed
library(RPostgres)

wrds <- dbConnect(Postgres(),
                  host     = "wrds-pgdata.wharton.upenn.edu",
                  port     = 9737,
                  dbname   = "wrds",
                  user     = "sinadavoudi", 
                  password = rstudioapi::askForPassword("WRDS Password"),
                  sslmode  = "require")

# Pull unique Compustat firm names (annual, North America)
comp_names <- dbGetQuery(wrds, "
  SELECT DISTINCT gvkey, conm 
  FROM comp.funda 
  WHERE indfmt = 'INDL' 
    AND datafmt = 'STD' 
    AND popsrc = 'D' 
    AND consol = 'C'
    AND fyear >= 2000
")

dbDisconnect(wrds)

setDT(comp_names)
cat("Compustat unique firms:", uniqueN(comp_names$gvkey), "\n")

# -------------------------------------------------------------------
# Clean Compustat names the same way we cleaned DIME
# -------------------------------------------------------------------
comp_names[, comp_clean := toupper(trimws(conm))]

suffixes <- c(
  "\\bINC\\.?\\b", "\\bCORP\\.?\\b", "\\bCORPORATION\\b",
  "\\bCO\\.?\\b", "\\bLLC\\b", "\\bLTD\\.?\\b", "\\bLIMITED\\b",
  "\\bLP\\b", "\\bL\\.P\\.\\b", "\\bPLC\\b", "\\bNA\\b",
  "\\bTHE\\b", ",", "\\.", "\\/"
)
for (p in suffixes) {
  comp_names[, comp_clean := gsub(p, " ", comp_clean)]
}
comp_names[, comp_clean := gsub("\\s+", " ", trimws(comp_clean))]

# Keep one row per gvkey (most recent conm)
comp_names <- unique(comp_names, by = "gvkey")
cat("Compustat unique gvkeys:", nrow(comp_names), "\n")

# -------------------------------------------------------------------
# EXACT merge first
# -------------------------------------------------------------------
firm_final[, merge_name := employer_clean]
comp_names[, merge_name := comp_clean]

exact <- merge(firm_final, comp_names[, .(gvkey, conm, merge_name)],
               by = "merge_name", all.x = FALSE, all.y = FALSE)
cat("\nExact matches:", nrow(exact), "\n")

# -------------------------------------------------------------------
# FUZZY merge for the rest
# -------------------------------------------------------------------
install.packages("stringdist")
library(stringdist)

unmatched_dime <- firm_final[!employer_clean %in% exact$employer_clean]
cat("Unmatched DIME firms for fuzzy:", nrow(unmatched_dime), "\n")

# For each unmatched DIME firm, find closest Compustat name
fuzzy_match <- function(dime_names, comp_names_vec, comp_gvkeys, comp_conm, max_dist = 0.15) {
  results <- data.table(
    employer_clean = character(),
    gvkey = character(),
    conm = character(),
    match_distance = numeric()
  )
  
  cat("Fuzzy matching", length(dime_names), "firms...\n")
  
  for (i in seq_along(dime_names)) {
    if (i %% 500 == 0) cat("  ", i, "of", length(dime_names), "\n")
    
    dists <- stringdist(dime_names[i], comp_names_vec, method = "jw")
    best <- which.min(dists)
    
    if (dists[best] <= max_dist) {
      results <- rbind(results, data.table(
        employer_clean = dime_names[i],
        gvkey = comp_gvkeys[best],
        conm = comp_conm[best],
        match_distance = round(dists[best], 4)
      ))
    }
  }
  return(results)
}

fuzzy <- fuzzy_match(
  unmatched_dime$employer_clean,
  comp_names$comp_clean,
  comp_names$gvkey,
  comp_names$conm,
  max_dist = 0.15
)

cat("Fuzzy matches:", nrow(fuzzy), "\n")

# Inspect worst fuzzy matches (most likely to be wrong)
cat("\n--- Worst 20 fuzzy matches (verify these) ---\n")
print(fuzzy[order(-match_distance)][1:20])

# -------------------------------------------------------------------
# Combine exact + fuzzy
# -------------------------------------------------------------------
exact_slim <- exact[, .(employer_clean, gvkey, conm, match_distance = 0)]
all_matches <- rbind(exact_slim, fuzzy)

cat("\n*** TOTAL MATCHED TO COMPUSTAT:", nrow(all_matches), 
    "out of", nrow(firm_final), 
    "(", round(100 * nrow(all_matches)/nrow(firm_final), 1), "%) ***\n")

# Merge back misalignment data
matched_final <- merge(firm_final, all_matches[, .(employer_clean, gvkey, conm, match_distance)],
                       by = "employer_clean", all.x = FALSE)

cat("Final matched sample:", nrow(matched_final), "firms\n")

# Save
save(matched_final, firm_final, all_matches, comp_names, 
     file = "misalignment_compustat_matched.rdata")
cat("Saved misalignment_compustat_matched.rdata\n")

# Quick summary of matched sample
cat("\n--- Summary of matched sample ---\n")
print(matched_final[, .(
  N = .N,
  mean_misalign = round(mean(misalign_abs), 3),
  median_misalign = round(median(misalign_abs), 3),
  mean_n_emp = round(mean(n_employees), 1),
  median_n_emp = median(n_employees)
)])


# -------------------------------------------------------------------
# Clean up fuzzy matches — tighten threshold
# -------------------------------------------------------------------

# The worst fuzzy matches are garbage. Let's keep only distance <= 0.10
fuzzy_tight <- fuzzy[match_distance <= 0.10]
cat("Fuzzy matches at distance <= 0.10:", nrow(fuzzy_tight), "\n")

# Check the worst ones at this tighter threshold
cat("\n--- Worst 20 fuzzy at <= 0.10 ---\n")
print(fuzzy_tight[order(-match_distance)][1:20])

# Recombine
all_matches_v2 <- rbind(exact_slim, fuzzy_tight)
cat("\nTotal matched (tighter):", nrow(all_matches_v2), "\n")

# Rebuild matched sample
matched_final <- merge(firm_final, 
                       all_matches_v2[, .(employer_clean, gvkey, conm, match_distance)],
                       by = "employer_clean", all.x = FALSE)
cat("Final matched sample:", nrow(matched_final), "firms\n")

# -------------------------------------------------------------------
# Summary stats for the matched Compustat sample
# -------------------------------------------------------------------
vars <- c("misalign_abs", "misalign_signed", "misalign_median",
          "ceo_cfscore", "emp_mean_cfscore", "emp_sd_cfscore", "n_employees")

sumstats <- matched_final[, lapply(.SD, function(x) {
  c(N      = sum(!is.na(x)),
    Mean   = round(mean(x, na.rm = TRUE), 3),
    SD     = round(sd(x, na.rm = TRUE), 3),
    P25    = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75    = round(quantile(x, 0.75, na.rm = TRUE), 3))
}), .SDcols = vars]

sumstats_t <- data.table(
  Statistic = c("N", "Mean", "SD", "P25", "Median", "P75"),
  sumstats
)

cat("\n==========================================================\n")
cat("SUMMARY STATS: CEO-Employee Misalignment (Compustat matched)\n")
cat("==========================================================\n")
print(sumstats_t)

# -------------------------------------------------------------------
# Recognizable firms check
# -------------------------------------------------------------------
cat("\n--- Top 30 by employee donors (Compustat matched) ---\n")
print(matched_final[order(-n_employees)][1:30,
                                         .(conm, employer_clean, ceo_name, ceo_cfscore, 
                                           emp_mean_cfscore, misalign_abs, n_employees, match_distance)])

# Save final
save(matched_final, all_matches_v2, file = "misalignment_compustat_final.rdata")
cat("\nSaved misalignment_compustat_final.rdata\n")



# -------------------------------------------------------------------
# Final cleanup
# -------------------------------------------------------------------

# 1. Remove known bad fuzzy matches (generic DIME names that matched wrong)
bad_matches <- c("INVESTOR", "HOME", "SERVICE", "CAPITAL", "CONNECT",
                 "INSIGHT", "CALIBER", "FOOD BANK", "HARRINGTON HOSPITAL",
                 "BELL ATLANTIC", "RED VENTURES", "CONVERSICA",
                 "EARTH", "PARAGON", "KELLOGG")
matched_final <- matched_final[!employer_clean %in% bad_matches]

# 2. Check for ISCO/CISCO type errors — remove where DIME name is very 
#    different from Compustat name and distance > 0.05
cat("Checking fuzzy matches for manual review...\n")
suspicious <- matched_final[match_distance > 0.05]
cat("Fuzzy matches to review:", nrow(suspicious), "\n")
print(suspicious[, .(employer_clean, conm, match_distance, 
                     ceo_name, n_employees)][order(-match_distance)])

# 3. Deduplicate: one row per employer_clean (keep lowest match_distance)
matched_final <- matched_final[order(match_distance)]
matched_final <- unique(matched_final, by = "employer_clean")
cat("\nAfter dedup by employer:", nrow(matched_final), "\n")

# -------------------------------------------------------------------
# Final summary stats table (the NS deliverable)
# -------------------------------------------------------------------
vars <- c("misalign_abs", "misalign_signed", "misalign_median",
          "ceo_cfscore", "emp_mean_cfscore", "emp_sd_cfscore", "n_employees")

sumstats <- matched_final[, lapply(.SD, function(x) {
  c(N      = sum(!is.na(x)),
    Mean   = round(mean(x, na.rm = TRUE), 3),
    SD     = round(sd(x, na.rm = TRUE), 3),
    P25    = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75    = round(quantile(x, 0.75, na.rm = TRUE), 3))
}), .SDcols = vars]

sumstats_t <- data.table(
  Statistic = c("N", "Mean", "SD", "P25", "Median", "P75"),
  sumstats
)

cat("\n==========================================================\n")
cat("TABLE 1: Summary Statistics — CEO-Employee Political Misalignment\n")
cat("Cross-sectional sample of Compustat firms with identifiable\n")
cat("CEO and employee donors in DIME (2000-2024)\n")
cat("==========================================================\n")
print(sumstats_t)

# Save final clean version
save(matched_final, sumstats_t, file = "misalignment_compustat_final.rdata")
cat("\nSaved.\n")

# -------------------------------------------------------------------
# Manual corrections for known duplicates / mismatches
# -------------------------------------------------------------------

# CISCO: "CISCO" donors belong with "CISCO SYSTEMS"
# Remove the bad ISCO match, merge CISCO donors into CISCO SYSTEMS
matched_final <- matched_final[!(employer_clean == "CISCO" & conm == "ISCO INC")]

# Check: how many similar duplicates exist?
# Look for employer_clean values that are substrings of other employer_clean values
cat("--- Potential duplicates (short name is substring of longer) ---\n")
emp_names <- sort(unique(matched_final$employer_clean))
for (i in seq_along(emp_names)) {
  short <- emp_names[i]
  if (nchar(short) <= 10) {
    longer <- emp_names[grepl(paste0("\\b", short, "\\b"), emp_names) & emp_names != short]
    if (length(longer) > 0) {
      cat(short, " --> ", paste(longer, collapse = ", "), "\n")
    }
  }
}



cat("Total rows:", nrow(matched_final), "\n")
cat("Unique employer_clean:", uniqueN(matched_final$employer_clean), "\n")
cat("Unique gvkey:", uniqueN(matched_final$gvkey), "\n")

# Which employers appear more than once?
dupes <- matched_final[, .N, by = employer_clean][N > 1]
cat("\nDuplicate employers:", nrow(dupes), "\n")
if (nrow(dupes) > 0) print(matched_final[employer_clean %in% dupes$employer_clean, 
                                         .(employer_clean, gvkey, conm, match_distance)])


# Remove remaining generic single-word employer names
generic <- c("BANK", "FINANCIAL", "HEALTHCARE", "SERVICES", "MEDIA",
             "NEWS", "SOUTHERN", "TECHNOLOGY", "COMPASS")
matched_final <- matched_final[!employer_clean %in% generic]
cat("After removing generics:", nrow(matched_final), "\n")

# Save final
save(matched_final, sumstats_t, file = "misalignment_compustat_final.rdata")



# -------------------------------------------------------------------
# 1. Distribution diagnostics
# -------------------------------------------------------------------
cat("=== Panel A: Full distribution ===\n")
cat("Skewness of misalign_abs:", round(mean(((matched_final$misalign_abs - mean(matched_final$misalign_abs)) / sd(matched_final$misalign_abs))^3), 3), "\n")

# Decile breakdown
matched_final[, decile := cut(misalign_abs, 
                              breaks = quantile(misalign_abs, probs = seq(0, 1, 0.1)), 
                              include.lowest = TRUE, labels = paste0("D", 1:10))]

cat("\n=== Misalignment by decile ===\n")
print(matched_final[, .(
  n = .N,
  mean_misalign = round(mean(misalign_abs), 3),
  mean_ceo_cf = round(mean(ceo_cfscore), 3),
  mean_emp_cf = round(mean(emp_mean_cfscore), 3),
  mean_n_emp = round(mean(n_employees), 0),
  median_n_emp = as.double(median(n_employees))
), by = decile][order(decile)])

# -------------------------------------------------------------------
# 2. CEO vs employee ideology breakdown
# -------------------------------------------------------------------
cat("\n=== CEO ideology distribution ===\n")
cat("Liberal CEOs (cfscore < -0.5):", matched_final[ceo_cfscore < -0.5, .N], "\n")
cat("Moderate CEOs (-0.5 to 0.5):", matched_final[ceo_cfscore >= -0.5 & ceo_cfscore <= 0.5, .N], "\n")
cat("Conservative CEOs (cfscore > 0.5):", matched_final[ceo_cfscore > 0.5, .N], "\n")

cat("\n=== Employee mean ideology distribution ===\n")
cat("Liberal workforce (< -0.5):", matched_final[emp_mean_cfscore < -0.5, .N], "\n")
cat("Moderate workforce (-0.5 to 0.5):", matched_final[emp_mean_cfscore >= -0.5 & emp_mean_cfscore <= 0.5, .N], "\n")
cat("Conservative workforce (> 0.5):", matched_final[emp_mean_cfscore > 0.5, .N], "\n")

# -------------------------------------------------------------------
# 3. Direction of misalignment
# -------------------------------------------------------------------
cat("\n=== Direction ===\n")
cat("CEO more conservative than employees (signed > 0):", matched_final[misalign_signed > 0, .N], 
    "(", round(100 * matched_final[misalign_signed > 0, .N] / nrow(matched_final), 1), "%)\n")
cat("CEO more liberal than employees (signed < 0):", matched_final[misalign_signed < 0, .N],
    "(", round(100 * matched_final[misalign_signed < 0, .N] / nrow(matched_final), 1), "%)\n")

# -------------------------------------------------------------------
# 4. Correlation matrix
# -------------------------------------------------------------------
cat("\n=== Correlations ===\n")
cor_vars <- c("misalign_abs", "ceo_cfscore", "emp_mean_cfscore", 
              "emp_sd_cfscore", "n_employees")
cor_mat <- round(cor(matched_final[, ..cor_vars], use = "complete.obs"), 3)
print(cor_mat)

# -------------------------------------------------------------------
# 5. Industry breakdown (need SIC from Compustat)
# Pull SIC codes for our gvkeys
# -------------------------------------------------------------------
cat("\n=== gvkeys for SIC lookup (paste into WRDS query) ===\n")
cat("Unique gvkeys:", uniqueN(matched_final$gvkey), "\n")

# Save gvkeys for WRDS pull
gvkeys_list <- unique(matched_final$gvkey)
save(gvkeys_list, file = "gvkeys_for_sic.rdata")

# -------------------------------------------------------------------
# 6. Histogram data (for LaTeX pgfplots)
# -------------------------------------------------------------------
cat("\n=== Histogram bins for misalign_abs ===\n")
h <- hist(matched_final$misalign_abs, breaks = 30, plot = FALSE)
hist_data <- data.table(midpoint = h$mids, count = h$counts, density = round(h$density, 4))
print(hist_data)

cat("\n=== Histogram bins for ceo_cfscore ===\n")
h2 <- hist(matched_final$ceo_cfscore, breaks = 30, plot = FALSE)
hist_data2 <- data.table(midpoint = h2$mids, count = h2$counts, density = round(h2$density, 4))
print(hist_data2)

cat("\n=== Histogram bins for emp_mean_cfscore ===\n")
h3 <- hist(matched_final$emp_mean_cfscore, breaks = 30, plot = FALSE)
hist_data3 <- data.table(midpoint = h3$mids, count = h3$counts, density = round(h3$density, 4))
print(hist_data3)

# -------------------------------------------------------------------
# 7. Scatter data: CEO cfscore vs employee mean cfscore
# -------------------------------------------------------------------
cat("\n=== Scatter: CEO vs Employee ideology (sample of points) ===\n")
set.seed(42)
scatter_sample <- matched_final[sample(.N, min(200, .N)), 
                                .(ceo_cfscore, emp_mean_cfscore, n_employees, misalign_abs)]
cat("Points for scatter:", nrow(scatter_sample), "\n")

# Also get the top recognizable firms for labeling
cat("\n=== Notable firms for scatter labels ===\n")
notable <- matched_final[conm %in% c(
  "MICROSOFT CORP", "BOEING CO", "WELLS FARGO & CO", "WALMART INC",
  "AT&T INC", "CISCO SYSTEMS INC", "GENERAL MOTORS CO", "FORD MOTOR CO",
  "COMCAST CORP", "BEST BUY CO INC", "KROGER CO", "FEDEX CORP",
  "CATERPILLAR INC", "MEDTRONIC PLC", "HUMANA INC",
  "SOUTHWEST AIRLINES", "UNITED AIRLINES INC", "AMERICAN AIRLINES INC",
  "HONEYWELL INTERNATIONAL INC", "RAYTHEON CO"
)]
print(notable[, .(conm, ceo_cfscore, emp_mean_cfscore, misalign_abs, n_employees)])
library(data.table)
library(fixest)
library(stringdist)
setFixest_notes(FALSE)

# ═══════════════════════════════════════════════
# 1. LOAD AND PARSE WARN DATA
# ═══════════════════════════════════════════════
warn <- fread("/Users/homemac/Desktop/WARN Database Master, Excluding 2026 - Sheet 1.csv")
cat("WARN raw:", nrow(warn), "rows\n")
cat("Columns:", paste(names(warn), collapse=", "), "\n\n")

# Parse date -> year
warn[, warn_date := as.Date(`WARN Received Date`, format="%m/%d/%Y")]
warn[, year := year(warn_date)]

# Parse workers to numeric (remove commas, handle ranges)
warn[, workers_raw := `Number of Workers`]
warn[, workers := as.numeric(gsub("[^0-9.]", "", gsub(",", "", workers_raw)))]

cat("Year range:", range(warn$year, na.rm=TRUE), "\n")
cat("Workers parsed:", sum(!is.na(warn$workers)), "of", nrow(warn), "\n")
cat("Year distribution (2000+):\n")
print(warn[year >= 2000, .N, by=year][order(year)])

# ═══════════════════════════════════════════════
# 2. CLEAN WARN COMPANY NAMES
# ═══════════════════════════════════════════════
# Strip facility suffixes first: remove after "(", ":", or " - "
warn[, company_raw := Company]
warn[, company_clean := company_raw]
warn[, company_clean := sub("\\s*\\(.*$", "", company_clean)]      # after (
warn[, company_clean := sub("\\s*:.*$", "", company_clean)]         # after :
warn[, company_clean := sub("\\s+- .*$", "", company_clean)]        # after " - "

# Standard cleaning per PROJECT_CONTEXT.md
clean_employer <- function(x) {
  x <- toupper(trimws(x))
  suffixes <- c("\\bINC\\.?\\b", "\\bCORP\\.?\\b", "\\bCORPORATION\\b",
                "\\bCO\\.?\\b", "\\bLLC\\b", "\\bLTD\\.?\\b", "\\bLIMITED\\b",
                "\\bLP\\b", "\\bL\\.P\\.\\b", "\\bPLC\\b", "\\bNA\\b",
                "\\bTHE\\b", ",", "\\.", "\\/")
  for (p in suffixes) x <- gsub(p, " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

warn[, company_clean := clean_employer(company_clean)]
warn <- warn[!is.na(year) & !is.na(workers) & workers > 0]
cat("\nAfter cleaning:", nrow(warn), "usable WARN notices\n")
cat("Unique company names:", uniqueN(warn$company_clean), "\n")

# ═══════════════════════════════════════════════
# 3. BUILD LOOKUP FROM COMPUSTAT + EMPLOYER PANEL
# ═══════════════════════════════════════════════

# Compustat names
load("compustat_outcomes.rdata")
cs <- as.data.table(compustat)
rm(compustat)
cs_names <- unique(cs[, .(gvkey, conm)])[!is.na(conm)]
cs_names[, name_clean := clean_employer(conm)]
cs_lookup <- unique(cs_names[, .(gvkey, name_clean)])
rm(cs); gc()

# Employer lookup (has employer_clean -> gvkey)
load("employer_lookup_for_panel.rdata")
emp_lookup <- as.data.table(all_exact)
if (!is.data.table(emp_lookup)) emp_lookup <- as.data.table(emp_lookup)
cat("Employer lookup:", nrow(emp_lookup), "rows, cols:", paste(names(emp_lookup), collapse=", "), "\n")

# Combine: get name_clean -> gvkey from both sources
if ("employer_clean" %in% names(emp_lookup)) {
  emp_lu <- unique(emp_lookup[, .(gvkey, name_clean = employer_clean)])
} else {
  emp_lu <- unique(emp_lookup[, .(gvkey, name_clean = clean_employer(get(names(emp_lookup)[2])))])
}
lookup <- unique(rbind(cs_lookup, emp_lu))
lookup <- lookup[!is.na(name_clean) & name_clean != ""]
cat("Combined lookup:", nrow(lookup), "unique name-gvkey pairs\n")

# ═══════════════════════════════════════════════
# 4. MATCHING
# ═══════════════════════════════════════════════

# Exact match
warn_names <- unique(warn[, .(company_clean)])
warn_names <- merge(warn_names, lookup, by.x="company_clean", by.y="name_clean", all.x=TRUE)

# Stats
n_exact <- sum(!is.na(warn_names$gvkey))
cat("\nExact match:", n_exact, "of", nrow(warn_names), "unique names (",
    round(100*n_exact/nrow(warn_names),1), "%)\n")

# Fuzzy match for unmatched with >= 50 workers (aggregate first to get big companies)
unmatched_names <- warn_names[is.na(gvkey), company_clean]
# Focus on companies with big layoffs
big_unmatched <- warn[company_clean %in% unmatched_names & workers >= 50, 
                      .(total_workers = sum(workers)), by=company_clean][order(-total_workers)]
cat("Unmatched companies with >=50 workers:", nrow(big_unmatched), "\n")

# Fuzzy match top unmatched against lookup
lookup_names <- unique(lookup$name_clean)
fuzzy_results <- list()

cat("Running fuzzy matching (Jaro-Winkler, max dist 0.08)...\n")
# Process in chunks for memory
chunk_size <- 500
n_chunks <- ceiling(nrow(big_unmatched) / chunk_size)

for (i in 1:n_chunks) {
  idx <- ((i-1)*chunk_size + 1):min(i*chunk_size, nrow(big_unmatched))
  batch <- big_unmatched$company_clean[idx]
  
  for (nm in batch) {
    dists <- stringdist(nm, lookup_names, method="jw")
    best_idx <- which.min(dists)
    best_dist <- dists[best_idx]
    if (best_dist <= 0.08) {
      best_match <- lookup_names[best_idx]
      gvk <- lookup[name_clean == best_match, gvkey[1]]
      fuzzy_results[[nm]] <- data.table(company_clean=nm, gvkey=gvk, 
                                         match_name=best_match, dist=best_dist)
    }
  }
}

fuzzy_dt <- rbindlist(fuzzy_results)
cat("Fuzzy matches found:", nrow(fuzzy_dt), "\n")
if (nrow(fuzzy_dt) > 0) {
  cat("Sample fuzzy matches:\n")
  print(head(fuzzy_dt[order(dist)], 20))
}

# Combine matches
all_matches <- rbind(
  warn_names[!is.na(gvkey), .(company_clean, gvkey)],
  fuzzy_dt[, .(company_clean, gvkey)]
)
all_matches <- unique(all_matches)

# Merge back to warn
warn <- merge(warn, all_matches, by="company_clean", all.x=TRUE)
cat("\nWARN notices matched:", sum(!is.na(warn$gvkey)), "of", nrow(warn),
    "(", round(100*mean(!is.na(warn$gvkey)),1), "%)\n")
cat("Matched workers:", format(sum(warn[!is.na(gvkey), workers]), big.mark=","), "of",
    format(sum(warn$workers), big.mark=","), "\n")

# ═══════════════════════════════════════════════
# 5. AGGREGATE PER GVKEY × YEAR
# ═══════════════════════════════════════════════
warn_agg <- warn[!is.na(gvkey), .(
  warn_any = 1L,
  warn_workers = sum(workers, na.rm=TRUE),
  warn_events = .N
), by=.(gvkey, year)]

cat("\n=== WARN AGGREGATES ===\n")
cat("Firm-years with WARN:", nrow(warn_agg), "\n")
cat("warn_events distribution:\n"); print(summary(warn_agg$warn_events))
cat("warn_workers distribution:\n"); print(summary(warn_agg$warn_workers))

# Top 20 firms
top20 <- warn_agg[, .(total_workers = sum(warn_workers), total_events = sum(warn_events)), 
                  by=gvkey][order(-total_workers)][1:20]
# Get firm names
load("compustat_outcomes.rdata")
cs <- as.data.table(get("compustat"))
cs_names2 <- unique(cs[, .(gvkey, conm)])
top20 <- merge(top20, cs_names2, by="gvkey", all.x=TRUE)
# Keep only first conm per gvkey
top20 <- top20[, .SD[1], by=gvkey][order(-total_workers)]
cat("\nTop 20 firms by WARN workers:\n")
print(top20[, .(conm, total_workers, total_events)])
rm(cs); gc()

# ═══════════════════════════════════════════════
# 6. MERGE TO REG_DATA
# ═══════════════════════════════════════════════
load("reg_data_v2.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")

# Remove old warn cols if present
for (col in c("warn_any","warn_workers","warn_events")) {
  if (col %in% names(reg_data)) reg_data[, (col) := NULL]
}

reg_data <- merge(reg_data, warn_agg[, .(gvkey, fyear=year, warn_any, warn_workers, warn_events)],
                  by=c("gvkey","fyear"), all.x=TRUE)

# Unmatched = 0
reg_data[is.na(warn_any), warn_any := 0L]
reg_data[is.na(warn_workers), warn_workers := 0L]
reg_data[is.na(warn_events), warn_events := 0L]

cat("\n=== WARN IN REG_DATA ===\n")
cat("warn_any = 1:", sum(reg_data$warn_any), "of", nrow(reg_data),
    "(", round(100*mean(reg_data$warn_any),1), "%)\n")
cat("warn_workers (conditional):", round(mean(reg_data[warn_any==1, warn_workers]),0), "mean,",
    round(median(reg_data[warn_any==1, warn_workers]),0), "median\n")

# ═══════════════════════════════════════════════
# 7. PRINT SUMMARIES
# ═══════════════════════════════════════════════
cat("\nwarn_events distribution in reg_data:\n")
print(table(reg_data$warn_events))

cat("\nwarn_any by cycle:\n")
print(reg_data[, .(N=.N, warn_pct=round(100*mean(warn_any),1), 
                   mean_workers=round(mean(warn_workers),0)), by=cycle][order(cycle)])

# ═══════════════════════════════════════════════
# 8. QUICK REGRESSION
# ═══════════════════════════════════════════════
rd <- reg_data[!sic2 %in% c(49, 60:69)]
rd[, recession := nber_recession]
rd[, lag_aqc_indicator := lag_acquisition]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

W1 <- feols(as.formula(paste("warn_any ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

W2 <- feols(as.formula(paste("warn_any ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("WARN REGRESSIONS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(W1, W2, 
       headers=c("W1:WARN×Recession","W2:WARN×PeerLayoff"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# ═══════════════════════════════════════════════
# SAVE
# ═══════════════════════════════════════════════
setnames(reg_data, "fyear", "fyear_lead")
save(reg_data, file="reg_data_v3.rdata")
cat("\nSaved reg_data_v3.rdata:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")

warn_lookup <- all_matches
save(warn_lookup, file="warn_match_lookup.rdata")
cat("Saved warn_match_lookup.rdata:", nrow(warn_lookup), "name-gvkey pairs\n")

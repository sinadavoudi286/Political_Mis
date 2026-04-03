library(data.table)

# Start fresh from the saved v2
load("reg_data_v2.rdata")
cat("Starting:", nrow(reg_data), "rows\n")

# Remove bad cols
reg_data[, c("ceo_turnover", "first_year_ceo", "quadrant") := NULL]

# Identify exact dupe rows — rows that are identical across ALL remaining columns
# First check what makes them different
dupe_keys <- reg_data[, .N, by = .(gvkey, fyear_lead)][N > 1]
cat("Dupe gvkey-fyear pairs:", nrow(dupe_keys), "\n")

# Look at one example more carefully
ex <- reg_data[gvkey == "004194" & fyear_lead == 2013]
cat("\nExample 004194/2013 — all cols equal?", all(ex[1, ] == ex[2, ], na.rm=TRUE), "\n")
# Find differing columns
diffs <- sapply(names(ex), function(col) !identical(ex[[col]][1], ex[[col]][2]))
cat("Differing cols:", names(ex)[diffs], "\n")

# Just deduplicate: for each gvkey-fyear, keep one row
# Priority: non-missing ceo_cfscore, then first
reg_data[, has_cfs := as.integer(!is.na(ceo_cfscore))]
setorder(reg_data, gvkey, fyear_lead, -has_cfs)
reg_data <- reg_data[!duplicated(reg_data[, .(gvkey, fyear_lead)])]
reg_data[, has_cfs := NULL]
cat("After dedup:", nrow(reg_data), "\n\n")

# Now compute CEO turnover properly from execucomp
load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)

# For each gvkey-year, who was the CEO? If multiple, keep all.
# We want: for a given gvkey at year t, was the CEO (execid) different from year t-1?
ec_slim <- unique(ec[, .(gvkey, year, execid)])

# For each row in reg_data: look up if the same execid was CEO at fyear_lead - 1
# Create a lookup of all gvkey-year-execid combos
ec_slim[, key_prev := paste(gvkey, year + 1, execid, sep = "_")]
prev_lookup <- ec_slim$key_prev

reg_data[, match_key := paste(gvkey, fyear_lead, execid, sep = "_")]
# If this CEO was also CEO the prior year, no turnover
reg_data[, was_ceo_prior_yr := match_key %in% prev_lookup]
reg_data[, ceo_turnover := as.integer(!was_ceo_prior_yr)]

# But: if there's no execucomp data for year t-1 at all, set to NA
ec_gvkey_yrs <- unique(ec_slim[, .(gvkey, year)])
ec_gvkey_yrs[, has_prior := paste(gvkey, year + 1, sep = "_")]
prior_exists <- ec_gvkey_yrs$has_prior
reg_data[, prior_key := paste(gvkey, fyear_lead, sep = "_")]
reg_data[!prior_key %in% prior_exists, ceo_turnover := NA_integer_]

# First year CEO: is this the first year this execid appears as CEO for this gvkey?
ec_first <- ec_slim[, .(first_yr = min(year)), by = .(gvkey, execid)]
reg_data <- merge(reg_data, ec_first, by = c("gvkey", "execid"), all.x = TRUE)
reg_data[, first_year_ceo := as.integer(fyear_lead == first_yr)]
reg_data[is.na(first_yr), first_year_ceo := NA_integer_]

# Clean up temp cols
reg_data[, c("match_key", "was_ceo_prior_yr", "prior_key", "first_yr") := NULL]

# Recreate quadrant
reg_data[, quadrant := fcase(
  cross_ceo_right == 1, "Cross: CEO Right / Emp Left",
  cross_ceo_left == 1,  "Cross: CEO Left / Emp Right",
  same_side_right == 1, "Same side: CEO more Right",
  same_side_left == 1,  "Same side: CEO more Left",
  default = "Exact tie or NA"
)]

# Final checks
cat("=== FINAL ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("gvkey+fyear dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n\n")
cat("CEO turnover:\n"); print(table(reg_data$ceo_turnover, useNA="ifany"))
cat("\nFirst year CEO:\n"); print(table(reg_data$first_year_ceo, useNA="ifany"))

save(reg_data, file = "reg_data_v2.rdata")
cat("\nSaved reg_data_v2.rdata\n")

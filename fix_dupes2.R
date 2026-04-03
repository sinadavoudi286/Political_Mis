library(data.table)
load("reg_data_v2.rdata")
cat("Starting rows:", nrow(reg_data), "\n")

# Drop the turnover cols from the bad merge and any exact-dupe rows
reg_data[, c("ceo_turnover", "first_year_ceo", "quadrant") := NULL]

# Remove exact duplicate rows introduced by the merge
reg_data <- unique(reg_data)
cat("After unique():", nrow(reg_data), "\n")

# Check remaining dupes
reg_data[, n := .N, by = .(gvkey, fyear_lead)]
cat("Remaining dupe pairs:", sum(reg_data$n > 1) / 2, "\n")
if (any(reg_data$n > 1)) {
  cat("Example:\n")
  print(reg_data[n > 1, .(gvkey, fyear_lead, cycle, execid, exec_fullname, ceo_cfscore)][1:6])
}
reg_data[, n := NULL]

# These remaining dupes are from the execucomp merge bringing in multiple CEOs
# per gvkey-year. Deduplicate: prefer non-missing ceo_cfscore, then first row
setorder(reg_data, gvkey, fyear_lead, -abs(ceo_cfscore))  # non-NA sorts before NA
reg_data <- reg_data[, .SD[1], by = .(gvkey, fyear_lead)]
cat("After dedup:", nrow(reg_data), "\n")

# Now redo CEO turnover properly, matching on execid
load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)

# One row per gvkey-year-execid
ec <- unique(ec[, .(gvkey, year, execid)])
setorder(ec, gvkey, year)

# For each gvkey-year, check if execid changed from prior year
# We need the CEO in the prior year for the SAME gvkey
# Get the "main" CEO per gvkey-year (could be multiple), take the one that was also CEO prior year
ec[, prior_execid := shift(execid, 1), by = gvkey]
ec[, prior_year := shift(year, 1), by = gvkey]
# Only valid if prior year is actually year-1
ec[prior_year != year - 1, prior_execid := NA]
ec[, ceo_turnover := as.integer(!is.na(prior_execid) & execid != prior_execid)]

# First year of this CEO at this firm
ec[, first_year_ceo := as.integer(year == min(year)), by = .(gvkey, execid)]

# Now merge matching on all three keys
turnover <- ec[, .(gvkey, fyear_lead = year, execid, ceo_turnover, first_year_ceo)]
# reg_data$execid might be numeric or character - align types
turnover[, execid := as.character(execid)]
reg_data[, execid := as.character(execid)]

reg_data <- merge(reg_data, turnover, by = c("gvkey", "fyear_lead", "execid"), all.x = TRUE)
cat("After turnover re-merge:", nrow(reg_data), "\n")

# Verify no new dupes
reg_data[, n := .N, by = .(gvkey, fyear_lead)]
cat("Dupe check:", sum(reg_data$n > 1), "rows in duplicate pairs\n")
reg_data[, n := NULL]

# Recreate quadrant label
reg_data[, quadrant := fcase(
  cross_ceo_right == 1, "Cross: CEO Right / Emp Left",
  cross_ceo_left == 1,  "Cross: CEO Left / Emp Right",
  same_side_right == 1, "Same side: CEO more Right",
  same_side_left == 1,  "Same side: CEO more Left",
  default = "Exact tie or NA"
)]

# Re-save
save(reg_data, file = "reg_data_v2.rdata")
cat("\n=== FINAL ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("Remaining gvkey+fyear dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n")
cat("CEO turnover freq:\n")
print(table(reg_data$ceo_turnover, useNA = "ifany"))
cat("First year CEO freq:\n")
print(table(reg_data$first_year_ceo, useNA = "ifany"))

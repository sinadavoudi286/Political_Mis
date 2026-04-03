library(data.table)
load("reg_data_v2.rdata")
cat("Starting rows:", nrow(reg_data), "\n")

# Drop the bad turnover cols and quadrant
reg_data[, c("ceo_turnover", "first_year_ceo", "quadrant") := NULL]

# The dupes are from the execucomp turnover merge. Some are exact dupes 
# (same execid appearing twice), some are different CEOs in same firm-year.
# Since unique() didn't help, the rows differ in turnover cols (now removed).
# Check again:
reg_data <- unique(reg_data)
cat("After unique():", nrow(reg_data), "\n")

# Check remaining
reg_data[, n := .N, by = .(gvkey, fyear_lead)]
cat("Still duped:", sum(reg_data$n > 1), "rows\n")
if (any(reg_data$n > 1)) {
  # These must be different execids for same gvkey-fyear (multiple CEOs)
  cat("Different execids:\n")
  print(reg_data[n > 1, .(gvkey, fyear_lead, execid, exec_fullname, ceo_cfscore)][1:8])
}
reg_data[, n := NULL]

# Deduplicate: prefer non-missing ceo_cfscore, then first row
reg_data[, has_cfscore := as.integer(!is.na(ceo_cfscore))]
setorder(reg_data, gvkey, fyear_lead, -has_cfscore)
reg_data <- reg_data[, .SD[1], by = .(gvkey, fyear_lead)]
reg_data[, has_cfscore := NULL]
cat("After dedup:", nrow(reg_data), "\n")

# Redo CEO turnover properly, matching on execid too
load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)
ec <- unique(ec[, .(gvkey, year, execid)])
setorder(ec, gvkey, year)
ec[, prior_execid := shift(execid, 1), by = gvkey]
ec[, prior_year := shift(year, 1), by = gvkey]
ec[prior_year != year - 1, prior_execid := NA]
ec[, ceo_turnover := as.integer(!is.na(prior_execid) & execid != prior_execid)]
ec[, first_year_ceo := as.integer(year == min(year)), by = .(gvkey, execid)]

turnover <- ec[, .(gvkey, fyear_lead = year, execid, ceo_turnover, first_year_ceo)]
turnover[, execid := as.character(execid)]
reg_data[, execid := as.character(execid)]

reg_data <- merge(reg_data, turnover, by = c("gvkey", "fyear_lead", "execid"), all.x = TRUE)
cat("After turnover merge:", nrow(reg_data), "\n")

# Final dupe check
cat("Remaining gvkey+fyear dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n")

# Recreate quadrant
reg_data[, quadrant := fcase(
  cross_ceo_right == 1, "Cross: CEO Right / Emp Left",
  cross_ceo_left == 1,  "Cross: CEO Left / Emp Right",
  same_side_right == 1, "Same side: CEO more Right",
  same_side_left == 1,  "Same side: CEO more Left",
  default = "Exact tie or NA"
)]

save(reg_data, file = "reg_data_v2.rdata")
cat("\n=== FINAL ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("CEO turnover:\n"); print(table(reg_data$ceo_turnover, useNA="ifany"))
cat("First year CEO:\n"); print(table(reg_data$first_year_ceo, useNA="ifany"))

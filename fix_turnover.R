library(data.table)
load("reg_data_v2.rdata")
cat("Rows:", nrow(reg_data), "\n")

# Drop bad turnover cols
reg_data[, c("ceo_turnover", "first_year_ceo") := NULL]

load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)
ec <- unique(ec[, .(gvkey, year, execid)])
setorder(ec, gvkey, year)

# Diagnostic: how many CEO transitions in execucomp?
ec[, prev_exec := shift(execid, 1), by = gvkey]
ec[, prev_yr := shift(year, 1), by = gvkey]
ec[, is_transition := (!is.na(prev_exec) & execid != prev_exec & prev_yr == year - 1)]
cat("ExecuComp transitions (consecutive years):", sum(ec$is_transition, na.rm=TRUE), "\n")

# For each gvkey-year in reg_data, determine if the CEO is different from prior year
# Strategy: build a lookup of who was CEO at each gvkey-year
# If multiple CEOs in a year, keep all
ceo_lookup <- ec[, .(gvkey, year, execid)]

# For each reg_data row, check if this CEO's execid was CEO at fyear_lead - 1
reg_data[, execid := as.character(execid)]
ceo_lookup[, execid := as.character(execid)]

# Method: merge reg_data with ceo_lookup on (gvkey, fyear_lead-1) to get prior year CEOs
reg_data[, prior_yr := fyear_lead - 1]
prior_ceos <- ceo_lookup[, .(gvkey, prior_yr = year, prior_execid = execid)]

# For each reg_data row, check if ANY prior year CEO matches current execid
# Also check if the firm had ANY CEO data in prior year
reg_data[, rid := .I]
check <- merge(reg_data[, .(rid, gvkey, prior_yr, execid)], 
               prior_ceos, by = c("gvkey", "prior_yr"), all.x = TRUE, allow.cartesian = TRUE)

# For each rid: was there a match?
check[, same_ceo := (execid == prior_execid)]
result <- check[, .(
  had_prior_data = any(!is.na(prior_execid)),
  was_ceo_prior  = any(same_ceo, na.rm = TRUE)
), by = rid]

reg_data <- merge(reg_data, result, by = "rid", all.x = TRUE)
reg_data[, ceo_turnover := fifelse(!had_prior_data, NA_integer_, as.integer(!was_ceo_prior))]
reg_data[, c("rid", "prior_yr", "had_prior_data", "was_ceo_prior") := NULL]

cat("CEO turnover:\n"); print(table(reg_data$ceo_turnover, useNA="ifany"))

# First year CEO
first_yr <- ec[, .(first_yr_ec = min(year)), by = .(gvkey, execid)]
first_yr[, execid := as.character(execid)]
reg_data <- merge(reg_data, first_yr, by = c("gvkey", "execid"), all.x = TRUE)
reg_data[, first_year_ceo := as.integer(fyear_lead == first_yr_ec)]
reg_data[is.na(first_yr_ec), first_year_ceo := NA_integer_]
reg_data[, first_yr_ec := NULL]

cat("First year CEO:\n"); print(table(reg_data$first_year_ceo, useNA="ifany"))

# Verify no dupes
cat("\ngvkey+fyear dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n")

save(reg_data, file = "reg_data_v2.rdata")
cat("\n=== Saved reg_data_v2.rdata ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")

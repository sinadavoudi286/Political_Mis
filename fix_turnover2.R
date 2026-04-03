library(data.table)
load("reg_data_v2.rdata")
reg_data[, c("ceo_turnover", "first_year_ceo") := NULL]

load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)
ec[, execid := as.character(execid)]

# For each gvkey-year, get the set of CEO execids
# A CEO turnover at year t means the CEO(s) in year t differ from year t-1
# Simplify: get the "primary" CEO per gvkey-year (first listed, or longest tenure)
# Actually: just get all CEOs per year, then compare sets

# For each gvkey-year, collect execids as a comma-separated sorted string
ceo_set <- ec[, .(ceo_ids = paste(sort(unique(execid)), collapse=",")), by = .(gvkey, year)]
setorder(ceo_set, gvkey, year)
ceo_set[, prev_ceo_ids := shift(ceo_ids, 1), by = gvkey]
ceo_set[, prev_year := shift(year, 1), by = gvkey]

# Turnover = CEO set changed from prior consecutive year
ceo_set[, turnover := as.integer(prev_year == year - 1 & ceo_ids != prev_ceo_ids)]
ceo_set[prev_year != year - 1 | is.na(prev_year), turnover := NA_integer_]

cat("Firm-year level turnovers in ExecuComp:\n")
print(table(ceo_set$turnover, useNA="ifany"))

# Now merge to reg_data
turnover_dt <- ceo_set[, .(gvkey, fyear_lead = year, ceo_turnover = turnover)]
reg_data <- merge(reg_data, turnover_dt, by = c("gvkey", "fyear_lead"), all.x = TRUE)

cat("\nCEO turnover in reg_data:\n")
print(table(reg_data$ceo_turnover, useNA="ifany"))

# First year CEO: is the execid in reg_data in their first year at this firm?
reg_data[, execid := as.character(execid)]
first_yr <- ec[, .(first_yr_ec = min(year)), by = .(gvkey, execid)]
reg_data <- merge(reg_data, first_yr, by = c("gvkey", "execid"), all.x = TRUE)
reg_data[, first_year_ceo := as.integer(fyear_lead == first_yr_ec)]
reg_data[is.na(first_yr_ec), first_year_ceo := NA_integer_]
reg_data[, first_yr_ec := NULL]

cat("\nFirst year CEO:\n")
print(table(reg_data$first_year_ceo, useNA="ifany"))

# Cross-check: turnovers should correlate with first_year_ceo
cat("\nTurnover × first_year_ceo:\n")
print(table(reg_data$ceo_turnover, reg_data$first_year_ceo, useNA="ifany"))

# Final checks
cat("\n=== FINAL ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("gvkey+fyear dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n")

save(reg_data, file = "reg_data_v2.rdata")
cat("Saved reg_data_v2.rdata\n")

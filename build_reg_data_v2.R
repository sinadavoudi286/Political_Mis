library(data.table)

# ── 1. Load reg_data and inspect ──
load("reg_data.rdata")
cat("=== reg_data loaded ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n\n")
cat("Column names:\n")
print(names(reg_data))
cat("\n")

# ── 2a. Quadrant indicators ──
reg_data[, cross_ceo_right := as.integer(ceo_cfscore > 0 & emp_mean_cfscore < 0)]
reg_data[, cross_ceo_left  := as.integer(ceo_cfscore < 0 & emp_mean_cfscore > 0)]
reg_data[, same_side_right := as.integer(ceo_cfscore > emp_mean_cfscore & cross_ceo_right == 0 & cross_ceo_left == 0)]
reg_data[, same_side_left  := as.integer(ceo_cfscore < emp_mean_cfscore & cross_ceo_right == 0 & cross_ceo_left == 0)]

# ── 2b. CEO absolute conservative ──
reg_data[, ceo_abs_conservative := as.integer(ceo_cfscore > 0)]

# ── 2c. log_n_donors ──
reg_data[, log_n_donors := log(n_emp_donors)]

# ── 2d. SIC2 ──
reg_data[, sic2 := floor(sich / 100)]
n_miss_sic <- sum(is.na(reg_data$sic2))
cat("SIC2 missing after sich:", n_miss_sic, "\n")

if (n_miss_sic > 0) {
  load("compustat_outcomes.rdata")
  # compustat_outcomes might be named differently
  cs <- get(ls(pattern = "compustat|cs|comp", envir = .GlobalEnv)[!ls(pattern = "compustat|cs|comp", envir = .GlobalEnv) %in% "reg_data"][1])
  if (!is.data.table(cs)) cs <- as.data.table(cs)
  cat("Compustat cols with 'sic':", grep("sic", names(cs), value = TRUE, ignore.case = TRUE), "\n")
  
  # Get sich from compustat for missing cases
  if ("sich" %in% names(cs)) {
    cs_sic <- cs[!is.na(sich), .(sich_cs = floor(sich[.N] / 100)), by = gvkey]
    reg_data <- merge(reg_data, cs_sic, by = "gvkey", all.x = TRUE)
    reg_data[is.na(sic2) & !is.na(sich_cs), sic2 := sich_cs]
    reg_data[, sich_cs := NULL]
    cat("SIC2 missing after compustat fill:", sum(is.na(reg_data$sic2)), "\n")
  }
  rm(cs); gc()
}

# ── 2e. CEO Turnover ──
load("execucomp_ceos.rdata")
ec <- get(ls(pattern = "exec|ceo", envir = .GlobalEnv)[!ls(pattern = "exec|ceo", envir = .GlobalEnv) %in% c("reg_data")][1])
if (!is.data.table(ec)) ec <- as.data.table(ec)
cat("ExecuComp cols:", head(names(ec), 20), "\n")

# Identify the year column
yr_col <- intersect(names(ec), c("year", "fyear", "YEAR"))[1]
eid_col <- intersect(names(ec), c("execid", "EXECID"))[1]
gvk_col <- intersect(names(ec), c("gvkey", "GVKEY"))[1]
cat("Using columns:", gvk_col, yr_col, eid_col, "\n")

# Get one CEO per gvkey-year (take first if multiple)
setnames(ec, c(gvk_col, yr_col, eid_col), c("gvkey_ec", "year_ec", "execid_ec"), skip_absent = TRUE)
ec_ceo <- unique(ec[, .(gvkey_ec, year_ec, execid_ec)])
setorder(ec_ceo, gvkey_ec, year_ec)

# Prior year CEO
ec_ceo[, prior_execid := shift(execid_ec, 1), by = gvkey_ec]
ec_ceo[, ceo_turnover := as.integer(!is.na(prior_execid) & execid_ec != prior_execid)]

# First year of this CEO at this firm
ec_ceo[, first_year_ceo := as.integer(year_ec == min(year_ec)), by = .(gvkey_ec, execid_ec)]

# reg_data uses fyear_lead (= cycle + 1) for the compustat merge
# We need to figure out what year variable to match on
cat("\nreg_data year-related cols:", grep("year|cycle|fyear", names(reg_data), value = TRUE), "\n")

# Match on gvkey and the fiscal year (fyear or fyear_lead)
if ("fyear" %in% names(reg_data)) {
  turnover_merge <- ec_ceo[, .(gvkey = gvkey_ec, fyear = year_ec, ceo_turnover, first_year_ceo)]
  reg_data <- merge(reg_data, turnover_merge, by = c("gvkey", "fyear"), all.x = TRUE)
} else if ("fyear_lead" %in% names(reg_data)) {
  turnover_merge <- ec_ceo[, .(gvkey = gvkey_ec, fyear_lead = year_ec, ceo_turnover, first_year_ceo)]
  reg_data <- merge(reg_data, turnover_merge, by = c("gvkey", "fyear_lead"), all.x = TRUE)
}
cat("CEO turnover coverage:", sum(!is.na(reg_data$ceo_turnover)), "of", nrow(reg_data), "\n")
cat("Turnover frequency:\n")
print(table(reg_data$ceo_turnover, useNA = "ifany"))
rm(ec, ec_ceo); gc()

# ── 2f. Donor coverage ──
# emp is Compustat employees in thousands
if ("emp" %in% names(reg_data)) {
  reg_data[, donor_coverage := fifelse(is.na(emp) | emp == 0, NA_real_, n_emp_donors / (emp * 1000))]
  cat("\nDonor coverage summary:\n")
  print(summary(reg_data$donor_coverage))
} else {
  cat("WARNING: 'emp' column not found in reg_data. Available cols with emp:", 
      grep("emp", names(reg_data), value = TRUE), "\n")
}

# ── 2g. Double-lag misalign_abs (cycle - 2) ──
setorder(reg_data, gvkey, cycle)
reg_data[, misalign_abs_L2 := shift(misalign_abs, 2), by = gvkey]
# Verify it's actually cycle - 2 (since cycles are biennial, shift(2) = 2 cycles back = 4 years)
# Actually check: are cycles consecutive for each gvkey?
cat("\nCycle spacing check (first 5 firms):\n")
print(reg_data[, .(cycles = paste(cycle, collapse=",")), by = gvkey][1:5])

# If cycles are biennial (2010,2012,2014...), shift(1) = prior cycle = cycle-2 in years
# shift(2) would be cycle-4 in years. The user says "cycle - 2" meaning the prior election cycle
# which is shift(1) for biennial data. But they also say "double-lag" so let me check.
# "misalign_abs from cycle - 2 (the prior election cycle)" — cycle-2 IS the prior election cycle
# since cycles are every 2 years. So shift(1) is correct.
reg_data[, misalign_abs_L2 := NULL]  # redo
reg_data[, misalign_abs_L2 := shift(misalign_abs, 1), by = gvkey]
# But verify the shifted value actually corresponds to cycle - 2
reg_data[, check_cycle := shift(cycle, 1), by = gvkey]
cat("\nDouble-lag check (sample):\n")
print(reg_data[!is.na(misalign_abs_L2), .(gvkey, cycle, check_cycle, misalign_abs, misalign_abs_L2)][1:10])
reg_data[, check_cycle := NULL]

cat("\nmisalign_abs_L2 coverage:", sum(!is.na(reg_data$misalign_abs_L2)), "of", nrow(reg_data), "\n")

# ── 3. Summary tables ──
cat("\n=== QUADRANT FREQUENCY TABLE ===\n")
quad_tab <- reg_data[, .(
  cross_ceo_right = sum(cross_ceo_right, na.rm=TRUE),
  cross_ceo_left = sum(cross_ceo_left, na.rm=TRUE),
  same_side_right = sum(same_side_right, na.rm=TRUE),
  same_side_left = sum(same_side_left, na.rm=TRUE)
)]
print(quad_tab)

cat("\nDetailed quadrant:\n")
reg_data[, quadrant := fcase(
  cross_ceo_right == 1, "Cross: CEO Right / Emp Left",
  cross_ceo_left == 1,  "Cross: CEO Left / Emp Right",
  same_side_right == 1, "Same side: CEO more Right",
  same_side_left == 1,  "Same side: CEO more Left",
  default = "Exact tie or NA"
)]
print(table(reg_data$quadrant, useNA = "ifany"))

cat("\n=== BY-CYCLE SUMMARY ===\n")
cycle_summary <- reg_data[, .(
  N = .N,
  mean_misalign_abs = round(mean(misalign_abs, na.rm=TRUE), 4),
  pct_ceo_conservative = round(100 * mean(ceo_abs_conservative, na.rm=TRUE), 1),
  mean_ceo_cfscore = round(mean(ceo_cfscore, na.rm=TRUE), 4),
  mean_emp_cfscore = round(mean(emp_mean_cfscore, na.rm=TRUE), 4)
), by = cycle]
setorder(cycle_summary, cycle)
print(cycle_summary)

# ── 4. Save ──
save(reg_data, file = "reg_data_v2.rdata")
cat("\n=== Saved reg_data_v2.rdata ===\n")
cat("Final dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("New columns added:\n")
new_cols <- c("cross_ceo_right", "cross_ceo_left", "same_side_right", "same_side_left",
              "ceo_abs_conservative", "log_n_donors", "sic2", "ceo_turnover", 
              "first_year_ceo", "donor_coverage", "misalign_abs_L2", "quadrant")
for (col in new_cols) {
  if (col %in% names(reg_data)) cat("  ", col, "\n")
}

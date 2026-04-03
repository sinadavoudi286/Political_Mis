library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
dt <- reg_data[!sic2 %in% c(49, 60:69)]
dt[, recession := nber_recession]
dt[, lag_aqc_indicator := lag_acquisition]
ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

cat("==========================================================\n")
cat("PART B: CEO TURNOVER EVENT STUDY\n")
cat("==========================================================\n\n")

load("execucomp_ceos.rdata")
load("ceo_dime_lookup.rdata")
load("compustat_outcomes.rdata")
setDT(execucomp_ceos)
setDT(ceo_dime_lookup)
setDT(compustat)

# B1: Identify CEO transitions
setorder(execucomp_ceos, gvkey, year)
execucomp_ceos[, prev_execid := shift(execid, 1), by = gvkey]
execucomp_ceos[, prev_year := shift(year, 1), by = gvkey]
execucomp_ceos[, turnover := (!is.na(prev_execid)) & (execid != prev_execid) &
                             (year - prev_year <= 2)]

turnovers <- execucomp_ceos[turnover == TRUE]
cat("B1: Total CEO turnovers identified:", nrow(turnovers), "\n")

# B2: Get CFscores for incoming and outgoing
turnovers <- merge(turnovers,
                   ceo_dime_lookup[, .(execid, ceo_cfscore_in = ceo_cfscore)],
                   by = "execid", all.x = TRUE)
turnovers <- merge(turnovers,
                   ceo_dime_lookup[, .(execid, ceo_cfscore_out = ceo_cfscore)],
                   by.x = "prev_execid", by.y = "execid", all.x = TRUE)

turnovers_both <- turnovers[!is.na(ceo_cfscore_in) & !is.na(ceo_cfscore_out)]
cat("B2: Turnovers with both CEO CFscores:", nrow(turnovers_both), "\n")

# B3: Get employee ideology from the panel
turnovers_both[, event_year := year]
emp_by_fy <- dt[, .(emp_mean_cfscore = emp_mean_cfscore[1]), by = .(gvkey, fyear)]

turnovers_both <- merge(turnovers_both, emp_by_fy,
                        by.x = c("gvkey", "event_year"), by.y = c("gvkey", "fyear"),
                        all.x = TRUE)
setnames(turnovers_both, "emp_mean_cfscore", "emp_mean_pre")

# Fill missing from nearby years
missing <- turnovers_both[is.na(emp_mean_pre)]
cat("  Missing emp ideology (exact year):", nrow(missing), "\n")

if (nrow(missing) > 0) {
  fill1 <- merge(missing[, .(gvkey, event_year)],
                 emp_by_fy, by.x = "gvkey", by.y = "gvkey", allow.cartesian = TRUE)
  fill1 <- fill1[abs(fyear - event_year) <= 1]
  fill1 <- fill1[order(abs(fyear - event_year))][, .SD[1], by = .(gvkey, event_year)]
  setnames(fill1, "emp_mean_cfscore", "emp_fill")
  turnovers_both <- merge(turnovers_both, fill1[, .(gvkey, event_year, emp_fill)],
                          by = c("gvkey", "event_year"), all.x = TRUE)
  turnovers_both[is.na(emp_mean_pre), emp_mean_pre := emp_fill]
  turnovers_both[, emp_fill := NULL]
}

turnovers_both <- turnovers_both[!is.na(emp_mean_pre)]
cat("B3: Turnovers with employee ideology:", nrow(turnovers_both), "\n")

# Compute misalignment change
turnovers_both[, misalign_pre := abs(ceo_cfscore_out - emp_mean_pre)]
turnovers_both[, misalign_post := abs(ceo_cfscore_in - emp_mean_pre)]
turnovers_both[, delta_misalign := misalign_post - misalign_pre]

cat("\nDistribution of delta_misalignment:\n")
print(summary(turnovers_both$delta_misalign))
cat("SD:", round(sd(turnovers_both$delta_misalign, na.rm=TRUE), 4), "\n")

# Treatment groups
turnovers_both[, high_increase := as.integer(delta_misalign > median(delta_misalign, na.rm=TRUE))]
turnovers_both[, large_increase := as.integer(delta_misalign > quantile(delta_misalign, 0.75, na.rm=TRUE))]
turnovers_both[, large_decrease := as.integer(delta_misalign < quantile(delta_misalign, 0.25, na.rm=TRUE))]

cat("\nTreatment groups:\n")
cat("  High increase (above median):", sum(turnovers_both$high_increase), "\n")
cat("  Large increase (top quartile):", sum(turnovers_both$large_increase), "\n")
cat("  Large decrease (bottom quartile):", sum(turnovers_both$large_decrease), "\n")

# B4: Build event-time panel
event_panel <- turnovers_both[, .(
  gvkey, event_year, delta_misalign, high_increase, large_increase, large_decrease,
  ceo_cfscore_in, ceo_cfscore_out, emp_mean_pre, misalign_pre, misalign_post
)]

event_panel <- event_panel[, .(event_time = -3:3), by = names(event_panel)]
event_panel[, fyear := event_year + event_time]

# Merge outcomes from compustat
comp_merge_vars <- intersect(
  c("gvkey", "fyear", "emp_change", "neg_spi_at", "layoff",
    "lag_size", "lag_roa", "lag_leverage", "lag_tobinq",
    "lag_sale_growth", "lag_loss", "lag_capx_at", "lag_rd_at",
    "lag_acquisition"),
  names(compustat)
)
cat("\nCompustat vars for merge:", paste(comp_merge_vars, collapse=", "), "\n")

event_panel <- merge(event_panel, compustat[, .SD, .SDcols = comp_merge_vars],
                     by = c("gvkey", "fyear"), all.x = TRUE)

if (!"layoff" %in% names(event_panel)) {
  event_panel[, layoff := as.integer(emp_change < -0.05)]
}

cat("\nB4: Event panel rows:", nrow(event_panel), "\n")
cat("  Non-missing layoff:", sum(!is.na(event_panel$layoff)), "\n")
cat("  Unique turnover events:", uniqueN(event_panel[, .(gvkey, event_year)]), "\n")

# B5: Event study regressions
cat("\n\n=== B5: EVENT STUDY RESULTS ===\n\n")
event_panel[, post := as.integer(event_time >= 0)]

# DiD: Post x High Increase
cat("DiD #1: Post x High Increase (above-median delta misalignment)\n")
did1 <- feols(layoff ~ post:high_increase + post + high_increase | gvkey + fyear,
              data = event_panel[!is.na(layoff)], cluster = ~gvkey)
etable(did1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# DiD for charges
cat("\nDiD #2: Post x High Increase — DV: Restructuring Charges\n")
n_spi <- sum(!is.na(event_panel$neg_spi_at))
cat("  N with non-missing neg_spi_at:", n_spi, "\n")
if (n_spi > 100) {
  did2 <- feols(neg_spi_at ~ post:high_increase + post + high_increase | gvkey + fyear,
                data = event_panel[!is.na(neg_spi_at)], cluster = ~gvkey)
  etable(did2, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
}

# Dynamic event study
cat("\nDynamic Event Study: Layoff ~ event_time x high_increase\n")
es1 <- feols(layoff ~ i(event_time, high_increase, ref = -1) | gvkey + fyear,
             data = event_panel[!is.na(layoff)], cluster = ~gvkey)
ct1 <- coeftable(es1)
print(ct1[grep("event_time", rownames(ct1)), ])

# DiD: Top quartile
cat("\nDiD #3: Post x Large Increase (top-quartile delta misalignment)\n")
did3 <- feols(layoff ~ post:large_increase + post + large_increase | gvkey + fyear,
              data = event_panel[!is.na(layoff)], cluster = ~gvkey)
etable(did3, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# Extreme comparison
extreme <- event_panel[large_increase == 1 | large_decrease == 1]
extreme[, treat := large_increase]
cat("\nDiD #4: Extreme comparison (top Q increase vs bottom Q decrease)\n")
did4 <- feols(layoff ~ post:treat + post + treat | gvkey + fyear,
              data = extreme[!is.na(layoff)], cluster = ~gvkey)
etable(did4, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# Dynamic extreme
cat("\nDynamic Event Study: Extreme comparison\n")
es2 <- feols(layoff ~ i(event_time, treat, ref = -1) | gvkey + fyear,
             data = extreme[!is.na(layoff)], cluster = ~gvkey)
ct2 <- coeftable(es2)
print(ct2[grep("event_time", rownames(ct2)), ])

# B6: Pre-trends
cat("\n\n=== B6: PRE-TRENDS CHECK ===\n")
pre_coefs <- grep("event_time::-3|event_time::-2", names(coef(es1)), value=TRUE)
cat("Pre-period coefficient names:", pre_coefs, "\n")
if (length(pre_coefs) >= 2) {
  pre_test <- wald(es1, pre_coefs)
  cat("Joint test of pre-trends (event_time -3 and -2):\n")
  print(pre_test)
}

# B7: Summary
cat("\n\n=== B7: SUMMARY ===\n")
cat("Total turnovers in ExecuComp:", nrow(turnovers), "\n")
cat("Both CEOs have DIME CFscores:", nrow(turnovers_both), "\n")
cat("Event panel turnover events:", uniqueN(event_panel[event_time==0, .(gvkey, event_year)]), "\n")
cat("Mean delta_misalignment:", round(mean(turnovers_both$delta_misalign, na.rm=TRUE), 4), "\n")
cat("SD delta_misalignment:", round(sd(turnovers_both$delta_misalign, na.rm=TRUE), 4), "\n")

# Save
results <- list(
  turnovers_both = turnovers_both,
  event_panel = event_panel,
  did1 = did1, did3 = did3, did4 = did4,
  es1 = es1, es2 = es2
)
if (exists("did2")) results$did2 <- did2
saveRDS(results, "diagnostic_and_eventstudy.rds")
cat("\nSaved diagnostic_and_eventstudy.rds\n")

library(fixest)
library(data.table)
load("reg_data_v2.rdata")
setDT(reg_data)

# Variable mapping:
# log_at -> size | tobinsq -> tobinq | sales_growth -> sale_growth
# capex_at -> capx_at | acq_indicator -> acquisition
# neg_spi_at_lead -> neg_spi_at (already t+1)
# layoff_indicator -> layoff
# peer_layoff_rate -> ind_layoff_rate
# misalign_abs_x_peer_layoff -> misalign_x_indlayoff (create)
# emp_mean_cfscore, emp_sd_cfscore, ceo_turnover, first_year_ceo already exist

reg_data[, misalign_x_indlayoff := misalign_abs * ind_layoff_rate]

controls <- c("size", "roa", "leverage", "tobinq", "sale_growth",
              "loss", "capx_at", "rd_at", "acquisition")

# ============================================================
# PART 1: Extended Table 5
# ============================================================
fml <- function(fe_str) {
  as.formula(paste(
    "neg_spi_at ~ misalign_abs + misalign_x_indlayoff +",
    paste(controls, collapse = " + "),
    "|", fe_str
  ))
}

t5_c1 <- feols(fml("gvkey + fyear_lead"),
               data=reg_data, cluster=~gvkey)

t5_c2 <- feols(as.formula(paste(
               "neg_spi_at ~ misalign_abs + misalign_x_indlayoff +",
               "emp_mean_cfscore + emp_sd_cfscore +",
               paste(controls, collapse="+"), "| gvkey + fyear_lead")),
               data=reg_data, cluster=~gvkey)

t5_c3 <- feols(as.formula(paste(
               "neg_spi_at ~ misalign_abs + misalign_x_indlayoff +",
               "ceo_turnover + first_year_ceo +",
               paste(controls, collapse="+"), "| gvkey + fyear_lead")),
               data=reg_data, cluster=~gvkey)

t5_c4 <- feols(fml("gvkey + fyear_lead"),
               data=reg_data, cluster=~gvkey+fyear_lead)

t5_c5 <- feols(fml("gvkey + sic2^fyear_lead"),
               data=reg_data, cluster=~gvkey)

cat("=== TABLE 5 EXTENDED: Peer Layoff Interaction ===\n")
etable(t5_c1, t5_c2, t5_c3, t5_c4, t5_c5,
       keep = c("misalign_abs", "misalign_x_indlayoff"),
       headers = c("Baseline", "+Emp Ideology", "+CEO Turnover",
                   "Double-Cluster", "Ind×Year FE"),
       title = "Table 5 Extended: Peer Layoff Interaction")

# Net effects for ind x year FE column
cat("\n--- Net effects under Ind x Year FE ---\n")
b1 <- coef(t5_c5)["misalign_abs"]
b2 <- coef(t5_c5)["misalign_x_indlayoff"]
vc <- vcov(t5_c5)[c("misalign_abs","misalign_x_indlayoff"),
                   c("misalign_abs","misalign_x_indlayoff")]

for (pct in list(c("p10",0.128), c("p50",0.235), c("p90",0.391))) {
  lc  <- b1 + b2 * pct[[2]]
  se  <- sqrt(t(c(1, pct[[2]])) %*% vc %*% c(1, pct[[2]]))
  pv  <- 2 * pt(-abs(lc/se), df = t5_c5$nobs - 1)
  cat(sprintf("  %s (%.3f): net = %.4f, SE = %.4f, p = %.3f\n",
              pct[[1]], pct[[2]], lc, se, pv))
}

cat("\nFootnote for Table 5 Column 5:\n")
cat("Column (5) replaces year fixed effects with industry x year fixed effects\n")
cat("(2-digit SIC x fiscal year). The standalone peer layoff rate is fully\n")
cat("absorbed by industry-year FE, so the interaction captures within-industry-\n")
cat("year variation. The main effect of misalignment and the constraint at low\n")
cat("peer layoff rates remain consistent with the baseline specification.\n")

# ============================================================
# PART 2: First Differences Robustness Table
# ============================================================
setorder(reg_data, gvkey, cycle)

# First differences
reg_data[, d_neg_spi_at := neg_spi_at - shift(neg_spi_at, 1), by=gvkey]
reg_data[, d_misalign_abs := misalign_abs - shift(misalign_abs, 1), by=gvkey]
reg_data[, d_size := size - shift(size, 1), by=gvkey]
reg_data[, d_roa := roa - shift(roa, 1), by=gvkey]
reg_data[, d_leverage := leverage - shift(leverage, 1), by=gvkey]
reg_data[, d_tobinq := tobinq - shift(tobinq, 1), by=gvkey]
reg_data[, d_sale_growth := sale_growth - shift(sale_growth, 1), by=gvkey]
reg_data[, d_layoff := layoff - shift(layoff, 1), by=gvkey]

# Interaction in differences: delta_misalign * level of ind_layoff
reg_data[, d_misalign_x_peer := d_misalign_abs * ind_layoff_rate]

fd1 <- feols(d_neg_spi_at ~ d_misalign_abs +
               d_size + d_roa + d_leverage + d_tobinq + d_sale_growth |
               fyear_lead,
             data=reg_data, cluster=~gvkey)

fd2 <- feols(d_neg_spi_at ~ d_misalign_abs + d_misalign_x_peer +
               d_size + d_roa + d_leverage + d_tobinq + d_sale_growth |
               fyear_lead,
             data=reg_data, cluster=~gvkey)

fd3 <- feols(d_layoff ~ d_misalign_abs +
               d_size + d_roa + d_leverage + d_tobinq + d_sale_growth |
               fyear_lead,
             data=reg_data, cluster=~gvkey)

cat("\n=== FIRST DIFFERENCES ROBUSTNESS TABLE ===\n")
etable(fd1, fd2, fd3,
       keep = c("d_misalign_abs", "d_misalign_x_peer"),
       headers = c("Delta Restructuring", "Delta Restr + Interaction",
                   "Delta Layoff"),
       title = "First Differences: Changes in Misalignment and Restructuring")

cat("\nInterpretation: FD eliminates all time-invariant firm characteristics.\n")
cat("d_misalign uses only within-firm changes in misalignment across cycles.\n")

# ============================================================
# PART 3: Layoff timing diagnosis
# ============================================================
setorder(reg_data, gvkey, cycle)
reg_data[, layoff_lag1  := shift(layoff, 1, type="lag"),  by=gvkey]
reg_data[, layoff_lead1 := shift(layoff, 1, type="lead"), by=gvkey]

make_layoff_fml <- function(dv) {
  as.formula(paste(
    dv, "~ misalign_abs +",
    paste(controls, collapse="+"),
    "| gvkey + fyear_lead"
  ))
}

lay_lag1    <- feols(make_layoff_fml("layoff_lag1"),  data=reg_data, cluster=~gvkey)
lay_contemp <- feols(make_layoff_fml("layoff"),       data=reg_data, cluster=~gvkey)
lay_lead1   <- feols(make_layoff_fml("layoff_lead1"), data=reg_data, cluster=~gvkey)

cat("\n=== LAYOFF TIMING DIAGNOSTIC ===\n")
etable(lay_lag1, lay_contemp, lay_lead1,
       keep = "misalign_abs",
       headers = c("t-1 (placebo)", "t (contemp)", "t+1 (lagged)"),
       title = "Layoff Indicator: Lead/Lag Structure")

cat("\nText for robustness section:\n")
cat("The layoff indicator does not follow the same timing pattern as\n")
cat("restructuring charges. The lagged specification (misalignment at t\n")
cat("predicting layoffs at t+1) yields a null coefficient, while the\n")
cat("contemporaneous specification is marginally significant with the\n")
cat("opposite sign. This suggests layoff decisions respond to contemporaneous\n")
cat("conditions rather than lagged organizational friction, inconsistent with\n")
cat("the timing assumption. We treat the layoff indicator as supplementary.\n")

# ============================================================
# SAVE
# ============================================================
saveRDS(list(
  t5_extended   = list(t5_c1, t5_c2, t5_c3, t5_c4, t5_c5),
  fd_models     = list(fd1, fd2, fd3),
  layoff_timing = list(lay_lag1, lay_contemp, lay_lead1)
), "prompt_c_results.rds")

cat("\n=== DONE. Saved to prompt_c_results.rds ===\n")

library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v2.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
rd <- reg_data[!sic2 %in% c(49, 60:69)]
rd[, recession := nber_recession]
rd[, lag_aqc_indicator := lag_acquisition]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

# ════════════════════════════════════════════════
# B1: LABOR INTENSITY THREE-WAY
# ════════════════════════════════════════════════
B1 <- feols(as.formula(paste(
  "neg_spi_at ~ misalign_abs * recession * lag_emp_sale +", ctrl, "| gvkey + fyear")),
  data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# B2: EMPLOYEE COHESION THREE-WAY
# ════════════════════════════════════════════════
B2 <- feols(as.formula(paste(
  "neg_spi_at ~ misalign_abs * recession * emp_sd_cfscore +", ctrl, "| gvkey + fyear")),
  data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# B3: PEER RESTRUCTURING
# ════════════════════════════════════════════════
# Leave-one-out mean: for each obs, mean of neg_spi_at of OTHER firms in same sic2×fyear
rd[, grp_sum := sum(neg_spi_at, na.rm=TRUE), by=.(sic2, fyear)]
rd[, grp_n := sum(!is.na(neg_spi_at)), by=.(sic2, fyear)]
rd[, peer_restr := fifelse(grp_n > 1, (grp_sum - fifelse(is.na(neg_spi_at), 0, neg_spi_at)) / (grp_n - as.integer(!is.na(neg_spi_at))), NA_real_)]
rd[, c("grp_sum","grp_n") := NULL]

cat("peer_restr summary:\n"); print(summary(rd$peer_restr))

B3 <- feols(as.formula(paste(
  "neg_spi_at ~ misalign_abs + misalign_abs:peer_restr +", ctrl, "| gvkey + fyear")),
  data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# CUMULATIVE CHARGES
# ════════════════════════════════════════════════
load("compustat_outcomes.rdata")
cs <- as.data.table(compustat)
rm(compustat); gc()

# Get neg_spi_at at t+1 and t+2 for each gvkey
cs_spi <- cs[!is.na(neg_spi_at), .(gvkey, fyear, neg_spi_at)]

# For reg_data: fyear is already the outcome year (cycle+1)
# We want neg_spi_at at fyear+1 and fyear+2 (i.e., cycle+2 and cycle+3)
rd <- merge(rd, cs_spi[, .(gvkey, fyear_p1 = fyear - 1, spi_t1 = neg_spi_at)],
            by.x=c("gvkey","fyear"), by.y=c("gvkey","fyear_p1"), all.x=TRUE)
rd <- merge(rd, cs_spi[, .(gvkey, fyear_p2 = fyear - 2, spi_t2 = neg_spi_at)],
            by.x=c("gvkey","fyear"), by.y=c("gvkey","fyear_p2"), all.x=TRUE)

# cum_2yr = neg_spi_at(t+1) + neg_spi_at(t+2)
# Note: current fyear already has neg_spi_at. We want FORWARD cumulative.
# spi_t1 = neg_spi_at at fyear+1, spi_t2 = neg_spi_at at fyear+2
rd[, cum_2yr := spi_t1 + spi_t2]

cat("\ncum_2yr coverage:", sum(!is.na(rd$cum_2yr)), "of", nrow(rd), "\n")
cat("cum_2yr summary:\n"); print(summary(rd$cum_2yr))
cat("neg_spi_at summary (for comparison):\n"); print(summary(rd$neg_spi_at))

C1 <- feols(as.formula(paste("cum_2yr ~ misalign_abs +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

C2 <- feols(as.formula(paste("cum_2yr ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

# Also run the standard model for comparison
M_base <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
                data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# RESULTS
# ════════════════════════════════════════════════
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("B1: LABOR INTENSITY THREE-WAY INTERACTION\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(B1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession","lag_emp_sale"))

cat("\nKey three-way interaction:\n")
b1 <- coef(B1); V1 <- vcov(B1)
tw <- "misalign_abs:recession:lag_emp_sale"
if (tw %in% names(b1)) {
  se_tw <- sqrt(V1[tw,tw])
  p_tw <- 2*pt(abs(b1[tw]/se_tw), df=degrees_freedom(B1,"t"), lower.tail=FALSE)
  cat(sprintf("  misalign_abs:recession:lag_emp_sale = %.5f (%.5f) p=%.4f %s\n",
              b1[tw], se_tw, p_tw, stars(p_tw)))
  cat("  Interpretation: Positive = recession release effect STRONGER in labor-intensive firms\n")
}

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("B2: EMPLOYEE COHESION THREE-WAY INTERACTION\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(B2, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession","emp_sd"))

cat("\nKey three-way interaction:\n")
b2 <- coef(B2); V2 <- vcov(B2)
tw2 <- "misalign_abs:recession:emp_sd_cfscore"
if (tw2 %in% names(b2)) {
  se_tw2 <- sqrt(V2[tw2,tw2])
  p_tw2 <- 2*pt(abs(b2[tw2]/se_tw2), df=degrees_freedom(B2,"t"), lower.tail=FALSE)
  cat(sprintf("  misalign_abs:recession:emp_sd_cfscore = %.5f (%.5f) p=%.4f %s\n",
              b2[tw2], se_tw2, p_tw2, stars(p_tw2)))
  cat("  Interpretation: Negative = recession release effect WEAKER when employees more dispersed\n")
  cat("  (i.e., constraint STRONGER when employees cohesive = low SD)\n")
}

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("B3: PEER RESTRUCTURING\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(B3, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","peer_restr"))

b3 <- coef(B3); V3 <- vcov(B3)
pr <- "misalign_abs:peer_restr"
if (pr %in% names(b3)) {
  se_pr <- sqrt(V3[pr,pr])
  p_pr <- 2*pt(abs(b3[pr]/se_pr), df=degrees_freedom(B3,"t"), lower.tail=FALSE)
  cat(sprintf("\n  misalign_abs:peer_restr = %.4f (%.4f) p=%.4f %s\n",
              b3[pr], se_pr, p_pr, stars(p_pr)))
  cat("  Interpretation: Positive = when peers restructure more, misalignment constraint weakens\n")
}

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("CUMULATIVE CHARGES (t+1 and t+2)\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(M_base, C1, C2,
       headers=c("Base:neg_spi_at","C1:cum_2yr","C2:cum_2yr×recess"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession"))

# Compare magnitudes
cat("\nComparison:\n")
bm <- coef(M_base); bc2 <- coef(C2)
cat(sprintf("  neg_spi_at: misalign_abs = %.5f, interaction = %.5f\n",
            bm["misalign_abs"], bm["misalign_abs:recession"]))
cat(sprintf("  cum_2yr:    misalign_abs = %.5f, interaction = %.5f\n",
            bc2["misalign_abs"], bc2["misalign_abs:recession"]))
ratio_main <- bc2["misalign_abs"] / bm["misalign_abs"]
ratio_int <- bc2["misalign_abs:recession"] / bm["misalign_abs:recession"]
cat(sprintf("  Ratio (cum/single): main = %.2f, interaction = %.2f\n", ratio_main, ratio_int))
cat("  If ratio > 1: effect accumulates over time. If ratio ≈ 1: it's one-year only.\n")

# ════════════════════════════════════════════════
# GRAND SUMMARY
# ════════════════════════════════════════════════
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("GRAND SUMMARY\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-35s %10s %8s %6s\n", "Test", "Coef", "p-value", "N"))
cat(paste(rep("─",65), collapse=""),"\n")

print_row <- function(label, mod, var) {
  b <- coef(mod); V <- vcov(mod)
  idx <- match(var, names(b))
  if (!is.na(idx)) {
    se <- sqrt(V[idx,idx])
    p <- 2*pt(abs(b[idx]/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
    cat(sprintf("%-35s %10.5f %7.4f%s %6d\n", label, b[idx], p, stars(p), mod$nobs))
  } else {
    cat(sprintf("%-35s %10s %8s %6d\n", label, "N/A", "N/A", mod$nobs))
  }
}

print_row("B1: misalign×recess×emp_sale", B1, "misalign_abs:recession:lag_emp_sale")
print_row("B2: misalign×recess×emp_sd", B2, "misalign_abs:recession:emp_sd_cfscore")
print_row("B3: misalign×peer_restr", B3, "misalign_abs:peer_restr")
print_row("C1: cum_2yr ~ misalign_abs", C1, "misalign_abs")
print_row("C2: cum_2yr ~ misalign×recess", C2, "misalign_abs:recession")

# Save
results <- list(B1=B1, B2=B2, B3=B3, C1=C1, C2=C2, M_base=M_base)
saveRDS(results, file="mechanism_tests.rds")
cat("\n\nSaved mechanism_tests.rds\n")

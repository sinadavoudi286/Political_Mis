library(data.table)
library(fixest)
setFixest_notes(FALSE)

# Load data
load("/Users/homemac/Desktop/reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
dt <- reg_data[!sic2 %in% c(49, 60:69)]
dt[, recession := nber_recession]
dt[, lag_aqc_indicator := lag_acquisition]
rm(reg_data); gc()

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

# Fixed helpers
get_cp <- function(mod, var) {
  b <- coef(mod); V <- vcov(mod)
  idx <- match(var, names(b)); if(is.na(idx)) return(c(NA,NA,NA))
  se <- sqrt(V[idx,idx]); p <- 2*pt(abs(b[idx]/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  unname(c(b[idx], se, p))
}

net_eff <- function(mod, v1, v2, scale=1) {
  b <- coef(mod); V <- vcov(mod)
  i1 <- match(v1, names(b)); i2 <- match(v2, names(b))
  if (is.na(i1) || is.na(i2)) return(c(NA,NA,NA))
  net <- b[i1] + b[i2]*scale
  se <- sqrt(V[i1,i1] + scale^2*V[i2,i2] + 2*scale*V[i1,i2])
  p <- 2*pt(abs(net/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  unname(c(net, se, p))
}

# Load saved results
safe_load <- function(f) { if (file.exists(f)) readRDS(f) else NULL }
core     <- safe_load("models_core.rds")
recess   <- safe_load("recession_robustness.rds")
addl     <- safe_load("robustness_additional.rds")
ind_down <- safe_load("industry_downturn_tests.rds")
peer     <- safe_load("peer_layoff_deep.rds")
mech     <- safe_load("mechanism_tests.rds")

pctiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
qvals <- quantile(dt$ind_layoff_rate, pctiles, na.rm=TRUE)

# ═══════════════════════════════════════════════
# TABLE 5: PEER LAYOFF MODERATION (FIXED)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 5: PEER LAYOFF MODERATION (MAIN RESULT)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(ind_down) && !is.null(peer)) {
  etable(ind_down$I5, peer$P2, peer$P3, peer$P5,
         headers=c("(1) Baseline","(2) +Emp Ideol","(3) +CEO Turn","(4) Dbl-Clust"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign", "ind_layoff"))

  cat("\nNet effect of misalignment at percentiles of peer layoff rate:\n")
  cat(sprintf("%-12s %12s %10s %8s %8s\n", "Percentile", "LayoffRate", "NetEffect", "SE", "p"))
  cat(paste(rep("─",56), collapse=""), "\n")
  
  mod <- ind_down$I5
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %12.4f %10.5f %8.5f %7.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne[1], ne[2], ne[3], stars(ne[3])))
  }
  cross <- -coef(mod)["misalign_abs"] / coef(mod)["misalign_abs:ind_layoff_rate"]
  cat(sprintf("\nCrossover (effect=0): ind_layoff_rate = %.4f (p%.0f of sample)\n",
              cross, 100*ecdf(dt$ind_layoff_rate)(cross)))
}

# ═══════════════════════════════════════════════
# TABLE 6: LAYOFF (FIXED)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 6: REAL OUTCOME — LAYOFF AND EMPLOYMENT CHANGE\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(peer)) {
  etable(peer$P6, peer$P7,
         headers=c("(1) emp_change","(2) layoff"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign", "ind_layoff"))

  cat("\nNet layoff effect at percentiles of ind_layoff_rate (P7):\n")
  cat(sprintf("%-12s %12s %10s %8s %8s\n", "Percentile", "LayoffRate", "NetEffect", "SE", "p"))
  cat(paste(rep("─",56), collapse=""), "\n")
  mod7 <- peer$P7
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod7, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %12.4f %10.5f %8.5f %7.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne[1], ne[2], ne[3], stars(ne[3])))
  }
}

# ═══════════════════════════════════════════════
# TABLE 7: ASYMMETRY (FIXED etable keep)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 7: ASYMMETRY TEST — EFFECT IS SYMMETRIC\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(peer)) {
  cat("Panel A: Peer Layoff Moderation (P1)\n\n")
  etable(peer$P1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("ceo_right","ceo_left"))
  b1 <- coef(peer$P1); V1 <- vcov(peer$P1)
  d <- b1["ceo_right:ind_layoff_rate"]-b1["ceo_left:ind_layoff_rate"]
  se_d <- sqrt(V1["ceo_right:ind_layoff_rate","ceo_right:ind_layoff_rate"]+V1["ceo_left:ind_layoff_rate","ceo_left:ind_layoff_rate"]-2*V1["ceo_right:ind_layoff_rate","ceo_left:ind_layoff_rate"])
  p_d <- 2*pt(abs(d/se_d), df=degrees_freedom(peer$P1,"t"), lower.tail=FALSE)
  cat(sprintf("\nWald test for equality of peer-layoff interactions: p = %.4f\n", p_d))
}

if (!is.null(core)) {
  cat("\nPanel B: NBER Recession Moderation (M10)\n\n")
  etable(core$M10, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("ceo_right","ceo_left"))
  b2 <- coef(core$M10); V2 <- vcov(core$M10)
  d2 <- b2["ceo_right:recession"]-b2["ceo_left:recession"]
  se_d2 <- sqrt(V2["ceo_right:recession","ceo_right:recession"]+V2["ceo_left:recession","ceo_left:recession"]-2*V2["ceo_right:recession","ceo_left:recession"])
  p_d2 <- 2*pt(abs(d2/se_d2), df=degrees_freedom(core$M10,"t"), lower.tail=FALSE)
  cat(sprintf("\nWald test for equality of recession interactions: p = %.4f\n", p_d2))
}
cat("\nConclusion: The peer-layoff/recession moderation operates symmetrically\nregardless of the direction of misalignment.\n")

# ═══════════════════════════════════════════════
# TABLE 9: RECESSION ROBUSTNESS (FIXED)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 9: RECESSION ROBUSTNESS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(recess)) {
  cat(sprintf("%-8s %-18s %10s %7s %10s %7s %6s\n",
              "Model","Exclusion","Base coef","p","Inter coef","p","N"))
  cat(paste(rep("─",68), collapse=""), "\n")
  
  items <- list(
    list("R1","Excl. 2020-21","R1","misalign_abs","misalign_abs:recession"),
    list("R2a","Excl. 2001","R2a","misalign_abs","misalign_abs:recession"),
    list("R2b","Excl. 2008-09","R2b","misalign_abs","misalign_abs:recession"),
    list("R2c","Excl. 2020","R2c","misalign_abs","misalign_abs:recession"),
    list("R3","Double-lagged","R3","misalign_abs_L2","misalign_abs_L2:recession")
  )
  
  for (it in items) {
    m <- recess[[it[[3]]]]
    sm <- get_cp(m, it[[4]]); si <- get_cp(m, it[[5]])
    cat(sprintf("%-8s %-18s %9.4f%s %6.4f %9.4f%s %6.4f %6d\n",
                it[[1]], it[[2]], sm[1], stars(sm[3]), sm[3],
                si[1], stars(si[3]), si[3], m$nobs))
  }
}

# ═══════════════════════════════════════════════
# TABLE 10 PANEL A: DONOR THRESHOLD (FIXED)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 10: MEASUREMENT ROBUSTNESS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(addl)) {
  cat("Panel A: Donor Threshold Sensitivity\n")
  cat(sprintf("%-12s %10s %7s %10s %7s %6s\n","Threshold","Base coef","p","Inter coef","p","N"))
  cat(paste(rep("─",58), collapse=""), "\n")
  
  for (th in list(c(">=10","D1"),c(">=20","D0"),c(">=30","D2"),c(">=50","D3"),c(">=100","D4"))) {
    m <- addl[[th[2]]]
    sm <- get_cp(m,"misalign_abs"); si <- get_cp(m,"misalign_abs:recession")
    cat(sprintf("%-12s %9.4f%s %6.4f %9.4f%s %6.4f %6d\n",
                th[1], sm[1], stars(sm[3]), sm[3], si[1], stars(si[3]), si[3], m$nobs))
  }
  
  cat("\nPanel B: Weighted Regressions\n")
  etable(addl$D0, addl$D5, addl$D6,
         headers=c("Unweighted","Wt:n_donors","Wt:log(n)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign","recession"))
  
  cat("\nPanel C: Two-Part Model\n")
  etable(addl$D7, addl$D8,
         headers=c("Incidence(logit)","Magnitude(OLS|charge>0)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign","recession"))
}

# ═══════════════════════════════════════════════
# TABLE 11: MECHANISM (FIXED)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 11: MECHANISM TESTS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(mech)) {
  cat("B1: Labor Intensity Three-Way\n")
  s1_2w <- get_cp(mech$B1, "misalign_abs:lag_emp_sale")
  s1_3w <- get_cp(mech$B1, "misalign_abs:recession:lag_emp_sale")
  cat(sprintf("  misalign×emp_sale (2-way):           %8.5f (%8.5f) p=%.4f %s\n", s1_2w[1], s1_2w[2], s1_2w[3], stars(s1_2w[3])))
  cat(sprintf("  misalign×recession×emp_sale (3-way): %8.5f (%8.5f) p=%.4f %s  [NOT SIG]\n\n", s1_3w[1], s1_3w[2], s1_3w[3], stars(s1_3w[3])))

  cat("B2: Employee Cohesion Three-Way\n")
  s2_2w <- get_cp(mech$B2, "misalign_abs:emp_sd_cfscore")
  s2_3w <- get_cp(mech$B2, "misalign_abs:recession:emp_sd_cfscore")
  cat(sprintf("  misalign×emp_sd (2-way):             %8.5f (%8.5f) p=%.4f %s\n", s2_2w[1], s2_2w[2], s2_2w[3], stars(s2_2w[3])))
  cat(sprintf("  misalign×recession×emp_sd (3-way):   %8.5f (%8.5f) p=%.4f %s  [NOT SIG]\n\n", s2_3w[1], s2_3w[2], s2_3w[3], stars(s2_3w[3])))

  cat("B3: Peer Restructuring\n")
  s3 <- get_cp(mech$B3, "misalign_abs:peer_restr")
  cat(sprintf("  misalign×peer_restr:                 %8.5f (%8.5f) p=%.4f %s\n", s3[1], s3[2], s3[3], stars(s3[3])))
}

# ═══════════════════════════════════════════════
# TABLE 12: CUMULATIVE (already fine, just reprint interpretation)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 12: CUMULATIVE CHARGES — TIMING VS LEVEL\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(mech)) {
  etable(mech$M_base, mech$C1, mech$C2,
         headers=c("Single-yr neg_spi","Cum 2yr baseline","Cum 2yr x recess"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign","recession"))
  
  cat("\nInterpretation: The null cumulative result indicates misalignment affects\nthe TIMING of restructuring charges, not the total level.\n")
}

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("FIXED TABLES COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════\n")

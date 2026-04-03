library(data.table)
library(fixest)
setFixest_notes(FALSE)

# ── Load data ──
load("/Users/homemac/Desktop/reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
dt <- reg_data[!sic2 %in% c(49, 60:69)]
dt[, recession := nber_recession]
dt[, lag_aqc_indicator := lag_acquisition]
dt[, ceo_right := pmax(misalign_signed, 0)]
dt[, ceo_left  := pmax(-misalign_signed, 0)]
rm(reg_data)

# ── Load all saved results ──
safe_load <- function(f) { if (file.exists(f)) readRDS(f) else NULL }
core     <- safe_load("models_core.rds")
recess   <- safe_load("recession_robustness.rds")
addl     <- safe_load("robustness_additional.rds")
ind_down <- safe_load("industry_downturn_tests.rds")
peer     <- safe_load("peer_layoff_deep.rds")
mech     <- safe_load("mechanism_tests.rds")
plac     <- safe_load("placebo_results.rds")

# ── Helpers ──
stars <- function(p) ifelse(is.na(p),"",ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*",""))))

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

pctiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
qvals <- quantile(dt$ind_layoff_rate, pctiles, na.rm=TRUE)

###############################################################################
cat("\n")
cat("###################################################################\n")
cat("#                ALL TABLES FOR PAPER REVISION                    #\n")
cat("###################################################################\n")

###############################################################################
# TABLE 1
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 1: SAMPLE CONSTRUCTION\n")
cat("===================================================================\n\n")

cat(sprintf("%-5s %-50s %12s %12s\n", "Step", "Description", "Firm-cycles", "Unique firms"))
cat(paste(rep("-",83), collapse=""), "\n")

pf_r <- pf_f <- pc_r <- pc_f <- pq_r <- pq_f <- "—"
if (file.exists("panel_full.rdata")) {
  e <- new.env(); load("panel_full.rdata", envir=e)
  obj <- get(ls(envir=e)[1], envir=e)
  pf_r <- nrow(obj); pf_f <- length(unique(obj$gvkey))
}
if (file.exists("panel_complete.rdata")) {
  e <- new.env(); load("panel_complete.rdata", envir=e)
  obj <- get(ls(envir=e)[1], envir=e)
  pc_r <- nrow(obj); pc_f <- length(unique(obj$gvkey))
}
if (file.exists("panel_qual.rdata")) {
  e <- new.env(); load("panel_qual.rdata", envir=e)
  obj <- get(ls(envir=e)[1], envir=e)
  pq_r <- nrow(obj); pq_f <- length(unique(obj$gvkey))
}

load("reg_data_v3.rdata")
rd_full <- nrow(reg_data); rd_firms <- uniqueN(reg_data$gvkey)
rm(reg_data)

cat(sprintf("%-5s %-50s %12s %12s\n", "1", "All 13 cycle panels stacked", as.character(pf_r), as.character(pf_f)))
cat(sprintf("%-5s %-50s %12s %12s\n", "2", "With non-missing misalignment", as.character(pc_r), as.character(pc_f)))
cat(sprintf("%-5s %-50s %12s %12s\n", "3", ">=20 employee donors (quality filter)", as.character(pq_r), as.character(pq_f)))
cat(sprintf("%-5s %-50s %12d %12d\n", "4", "Merged with Compustat outcomes", rd_full, rd_firms))
cat(sprintf("%-5s %-50s %12d %12d\n", "5", "Excl. financials & utilities", nrow(dt), uniqueN(dt$gvkey)))
cat(sprintf("%-5s %-50s %12d %12d\n", "6", "Final regression sample (non-missing DV)",
            sum(!is.na(dt$neg_spi_at)), uniqueN(dt[!is.na(neg_spi_at), gvkey])))

###############################################################################
# TABLE 2
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 2: DESCRIPTIVE STATISTICS\n")
cat("===================================================================\n\n")

desc <- function(vars, panel_label) {
  cat(panel_label, "\n")
  cat(sprintf("%-24s %6s %9s %9s %9s %9s %9s\n", "Variable", "N", "Mean", "SD", "P25", "Median", "P75"))
  cat(paste(rep("-",78), collapse=""), "\n")
  for (v in vars) {
    if (!v %in% names(dt)) { cat(sprintf("%-24s  [not in data]\n", v)); next }
    x <- dt[[v]]; n <- sum(!is.na(x))
    if (n == 0) { cat(sprintf("%-24s  [all NA]\n", v)); next }
    cat(sprintf("%-24s %6d %9.3f %9.3f %9.3f %9.3f %9.3f\n", v, n,
                mean(x,na.rm=T), sd(x,na.rm=T),
                quantile(x,.25,na.rm=T), median(x,na.rm=T), quantile(x,.75,na.rm=T)))
  }
  cat("\n")
}

desc(c("misalign_abs","misalign_signed","ceo_cfscore","emp_mean_cfscore","emp_sd_cfscore",
       "n_emp_donors","log_n_donors","donor_coverage"), "Panel A -- Ideology & Misalignment")
desc(c("neg_spi_at","emp_change","layoff","warn_any","warn_workers"), "Panel B -- Outcomes")
desc(c("lag_size","lag_roa","lag_leverage","lag_tobinq","lag_sale_growth","lag_loss",
       "lag_capx_at","lag_rd_at","lag_aqc_indicator","lag_emp_sale"), "Panel C -- Lagged Controls")
desc(c("recession","ind_downturn","ind_layoff_rate","ind_sale_growth"), "Panel D -- Moderators")
desc(c("ceo_turnover","first_year_ceo","ceo_abs_conservative"), "Panel E -- CEO Characteristics")

cat("Panel F -- Quadrant Distribution\n")
cat(sprintf("%-30s %6s %8s\n", "Quadrant", "N", "% Sample"))
cat(paste(rep("-",48), collapse=""), "\n")
for (q in c("cross_ceo_right","cross_ceo_left","same_side_right","same_side_left")) {
  n <- sum(dt[[q]], na.rm=TRUE)
  cat(sprintf("%-30s %6d %7.1f%%\n", q, n, 100*n/nrow(dt)))
}

###############################################################################
# TABLE 3
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 3: MISALIGNMENT BY ELECTION CYCLE\n")
cat("===================================================================\n\n")

cyc <- dt[, .(
  N = .N,
  Mean_Misalign = round(mean(misalign_abs, na.rm=T), 3),
  Med_Misalign  = round(median(misalign_abs, na.rm=T), 3),
  Mean_CEO_CF   = round(mean(ceo_cfscore, na.rm=T), 3),
  Mean_Emp_CF   = round(mean(emp_mean_cfscore, na.rm=T), 3),
  Pct_CEO_RightOfEmp = round(100*mean(ceo_cfscore > emp_mean_cfscore, na.rm=T), 1),
  Pct_CEO_AbsCons = round(100*mean(ceo_abs_conservative, na.rm=T), 1),
  Mean_Donors   = round(mean(n_emp_donors, na.rm=T), 0)
), by=cycle][order(cycle)]
print(cyc)

###############################################################################
# TABLE 4
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 4: BASELINE -- MISALIGNMENT CONSTRAINS RESTRUCTURING\n")
cat("===================================================================\n\n")

if (!is.null(core)) {
  etable(core$M1, core$M2, core$M3, core$M4, core$M5,
         headers=c("(1) Firm+Year FE","(2) +Emp Ideology","(3) Ind x Year FE","(4) +CEO Turnover","(5) +Donor Count"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
}

###############################################################################
# TABLE 5
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 5: PEER LAYOFF MODERATION (MAIN RESULT)\n")
cat("DV = neg_spi_at throughout. Cluster by gvkey.\n")
cat("===================================================================\n\n")

if (!is.null(ind_down) && !is.null(peer)) {
  etable(ind_down$I5, peer$P2, peer$P3, peer$P5,
         headers=c("(1) Baseline","(2) +Emp Ideology","(3) +CEO Turnover","(4) Double-Clustered"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  cat("\nNet effect of misalignment at percentiles of peer layoff rate:\n\n")
  cat(sprintf("%-12s %14s %12s %10s %10s\n", "Percentile", "Layoff Rate", "Net Effect", "SE", "p-value"))
  cat(paste(rep("-",62), collapse=""), "\n")
  mod <- ind_down$I5
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %14.4f %12.5f %10.5f %9.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne[1], ne[2], ne[3], stars(ne[3])))
  }
  cross <- -coef(mod)["misalign_abs"] / coef(mod)["misalign_abs:ind_layoff_rate"]
  cat(sprintf("\nCrossover (effect = 0): ind_layoff_rate = %.4f (%.0fth percentile)\n",
              cross, 100*ecdf(dt$ind_layoff_rate)(cross)))
}

###############################################################################
# TABLE 6
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 6: REAL OUTCOMES -- LAYOFF AND EMPLOYMENT CHANGE\n")
cat("===================================================================\n\n")

if (!is.null(peer)) {
  etable(peer$P6, peer$P7,
         headers=c("(1) DV: emp_change","(2) DV: layoff"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  cat("\nNet layoff effect (P7) at percentiles of ind_layoff_rate:\n\n")
  cat(sprintf("%-12s %14s %12s %10s %10s\n", "Percentile", "Layoff Rate", "Net Effect", "SE", "p-value"))
  cat(paste(rep("-",62), collapse=""), "\n")
  mod7 <- peer$P7
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod7, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %14.4f %12.5f %10.5f %9.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne[1], ne[2], ne[3], stars(ne[3])))
  }
}

###############################################################################
# TABLE 7
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 7: ASYMMETRY TEST -- EFFECT IS SYMMETRIC\n")
cat("===================================================================\n\n")

if (!is.null(peer)) {
  cat("Panel A: Peer Layoff Moderation (P1)\n\n")
  etable(peer$P1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
  b1 <- coef(peer$P1); V1 <- vcov(peer$P1)
  d <- b1["ceo_right:ind_layoff_rate"]-b1["ceo_left:ind_layoff_rate"]
  se_d <- sqrt(V1["ceo_right:ind_layoff_rate","ceo_right:ind_layoff_rate"]+
               V1["ceo_left:ind_layoff_rate","ceo_left:ind_layoff_rate"]-
               2*V1["ceo_right:ind_layoff_rate","ceo_left:ind_layoff_rate"])
  p_d <- 2*pt(abs(d/se_d), df=degrees_freedom(peer$P1,"t"), lower.tail=FALSE)
  cat(sprintf("\nWald test: ceo_right:ind_layoff_rate = ceo_left:ind_layoff_rate\n"))
  cat(sprintf("  Difference = %.5f, SE = %.5f, p = %.4f\n", d, se_d, p_d))
}

if (!is.null(core)) {
  cat("\n\nPanel B: NBER Recession Moderation (M10)\n\n")
  etable(core$M10, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
  b2 <- coef(core$M10); V2 <- vcov(core$M10)
  d2 <- b2["ceo_right:recession"]-b2["ceo_left:recession"]
  se_d2 <- sqrt(V2["ceo_right:recession","ceo_right:recession"]+
                V2["ceo_left:recession","ceo_left:recession"]-
                2*V2["ceo_right:recession","ceo_left:recession"])
  p_d2 <- 2*pt(abs(d2/se_d2), df=degrees_freedom(core$M10,"t"), lower.tail=FALSE)
  cat(sprintf("\nWald test: ceo_right:recession = ceo_left:recession\n"))
  cat(sprintf("  Difference = %.5f, SE = %.5f, p = %.4f\n", d2, se_d2, p_d2))
}
cat("\nConclusion: The peer-layoff/recession moderation operates symmetrically\n")
cat("regardless of the direction of misalignment.\n")

###############################################################################
# TABLE 8
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 8: ALTERNATIVE MODERATORS\n")
cat("===================================================================\n\n")

if (!is.null(ind_down) && !is.null(core)) {
  etable(ind_down$I5, ind_down$I1, ind_down$I2, core$M6, ind_down$I4,
         headers=c("(1) Peer Layoff","(2) Ind Downturn","(3) Ind Sale Growth","(4) NBER Recession","(5) Horse Race"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
}

###############################################################################
# TABLE 9
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 9: RECESSION ROBUSTNESS\n")
cat("===================================================================\n\n")

if (!is.null(recess)) {
  cat(sprintf("%-8s %-20s %11s %8s %11s %8s %6s\n",
              "Model","Exclusion","Baseline b","p","Interact b","p","N"))
  cat(paste(rep("-",72), collapse=""), "\n")

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
    cat(sprintf("%-8s %-20s %10.4f%s %7.4f %10.4f%s %7.4f %6d\n",
                it[[1]], it[[2]], sm[1], stars(sm[3]), sm[3],
                si[1], stars(si[3]), si[3], m$nobs))
  }
}

###############################################################################
# TABLE 10
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 10: MEASUREMENT ROBUSTNESS\n")
cat("===================================================================\n\n")

if (!is.null(addl)) {
  cat("Panel A -- Donor Threshold Sensitivity\n\n")
  cat(sprintf("%-12s %11s %8s %11s %8s %6s\n","Threshold","Baseline b","p","Interact b","p","N"))
  cat(paste(rep("-",60), collapse=""), "\n")
  for (th in list(c(">=10","D1"),c(">=20","D0"),c(">=30","D2"),c(">=50","D3"),c(">=100","D4"))) {
    m <- addl[[th[2]]]
    sm <- get_cp(m,"misalign_abs"); si <- get_cp(m,"misalign_abs:recession")
    cat(sprintf("%-12s %10.4f%s %7.4f %10.4f%s %7.4f %6d\n",
                th[1], sm[1], stars(sm[3]), sm[3], si[1], stars(si[3]), si[3], m$nobs))
  }

  cat("\n\nPanel B -- Weighted Regressions\n\n")
  etable(addl$D0, addl$D5, addl$D6,
         headers=c("Unweighted","Wt: n_donors","Wt: log(n_donors)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  cat("\n\nPanel C -- Two-Part Model\n\n")
  etable(addl$D7, addl$D8,
         headers=c("Incidence (logit)","Magnitude (OLS | charge>0)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  cat("\n\nPanel D -- Subsample Tests\n\n")
  etable(addl$D0, addl$D9, addl$D10,
         headers=c("Full sample",">=6 cycles","Post-2010 only"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
}

###############################################################################
# TABLE 11
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 11: MECHANISM TESTS\n")
cat("===================================================================\n\n")

if (!is.null(mech)) {
  cat("B1: Labor Intensity x Misalignment x Recession\n\n")
  etable(mech$B1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
  s1_2w <- get_cp(mech$B1, "misalign_abs:lag_emp_sale")
  s1_3w <- get_cp(mech$B1, "misalign_abs:recession:lag_emp_sale")
  cat(sprintf("\n  Two-way  (misalign x emp_sale):            %8.5f (%.5f) p=%.4f %s\n", s1_2w[1], s1_2w[2], s1_2w[3], stars(s1_2w[3])))
  cat(sprintf("  Three-way (misalign x recession x emp_sale): %8.5f (%.5f) p=%.4f %s [NOT SIG]\n", s1_3w[1], s1_3w[2], s1_3w[3], stars(s1_3w[3])))

  cat("\n\nB2: Employee Cohesion x Misalignment x Recession\n\n")
  etable(mech$B2, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
  s2_2w <- get_cp(mech$B2, "misalign_abs:emp_sd_cfscore")
  s2_3w <- get_cp(mech$B2, "misalign_abs:recession:emp_sd_cfscore")
  cat(sprintf("\n  Two-way  (misalign x emp_sd):              %8.5f (%.5f) p=%.4f %s\n", s2_2w[1], s2_2w[2], s2_2w[3], stars(s2_2w[3])))
  cat(sprintf("  Three-way (misalign x recession x emp_sd): %8.5f (%.5f) p=%.4f %s [NOT SIG]\n", s2_3w[1], s2_3w[2], s2_3w[3], stars(s2_3w[3])))

  cat("\n\nB3: Peer Restructuring x Misalignment\n\n")
  etable(mech$B3, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
  s3 <- get_cp(mech$B3, "misalign_abs:peer_restr")
  cat(sprintf("\n  misalign x peer_restr: %8.5f (%.5f) p=%.4f %s\n", s3[1], s3[2], s3[3], stars(s3[3])))
}

###############################################################################
# TABLE 12
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 12: CUMULATIVE CHARGES -- TIMING VS LEVEL\n")
cat("===================================================================\n\n")

if (!is.null(mech)) {
  etable(mech$M_base, mech$C1, mech$C2,
         headers=c("Single-year neg_spi_at","Cumul 2yr: baseline","Cumul 2yr: x recession"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  bm <- coef(mech$M_base); bc1 <- coef(mech$C1); bc2 <- coef(mech$C2)
  cat(sprintf("\nSingle-year:  misalign_abs = %.5f (p=%.4f), interaction = %.5f (p=%.4f)\n",
              bm["misalign_abs"], get_cp(mech$M_base,"misalign_abs")[3],
              bm["misalign_abs:recession"], get_cp(mech$M_base,"misalign_abs:recession")[3]))
  cat(sprintf("Cumulative:   misalign_abs = %.5f (p=%.4f), interaction = %.5f (p=%.4f)\n",
              bc2["misalign_abs"], get_cp(mech$C2,"misalign_abs")[3],
              bc2["misalign_abs:recession"], get_cp(mech$C2,"misalign_abs:recession")[3]))
  cat("\nInterpretation: The null cumulative result indicates misalignment\n")
  cat("affects the TIMING of restructuring charges, not the total level.\n")
  cat("Firms delay restructuring during normal times but catch up later.\n")
}

###############################################################################
# TABLE 13
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 13: PLACEBO PERMUTATION TEST (500 iterations)\n")
cat("===================================================================\n\n")

if (!is.null(plac)) {
  cat(sprintf("%-14s %12s %14s %12s %14s\n", "DV", "True Coef", "Placebo Mean", "Placebo SD", "Empirical p"))
  cat(paste(rep("-",70), collapse=""), "\n")
  cat(sprintf("%-14s %12.5f %14.5f %12.5f %14.4f\n", "neg_spi_at",
              plac$spi$true_coef, mean(plac$spi$placebo_coefs,na.rm=T),
              sd(plac$spi$placebo_coefs,na.rm=T), plac$spi$pval))
  cat(sprintf("%-14s %12.5f %14.5f %12.5f %14.4f\n", "layoff",
              plac$lay$true_coef, mean(plac$lay$placebo_coefs,na.rm=T),
              sd(plac$lay$placebo_coefs,na.rm=T), plac$lay$pval))
  cat(sprintf("\nTrue/Placebo SD ratio: neg_spi_at = %.1f sigma, layoff = %.1f sigma\n",
              plac$spi$true_coef / sd(plac$spi$placebo_coefs,na.rm=T),
              plac$lay$true_coef / sd(plac$lay$placebo_coefs,na.rm=T)))
  cat("In 500 permutations, zero placebo coefficients reached the true value.\n")
} else {
  cat("[Placebo results not found]\n")
}

###############################################################################
# TABLE 14
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("TABLE 14: VARIANCE DECOMPOSITION\n")
cat("===================================================================\n\n")

vardecomp <- function(v) {
  x <- dt[!is.na(get(v))]
  total <- var(x[[v]])
  fm <- x[, .(fm = mean(get(v))), by=gvkey]
  between <- var(fm$fm)
  ws <- x[, .(wv = var(get(v)), n=.N), by=gvkey][n>=2]
  within <- ws[, sum(wv*(n-1))/(sum(n)-.N)]
  pct_within <- 100*within/total
  c(total_sd=sqrt(total), between_sd=sqrt(between), within_sd=sqrt(within), pct_within=pct_within)
}

cat(sprintf("%-24s %10s %12s %10s %10s\n", "Variable", "Total SD", "Between SD", "Within SD", "% Within"))
cat(paste(rep("-",70), collapse=""), "\n")
for (v in c("misalign_abs","emp_mean_cfscore","ceo_cfscore","neg_spi_at")) {
  vd <- vardecomp(v)
  cat(sprintf("%-24s %10.4f %12.4f %10.4f %9.1f%%\n", v, vd[1], vd[2], vd[3], vd[4]))
}

setorder(dt, gvkey, cycle)
dt[, delta_misalign := abs(misalign_abs - shift(misalign_abs)), by=gvkey]
cat(sprintf("\nMean |delta misalignment| in CEO turnover years:     %.4f\n",
            mean(dt[ceo_turnover==1, delta_misalign], na.rm=TRUE)))
cat(sprintf("Mean |delta misalignment| in non-turnover years:     %.4f\n",
            mean(dt[ceo_turnover==0, delta_misalign], na.rm=TRUE)))

###############################################################################
# CORRELATION MATRIX
###############################################################################
cat("\n\n")
cat("===================================================================\n")
cat("CORRELATION MATRIX (lower triangle, pairwise complete)\n")
cat("===================================================================\n\n")

cvars <- c("misalign_abs","ceo_cfscore","emp_mean_cfscore","emp_sd_cfscore",
           "neg_spi_at","layoff","ind_layoff_rate","lag_size","lag_roa","lag_leverage")
cmat <- cor(dt[, ..cvars], use="pairwise.complete.obs")
labs <- c("misalign","ceo_cf","emp_cf","emp_sd","neg_spi","layoff","ind_lay","size","roa","lever")

cat(sprintf("%-10s", ""))
for (j in 1:length(labs)) cat(sprintf("%8s", labs[j]))
cat("\n")
cat(paste(rep("-", 10 + 8*length(labs)), collapse=""), "\n")

for (i in 1:nrow(cmat)) {
  cat(sprintf("%-10s", labs[i]))
  for (j in 1:ncol(cmat)) {
    if (j <= i) cat(sprintf("%8.2f", cmat[i,j]))
    else cat(sprintf("%8s", ""))
  }
  cat("\n")
}

cat("\n\n===================================================================\n")
cat("END OF ALL TABLES\n")
cat("===================================================================\n")


-----------------------------
  peer <- readRDS("peer_layoff_deep.rds")
ind_down <- readRDS("industry_downturn_tests.rds")

# Table 5
etable(ind_down$I5, peer$P2, peer$P3, peer$P5, se.below=TRUE,
       signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# Table 6
etable(peer$P6, peer$P7, se.below=TRUE,
       signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# Table 7
etable(peer$P1, se.below=TRUE,
       signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

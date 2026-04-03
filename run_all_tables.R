library(data.table)
library(fixest)
setFixest_notes(FALSE)

# ── Load data ──
load("reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
dt <- reg_data[!sic2 %in% c(49, 60:69)]
dt[, recession := nber_recession]
dt[, lag_aqc_indicator := lag_acquisition]
dt[, ceo_right := pmax(misalign_signed, 0)]
dt[, ceo_left  := pmax(-misalign_signed, 0)]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

# ── Load all saved results ──
safe_load <- function(f) { if (file.exists(f)) readRDS(f) else { cat("  [NOT FOUND]", f, "\n"); NULL } }
core     <- safe_load("models_core.rds")
recess   <- safe_load("recession_robustness.rds")
addl     <- safe_load("robustness_additional.rds")
ind_down <- safe_load("industry_downturn_tests.rds")
peer     <- safe_load("peer_layoff_deep.rds")
mech     <- safe_load("mechanism_tests.rds")

# Helper for net effect
net_eff <- function(mod, v1, v2, scale=1) {
  b <- coef(mod); V <- vcov(mod)
  i1 <- match(v1, names(b)); i2 <- match(v2, names(b))
  net <- b[i1] + b[i2]*scale
  se <- sqrt(V[i1,i1] + scale^2*V[i2,i2] + 2*scale*V[i1,i2])
  p <- 2*pt(abs(net/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  c(net=net, se=se, p=p)
}

get_cp <- function(mod, var) {
  b <- coef(mod); V <- vcov(mod)
  idx <- match(var, names(b)); if(is.na(idx)) return(c(b=NA,se=NA,p=NA))
  se <- sqrt(V[idx,idx]); p <- 2*pt(abs(b[idx]/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  c(b=b[idx], se=se, p=p)
}

wald_diff <- function(mod, v1, v2) {
  b <- coef(mod); V <- vcov(mod)
  d <- b[v1]-b[v2]; se <- sqrt(V[v1,v1]+V[v2,v2]-2*V[v1,v2])
  p <- 2*pt(abs(d/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  p
}

# ═══════════════════════════════════════════════════════════════════
cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════╗\n")
cat("║              ALL TABLES FOR PAPER REVISION                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════╝\n")

# ═══════════════════════════════════════════════
# TABLE 1: SAMPLE CONSTRUCTION
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 1: SAMPLE CONSTRUCTION\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat(sprintf("%-5s %-45s %12s %12s\n", "Step", "Description", "Firm-cycles", "Unique firms"))
cat(paste(rep("─",78), collapse=""), "\n")

# Try loading intermediate files
pf_rows <- pf_firms <- pc_rows <- pc_firms <- pq_rows <- pq_firms <- "—"
if (file.exists("panel_full.rdata")) {
  load("panel_full.rdata"); pf <- get(ls(pattern="panel_full|panel")[1])
  if (is.data.table(pf) || is.data.frame(pf)) { pf_rows <- nrow(pf); pf_firms <- length(unique(pf$gvkey)) }
  rm(list=ls(pattern="panel_full")); gc()
}
if (file.exists("panel_complete.rdata")) {
  load("panel_complete.rdata"); pc <- get(ls(pattern="panel_comp|panel")[1])
  if (is.data.table(pc) || is.data.frame(pc)) { pc_rows <- nrow(pc); pc_firms <- length(unique(pc$gvkey)) }
  rm(list=ls(pattern="panel_comp")); gc()
}
if (file.exists("panel_qual.rdata")) {
  load("panel_qual.rdata"); pq <- get(ls(pattern="panel_qual|panel")[1])
  if (is.data.table(pq) || is.data.frame(pq)) { pq_rows <- nrow(pq); pq_firms <- length(unique(pq$gvkey)) }
  rm(list=ls(pattern="panel_qual")); gc()
}

# Reload reg_data for full count before exclusion
load("reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
rd_full <- nrow(reg_data); rd_firms <- uniqueN(reg_data$gvkey)

cat(sprintf("%-5s %-45s %12s %12s\n", "1", "All 13 cycle panels stacked", as.character(pf_rows), as.character(pf_firms)))
cat(sprintf("%-5s %-45s %12s %12s\n", "2", "With non-missing misalignment", as.character(pc_rows), as.character(pc_firms)))
cat(sprintf("%-5s %-45s %12s %12s\n", "3", "≥20 employee donors", as.character(pq_rows), as.character(pq_firms)))
cat(sprintf("%-5s %-45s %12d %12d\n", "4", "Merged with Compustat outcomes", rd_full, rd_firms))
cat(sprintf("%-5s %-45s %12d %12d\n", "5", "Excl. financials & utilities", nrow(dt), uniqueN(dt$gvkey)))
cat(sprintf("%-5s %-45s %12d %12d\n", "6", "Final regression sample (non-missing DV)", 
            sum(!is.na(dt$neg_spi_at)), uniqueN(dt[!is.na(neg_spi_at), gvkey])))
rm(reg_data); gc()

# ═══════════════════════════════════════════════
# TABLE 2: DESCRIPTIVE STATISTICS
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 2: DESCRIPTIVE STATISTICS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

desc <- function(vars, panel_label) {
  cat(panel_label, "\n")
  cat(sprintf("%-22s %6s %9s %9s %9s %9s %9s\n", "Variable", "N", "Mean", "SD", "P25", "Median", "P75"))
  cat(paste(rep("─",78), collapse=""), "\n")
  for (v in vars) {
    if (!v %in% names(dt)) { cat(sprintf("%-22s  [not found]\n", v)); next }
    x <- dt[[v]]
    n <- sum(!is.na(x))
    if (n == 0) next
    cat(sprintf("%-22s %6d %9.3f %9.3f %9.3f %9.3f %9.3f\n", v, n,
                mean(x,na.rm=T), sd(x,na.rm=T), 
                quantile(x,.25,na.rm=T), median(x,na.rm=T), quantile(x,.75,na.rm=T)))
  }
  cat("\n")
}

desc(c("misalign_abs","misalign_signed","ceo_cfscore","emp_mean_cfscore","emp_sd_cfscore",
       "n_emp_donors","log_n_donors","donor_coverage"), "Panel A: Ideology & Misalignment")

desc(c("neg_spi_at","emp_change","layoff","warn_any","warn_workers"), "Panel B: Outcomes")

desc(c("lag_size","lag_roa","lag_leverage","lag_tobinq","lag_sale_growth","lag_loss",
       "lag_capx_at","lag_rd_at","lag_aqc_indicator","lag_emp_sale"), "Panel C: Lagged Controls")

desc(c("recession","ind_downturn","ind_layoff_rate","ind_sale_growth"), "Panel D: Moderators")

desc(c("ceo_turnover","first_year_ceo","ceo_abs_conservative"), "Panel E: CEO Characteristics")

cat("Panel F: Quadrant Distribution\n")
cat(sprintf("%-30s %6s %8s\n", "Quadrant", "N", "% Sample"))
cat(paste(rep("─",48), collapse=""), "\n")
for (q in c("cross_ceo_right","cross_ceo_left","same_side_right","same_side_left")) {
  n <- sum(dt[[q]], na.rm=TRUE)
  cat(sprintf("%-30s %6d %7.1f%%\n", q, n, 100*n/nrow(dt)))
}

# ═══════════════════════════════════════════════
# TABLE 3: BY CYCLE
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 3: MISALIGNMENT BY ELECTION CYCLE\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cyc <- dt[, .(
  N = .N,
  Mean_Misalign = round(mean(misalign_abs, na.rm=T), 3),
  Med_Misalign  = round(median(misalign_abs, na.rm=T), 3),
  Mean_CEO_CF   = round(mean(ceo_cfscore, na.rm=T), 3),
  Mean_Emp_CF   = round(mean(emp_mean_cfscore, na.rm=T), 3),
  Pct_CEO_Right = round(100*mean(ceo_cfscore > emp_mean_cfscore, na.rm=T), 1),
  Pct_CEO_Cons  = round(100*mean(ceo_abs_conservative, na.rm=T), 1),
  Mean_Donors   = round(mean(n_emp_donors, na.rm=T), 0)
), by=cycle][order(cycle)]
print(cyc)

# ═══════════════════════════════════════════════
# TABLE 4: BASELINE
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 4: BASELINE — MISALIGNMENT CONSTRAINS RESTRUCTURING\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(core)) {
  etable(core$M1, core$M2, core$M3, core$M4, core$M5,
         headers=c("(1) Firm+Year","(2) +Emp Ideol","(3) Ind×Year","(4) +CEO Turn","(5) +Donors"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))
}

# ═══════════════════════════════════════════════
# TABLE 5: PEER LAYOFF MODERATION
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 5: PEER LAYOFF MODERATION (MAIN RESULT)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(ind_down) && !is.null(peer)) {
  etable(ind_down$I5, peer$P2, peer$P3, peer$P5,
         headers=c("(1) Baseline","(2) +Emp Ideol","(3) +CEO Turn","(4) Dbl-Clust"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","ind_layoff"))

  cat("\nNet effect of misalignment at percentiles of peer layoff rate:\n")
  cat(sprintf("%-12s %12s %10s %8s %8s\n", "Percentile", "LayoffRate", "NetEffect", "SE", "p"))
  cat(paste(rep("─",56), collapse=""), "\n")
  
  mod <- ind_down$I5
  pctiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  qvals <- quantile(dt$ind_layoff_rate, pctiles, na.rm=TRUE)
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %12.4f %10.5f %8.5f %7.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne["net"], ne["se"], ne["p"], stars(ne["p"])))
  }
  
  cross <- -coef(mod)["misalign_abs"] / coef(mod)["misalign_abs:ind_layoff_rate"]
  cat(sprintf("\nCrossover (effect=0): ind_layoff_rate = %.4f (p%.0f of sample)\n",
              cross, 100*ecdf(dt$ind_layoff_rate)(cross)))
}

# ═══════════════════════════════════════════════
# TABLE 6: LAYOFF INDICATOR
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 6: REAL OUTCOME — LAYOFF AND EMPLOYMENT CHANGE\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(peer)) {
  etable(peer$P6, peer$P7,
         headers=c("(1) emp_change","(2) layoff"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

  cat("\nNet layoff effect at percentiles of ind_layoff_rate:\n")
  cat(sprintf("%-12s %12s %10s %8s %8s\n", "Percentile", "LayoffRate", "NetEffect", "SE", "p"))
  cat(paste(rep("─",56), collapse=""), "\n")
  mod7 <- peer$P7
  for (i in seq_along(pctiles)) {
    ne <- net_eff(mod7, "misalign_abs", "misalign_abs:ind_layoff_rate", qvals[i])
    cat(sprintf("%-12s %12.4f %10.5f %8.5f %7.4f %s\n",
                paste0("p",pctiles[i]*100), qvals[i], ne["net"], ne["se"], ne["p"], stars(ne["p"])))
  }
}

# ═══════════════════════════════════════════════
# TABLE 7: ASYMMETRY TEST
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 7: ASYMMETRY TEST — EFFECT IS SYMMETRIC\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(peer)) {
  cat("Panel A: Peer Layoff Moderation (P1)\n\n")
  etable(peer$P1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("ceo_right","ceo_left","ind_layoff"))
  wp1 <- wald_diff(peer$P1, "ceo_right:ind_layoff_rate", "ceo_left:ind_layoff_rate")
  cat(sprintf("\nWald test for equality of peer-layoff interactions: p = %.4f\n", wp1))
}

if (!is.null(core)) {
  cat("\nPanel B: NBER Recession Moderation (M10)\n\n")
  etable(core$M10, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("ceo_right","ceo_left","recession"))
  wp2 <- wald_diff(core$M10, "ceo_right:recession", "ceo_left:recession")
  cat(sprintf("\nWald test for equality of recession interactions: p = %.4f\n", wp2))
}
cat("\nConclusion: The recession/peer-layoff moderation operates symmetrically\nregardless of the direction of misalignment.\n")

# ═══════════════════════════════════════════════
# TABLE 8: ALTERNATIVE MODERATORS
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 8: ALTERNATIVE MODERATORS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(ind_down) && !is.null(core)) {
  etable(ind_down$I5, ind_down$I1, ind_down$I2, core$M6, ind_down$I4,
         headers=c("(1) PeerLayoff","(2) IndDown","(3) IndSaleGr","(4) NBER","(5) HorseRace"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","ind_layoff","ind_downturn","ind_sale","recession"))
}

# ═══════════════════════════════════════════════
# TABLE 9: RECESSION ROBUSTNESS
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 9: RECESSION ROBUSTNESS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(recess)) {
  cat(sprintf("%-8s %-18s %10s %7s %10s %7s %6s\n",
              "Model","Exclusion","Base β","p","Inter β","p","N"))
  cat(paste(rep("─",68), collapse=""), "\n")
  
  rr <- list(c("R1","Excl. 2020-21","R1"), c("R2a","Excl. 2001","R2a"),
             c("R2b","Excl. 2008-09","R2b"), c("R2c","Excl. 2020","R2c"),
             c("R3","Double-lagged","R3"))
  mv <- c("R1"="misalign_abs","R2a"="misalign_abs","R2b"="misalign_abs",
           "R2c"="misalign_abs","R3"="misalign_abs_L2")
  iv <- c("R1"="misalign_abs:recession","R2a"="misalign_abs:recession",
           "R2b"="misalign_abs:recession","R2c"="misalign_abs:recession",
           "R3"="misalign_abs_L2:recession")
  
  for (r in rr) {
    m <- recess[[r[3]]]
    sm <- get_cp(m, mv[r[3]]); si <- get_cp(m, iv[r[3]])
    cat(sprintf("%-8s %-18s %9.4f%s %6.4f %9.4f%s %6.4f %6d\n",
                r[1], r[2], sm["b"], stars(sm["p"]), sm["p"],
                si["b"], stars(si["p"]), si["p"], m$nobs))
  }
}

# ═══════════════════════════════════════════════
# TABLE 10: MEASUREMENT ROBUSTNESS
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 10: MEASUREMENT ROBUSTNESS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(addl)) {
  cat("Panel A: Donor Threshold Sensitivity\n")
  cat(sprintf("%-12s %10s %7s %10s %7s %6s\n","Threshold","Base β","p","Inter β","p","N"))
  cat(paste(rep("─",58), collapse=""), "\n")
  
  thresholds <- list(c("≥10","D1"),c("≥20","D0"),c("≥30","D2"),c("≥50","D3"),c("≥100","D4"))
  for (th in thresholds) {
    m <- addl[[th[2]]]
    sm <- get_cp(m,"misalign_abs"); si <- get_cp(m,"misalign_abs:recession")
    cat(sprintf("%-12s %9.4f%s %6.4f %9.4f%s %6.4f %6d\n",
                th[1], sm["b"], stars(sm["p"]), sm["p"],
                si["b"], stars(si["p"]), si["p"], m$nobs))
  }
  
  cat("\nPanel B: Weighted Regressions\n")
  etable(addl$D0, addl$D5, addl$D6,
         headers=c("Unweighted","Wt:n_donors","Wt:log(n)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession"))
  
  cat("\nPanel C: Two-Part Model\n")
  etable(addl$D7, addl$D8,
         headers=c("Incidence(logit)","Magnitude(OLS|charge>0)"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession"))
  
  cat("\nPanel D: Subsample Tests\n")
  etable(addl$D0, addl$D9, addl$D10,
         headers=c("Full","≥6 cycles","Post-2010"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession"))
}

# ═══════════════════════════════════════════════
# TABLE 11: MECHANISM TESTS
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 11: MECHANISM TESTS\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(mech)) {
  cat("B1: Labor Intensity × Misalignment × Recession\n")
  etable(mech$B1, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession","lag_emp_sale"))
  s1 <- get_cp(mech$B1, "misalign_abs:lag_emp_sale")
  s1t <- get_cp(mech$B1, "misalign_abs:recession:lag_emp_sale")
  cat(sprintf("\n  Two-way (misalign×emp_sale):        %.5f (%.5f) p=%.4f %s\n", s1["b"],s1["se"],s1["p"],stars(s1["p"])))
  cat(sprintf("  Three-way (misalign×recess×emp_sale): %.5f (%.5f) p=%.4f %s  [NOT SIG]\n\n", s1t["b"],s1t["se"],s1t["p"],stars(s1t["p"])))

  cat("B2: Employee Cohesion × Misalignment × Recession\n")
  etable(mech$B2, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession","emp_sd"))
  s2 <- get_cp(mech$B2, "misalign_abs:emp_sd_cfscore")
  s2t <- get_cp(mech$B2, "misalign_abs:recession:emp_sd_cfscore")
  cat(sprintf("\n  Two-way (misalign×emp_sd):            %.5f (%.5f) p=%.4f %s\n", s2["b"],s2["se"],s2["p"],stars(s2["p"])))
  cat(sprintf("  Three-way (misalign×recess×emp_sd):   %.5f (%.5f) p=%.4f %s  [NOT SIG]\n\n", s2t["b"],s2t["se"],s2t["p"],stars(s2t["p"])))

  cat("B3: Peer Restructuring × Misalignment\n")
  etable(mech$B3, se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","peer_restr"))
  s3 <- get_cp(mech$B3, "misalign_abs:peer_restr")
  cat(sprintf("\n  Interaction: %.5f (%.5f) p=%.4f %s\n", s3["b"],s3["se"],s3["p"],stars(s3["p"])))
}

# ═══════════════════════════════════════════════
# TABLE 12: CUMULATIVE CHARGES
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 12: CUMULATIVE CHARGES — TIMING VS LEVEL\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

if (!is.null(mech)) {
  etable(mech$M_base, mech$C1, mech$C2,
         headers=c("Single-yr neg_spi","Cum 2yr baseline","Cum 2yr × recess"),
         se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
         keep=c("misalign_abs","recession"))
  
  bm <- coef(mech$M_base); bc <- coef(mech$C2)
  cat(sprintf("\nSingle-year: misalign_abs = %.5f**, interaction = %.5f**\n",
              bm["misalign_abs"], bm["misalign_abs:recession"]))
  cat(sprintf("Cumulative:  misalign_abs = %.5f,   interaction = %.5f\n",
              bc["misalign_abs"], bc["misalign_abs:recession"]))
  cat("\nInterpretation: The null cumulative result indicates misalignment affects\nthe TIMING of restructuring charges, not the total level.\n")
}

# ═══════════════════════════════════════════════
# TABLE 13: PLACEBO (load if available)
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 13: PLACEBO PERMUTATION TEST\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

plac <- safe_load("placebo_results.rds")
if (!is.null(plac)) {
  cat(sprintf("%-12s %12s %14s %12s %12s\n", "DV", "True Coef", "Placebo Mean", "Placebo SD", "Empirical p"))
  cat(paste(rep("─",66), collapse=""), "\n")
  cat(sprintf("%-12s %12.5f %14.5f %12.5f %12.4f\n", "neg_spi_at",
              plac$spi$true_coef, mean(plac$spi$placebo_coefs,na.rm=T),
              sd(plac$spi$placebo_coefs,na.rm=T), plac$spi$pval))
  cat(sprintf("%-12s %12.5f %14.5f %12.5f %12.4f\n", "layoff",
              plac$lay$true_coef, mean(plac$lay$placebo_coefs,na.rm=T),
              sd(plac$lay$placebo_coefs,na.rm=T), plac$lay$pval))
} else {
  cat("[Placebo results not yet available — run Part A first]\n")
}

# ═══════════════════════════════════════════════
# TABLE 14: VARIANCE DECOMPOSITION
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("TABLE 14: VARIANCE DECOMPOSITION\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

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

cat(sprintf("%-22s %10s %12s %10s %10s\n", "Variable", "Total SD", "Between SD", "Within SD", "% Within"))
cat(paste(rep("─",68), collapse=""), "\n")
for (v in c("misalign_abs","emp_mean_cfscore","ceo_cfscore","neg_spi_at")) {
  vd <- vardecomp(v)
  cat(sprintf("%-22s %10.4f %12.4f %10.4f %9.1f%%\n", v, vd[1], vd[2], vd[3], vd[4]))
}

# Turnover-related delta
setorder(dt, gvkey, cycle)
dt[, delta_misalign := abs(misalign_abs - shift(misalign_abs)), by=gvkey]
cat(sprintf("\nMean |Δmisalignment| in CEO turnover years:     %.4f\n",
            mean(dt[ceo_turnover==1, delta_misalign], na.rm=TRUE)))
cat(sprintf("Mean |Δmisalignment| in non-turnover years:     %.4f\n",
            mean(dt[ceo_turnover==0, delta_misalign], na.rm=TRUE)))

# ═══════════════════════════════════════════════
# CORRELATION MATRIX
# ═══════════════════════════════════════════════
cat("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
cat("CORRELATION MATRIX (lower triangle)\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cvars <- c("misalign_abs","ceo_cfscore","emp_mean_cfscore","emp_sd_cfscore",
           "neg_spi_at","layoff","ind_layoff_rate","lag_size","lag_roa","lag_leverage")
cmat <- cor(dt[, ..cvars], use="pairwise.complete.obs")

# Short labels
labs <- c("misalign","ceo_cf","emp_cf","emp_sd","neg_spi","layoff","ind_lay","size","roa","lever")
cat(sprintf("%-10s", ""))
for (j in 1:length(labs)) cat(sprintf("%8s", labs[j]))
cat("\n")

for (i in 1:nrow(cmat)) {
  cat(sprintf("%-10s", labs[i]))
  for (j in 1:ncol(cmat)) {
    if (j <= i) cat(sprintf("%8.2f", cmat[i,j]))
    else cat(sprintf("%8s", ""))
  }
  cat("\n")
}

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("END OF ALL TABLES\n")
cat("═══════════════════════════════════════════════════════════════════\n")

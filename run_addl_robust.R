library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v2.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
rd <- reg_data[!sic2 %in% c(49, 60:69)]
rd[, recession := nber_recession]
rd[, lag_aqc_indicator := lag_acquisition]
rd[, charge_ind := as.integer(neg_spi_at > 0)]
rd[, charge_mag := fifelse(neg_spi_at > 0, neg_spi_at, NA_real_)]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"
base_fml <- paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

get_stats <- function(mod, main="misalign_abs", inter="misalign_abs:recession") {
  b <- coef(mod); V <- vcov(mod)
  im <- match(main, names(b)); ii <- match(inter, names(b))
  se_m <- sqrt(V[im,im]); p_m <- 2*pt(abs(b[im]/se_m), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  se_i <- sqrt(V[ii,ii]); p_i <- 2*pt(abs(b[ii]/se_i), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  net <- b[im]+b[ii]; se_n <- sqrt(V[im,im]+V[ii,ii]+2*V[im,ii])
  p_n <- 2*pt(abs(net/se_n), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  list(main_b=b[im], main_se=se_m, main_p=p_m,
       int_b=b[ii], int_se=se_i, int_p=p_i,
       net=net, net_se=se_n, net_p=p_n, N=mod$nobs)
}

# ════════════════════════════════════════════════
# DONOR THRESHOLD SENSITIVITY
# ════════════════════════════════════════════════
cat("Donor threshold sample sizes:\n")
for (thr in c(10,20,30,50,100)) {
  cat(sprintf("  >= %3d donors: %d obs\n", thr, nrow(rd[n_emp_donors >= thr & !is.na(neg_spi_at)])))
}

D1 <- feols(as.formula(base_fml), data=rd[n_emp_donors >= 10], cluster=~gvkey)
D2 <- feols(as.formula(base_fml), data=rd[n_emp_donors >= 30], cluster=~gvkey)
D3 <- feols(as.formula(base_fml), data=rd[n_emp_donors >= 50], cluster=~gvkey)
D4 <- feols(as.formula(base_fml), data=rd[n_emp_donors >= 100], cluster=~gvkey)

# Also the base (>=20, which is the current sample)
D0 <- feols(as.formula(base_fml), data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# WEIGHTED REGRESSIONS
# ════════════════════════════════════════════════
D5 <- feols(as.formula(base_fml), data=rd, weights=~n_emp_donors, cluster=~gvkey)
D6 <- feols(as.formula(base_fml), data=rd, weights=~log_n_donors, cluster=~gvkey)

# ════════════════════════════════════════════════
# TWO-PART MODEL
# ════════════════════════════════════════════════
cat("\nTwo-part model stats:\n")
cat("  charge_ind=1:", sum(rd$charge_ind, na.rm=TRUE), "of", sum(!is.na(rd$charge_ind)), 
    "(", round(100*mean(rd$charge_ind, na.rm=TRUE),1), "%)\n")
cat("  charge_mag mean (conditional):", round(mean(rd$charge_mag, na.rm=TRUE), 4), "\n")

# D7: logit for incidence (use feglm)
D7 <- feglm(as.formula(paste("charge_ind ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
             data=rd, family=binomial, cluster=~gvkey)

# D8: OLS for magnitude (conditional on charge > 0)
D8 <- feols(as.formula(paste("charge_mag ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

# ════════════════════════════════════════════════
# OTHER
# ════════════════════════════════════════════════
# D9: Firms with >= 6 cycles
firm_cycles <- rd[, .N, by=gvkey]
long_firms <- firm_cycles[N >= 6, gvkey]
cat("\nFirms with >= 6 cycles:", length(long_firms), "firms,", 
    nrow(rd[gvkey %in% long_firms]), "obs\n")

D9 <- feols(as.formula(base_fml), data=rd[gvkey %in% long_firms], cluster=~gvkey)

# D10: cycles >= 2010
D10 <- feols(as.formula(base_fml), data=rd[cycle >= 2010], cluster=~gvkey)

# ════════════════════════════════════════════════
# VARIANCE DECOMPOSITION
# ════════════════════════════════════════════════
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("VARIANCE DECOMPOSITION OF misalign_abs\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

rd_v <- rd[!is.na(misalign_abs)]
total_var <- var(rd_v$misalign_abs)
firm_means <- rd_v[, .(fm = mean(misalign_abs)), by=gvkey]
between_var <- var(firm_means$fm)
# Within = total - between (law of total variance adjusted for unbalanced panel)
# More precise: compute within as mean of within-firm variances weighted by n
within_stats <- rd_v[, .(wvar = var(misalign_abs), n = .N), by=gvkey][n >= 2]
within_var <- within_stats[, sum(wvar * (n-1)) / (sum(n) - .N)]

cat(sprintf("  Total variance:   %.4f\n", total_var))
cat(sprintf("  Between-firm:     %.4f (%.1f%%)\n", between_var, 100*between_var/total_var))
cat(sprintf("  Within-firm:      %.4f (%.1f%%)\n", within_var, 100*within_var/total_var))
cat(sprintf("  (Between+Within ≈ %.4f vs Total %.4f — gap due to unbalanced panel)\n",
            between_var + within_var, total_var))

# Mean absolute change across cycles
rd_v2 <- copy(rd_v)
setorder(rd_v2, gvkey, cycle)
rd_v2[, delta_misalign := abs(misalign_abs - shift(misalign_abs)), by=gvkey]

# CEO turnover: did the firm experience turnover in this or adjacent year?
cat("\nMean |Δmisalign_abs| across cycles:\n")
cat(sprintf("  All firms:                  %.4f\n", mean(rd_v2$delta_misalign, na.rm=TRUE)))

# Compare firms with/without any CEO turnover
firms_with_turnover <- rd_v2[ceo_turnover == 1, unique(gvkey)]
cat(sprintf("  Firms WITH CEO turnover:    %.4f (N firms = %d)\n",
    mean(rd_v2[gvkey %in% firms_with_turnover, delta_misalign], na.rm=TRUE), length(firms_with_turnover)))
cat(sprintf("  Firms WITHOUT CEO turnover: %.4f (N firms = %d)\n",
    mean(rd_v2[!gvkey %in% firms_with_turnover, delta_misalign], na.rm=TRUE),
    uniqueN(rd_v2[!gvkey %in% firms_with_turnover, gvkey])))

# More precise: compare delta at turnover events vs non-events
cat("\nMean |Δmisalign_abs| at turnover events vs non-events:\n")
cat(sprintf("  Turnover year:     %.4f (N = %d)\n",
    mean(rd_v2[ceo_turnover == 1, delta_misalign], na.rm=TRUE),
    sum(!is.na(rd_v2[ceo_turnover == 1, delta_misalign]))))
cat(sprintf("  Non-turnover year: %.4f (N = %d)\n",
    mean(rd_v2[ceo_turnover == 0, delta_misalign], na.rm=TRUE),
    sum(!is.na(rd_v2[ceo_turnover == 0, delta_misalign]))))

# ════════════════════════════════════════════════
# RESULTS
# ════════════════════════════════════════════════
cat("\n\n════════════════════════════════════════════════════════════════════\n")
cat("DONOR THRESHOLD SENSITIVITY\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

etable(D0, D1, D2, D3, D4,
       headers=c("D0:>=20","D1:>=10","D2:>=30","D3:>=50","D4:>=100"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession"))

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("COMPACT DONOR THRESHOLD TABLE\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("%-10s %9s %7s %9s %7s %9s %7s %6s\n",
            "Threshold","Main_b","Main_p","Inter_b","Inter_p","Net_b","Net_p","N"))
cat(paste(rep("─",72), collapse=""),"\n")
for (nm in c("D0","D1","D2","D3","D4")) {
  m <- get(nm)
  s <- get_stats(m)
  lbl <- switch(nm, D0=">=20", D1=">=10", D2=">=30", D3=">=50", D4=">=100")
  cat(sprintf("%-10s %8.4f%s %7.4f %8.4f%s %7.4f %8.4f%s %7.4f %6d\n",
              lbl, s$main_b, stars(s$main_p), s$main_p,
              s$int_b, stars(s$int_p), s$int_p,
              s$net, stars(s$net_p), s$net_p, s$N))
}

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("WEIGHTED REGRESSIONS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(D0, D5, D6,
       headers=c("Unweighted","Wt:n_donors","Wt:log(n)"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession"))

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("TWO-PART MODEL\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(D7, D8,
       headers=c("D7:Incidence(logit)","D8:Magnitude(OLS)"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession"))

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("OTHER ROBUSTNESS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
etable(D0, D9, D10,
       headers=c("D0:Full","D9:>=6cycles","D10:post2010"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1),
       keep=c("misalign_abs","recession"))

# ── Grand summary ──
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("GRAND SUMMARY: ALL ADDITIONAL ROBUSTNESS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("%-25s %9s %7s %9s %7s %6s\n","Model","Main_b","Main_p","Inter_b","Inter_p","N"))
cat(paste(rep("─",68), collapse=""),"\n")

all_mods <- list(
  "D0: Base (>=20)"=D0, "D1: >=10 donors"=D1, "D2: >=30 donors"=D2,
  "D3: >=50 donors"=D3, "D4: >=100 donors"=D4,
  "D5: Wt by n_donors"=D5, "D6: Wt by log(n)"=D6,
  "D7: Incidence (logit)"=D7, "D8: Magnitude (OLS)"=D8,
  "D9: >=6 cycles"=D9, "D10: Post-2010"=D10)

for (nm in names(all_mods)) {
  m <- all_mods[[nm]]
  b <- coef(m); V <- vcov(m)
  im <- match("misalign_abs", names(b))
  ii <- match("misalign_abs:recession", names(b))
  se_m <- sqrt(V[im,im]); p_m <- 2*pt(abs(b[im]/se_m), df=degrees_freedom(m,"t"), lower.tail=FALSE)
  se_i <- sqrt(V[ii,ii]); p_i <- 2*pt(abs(b[ii]/se_i), df=degrees_freedom(m,"t"), lower.tail=FALSE)
  cat(sprintf("%-25s %8.4f%s %7.4f %8.4f%s %7.4f %6d\n",
              nm, b[im], stars(p_m), p_m, b[ii], stars(p_i), p_i, m$nobs))
}

# Save
results <- list(D0=D0,D1=D1,D2=D2,D3=D3,D4=D4,D5=D5,D6=D6,D7=D7,D8=D8,D9=D9,D10=D10)
saveRDS(results, file="robustness_additional.rds")
cat("\n\nSaved robustness_additional.rds\n")

library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v2.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
rd <- reg_data[!sic2 %in% c(49, 60:69)]
rd[, recession := nber_recession]
rd[, lag_aqc_indicator := lag_acquisition]
rd[, ceo_right := pmax(misalign_signed, 0)]
rd[, ceo_left  := pmax(-misalign_signed, 0)]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"
stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

cat("ind_layoff_rate quantiles:\n")
print(quantile(rd$ind_layoff_rate, c(0.1,0.25,0.5,0.75,0.9), na.rm=TRUE))
cat("N obs with non-missing ind_layoff_rate:", sum(!is.na(rd$ind_layoff_rate)), "\n\n")

# ════════════════════════════════════════════════
# A: FULL BATTERY
# ════════════════════════════════════════════════

P1 <- feols(as.formula(paste("neg_spi_at ~ ceo_right + ceo_left + ceo_right:ind_layoff_rate + ceo_left:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

P2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate + emp_mean_cfscore + emp_sd_cfscore +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

P3 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate + ceo_turnover + first_year_ceo +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

P4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + sic2^fyear")),
            data=rd, cluster=~gvkey)

P5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey+fyear)

P6 <- feols(as.formula(paste("emp_change ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

P7 <- feols(as.formula(paste("layoff ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

# Also re-run the baseline I5 for net effect computations
I5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data=rd, cluster=~gvkey)

# ── Print tables ──
cat("════════════════════════════════════════════════════════════════════\n")
cat("PANEL A: PEER LAYOFF ROBUSTNESS (DV = neg_spi_at except P6, P7)\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

etable(I5, P1, P2, P3, P4, P5,
       headers=c("I5:Base","P1:Asymm","P2:EmpIdeol","P3:Turnover","P4:Ind×YrFE","P5:DblClust"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("PANEL A (cont): ALTERNATIVE DVs\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

etable(P6, P7,
       headers=c("P6:emp_change","P7:layoff"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# ── Wald test P1 ──
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("WALD TEST (P1): ceo_right:ind_layoff_rate = ceo_left:ind_layoff_rate\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

b1 <- coef(P1); V1 <- vcov(P1)
nm_r <- "ceo_right:ind_layoff_rate"; nm_l <- "ceo_left:ind_layoff_rate"
diff <- b1[nm_r] - b1[nm_l]
se_d <- sqrt(V1[nm_r,nm_r] + V1[nm_l,nm_l] - 2*V1[nm_r,nm_l])
t_d <- diff/se_d
p_d <- 2*pt(abs(t_d), df=degrees_freedom(P1, type="t"), lower.tail=FALSE)
cat(sprintf("  ceo_right:ind_layoff_rate:  %8.5f (%.5f) p=%.4f %s\n", b1[nm_r], sqrt(V1[nm_r,nm_r]),
    2*pt(abs(b1[nm_r]/sqrt(V1[nm_r,nm_r])), df=degrees_freedom(P1,"t"), lower.tail=FALSE),
    stars(2*pt(abs(b1[nm_r]/sqrt(V1[nm_r,nm_r])), df=degrees_freedom(P1,"t"), lower.tail=FALSE))))
cat(sprintf("  ceo_left:ind_layoff_rate:   %8.5f (%.5f) p=%.4f %s\n", b1[nm_l], sqrt(V1[nm_l,nm_l]),
    2*pt(abs(b1[nm_l]/sqrt(V1[nm_l,nm_l])), df=degrees_freedom(P1,"t"), lower.tail=FALSE),
    stars(2*pt(abs(b1[nm_l]/sqrt(V1[nm_l,nm_l])), df=degrees_freedom(P1,"t"), lower.tail=FALSE))))
cat(sprintf("  Difference:                 %8.5f (%.5f) t=%.3f p=%.4f\n", diff, se_d, t_d, p_d))

# ── Compact summary ──
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("COMPACT SUMMARY: KEY COEFFICIENTS ACROSS ALL SPECS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

get_ci <- function(mod, var) {
  b <- coef(mod); V <- vcov(mod)
  idx <- match(var, names(b)); if(is.na(idx)) return(NULL)
  se <- sqrt(V[idx,idx])
  p <- 2*pt(abs(b[idx]/se), df=degrees_freedom(mod,"t"), lower.tail=FALSE)
  sprintf("%8.5f (%.5f) p=%.4f %s", b[idx], se, p, stars(p))
}

mods <- list(I5=I5, P2=P2, P3=P3, P4=P4, P5=P5)
cat(sprintf("%-12s %-40s %-40s %6s\n", "Model", "misalign_abs", "misalign_abs:ind_layoff_rate", "N"))
cat(paste(rep("─",105), collapse=""), "\n")
for (nm in names(mods)) {
  m <- mods[[nm]]
  cat(sprintf("%-12s %-40s %-40s %6d\n", nm,
      get_ci(m, "misalign_abs"), get_ci(m, "misalign_abs:ind_layoff_rate"), m$nobs))
}

cat(sprintf("\n%-12s %-40s %-40s %6s\n", "Model", "misalign_abs", "misalign_abs:ind_layoff_rate", "N"))
cat("Alternative DVs:\n")
for (nm in c("P6","P7")) {
  m <- get(nm)
  cat(sprintf("%-12s %-40s %-40s %6d\n", nm,
      get_ci(m, "misalign_abs"), get_ci(m, "misalign_abs:ind_layoff_rate"), m$nobs))
}

# ════════════════════════════════════════════════
# B: NET EFFECT AT DIFFERENT PERCENTILES
# ════════════════════════════════════════════════
cat("\n\n════════════════════════════════════════════════════════════════════\n")
cat("PANEL B: NET EFFECT OF MISALIGNMENT AT DIFFERENT IND_LAYOFF_RATE LEVELS\n")
cat("(Based on I5: baseline peer layoff model)\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

b <- coef(I5); V <- vcov(I5)
im <- match("misalign_abs", names(b))
ii <- match("misalign_abs:ind_layoff_rate", names(b))

pctiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)
qvals <- quantile(rd$ind_layoff_rate, pctiles, na.rm=TRUE)

cat(sprintf("%-12s %12s %10s %8s %8s %8s\n",
            "Percentile", "LayoffRate", "NetEffect", "SE", "t", "p"))
cat(paste(rep("─",68), collapse=""), "\n")

crossover <- NA
for (i in seq_along(pctiles)) {
  lr <- qvals[i]
  net <- b[im] + b[ii] * lr
  se_net <- sqrt(V[im,im] + lr^2 * V[ii,ii] + 2*lr*V[im,ii])
  t_net <- net/se_net
  p_net <- 2*pt(abs(t_net), df=degrees_freedom(I5,"t"), lower.tail=FALSE)
  cat(sprintf("%-12s %12.4f %10.5f %8.5f %8.3f %8.4f %s\n",
              paste0("p", pctiles[i]*100), lr, net, se_net, t_net, p_net, stars(p_net)))
}

# Exact crossover point
crossover <- -b[im] / b[ii]
cat(sprintf("\nCrossover point (effect = 0): ind_layoff_rate = %.4f\n", crossover))
cat(sprintf("  This is the %.1f percentile of the sample distribution\n",
            100 * ecdf(rd$ind_layoff_rate)(crossover)))

# ════════════════════════════════════════════════
# C: VISUALIZATION
# ════════════════════════════════════════════════
pdf("peer_layoff_marginal.pdf", width=7, height=5)

lr_range <- seq(0, 0.50, by=0.005)
net_vals <- b[im] + b[ii] * lr_range
se_vals <- sqrt(V[im,im] + lr_range^2 * V[ii,ii] + 2*lr_range*V[im,ii])
ci_lo <- net_vals - 1.96*se_vals
ci_hi <- net_vals + 1.96*se_vals

par(mar=c(5,5,3,2))
plot(lr_range, net_vals, type="l", lwd=2.5, col="darkblue",
     xlab="Industry Peer Layoff Rate", 
     ylab="Marginal Effect of Misalignment on Restructuring",
     main="Marginal Effect of CEO-Employee Misalignment\nby Industry Peer Layoff Rate",
     ylim=c(min(ci_lo), max(ci_hi)),
     cex.lab=1.1, cex.main=1.1)
polygon(c(lr_range, rev(lr_range)), c(ci_lo, rev(ci_hi)),
        col=adjustcolor("steelblue", 0.2), border=NA)
abline(h=0, lty=2, col="gray40")
abline(v=crossover, lty=3, col="red", lwd=1.5)

# Mark percentile points
qv <- quantile(rd$ind_layoff_rate, c(0.25,0.5,0.75), na.rm=TRUE)
for (q in qv) {
  net_q <- b[im] + b[ii] * q
  points(q, net_q, pch=16, cex=1.3, col="darkred")
}

# Add rug for data density
rug(rd$ind_layoff_rate[!is.na(rd$ind_layoff_rate)], col=adjustcolor("gray50",0.3))

# Legend
legend("topleft", 
       legend=c("Marginal effect", "95% CI", 
                sprintf("Crossover = %.3f (p%.0f)", crossover, 100*ecdf(rd$ind_layoff_rate)(crossover)),
                "25th / 50th / 75th pctile"),
       lty=c(1, NA, 3, NA), lwd=c(2.5, NA, 1.5, NA),
       col=c("darkblue", adjustcolor("steelblue",0.3), "red", "darkred"),
       pch=c(NA, 15, NA, 16), pt.cex=c(NA, 2, NA, 1.3),
       bty="n", cex=0.85)

# Annotation
text(0.42, min(ci_lo)*0.3, "Constraint\n(less restructuring)", col="darkblue", cex=0.8, font=3)
text(0.05, max(ci_hi)*0.6, "Facilitation\n(more restructuring)", col="darkblue", cex=0.8, font=3)

dev.off()
cat("\nSaved peer_layoff_marginal.pdf\n")

# ── Save all ──
results <- list(I5=I5, P1=P1, P2=P2, P3=P3, P4=P4, P5=P5, P6=P6, P7=P7)
saveRDS(results, file="peer_layoff_deep.rds")
cat("Saved peer_layoff_deep.rds\n")

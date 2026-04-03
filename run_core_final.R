library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v2.rdata")
reg_data <- reg_data[!sic2 %in% c(49, 60:69)]
setnames(reg_data, "fyear_lead", "fyear")
reg_data[, recession := nber_recession]
reg_data[, ceo_right := pmax(misalign_signed, 0)]
reg_data[, ceo_left  := pmax(-misalign_signed, 0)]
reg_data[, lag_aqc_indicator := lag_acquisition]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

# TABLE 2
M1  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M2  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + emp_mean_cfscore + emp_sd_cfscore +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M3  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", ctrl, "| gvkey + sic2^fyear")), data=reg_data, cluster=~gvkey)
M4  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_turnover + first_year_ceo +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M5  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + log_n_donors +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)

# TABLE 3
M6  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M7  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession + emp_mean_cfscore + emp_sd_cfscore +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M8  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + sic2^fyear")), data=reg_data, cluster=~gvkey)
M9  <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession + ceo_turnover + first_year_ceo +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M10 <- feols(as.formula(paste("neg_spi_at ~ ceo_right + ceo_left + ceo_right:recession + ceo_left:recession +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)
M11 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_cfscore +", ctrl, "| gvkey + fyear")), data=reg_data, cluster=~gvkey)

# ── Print tables ──
cat("================================================================\n")
cat("TABLE 2: BASELINE MODELS\n")
cat("================================================================\n\n")
etable(M1, M2, M3, M4, M5,
       headers=c("M1:Base","M2:EmpIdeol","M3:Ind×Yr","M4:Turnover","M5:Donors"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

cat("\n================================================================\n")
cat("TABLE 3: RECESSION & ASYMMETRY MODELS\n")
cat("================================================================\n\n")
etable(M6, M7, M8, M9, M10, M11,
       headers=c("M6:Recess","M7:+EmpIdeo","M8:Ind×Yr","M9:+Turnov","M10:Asymm","M11:CEOlvl"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# ── Net recession effects ──
cat("\n================================================================\n")
cat("NET RECESSION EFFECT: coef(misalign_abs) + coef(misalign_abs:recession)\n")
cat("================================================================\n\n")

for (m_name in c("M6","M7","M8","M9")) {
  mod <- get(m_name)
  b <- coef(mod); V <- vcov(mod)
  idx_m <- grep("^misalign_abs$", names(b))
  idx_i <- grep("misalign_abs:recession", names(b))
  net <- b[idx_m] + b[idx_i]
  se_net <- sqrt(V[idx_m,idx_m] + V[idx_i,idx_i] + 2*V[idx_m,idx_i])
  t_net <- net / se_net
  p_net <- 2 * pt(abs(t_net), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  stars <- ifelse(p_net<0.01,"***",ifelse(p_net<0.05,"**",ifelse(p_net<0.1,"*","")))
  cat(sprintf("%s: Net = %.5f  SE = %.5f  t = %.3f  p = %.4f %s\n", m_name, net, se_net, t_net, p_net, stars))
}

# ── Wald test M10 ──
cat("\n================================================================\n")
cat("WALD TEST (M10): H0: coef(ceo_right:recession) = coef(ceo_left:recession)\n")
cat("================================================================\n\n")
b10 <- coef(M10); V10 <- vcov(M10)
nm_r <- "ceo_right:recession"; nm_l <- "ceo_left:recession"
diff <- b10[nm_r] - b10[nm_l]
se_diff <- sqrt(V10[nm_r,nm_r] + V10[nm_l,nm_l] - 2*V10[nm_r,nm_l])
t_diff <- diff / se_diff
p_diff <- 2 * pt(abs(t_diff), df=degrees_freedom(M10, type="t"), lower.tail=FALSE)
cat(sprintf("Difference = %.5f  SE = %.5f  t = %.3f  p = %.4f\n", diff, se_diff, t_diff, p_diff))
cat("\nM10 key coefficients:\n")
for (nm in c("ceo_right","ceo_left","ceo_right:recession","ceo_left:recession")) {
  se <- sqrt(V10[nm,nm])
  p <- 2*pt(abs(b10[nm]/se), df=degrees_freedom(M10, type="t"), lower.tail=FALSE)
  stars <- ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))
  cat(sprintf("  %-25s  %.5f  (%.5f)  p=%.4f %s\n", nm, b10[nm], se, p, stars))
}

# ── Save ──
models_core <- list(M1=M1,M2=M2,M3=M3,M4=M4,M5=M5,M6=M6,M7=M7,M8=M8,M9=M9,M10=M10,M11=M11)
saveRDS(models_core, file="models_core.rds")
cat("\nSaved models_core.rds\n")

library(data.table)
library(fixest)
setFixest_notes(FALSE)

# ── Load and prep ──
load("reg_data_v2.rdata")

# Exclude utilities and financials
reg_data <- reg_data[!sic2 %in% c(49, 60:69)]
cat("After industry exclusion:", nrow(reg_data), "rows\n")

# Rename fyear_lead to fyear for clarity in FE
setnames(reg_data, "fyear_lead", "fyear")

# Create recession variable (use nber_recession)
reg_data[, recession := nber_recession]

# Create asymmetric decomposition
# signed_misalign = ceo - emp (positive = CEO more right)
reg_data[, ceo_right := pmax(misalign_signed, 0)]
reg_data[, ceo_left  := pmax(-misalign_signed, 0)]

# lag_aqc_indicator: use lag_acquisition (binary)
reg_data[, lag_aqc_indicator := lag_acquisition]

cat("Key var summaries:\n")
cat("  neg_spi_at:  N=", sum(!is.na(reg_data$neg_spi_at)), " mean=", 
    round(mean(reg_data$neg_spi_at, na.rm=TRUE), 4), "\n")
cat("  misalign_abs: N=", sum(!is.na(reg_data$misalign_abs)), " mean=", 
    round(mean(reg_data$misalign_abs, na.rm=TRUE), 4), "\n")
cat("  recession:   N=", sum(!is.na(reg_data$recession)), " sum=", 
    sum(reg_data$recession, na.rm=TRUE), "\n")
cat("  ceo_turnover: N=", sum(!is.na(reg_data$ceo_turnover)), "\n")

# ── Define control formula ──
ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

# ════════════════════════════════════════
# TABLE 2: BASELINE
# ════════════════════════════════════════
M1 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

M2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + emp_mean_cfscore + emp_sd_cfscore +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

M3 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", ctrl, "| gvkey + sic2^fyear")),
            data = reg_data, cluster = ~gvkey)

M4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_turnover + first_year_ceo +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

M5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + log_n_donors +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

cat("\n")
cat("================================================================\n")
cat("TABLE 2: BASELINE MODELS\n")
cat("================================================================\n\n")
etable(M1, M2, M3, M4, M5,
       headers = c("M1:Base", "M2:EmpIdeol", "M3:Ind×Yr", "M4:Turnover", "M5:Donors"),
       se.below = TRUE, signif.code = c("***"=0.01, "**"=0.05, "*"=0.1))

# ════════════════════════════════════════
# TABLE 3: RECESSION INTERACTION
# ════════════════════════════════════════
M6 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

M7 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession + emp_mean_cfscore + emp_sd_cfscore +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

M8 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + sic2^fyear")),
            data = reg_data, cluster = ~gvkey)

M9 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession + ceo_turnover + first_year_ceo +", ctrl, "| gvkey + fyear")),
            data = reg_data, cluster = ~gvkey)

# ── ASYMMETRIC ──
M10 <- feols(as.formula(paste("neg_spi_at ~ ceo_right + ceo_left + ceo_right:recession + ceo_left:recession +", ctrl, "| gvkey + fyear")),
             data = reg_data, cluster = ~gvkey)

# ── CEO LEVEL TEST ──
M11 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_cfscore +", ctrl, "| gvkey + fyear")),
             data = reg_data, cluster = ~gvkey)

cat("\n")
cat("================================================================\n")
cat("TABLE 3: RECESSION & ASYMMETRY MODELS\n")
cat("================================================================\n\n")
etable(M6, M7, M8, M9, M10, M11,
       headers = c("M6:Recess", "M7:+EmpIdeo", "M8:Ind×Yr", "M9:+Turnov", "M10:Asymm", "M11:CEOlvl"),
       se.below = TRUE, signif.code = c("***"=0.01, "**"=0.05, "*"=0.1))

# ════════════════════════════════════════
# NET RECESSION EFFECTS (M6-M9)
# ════════════════════════════════════════
cat("\n")
cat("================================================================\n")
cat("NET RECESSION EFFECT: coef(misalign_abs) + coef(misalign_abs:recession)\n")
cat("================================================================\n\n")

for (m_name in c("M6", "M7", "M8", "M9")) {
  mod <- get(m_name)
  b <- coef(mod)
  V <- vcov(mod)
  
  # Find the coefficient names
  idx_main <- grep("^misalign_abs$", names(b))
  idx_inter <- grep("misalign_abs:recession", names(b))
  
  net <- b[idx_main] + b[idx_inter]
  se_net <- sqrt(V[idx_main, idx_main] + V[idx_inter, idx_inter] + 2 * V[idx_main, idx_inter])
  t_net <- net / se_net
  p_net <- 2 * pt(abs(t_net), df = degrees_freedom(mod, type = "t"), lower.tail = FALSE)
  
  cat(sprintf("%s: Net effect = %.5f, SE = %.5f, t = %.3f, p = %.4f %s\n",
              m_name, net, se_net, t_net, p_net,
              ifelse(p_net < 0.01, "***", ifelse(p_net < 0.05, "**", ifelse(p_net < 0.1, "*", "")))))
}

# ════════════════════════════════════════
# WALD TEST: M10 asymmetry in recession
# ════════════════════════════════════════
cat("\n")
cat("================================================================\n")
cat("WALD TEST (M10): H0: coef(ceo_right:recession) == coef(ceo_left:recession)\n")
cat("================================================================\n\n")

wt <- wald(M10, "ceo_right:recession - ceo_left:recession", print = FALSE)
cat("Wald stat:", round(wt$stat, 3), "  p-value:", round(wt$p, 4), "\n")
# Also print the individual coefficients
b10 <- coef(M10)
cat("\nM10 key coefficients:\n")
for (nm in grep("ceo_right|ceo_left", names(b10), value=TRUE)) {
  se10 <- sqrt(vcov(M10)[nm, nm])
  cat(sprintf("  %-30s  coef = %8.5f  SE = %.5f  p = %.4f\n", nm, b10[nm], se10,
              2 * pt(abs(b10[nm]/se10), df = degrees_freedom(M10, type="t"), lower.tail=FALSE)))
}

# ── Save all models ──
models_core <- list(M1=M1, M2=M2, M3=M3, M4=M4, M5=M5, 
                    M6=M6, M7=M7, M8=M8, M9=M9, M10=M10, M11=M11)
saveRDS(models_core, file = "models_core.rds")
cat("\nSaved models_core.rds (11 models)\n")

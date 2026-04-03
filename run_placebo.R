library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v3.rdata")
if (!"fyear" %in% names(reg_data)) setnames(reg_data, "fyear_lead", "fyear")
dt <- reg_data[!sic2 %in% c(49, 60:69)]
dt[, recession := nber_recession]
dt[, lag_aqc_indicator := lag_acquisition]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

# ═══════════════════════════════════════════════
# A1: PLACEBO FOR neg_spi_at
# ═══════════════════════════════════════════════
cat("=== A1: PLACEBO FOR neg_spi_at ===\n")
fml_spi <- as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate_shuf +", ctrl, "| gvkey + fyear"))
fml_spi_true <- as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear"))

true_spi <- feols(fml_spi_true, data=dt, cluster=~gvkey)
true_coef_spi <- coef(true_spi)["misalign_abs:ind_layoff_rate"]
cat("True coefficient (neg_spi_at):", true_coef_spi, "\n\n")

# Get unique sic2 × fyear combos with their ind_layoff_rate
ilr_table <- unique(dt[!is.na(ind_layoff_rate), .(sic2, fyear, ind_layoff_rate)])
cat("Unique sic2×fyear cells:", nrow(ilr_table), "\n")

set.seed(42)
placebo_spi <- numeric(500)
t0 <- Sys.time()

for (i in 1:500) {
  # Shuffle ind_layoff_rate across sic2×fyear cells
  shuf <- copy(ilr_table)
  shuf[, ind_layoff_rate_shuf := sample(ind_layoff_rate)]
  
  # Merge to firm data
  dt_shuf <- merge(dt, shuf[, .(sic2, fyear, ind_layoff_rate_shuf)], by=c("sic2","fyear"), all.x=TRUE)
  
  # Estimate
  m <- tryCatch(feols(fml_spi, data=dt_shuf, cluster=~gvkey), error=function(e) NULL)
  if (!is.null(m)) {
    placebo_spi[i] <- coef(m)["misalign_abs:ind_layoff_rate_shuf"]
  } else {
    placebo_spi[i] <- NA
  }
  
  if (i %% 100 == 0) cat(sprintf("  A1: %d/500 done (%.1f min)\n", i, difftime(Sys.time(), t0, units="mins")))
}

pval_spi <- mean(placebo_spi >= true_coef_spi, na.rm=TRUE)
cat(sprintf("\nA1 RESULT: True=%.5f, Placebo mean=%.5f, SD=%.5f, Empirical p=%.4f\n",
            true_coef_spi, mean(placebo_spi, na.rm=TRUE), sd(placebo_spi, na.rm=TRUE), pval_spi))

# ═══════════════════════════════════════════════
# A2: PLACEBO FOR layoff
# ═══════════════════════════════════════════════
cat("\n=== A2: PLACEBO FOR layoff ===\n")
fml_lay <- as.formula(paste("layoff ~ misalign_abs + misalign_abs:ind_layoff_rate_shuf +", ctrl, "| gvkey + fyear"))
fml_lay_true <- as.formula(paste("layoff ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear"))

true_lay <- feols(fml_lay_true, data=dt, cluster=~gvkey)
true_coef_lay <- coef(true_lay)["misalign_abs:ind_layoff_rate"]
cat("True coefficient (layoff):", true_coef_lay, "\n\n")

set.seed(42)
placebo_lay <- numeric(500)
t0 <- Sys.time()

for (i in 1:500) {
  shuf <- copy(ilr_table)
  shuf[, ind_layoff_rate_shuf := sample(ind_layoff_rate)]
  dt_shuf <- merge(dt, shuf[, .(sic2, fyear, ind_layoff_rate_shuf)], by=c("sic2","fyear"), all.x=TRUE)
  
  m <- tryCatch(feols(fml_lay, data=dt_shuf, cluster=~gvkey), error=function(e) NULL)
  if (!is.null(m)) {
    placebo_lay[i] <- coef(m)["misalign_abs:ind_layoff_rate_shuf"]
  } else {
    placebo_lay[i] <- NA
  }
  
  if (i %% 100 == 0) cat(sprintf("  A2: %d/500 done (%.1f min)\n", i, difftime(Sys.time(), t0, units="mins")))
}

pval_lay <- mean(placebo_lay >= true_coef_lay, na.rm=TRUE)
cat(sprintf("\nA2 RESULT: True=%.5f, Placebo mean=%.5f, SD=%.5f, Empirical p=%.4f\n",
            true_coef_lay, mean(placebo_lay, na.rm=TRUE), sd(placebo_lay, na.rm=TRUE), pval_lay))

# Save
placebo_results <- list(
  spi = list(true_coef=true_coef_spi, placebo_coefs=placebo_spi, pval=pval_spi),
  lay = list(true_coef=true_coef_lay, placebo_coefs=placebo_lay, pval=pval_lay)
)
saveRDS(placebo_results, "placebo_results.rds")
cat("\nSaved placebo_results.rds\n")

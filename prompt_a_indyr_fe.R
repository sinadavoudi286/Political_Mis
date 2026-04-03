# PROMPT A: Industry × Year FE robustness
library(fixest)
library(data.table)
load("reg_data_v2.rdata")
setDT(reg_data)

# Variable mapping:
# log_at -> size (already log(at))
# tobinsq -> tobinq
# sales_growth -> sale_growth  
# capex_at -> capx_at
# acq_indicator -> acquisition
# peer_layoff_rate -> ind_layoff_rate
# neg_spi_at_lead -> neg_spi_at (already t+1 in this data)
# layoff_indicator -> layoff
# misalign_abs_x_peer_layoff -> create interaction

reg_data[, misalign_x_indlayoff := misalign_abs * ind_layoff_rate]

controls <- c("size", "roa", "leverage", "tobinq", "sale_growth",
              "loss", "capx_at", "rd_at", "acquisition")

fml_base <- as.formula(paste(
  "neg_spi_at ~",
  "misalign_abs + misalign_x_indlayoff +",
  paste(controls, collapse = " + "),
  "| gvkey + fyear_lead"
))

fml_indyr <- as.formula(paste(
  "neg_spi_at ~",
  "misalign_abs + misalign_x_indlayoff +",
  paste(controls, collapse = " + "),
  "| gvkey + sic2^fyear_lead"
))

fml_layoff_indyr <- as.formula(paste(
  "layoff ~",
  "misalign_abs + misalign_x_indlayoff +",
  paste(controls, collapse = " + "),
  "| gvkey + sic2^fyear_lead"
))

m1 <- feols(fml_base,    data = reg_data, cluster = ~gvkey)
m2 <- feols(fml_indyr,   data = reg_data, cluster = ~gvkey)
m3 <- feols(fml_layoff_indyr, data = reg_data, cluster = ~gvkey)

cat("=============================================================\n")
cat("Table: Interaction under Industry x Year FE\n")
cat("=============================================================\n\n")
etable(m1, m2, m3,
       keep = c("misalign_abs", "misalign_x_indlayoff"),
       headers = c("Firm+Year FE", "Firm+Ind×Year FE", "Layoff+Ind×Year FE"),
       title = "Table: Interaction under Industry x Year FE")

# Net effects at p10/p50/p90 of ind_layoff_rate for m2
p10 <- quantile(reg_data$ind_layoff_rate, 0.10, na.rm=TRUE)
p50 <- quantile(reg_data$ind_layoff_rate, 0.50, na.rm=TRUE)
p90 <- quantile(reg_data$ind_layoff_rate, 0.90, na.rm=TRUE)

b1 <- coef(m2)["misalign_abs"]
b2 <- coef(m2)["misalign_x_indlayoff"]
se_mat <- vcov(m2)

cat("\n=== Net effect of misalign_abs at different ind_layoff_rate levels ===\n")
for (pct_val in list(c("p10", p10), c("p50", p50), c("p90", p90))) {
  lv <- as.numeric(pct_val[[2]])
  lc <- b1 + b2 * lv
  grad <- c(1, lv)
  se_lc <- sqrt(t(grad) %*% se_mat[c("misalign_abs","misalign_x_indlayoff"),
                                    c("misalign_abs","misalign_x_indlayoff")] %*% grad)
  cat(sprintf("Net effect at %s (%.3f): coef=%.4f, SE=%.4f, p=%.3f\n",
              pct_val[[1]], lv, lc, se_lc,
              2*pt(-abs(lc/se_lc), df=m2$nobs-1)))
}

saveRDS(list(m1=m1, m2=m2, m3=m3), "indyr_fe_robustness.rds")
cat("\nSaved to indyr_fe_robustness.rds\n")

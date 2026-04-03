library(data.table)
library(fixest)
setwd("/Users/myoffice/Desktop")

load("reg_data.rdata")
setDT(reg_data)

# ---- Create recession/stress variables ----
reg_data[, nber_recession := as.integer(fyear_lead %in% c(2001, 2008, 2009, 2020))]
cat('NBER recession obs:', sum(reg_data$nber_recession, na.rm=TRUE), '\n')
cat('Non-recession obs:', sum(reg_data$nber_recession == 0, na.rm=TRUE), '\n')

reg_data[, econ_stress := as.integer(fyear_lead %in% c(2001, 2002, 2008, 2009, 2010, 2020, 2021))]
reg_data[, firm_downturn := as.integer(!is.na(lag_sale_growth) & lag_sale_growth < -0.10)]

cat('Econ stress obs:', sum(reg_data$econ_stress, na.rm=TRUE), '\n')
cat('Firm downturn obs:', sum(reg_data$firm_downturn, na.rm=TRUE), '\n')
cat('Post-2010 obs:', sum(reg_data$cycle >= 2010, na.rm=TRUE), '\n')

controls <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_acquisition"

# ---- RECESSION INTERACTIONS ----
cat("\nRunning M9a: NBER recession interaction...\n")
m9a <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * nber_recession +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M9b: Econ stress interaction...\n")
m9b <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * econ_stress +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M9c: Firm downturn interaction...\n")
m9c <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * firm_downturn +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# ---- SUBSAMPLE AND EXTENSIONS ----
cat("Running M16: Triple interaction...\n")
m16 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * nber_recession * lag_emp_sale +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M17: Recession subsample...\n")
m17 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[nber_recession == 1], cluster = "gvkey")

cat("Running M18: Non-recession subsample...\n")
m18 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[nber_recession == 0], cluster = "gvkey")

cat("Running M19: Post-2010 subsample...\n")
m19 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[cycle >= 2010], cluster = "gvkey")

cat("Running M20: Asymmetric + recession...\n")
m20 <- feols(as.formula(paste("neg_spi_at ~ ceo_more_conservative * nber_recession + ceo_more_liberal * nber_recession +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M21: Emp change + recession...\n")
m21 <- feols(as.formula(paste("emp_change ~ misalign_abs * nber_recession +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M22: Firm downturn...\n")
m22 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * firm_downturn +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# ---- ROBUSTNESS ----
cat("Running M12: Lagged DV...\n")
m12 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + lag_neg_spi_at +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M13: High misalignment binary...\n")
reg_data[, high_misalign := as.integer(misalign_abs > median(misalign_abs, na.rm=TRUE))]
m13 <- feols(as.formula(paste("neg_spi_at ~ high_misalign +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

cat("Running M14: Double-clustered...\n")
m14 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = c("gvkey", "fyear_lead"))

# ---- PRINT ALL RESULTS ----
cat('\n========= RECESSION SPECIFICATIONS =========\n')
etable(m9a, m9b, m9c,
       headers = c("NBER Recession", "Econ Stress", "Firm Downturn"))

cat('\n========= SUBSAMPLE AND EXTENSIONS =========\n')
etable(m16, m17, m18, m19,
       headers = c("Triple Interact", "Recession Only", "Non-Recession", "Post-2010"))

cat('\n========= ASYMMETRIC + EMP CHANGE + FIRM DOWNTURN =========\n')
etable(m20, m21, m22,
       headers = c("Asym+Recession", "EmpChg+Recession", "Firm Downturn"))

cat('\n========= ROBUSTNESS =========\n')
etable(m12, m13, m14,
       headers = c("Lag DV", "High Misalign", "2-way Cluster"))

# ---- KEY COEFFICIENTS SUMMARY ----
cat("\n=== KEY COEFFICIENTS SUMMARY ===\n\n")

extract_key <- function(model, name, var) {
  ct <- coeftable(model)
  if (var %in% rownames(ct)) {
    row <- ct[var, ]
    nobs <- model$nobs
    wr2 <- tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA)
    nf <- tryCatch(length(unique(model$fixef_id$gvkey)), error = function(e) NA)
    data.table(Model = name, Variable = var,
               Coef = round(row[1], 5), SE = round(row[2], 5),
               pvalue = round(row[4], 4), N = nobs, Firms = nf,
               Within_R2 = round(wr2, 4))
  } else NULL
}

keys <- rbindlist(list(
  extract_key(m9a, "M9a NBER", "misalign_abs"),
  extract_key(m9a, "M9a NBER", "misalign_abs:nber_recession"),
  extract_key(m9b, "M9b Stress", "misalign_abs"),
  extract_key(m9b, "M9b Stress", "misalign_abs:econ_stress"),
  extract_key(m9c, "M9c FirmDown", "misalign_abs"),
  extract_key(m9c, "M9c FirmDown", "misalign_abs:firm_downturn"),
  extract_key(m16, "M16 Triple", "misalign_abs"),
  extract_key(m16, "M16 Triple", "misalign_abs:nber_recession"),
  extract_key(m16, "M16 Triple", "misalign_abs:nber_recession:lag_emp_sale"),
  extract_key(m17, "M17 Recession", "misalign_abs"),
  extract_key(m18, "M18 Non-Recess", "misalign_abs"),
  extract_key(m19, "M19 Post-2010", "misalign_abs"),
  extract_key(m20, "M20 AsymRecess", "ceo_more_conservative"),
  extract_key(m20, "M20 AsymRecess", "ceo_more_conservative:nber_recession"),
  extract_key(m20, "M20 AsymRecess", "ceo_more_liberal"),
  extract_key(m20, "M20 AsymRecess", "ceo_more_liberal:nber_recession"),
  extract_key(m21, "M21 EmpChg", "misalign_abs"),
  extract_key(m21, "M21 EmpChg", "misalign_abs:nber_recession"),
  extract_key(m22, "M22 FirmDown", "misalign_abs"),
  extract_key(m22, "M22 FirmDown", "misalign_abs:firm_downturn"),
  extract_key(m12, "M12 LagDV", "misalign_abs"),
  extract_key(m13, "M13 HighMis", "high_misalign"),
  extract_key(m14, "M14 2wayCl", "misalign_abs")
), fill = TRUE)

print(keys, nrows = 30)

save(reg_data, file = "reg_data.rdata")
cat("\nSaved updated reg_data.rdata\n")
cat("Done!\n")

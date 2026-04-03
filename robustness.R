## ============================================================
## robustness.R — Additional specifications
## ============================================================

library(data.table)
library(fixest)
setwd("/Users/myoffice/Desktop")

load("reg_data.rdata")
load("panel_qual.rdata")
load("compustat_outcomes.rdata")
setDT(reg_data)
setDT(panel_qual)
setDT(compustat)

controls <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_acquisition"

## ============================================================
cat("=== M5: Contemporaneous specification (cycle = fyear) ===\n")
panel_qual[, fyear_contemp := cycle]
reg_data2 <- merge(panel_qual, compustat,
                   by.x = c("gvkey", "fyear_contemp"),
                   by.y = c("gvkey", "fyear"),
                   all.x = TRUE, suffixes = c("", ".cs"))
reg_data2 <- reg_data2[exclude_industry == 0]
cat("Contemp sample:", nrow(reg_data2), "rows,", uniqueN(reg_data2$gvkey), "firms\n")

m5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + cycle")),
            data = reg_data2, vcov = ~gvkey)

## ============================================================
cat("=== M6: Signed misalignment ===\n")
m6 <- feols(as.formula(paste("neg_spi_at ~ misalign_signed +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

## ============================================================
cat("=== M7: Asymmetric effects ===\n")
reg_data[, ceo_more_conservative := pmax(misalign_signed, 0)]
reg_data[, ceo_more_liberal := pmax(-misalign_signed, 0)]
m7 <- feols(as.formula(paste("neg_spi_at ~ ceo_more_conservative + ceo_more_liberal +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

## ============================================================
cat("=== M8: Layoff binary (LPM) ===\n")
# Try logit first, fall back to LPM
tryCatch({
  m8 <- feglm(as.formula(paste("layoff ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
              data = reg_data, vcov = ~gvkey, family = binomial)
  cat("Logit converged\n")
  m8_type <- "logit"
}, error = function(e) {
  cat("Logit failed:", conditionMessage(e), "\n")
  cat("Using LPM instead\n")
  m8 <<- feols(as.formula(paste("layoff ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
               data = reg_data, vcov = ~gvkey)
  m8_type <<- "LPM"
})

## ============================================================
cat("=== M9: Downturn interaction ===\n")
reg_data[, downturn := as.integer(fyear_lead %in% c(2008, 2009, 2010, 2020))]
cat("Downturn obs:", sum(reg_data$downturn), "of", nrow(reg_data), "\n")
m9 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * downturn +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

## ============================================================
cat("=== M10: CEO ideology level control ===\n")
m10 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_cfscore +", controls, "| gvkey + fyear_lead")),
             data = reg_data, vcov = ~gvkey)

## ============================================================
cat("=== M11: Balanced panel (4+ cycles) ===\n")
freq <- reg_data[!is.na(misalign_abs), .N, by = gvkey][N >= 4]
cat("Firms with 4+ cycles:", nrow(freq), "\n")
m11 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[gvkey %in% freq$gvkey], vcov = ~gvkey)

## ============================================================
cat("\n=== FULL REGRESSION TABLE ===\n\n")

etable(m5, m6, m7, m8, m9, m10, m11,
       headers = c("Contemp", "Signed", "Asymmetric", "Layoff", "Downturn", "CEO Level", "Balanced 4+"),
       se.below = TRUE,
       fitstat = c("n", "r2", "wr2"))

## ============================================================
cat("\n=== KEY COEFFICIENTS SUMMARY ===\n\n")

models <- list(m5 = m5, m6 = m6, m7 = m7, m8 = m8, m9 = m9, m10 = m10, m11 = m11)
cat(sprintf("%-12s  %-25s  %8s  %8s  %8s  %6s  %6s\n",
            "Model", "Key Variable", "Coef", "SE", "p-value", "N", "W-R2"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (nm in names(models)) {
  m <- models[[nm]]
  ct <- coeftable(m)
  # Get the first misalignment-related variable
  mis_vars <- grep("misalign|ceo_more", rownames(ct), value = TRUE)
  for (v in mis_vars) {
    cat(sprintf("%-12s  %-25s  %8.4f  %8.4f  %8.4f  %6d  %6.4f\n",
                nm, v, ct[v, 1], ct[v, 2], ct[v, 4],
                m$nobs, fitstat(m, "wr2")[[1]]))
  }
}

## Also report unique firms per model
cat("\n=== UNIQUE FIRMS PER MODEL ===\n")
cat("M5  (Contemp):", uniqueN(reg_data2[!is.na(neg_spi_at) & !is.na(lag_size)]$gvkey), "\n")
cat("M6  (Signed):", uniqueN(reg_data[!is.na(neg_spi_at) & !is.na(lag_size)]$gvkey), "\n")
cat("M7  (Asymmetric):", uniqueN(reg_data[!is.na(neg_spi_at) & !is.na(lag_size)]$gvkey), "\n")
cat("M8  (Layoff):", uniqueN(reg_data[!is.na(layoff) & !is.na(lag_size)]$gvkey), "\n")
cat("M9  (Downturn):", uniqueN(reg_data[!is.na(neg_spi_at) & !is.na(lag_size)]$gvkey), "\n")
cat("M10 (CEO Level):", uniqueN(reg_data[!is.na(neg_spi_at) & !is.na(lag_size)]$gvkey), "\n")
cat("M11 (Balanced):", nrow(freq), "\n")

## Save updated reg_data with new variables
save(reg_data, file = "reg_data.rdata")
cat("\nSaved updated reg_data.rdata\n")
cat("Done!\n")

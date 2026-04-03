## ============================================================
## merge_and_regress.R — Merge misalignment panel with Compustat + run regressions
## ============================================================

library(data.table)
library(fixest)
setwd("/Users/myoffice/Desktop")

cat("=== STEP 1: Load data ===\n")
load("panel_qual.rdata")   # panel_qual
load("compustat_outcomes.rdata")  # compustat
setDT(panel_qual)
setDT(compustat)

cat("Panel qual:", nrow(panel_qual), "firm-cycles\n")
cat("Compustat:", nrow(compustat), "firm-years\n")

## ============================================================
cat("\n=== STEP 2: Map cycles to fiscal years ===\n")

# Lagged specification: misalignment at cycle t → outcomes at fyear t+1
panel_qual[, fyear_lead := cycle + 1L]

# Contemporaneous: misalignment at cycle t → outcomes at fyear t
panel_qual[, fyear_contemp := cycle]

cat("Cycle range:", range(panel_qual$cycle), "\n")
cat("Lagged fyear range:", range(panel_qual$fyear_lead), "\n")

## ============================================================
cat("\n=== STEP 3: Merge — Lagged specification ===\n")

# Keep only needed panel columns for merge
panel_cols <- c("gvkey", "cycle", "fyear_lead", "fyear_contemp",
                "emp_mean_cfscore", "emp_median_cfscore", "emp_sd_cfscore",
                "n_emp_donors", "n_contributions",
                "execid", "exec_fullname", "ceo_cfscore",
                "misalign_abs", "misalign_signed", "conm")

# Use only columns that exist
panel_cols <- intersect(panel_cols, names(panel_qual))
panel_slim <- panel_qual[, ..panel_cols]

# Lagged merge: misalignment at t predicts outcomes at t+1
reg_data <- merge(panel_slim, compustat,
                  by.x = c("gvkey", "fyear_lead"),
                  by.y = c("gvkey", "fyear"),
                  all.x = TRUE, suffixes = c("", ".cs"))

cat("After merge:", nrow(reg_data), "rows\n")
cat("With Compustat data:", sum(!is.na(reg_data$at)), "\n")

# Drop rows without Compustat match
reg_data <- reg_data[!is.na(at)]
cat("After dropping no-Compustat:", nrow(reg_data), "rows\n")

## ============================================================
cat("\n=== STEP 4: Exclude financials and utilities ===\n")
cat("exclude_industry flag distribution:\n")
print(table(reg_data$exclude_industry, useNA = "ifany"))

reg_data <- reg_data[exclude_industry == FALSE | is.na(exclude_industry)]
cat("After excluding fin/util:", nrow(reg_data), "rows\n")
cat("Unique firms:", uniqueN(reg_data$gvkey), "\n")
cat("Unique cycles:", uniqueN(reg_data$cycle), "\n")

## ============================================================
cat("\n=== STEP 5: Summary stats of regression sample ===\n")

# Key variables
summ_vars <- c("misalign_abs", "misalign_signed", "ceo_cfscore", "emp_mean_cfscore",
               "neg_spi_at", "emp_change", "n_emp_donors",
               "lag_size", "lag_roa", "lag_leverage", "lag_tobinq",
               "lag_sale_growth", "lag_loss", "lag_capx_at", "lag_rd_at",
               "lag_acquisition", "lag_emp_sale")

summ_vars <- intersect(summ_vars, names(reg_data))

cat("\nVariable means and SDs:\n")
summ_dt <- data.table(
  variable = summ_vars,
  n = sapply(summ_vars, function(v) sum(!is.na(reg_data[[v]]))),
  mean = sapply(summ_vars, function(v) round(mean(reg_data[[v]], na.rm = TRUE), 4)),
  sd = sapply(summ_vars, function(v) round(sd(reg_data[[v]], na.rm = TRUE), 4)),
  min = sapply(summ_vars, function(v) round(min(reg_data[[v]], na.rm = TRUE), 4)),
  median = sapply(summ_vars, function(v) round(median(reg_data[[v]], na.rm = TRUE), 4)),
  max = sapply(summ_vars, function(v) round(max(reg_data[[v]], na.rm = TRUE), 4))
)
print(summ_dt, nrows = 20)

## ============================================================
cat("\n=== STEP 6: Run regressions ===\n")

# Define controls
controls <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_acquisition"

# Model 1: Pooled OLS — restructuring charges
cat("\nModel 1: Pooled OLS (neg_spi_at ~ misalign_abs + controls)\n")
m1 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls)),
            data = reg_data, vcov = ~gvkey)

# Model 2: Firm FE + Year FE — restructuring charges
cat("Model 2: Firm + Year FE (neg_spi_at)\n")
m2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

# Model 3: Firm FE + Year FE — employee change (layoff proxy)
cat("Model 3: Firm + Year FE (emp_change)\n")
m3 <- feols(as.formula(paste("emp_change ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

# Model 4: Moderation by labor intensity
cat("Model 4: Firm + Year FE (neg_spi_at ~ misalign_abs * lag_emp_sale)\n")
m4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * lag_emp_sale +", controls, "| gvkey + fyear_lead")),
            data = reg_data, vcov = ~gvkey)

## ============================================================
cat("\n=== REGRESSION TABLE ===\n\n")

etable(m1, m2, m3, m4,
       headers = c("OLS", "FE: Restructure", "FE: Emp Change", "FE: Interaction"),
       se.below = TRUE,
       fitstat = c("n", "r2", "ar2", "wr2"))

# Also print individual model summaries for clarity
cat("\n\n=== Individual Model Details ===\n")
cat("\n--- Model 1: Pooled OLS (neg_spi_at) ---\n")
summary(m1)

cat("\n--- Model 2: Firm + Year FE (neg_spi_at) ---\n")
summary(m2)

cat("\n--- Model 3: Firm + Year FE (emp_change) ---\n")
summary(m3)

cat("\n--- Model 4: Interaction (neg_spi_at ~ misalign * labor_intensity) ---\n")
summary(m4)

## ============================================================
cat("\n=== Observations per cycle in regression sample ===\n")
print(reg_data[, .N, by = cycle][order(cycle)])

## ============================================================
cat("\n=== Save reg_data ===\n")
save(reg_data, file = "reg_data.rdata")
cat("Saved: reg_data.rdata (", nrow(reg_data), "rows)\n")

cat("\nDone!\n")

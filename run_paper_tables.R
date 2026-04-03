library(data.table)
library(fixest)
setwd("/Users/myoffice/Desktop")

load("reg_data.rdata")
setDT(reg_data)

# Load panel_qual for contemporaneous spec
load("panel_qual.rdata")
setDT(panel_qual)
load("compustat_outcomes.rdata")
setDT(compustat)

# ---- Recreate variables ----
reg_data[, nber_recession := as.integer(fyear_lead %in% c(2001, 2008, 2009, 2020))]
reg_data[, econ_stress := as.integer(fyear_lead %in% c(2001, 2002, 2008, 2009, 2010, 2020, 2021))]
reg_data[, firm_downturn := as.integer(!is.na(lag_sale_growth) & lag_sale_growth < -0.10)]
reg_data[, high_misalign := as.integer(misalign_abs > median(misalign_abs, na.rm = TRUE))]

controls <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_acquisition"

# ---- Run key models ----
cat("Running models...\n")
m1 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls)),
            data = reg_data, cluster = "gvkey")
m2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")
m4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * lag_emp_sale +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")
m9a <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * nber_recession +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")
m18 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[nber_recession == 0], cluster = "gvkey")
m20 <- feols(as.formula(paste("neg_spi_at ~ ceo_more_conservative * nber_recession + ceo_more_liberal * nber_recession +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# ========================================================
# 1. FULL REGRESSION TABLES
# ========================================================

cat('\n\n################################################################\n')
cat('# 1. FULL REGRESSION TABLES\n')
cat('################################################################\n\n')

cat('\n--- Combined etable ---\n')
etable(m1, m2, m9a, m18, m20,
       headers = c("OLS", "FE Main", "NBER Recession", "Non-Recession", "Asymmetric+Rec"),
       fitstat = c('n', 'wr2', 'aic'),
       digits = 4,
       se.below = TRUE)

cat('\n===== M2: Main FE =====\n')
summary(m2)

cat('\n===== M9a: NBER Recession =====\n')
summary(m9a)

cat('\n===== M18: Non-recession subsample =====\n')
summary(m18)

cat('\n===== M20: Asymmetric + Recession =====\n')
summary(m20)

cat('\n===== M4: Labor intensity =====\n')
summary(m4)

# ========================================================
# 2. SUMMARY STATISTICS
# ========================================================

cat('\n\n################################################################\n')
cat('# 2. SUMMARY STATISTICS\n')
cat('################################################################\n\n')

sumstat_vars <- c('misalign_abs', 'misalign_signed', 'ceo_cfscore',
  'emp_mean_cfscore', 'emp_sd_cfscore', 'n_emp_donors',
  'neg_spi_at', 'emp_change', 'layoff',
  'lag_size', 'lag_roa', 'lag_leverage', 'lag_tobinq',
  'lag_sale_growth', 'lag_loss', 'lag_capx_at', 'lag_rd_at',
  'lag_acquisition', 'lag_emp_sale',
  'nber_recession', 'firm_downturn')

# Check which vars exist
sumstat_vars <- intersect(sumstat_vars, names(reg_data))

sumstats <- reg_data[!is.na(misalign_abs), lapply(.SD, function(x) {
  c(N = sum(!is.na(x)),
    Mean = round(mean(x, na.rm=TRUE), 4),
    SD = round(sd(x, na.rm=TRUE), 4),
    P25 = round(quantile(x, 0.25, na.rm=TRUE), 4),
    Median = round(quantile(x, 0.50, na.rm=TRUE), 4),
    P75 = round(quantile(x, 0.75, na.rm=TRUE), 4))
}), .SDcols = sumstat_vars]

sumstats_t <- data.table(Stat = c('N','Mean','SD','P25','Median','P75'), sumstats)
cat('\n===== SUMMARY STATISTICS =====\n')
print(sumstats_t)

# ========================================================
# 3. CORRELATION MATRIX
# ========================================================

cat('\n\n################################################################\n')
cat('# 3. CORRELATION MATRIX\n')
cat('################################################################\n\n')

cor_vars <- c('misalign_abs', 'neg_spi_at', 'emp_change',
  'ceo_cfscore', 'emp_mean_cfscore', 'lag_size', 'lag_roa',
  'lag_leverage', 'lag_tobinq', 'lag_emp_sale', 'nber_recession')

cor_mat <- round(cor(reg_data[, ..cor_vars], use = 'pairwise.complete.obs'), 3)
cat('\n===== CORRELATION MATRIX =====\n')
print(cor_mat)

# ========================================================
# 4. FALSIFICATION TEST
# ========================================================

cat('\n\n################################################################\n')
cat('# 4. FALSIFICATION TEST: ALIGNMENT TYPE × RECESSION\n')
cat('################################################################\n\n')

# Create alignment categories
reg_data[, ceo_conservative := as.integer(ceo_cfscore > 0)]
reg_data[, emp_conservative := as.integer(emp_mean_cfscore > 0)]
reg_data[, alignment_type := paste0(
  ifelse(ceo_conservative == 1, 'ConsCEO', 'LibCEO'), '_',
  ifelse(emp_conservative == 1, 'ConsEmp', 'LibEmp'))]

cat('\n===== ALIGNMENT TYPE COUNTS =====\n')
print(reg_data[!is.na(misalign_abs), .N, by = alignment_type][order(-N)])

# Create indicators
reg_data[, consCEO_libEmp := as.integer(ceo_conservative == 1 & emp_conservative == 0)]
reg_data[, consCEO_consEmp := as.integer(ceo_conservative == 1 & emp_conservative == 1)]
reg_data[, libCEO_libEmp := as.integer(ceo_conservative == 0 & emp_conservative == 0)]
reg_data[, libCEO_consEmp := as.integer(ceo_conservative == 0 & emp_conservative == 1)]

cat('\nType counts in regression sample:\n')
cat('ConsCEO_LibEmp:', sum(reg_data$consCEO_libEmp, na.rm=TRUE), '\n')
cat('ConsCEO_ConsEmp:', sum(reg_data$consCEO_consEmp, na.rm=TRUE), '\n')
cat('LibCEO_LibEmp:', sum(reg_data$libCEO_libEmp, na.rm=TRUE), '\n')
cat('LibCEO_ConsEmp:', sum(reg_data$libCEO_consEmp, na.rm=TRUE), '\n')

# M23: Alignment type × recession
cat('\nRunning M23...\n')
m23 <- feols(as.formula(paste(
  "neg_spi_at ~ consCEO_libEmp * nber_recession +",
  "consCEO_consEmp * nber_recession +",
  "libCEO_consEmp * nber_recession +",
  controls, "| gvkey + fyear_lead")),
  data = reg_data, cluster = "gvkey")

cat('\n===== M23: ALIGNMENT TYPE × RECESSION =====\n')
summary(m23)

# M24: Simpler version
reg_data[, misaligned := as.integer(misalign_abs > median(misalign_abs, na.rm=TRUE))]
reg_data[, aligned_conservative := as.integer(consCEO_consEmp == 1)]

cat('\nRunning M24...\n')
m24 <- feols(as.formula(paste(
  "neg_spi_at ~ misaligned * nber_recession +",
  "aligned_conservative * nber_recession +",
  controls, "| gvkey + fyear_lead")),
  data = reg_data, cluster = "gvkey")

cat('\n===== M24: MISALIGNED vs ALIGNED CONSERVATIVE × RECESSION =====\n')
summary(m24)

save(reg_data, file = "reg_data.rdata")
cat('\nSaved updated reg_data.rdata\n')
cat('Done!\n')

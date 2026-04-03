## ============================================================
## build_full_panel.R — Parts A through D: Full 13-cycle panel
## ============================================================

library(data.table)
library(fixest)
setwd("/Users/myoffice/Desktop")

cat("========================================\n")
cat("PART A: Stack all 13 cycles\n")
cat("========================================\n")

## ---- Load all 13 employee panels ----
cycles <- seq(2000, 2024, by = 2)
panels <- list()
for (yr in cycles) {
  f <- paste0("employee_panel_", yr, ".rdata")
  if (!file.exists(f)) { cat("MISSING:", f, "\n"); next }
  env <- new.env()
  load(f, envir = env)
  obj <- get(ls(envir = env)[1], envir = env)
  setDT(obj)
  panels[[as.character(yr)]] <- obj
  cat("Loaded", f, ":", nrow(obj), "firms\n")
}

panel <- rbindlist(panels, fill = TRUE)
cat("\nStacked panel:", nrow(panel), "firm-cycles\n")
cat("Unique gvkeys:", uniqueN(panel$gvkey), "\n")
cat("Cycles:", sort(unique(panel$cycle)), "\n")

## ---- Merge CEO identity from ExecuComp ----
load("execucomp_ceos.rdata")
setDT(execucomp_ceos)
ceo_slim <- execucomp_ceos[, .(gvkey, year, execid, exec_fullname)]

# Primary merge: cycle = year
panel <- merge(panel, ceo_slim,
               by.x = c("gvkey", "cycle"),
               by.y = c("gvkey", "year"),
               all.x = TRUE)

cat("\nAfter ExecuComp merge (exact year):", sum(!is.na(panel$execid)),
    "of", nrow(panel), "firm-cycles have CEO\n")

# Year-1 fallback for missing CEOs
missing_idx <- which(is.na(panel$execid))
if (length(missing_idx) > 0) {
  missing_keys <- panel[missing_idx, .(gvkey, cycle)]
  missing_keys[, year_minus1 := cycle - 1L]
  fallback <- merge(missing_keys, ceo_slim,
                    by.x = c("gvkey", "year_minus1"),
                    by.y = c("gvkey", "year"),
                    all.x = TRUE)
  fallback <- fallback[!is.na(execid)]
  fallback <- unique(fallback, by = c("gvkey", "cycle"))

  if (nrow(fallback) > 0) {
    panel[fallback, on = .(gvkey, cycle),
          c("execid", "exec_fullname") := .(i.execid, i.exec_fullname)]
    cat("After year-1 fallback:", sum(!is.na(panel$execid)), "firm-cycles have CEO\n")
  }
}

## ---- Merge CEO CFscore from DIME lookup ----
load("ceo_dime_lookup.rdata")
setDT(ceo_dime_lookup)
# Check column name
ceo_col <- intersect(names(ceo_dime_lookup), c("ceo_cfscore", "cfscore"))
if (length(ceo_col) == 0) stop("No cfscore column in ceo_dime_lookup")
if (ceo_col[1] != "ceo_cfscore") {
  setnames(ceo_dime_lookup, ceo_col[1], "ceo_cfscore")
}

panel <- merge(panel, ceo_dime_lookup[, .(execid, ceo_cfscore)],
               by = "execid", all.x = TRUE)

cat("With CEO CFscore:", sum(!is.na(panel$ceo_cfscore)), "of", nrow(panel), "\n")

## ---- Add company names ----
load("employer_lookup_for_panel.rdata")
setDT(all_exact)
firm_names <- unique(all_exact[, .(gvkey, conm)])
firm_names <- unique(firm_names, by = "gvkey")
panel <- merge(panel, firm_names, by = "gvkey", all.x = TRUE)

## ---- Compute misalignment ----
panel[, misalign_abs := abs(ceo_cfscore - emp_mean_cfscore)]
panel[, misalign_signed := ceo_cfscore - emp_mean_cfscore]
panel[, ceo_more_conservative := pmax(misalign_signed, 0)]
panel[, ceo_more_liberal := pmax(-misalign_signed, 0)]

## ---- Create panel versions ----
panel_complete <- panel[!is.na(misalign_abs)]
panel_qual <- panel_complete[n_emp_donors >= 20]

cat("\n=== PART A SUMMARY ===\n")
cat("Total firm-cycles:", nrow(panel), "\n")
cat("With CEO identified:", sum(!is.na(panel$execid)), "\n")
cat("With CEO CFscore:", sum(!is.na(panel$ceo_cfscore)), "\n")
cat("With misalignment:", nrow(panel_complete), "\n")
cat("Quality filtered (>=20 donors):", nrow(panel_qual), "\n")
cat("Unique firms (qual):", uniqueN(panel_qual$gvkey), "\n")

freq <- panel_qual[, .N, by = gvkey]
cat("Firms with 4+ cycles:", sum(freq$N >= 4), "\n")
cat("Firms in all 13 cycles:", sum(freq$N == 13), "\n")

cat("\n--- Mean |misalignment| by cycle ---\n")
cycle_stats <- panel_qual[, .(
  N = .N,
  mean_misalign = round(mean(misalign_abs), 3),
  median_misalign = round(median(misalign_abs), 3),
  pct_ceo_conservative = round(100 * mean(misalign_signed > 0), 1)
), by = cycle]
setorder(cycle_stats, cycle)
print(cycle_stats)

save(panel, file = "panel_full.rdata")
save(panel_complete, file = "panel_complete.rdata")
save(panel_qual, file = "panel_qual.rdata")
cat("\nSaved panel_full.rdata, panel_complete.rdata, panel_qual.rdata\n")

cat("\n========================================\n")
cat("PART B: Merge with Compustat\n")
cat("========================================\n")

load("compustat_outcomes.rdata")
setDT(compustat)

## Lagged specification
panel_qual[, fyear_lead := cycle + 1L]
reg_data <- merge(panel_qual, compustat,
                  by.x = c("gvkey", "fyear_lead"),
                  by.y = c("gvkey", "fyear"),
                  all.x = TRUE)

# Exclude financials and utilities
reg_data <- reg_data[exclude_industry == 0 | is.na(exclude_industry)]
reg_data <- reg_data[!is.na(neg_spi_at) | !is.na(emp_change)]

cat("Regression data after industry exclusion:", nrow(reg_data), "rows\n")
cat("Unique firms:", uniqueN(reg_data$gvkey), "\n")

## Additional variables
reg_data[, high_misalign := as.integer(misalign_abs > median(misalign_abs, na.rm = TRUE))]
reg_data[!is.na(misalign_abs), misalign_tercile := cut(misalign_abs,
  breaks = quantile(misalign_abs, c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Low", "Medium", "High"), include.lowest = TRUE)]
reg_data[, downturn := as.integer(fyear_lead %in% c(2001, 2002, 2008, 2009, 2020))]
reg_data[, post_2016 := as.integer(cycle >= 2016)]

cat("Downturn obs:", sum(reg_data$downturn, na.rm = TRUE), "\n")
cat("Post-2016 obs:", sum(reg_data$post_2016, na.rm = TRUE), "\n")

save(reg_data, file = "reg_data.rdata")
cat("Saved reg_data.rdata\n")

## ---- Summary stats of key variables ----
cat("\n--- Summary Statistics (regression sample) ---\n")
summ_vars <- c("misalign_abs", "misalign_signed", "neg_spi_at", "emp_change",
               "ceo_cfscore", "emp_mean_cfscore", "n_emp_donors",
               "lag_size", "lag_roa", "lag_leverage", "lag_tobinq",
               "lag_sale_growth", "lag_loss", "lag_capx_at", "lag_rd_at",
               "lag_acquisition", "lag_emp_sale")

summ_table <- rbindlist(lapply(summ_vars, function(v) {
  x <- reg_data[[v]]
  if (is.null(x)) return(NULL)
  x <- x[!is.na(x)]
  data.table(Variable = v, N = length(x),
             Mean = round(mean(x), 4), SD = round(sd(x), 4),
             P25 = round(quantile(x, 0.25), 4),
             Median = round(median(x), 4),
             P75 = round(quantile(x, 0.75), 4))
}))
print(summ_table)

cat("\n========================================\n")
cat("PART C: Run all 15 regressions\n")
cat("========================================\n")

controls <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_acquisition"

# M1: Pooled OLS
cat("Running M1...\n")
m1 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls)),
            data = reg_data, cluster = "gvkey")

# M2: Firm + Year FE (main specification)
cat("Running M2...\n")
m2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M3: Employee change
cat("Running M3...\n")
m3 <- feols(as.formula(paste("emp_change ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M4: Labor intensity interaction
cat("Running M4...\n")
m4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * lag_emp_sale +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M5: Contemporaneous
cat("Running M5...\n")
reg_data_contemp <- merge(panel_qual, compustat,
                          by.x = c("gvkey", "cycle"),
                          by.y = c("gvkey", "fyear"),
                          all.x = TRUE)
reg_data_contemp <- reg_data_contemp[exclude_industry == 0 | is.na(exclude_industry)]
reg_data_contemp <- reg_data_contemp[!is.na(neg_spi_at)]
m5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + cycle")),
            data = reg_data_contemp, cluster = "gvkey")

# M6: Signed misalignment
cat("Running M6...\n")
m6 <- feols(as.formula(paste("neg_spi_at ~ misalign_signed +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M7: Asymmetric
cat("Running M7...\n")
m7 <- feols(as.formula(paste("neg_spi_at ~ ceo_more_conservative + ceo_more_liberal +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M8: Layoff LPM
cat("Running M8...\n")
m8 <- feols(as.formula(paste("layoff ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
            data = reg_data, cluster = "gvkey")

# M9: Downturn interaction (firm FE only — downturn collinear with year FE)
cat("Running M9...\n")
m9 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * downturn +", controls, "| gvkey")),
            data = reg_data, cluster = "gvkey")

# M10: CEO ideology level
cat("Running M10...\n")
m10 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + ceo_cfscore +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# M11: Balanced panel (4+ cycles)
cat("Running M11...\n")
freq_reg <- reg_data[!is.na(misalign_abs), .N, by = gvkey][N >= 4]
m11 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data[gvkey %in% freq_reg$gvkey], cluster = "gvkey")

# M12: Lagged DV
cat("Running M12...\n")
m12 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + lag_neg_spi_at +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# M13: High misalignment binary
cat("Running M13...\n")
m13 <- feols(as.formula(paste("neg_spi_at ~ high_misalign +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = "gvkey")

# M14: Double-clustered SE
cat("Running M14...\n")
m14 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs +", controls, "| gvkey + fyear_lead")),
             data = reg_data, cluster = c("gvkey", "fyear_lead"))

# M15: Post-2016 interaction (firm FE only — post_2016 collinear with year FE)
cat("Running M15...\n")
m15 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs * post_2016 +", controls, "| gvkey")),
             data = reg_data, cluster = "gvkey")

cat("\n========================================\n")
cat("PART D: Output\n")
cat("========================================\n")

cat("\n--- Table 1: Main Results ---\n")
etable(m1, m2, m3, m4,
       headers = c("OLS", "FE Main", "Emp Change", "Labor Int"))

cat("\n--- Table 2: Timing and Direction ---\n")
etable(m5, m6, m7,
       headers = c("Contemp", "Signed", "Asymmetric"))

cat("\n--- Table 3: Robustness ---\n")
etable(m8, m9, m10, m11, m12, m13, m14, m15,
       headers = c("Layoff", "Downturn", "CEO Level", "Balanced",
                    "Lag DV", "High Mis", "2-way Clust", "Post-2016"))

## ---- Key coefficients summary ----
cat("\n=== KEY COEFFICIENTS SUMMARY ===\n\n")

extract_key <- function(model, name, var, n_firms = NULL) {
  ct <- coeftable(model)
  if (var %in% rownames(ct)) {
    row <- ct[var, ]
    nobs <- model$nobs
    wr2 <- tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA)
    nf <- if (!is.null(n_firms)) n_firms else
      tryCatch(length(unique(model$fixef_id$gvkey)), error = function(e) NA)
    data.table(Model = name, Variable = var,
               Coef = round(row[1], 5), SE = round(row[2], 5),
               pvalue = round(row[4], 4), N = nobs, Firms = nf,
               Within_R2 = round(wr2, 4))
  } else NULL
}

keys <- rbindlist(list(
  extract_key(m1, "M1 OLS", "misalign_abs"),
  extract_key(m2, "M2 FE Main", "misalign_abs"),
  extract_key(m3, "M3 Emp Change", "misalign_abs"),
  extract_key(m4, "M4 Labor Int", "misalign_abs"),
  extract_key(m4, "M4 Labor Int", "misalign_abs:lag_emp_sale"),
  extract_key(m5, "M5 Contemp", "misalign_abs"),
  extract_key(m6, "M6 Signed", "misalign_signed"),
  extract_key(m7, "M7 Asymmetric", "ceo_more_conservative"),
  extract_key(m7, "M7 Asymmetric", "ceo_more_liberal"),
  extract_key(m8, "M8 Layoff", "misalign_abs"),
  extract_key(m9, "M9 Downturn", "misalign_abs"),
  extract_key(m9, "M9 Downturn", "misalign_abs:downturn"),
  extract_key(m10, "M10 CEO Level", "misalign_abs"),
  extract_key(m10, "M10 CEO Level", "ceo_cfscore"),
  extract_key(m11, "M11 Balanced", "misalign_abs"),
  extract_key(m12, "M12 Lag DV", "misalign_abs"),
  extract_key(m13, "M13 High Mis", "high_misalign"),
  extract_key(m14, "M14 2-way Clust", "misalign_abs"),
  extract_key(m15, "M15 Post-2016", "misalign_abs"),
  extract_key(m15, "M15 Post-2016", "misalign_abs:post_2016")
), fill = TRUE)
print(keys, nrows = 30)

## ---- Correlation matrix ----
cat("\n--- Correlation Matrix ---\n")
cor_vars <- c("misalign_abs", "neg_spi_at", "emp_change", "ceo_cfscore",
              "emp_mean_cfscore", "lag_size", "lag_roa", "lag_leverage", "lag_tobinq")
cor_data <- reg_data[, ..cor_vars]
cor_mat <- round(cor(cor_data, use = "pairwise.complete.obs"), 3)
print(cor_mat)

## ---- Misalignment by cycle ----
cat("\n--- Misalignment by Cycle (regression sample) ---\n")
cycle_reg <- reg_data[!is.na(misalign_abs), .(
  N = .N,
  mean_misalign = round(mean(misalign_abs), 3),
  median_misalign = round(median(misalign_abs), 3),
  pct_ceo_conservative = round(100 * mean(misalign_signed > 0), 1),
  mean_neg_spi = round(mean(neg_spi_at, na.rm = TRUE), 4)
), by = cycle]
setorder(cycle_reg, cycle)
print(cycle_reg)

save(reg_data, file = "reg_data.rdata")
cat("\nAll done! Saved reg_data.rdata\n")

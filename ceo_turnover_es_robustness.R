library(data.table)
library(fixest)

# Load saved results
res <- readRDS("ceo_turnover_event_study.rds")
transitions <- res$transitions

cat("=== Full transitions sample:", nrow(transitions), "===\n\n")

# ============================================================
# Load outcome data from reg_data
# ============================================================
load("reg_data_v2.rdata")
setDT(reg_data)

outcome_vars <- c("gvkey", "fyear_lead", "neg_spi_at", "layoff",
                  "lag_size", "lag_roa", "lag_leverage", "lag_tobinq", "lag_sale_growth")
outcomes <- unique(reg_data[, ..outcome_vars])
setnames(outcomes, "fyear_lead", "fyear")

# ============================================================
# Function to build event panel and run specs for a given split
# ============================================================
run_event_study <- function(trans_sub, label) {
  cat("\n################################################################\n")
  cat("  ", label, "\n")
  cat("  N transitions:", nrow(trans_sub), 
      " | high:", sum(trans_sub$treat == 1), 
      " | low:", sum(trans_sub$treat == 0), "\n")
  cat("################################################################\n")
  
  # Build event panel
  ep <- trans_sub[, .(event_time = -3:3), 
                  by = .(gvkey, event_year = fyear, treat, 
                         delta_misalignment, abs_delta_misalignment)]
  ep[, fyear := event_year + event_time]
  ep[, post := as.integer(event_time >= 0)]
  
  ep <- merge(ep, outcomes, by = c("gvkey", "fyear"), all.x = TRUE)
  
  cat("  Panel rows:", nrow(ep), 
      " | with neg_spi_at:", sum(!is.na(ep$neg_spi_at)),
      " | with layoff:", sum(!is.na(ep$layoff)), "\n\n")
  
  # --- Event study: neg_spi_at ---
  cat("--- Event Study: neg_spi_at ---\n")
  es1 <- tryCatch(
    feols(neg_spi_at ~ i(event_time, treat, ref = -1) +
            lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth |
            gvkey + fyear, data = ep, cluster = ~gvkey),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(es1)) {
    ct <- coeftable(es1)
    idx <- grep("event_time.*treat", rownames(ct))
    print(ct[idx, , drop = FALSE])
  }
  
  # --- Event study: layoff ---
  cat("\n--- Event Study: layoff ---\n")
  es2 <- tryCatch(
    feols(layoff ~ i(event_time, treat, ref = -1) +
            lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth |
            gvkey + fyear, data = ep, cluster = ~gvkey),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(es2)) {
    ct <- coeftable(es2)
    idx <- grep("event_time.*treat", rownames(ct))
    print(ct[idx, , drop = FALSE])
  }
  
  # --- DiD: neg_spi_at ---
  cat("\n--- DiD: neg_spi_at ---\n")
  did1 <- tryCatch(
    feols(neg_spi_at ~ post:treat + post + treat +
            lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth |
            gvkey + fyear, data = ep, cluster = ~gvkey),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(did1)) {
    ct <- coeftable(did1)
    idx <- grep("post|treat", rownames(ct))
    print(ct[idx, , drop = FALSE])
  }
  
  # --- DiD: layoff ---
  cat("\n--- DiD: layoff ---\n")
  did2 <- tryCatch(
    feols(layoff ~ post:treat + post + treat +
            lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth |
            gvkey + fyear, data = ep, cluster = ~gvkey),
    error = function(e) { cat("  ERROR:", e$message, "\n"); NULL }
  )
  if (!is.null(did2)) {
    ct <- coeftable(did2)
    idx <- grep("post|treat", rownames(ct))
    print(ct[idx, , drop = FALSE])
  }
  
  cat("\n")
}

# ============================================================
# Compute tercile, quartile, quintile cutoffs
# ============================================================
transitions[, abs_dm := abs_delta_misalignment]

cat("=== |delta_misalignment| quantiles ===\n")
cat("Terciles:", round(quantile(transitions$abs_dm, c(1/3, 2/3)), 4), "\n")
cat("Quartiles:", round(quantile(transitions$abs_dm, c(0.25, 0.75)), 4), "\n")
cat("Quintiles:", round(quantile(transitions$abs_dm, c(0.2, 0.8)), 4), "\n\n")

# --- TERCILE: top vs bottom third ---
t3 <- quantile(transitions$abs_dm, c(1/3, 2/3))
trans_tercile <- transitions[abs_dm <= t3[1] | abs_dm >= t3[2]]
trans_tercile[, treat := as.integer(abs_dm >= t3[2])]
run_event_study(trans_tercile, "TERCILE: Top 1/3 vs Bottom 1/3 of |delta_misalignment|")

# --- QUARTILE: top vs bottom quartile ---
q4 <- quantile(transitions$abs_dm, c(0.25, 0.75))
trans_quartile <- transitions[abs_dm <= q4[1] | abs_dm >= q4[2]]
trans_quartile[, treat := as.integer(abs_dm >= q4[2])]
run_event_study(trans_quartile, "QUARTILE: Top 25% vs Bottom 25% of |delta_misalignment|")

# --- QUINTILE: top vs bottom quintile ---
q5 <- quantile(transitions$abs_dm, c(0.2, 0.8))
trans_quintile <- transitions[abs_dm <= q5[1] | abs_dm >= q5[2]]
trans_quintile[, treat := as.integer(abs_dm >= q5[2])]
run_event_study(trans_quintile, "QUINTILE: Top 20% vs Bottom 20% of |delta_misalignment|")

# ============================================================
# Also re-run median split for comparison
# ============================================================
trans_median <- copy(transitions)
trans_median[, treat := high_delta]
run_event_study(trans_median, "MEDIAN SPLIT (baseline): Above vs Below median |delta_misalignment|")

cat("\nDone.\n")

# PROMPT B: Pre-trend / lead-lag tests
library(fixest)
library(data.table)
load("reg_data_v2.rdata")
setDT(reg_data)

controls <- c("size", "roa", "leverage", "tobinq", "sale_growth",
              "loss", "capx_at", "rd_at", "acquisition")

# reg_data is organized by cycle (biennial) with fyear_lead = cycle + 1
# neg_spi_at is already the t+1 outcome
# We need to create shifted versions across cycles

setorder(reg_data, gvkey, cycle)

# Create lead/lag DVs across cycles
# neg_spi_at is at t+1 (main spec)
# Shift forward one cycle = t+3 outcome (lead2 — next cycle's outcome)
# Shift backward one cycle = t-1 outcome (contemporaneous with prior cycle's misalignment)
reg_data[, neg_spi_at_lead2 := shift(neg_spi_at, n=1, type="lead"), by = gvkey]
reg_data[, neg_spi_at_contemp := shift(neg_spi_at, n=1, type="lag"), by = gvkey]
reg_data[, neg_spi_at_lag1 := shift(neg_spi_at_contemp, n=1, type="lag"), by = gvkey]

# Same for layoff
reg_data[, layoff_lead2 := shift(layoff, n=1, type="lead"), by = gvkey]
reg_data[, layoff_contemp := shift(layoff, n=1, type="lag"), by = gvkey]
reg_data[, layoff_lag1 := shift(layoff_contemp, n=1, type="lag"), by = gvkey]

make_fml <- function(dv) {
  as.formula(paste(
    dv, "~",
    "misalign_abs +",
    paste(controls, collapse = " + "),
    "| gvkey + fyear_lead"
  ))
}

cat("=============================================================\n")
cat("TEST 1: Lead/Lag Structure (misalign_abs coefficient)\n")
cat("=============================================================\n")
cat("Note: data is biennial, so 'lag' = 2 calendar years back\n\n")

m_lag1    <- feols(make_fml("neg_spi_at_lag1"),   data=reg_data, cluster=~gvkey)
m_contemp <- feols(make_fml("neg_spi_at_contemp"), data=reg_data, cluster=~gvkey)
m_lead1   <- feols(make_fml("neg_spi_at"),         data=reg_data, cluster=~gvkey)
m_lead2   <- feols(make_fml("neg_spi_at_lead2"),   data=reg_data, cluster=~gvkey)

etable(m_lag1, m_contemp, m_lead1, m_lead2,
       keep = "misalign_abs",
       headers = c("t-1 (placebo)", "t (contemp)", "t+1 (main)", "t+2 (fade?)"),
       title = "Lead/Lag: Misalignment -> Restructuring (neg_spi_at)")

# Also for layoff
cat("\n--- Layoff DV ---\n")
ml_lag1    <- feols(make_fml("layoff_lag1"),   data=reg_data, cluster=~gvkey)
ml_contemp <- feols(make_fml("layoff_contemp"), data=reg_data, cluster=~gvkey)
ml_lead1   <- feols(make_fml("layoff"),         data=reg_data, cluster=~gvkey)
ml_lead2   <- feols(make_fml("layoff_lead2"),   data=reg_data, cluster=~gvkey)

etable(ml_lag1, ml_contemp, ml_lead1, ml_lead2,
       keep = "misalign_abs",
       headers = c("t-1 (placebo)", "t (contemp)", "t+1 (main)", "t+2 (fade?)"),
       title = "Lead/Lag: Misalignment -> Layoff")

# --- TEST 2: First Differences ---
cat("\n=============================================================\n")
cat("TEST 2: First Differences\n")
cat("=============================================================\n\n")

reg_data[, d_misalign := misalign_abs - shift(misalign_abs, 1), by=gvkey]
reg_data[, d_neg_spi  := neg_spi_at - shift(neg_spi_at, 1), by=gvkey]
reg_data[, d_layoff   := layoff - shift(layoff, 1), by=gvkey]

m_fd_spi <- feols(d_neg_spi ~ d_misalign +
                size + roa + leverage + tobinq + sale_growth |
                fyear_lead,
              data = reg_data, cluster = ~gvkey)
cat("--- First Differences: neg_spi_at ---\n")
print(summary(m_fd_spi))

m_fd_layoff <- feols(d_layoff ~ d_misalign +
                size + roa + leverage + tobinq + sale_growth |
                fyear_lead,
              data = reg_data, cluster = ~gvkey)
cat("--- First Differences: layoff ---\n")
print(summary(m_fd_layoff))

# --- TEST 3: Interaction Pre-trend ---
cat("\n=============================================================\n")
cat("TEST 3: Interaction Pre-Trend (true vs placebo)\n")
cat("=============================================================\n\n")

reg_data[, misalign_x_indlayoff := misalign_abs * ind_layoff_rate]
reg_data[, misalign_x_indlayoff_lag := misalign_abs * 
              shift(ind_layoff_rate, 1), by=gvkey]

m_placebo_int <- feols(
  neg_spi_at_lag1 ~ misalign_abs + misalign_x_indlayoff_lag +
    size + roa + leverage + tobinq + sale_growth +
    loss + capx_at + rd_at + acquisition | gvkey + fyear_lead,
  data = reg_data, cluster = ~gvkey)

m_true_int <- feols(
  neg_spi_at ~ misalign_abs + misalign_x_indlayoff +
    size + roa + leverage + tobinq + sale_growth +
    loss + capx_at + rd_at + acquisition | gvkey + fyear_lead,
  data = reg_data, cluster = ~gvkey)

etable(m_placebo_int, m_true_int,
       keep = c("misalign_abs", "misalign_x_indlayoff"),
       headers = c("Placebo: t-1 outcome", "True: t+1 outcome"))

saveRDS(list(m_lag1=m_lag1, m_contemp=m_contemp, m_lead1=m_lead1,
             m_lead2=m_lead2, m_fd_spi=m_fd_spi, m_fd_layoff=m_fd_layoff,
             m_placebo_int=m_placebo_int, m_true_int=m_true_int,
             ml_lag1=ml_lag1, ml_contemp=ml_contemp, ml_lead1=ml_lead1,
             ml_lead2=ml_lead2),
        "pretrend_tests.rds")
cat("\nSaved to pretrend_tests.rds\n")

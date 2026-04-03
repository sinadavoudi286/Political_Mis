library(data.table)
library(fixest)
setFixest_notes(FALSE)

# ── Step 1: Build industry downturn measures from full Compustat ──
load("compustat_outcomes.rdata")
cs <- copy(compustat)
rm(compustat); gc()

# Exclude financials/utilities
cs <- cs[!sic2 %in% c(49, 60:69)]
cat("Compustat after excl:", nrow(cs), "rows,", uniqueN(cs$sic2), "industries\n")

# Compute industry-level measures by sic2 × fyear
ind <- cs[!is.na(sic2) & !is.na(fyear), .(
  ind_sale_growth  = median(sale_growth, na.rm = TRUE),
  ind_neg_spi      = median(neg_spi_at, na.rm = TRUE),
  ind_layoff_rate  = mean(emp_change < -0.05, na.rm = TRUE),
  n_firms          = .N
), by = .(sic2, fyear)]

ind[, ind_downturn := as.integer(ind_sale_growth < 0)]
ind <- ind[!is.na(ind_sale_growth)]  # drop cells with all-NA sales growth

cat("\n=== INDUSTRY DOWNTURN MEASURES ===\n")
cat("Total sic2-fyear cells:", nrow(ind), "\n")
cat("Cells with ind_downturn=1:", sum(ind$ind_downturn), "(",
    round(100*mean(ind$ind_downturn),1), "%)\n")
cat("Compare: NBER recession covers ~4 of ~25 years (16%)\n\n")

# Distribution by year
cat("ind_downturn rate by fyear:\n")
yr_tab <- ind[fyear >= 2000 & fyear <= 2024, .(
  n_industries = .N,
  n_downturn = sum(ind_downturn),
  pct_downturn = round(100*mean(ind_downturn),1),
  med_sale_growth = round(median(ind_sale_growth, na.rm=TRUE),3)
), by = fyear][order(fyear)]
print(yr_tab)

cat("\nind_sale_growth summary:\n"); print(summary(ind$ind_sale_growth))
cat("ind_layoff_rate summary:\n"); print(summary(ind$ind_layoff_rate))

rm(cs); gc()

# ── Step 2: Merge to reg_data_v2 ──
load("reg_data_v2.rdata")
# fyear_lead is the Compustat year
if (!"fyear" %in% names(reg_data)) {
  setnames(reg_data, "fyear_lead", "fyear")
}

# Merge
merge_cols <- c("ind_sale_growth", "ind_neg_spi", "ind_downturn", "ind_layoff_rate", "n_firms")
# Remove if already present
for (col in merge_cols) if (col %in% names(reg_data)) reg_data[, (col) := NULL]

reg_data <- merge(reg_data, ind[, c("sic2","fyear", merge_cols), with=FALSE],
                  by = c("sic2","fyear"), all.x = TRUE)

cat("\n=== MERGE RESULTS ===\n")
cat("reg_data rows:", nrow(reg_data), "\n")
cat("ind_downturn coverage:", sum(!is.na(reg_data$ind_downturn)), "of", nrow(reg_data), "\n")
cat("ind_downturn=1 in sample:", sum(reg_data$ind_downturn, na.rm=TRUE), "(",
    round(100*mean(reg_data$ind_downturn, na.rm=TRUE),1), "%)\n")
cat("NBER recession=1 in sample:", sum(reg_data$nber_recession, na.rm=TRUE), "(",
    round(100*mean(reg_data$nber_recession, na.rm=TRUE),1), "%)\n")

# Save updated reg_data
# Restore name for saving
setnames(reg_data, "fyear", "fyear_lead")
save(reg_data, file = "reg_data_v2.rdata")
cat("Saved reg_data_v2.rdata with industry measures\n")
setnames(reg_data, "fyear_lead", "fyear")

# ── Step 3: Regressions ──
rd <- reg_data[!sic2 %in% c(49, 60:69)]
rd[, recession := nber_recession]
rd[, lag_aqc_indicator := lag_acquisition]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"

I1 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_downturn +", ctrl, "| gvkey + fyear")),
            data = rd, cluster = ~gvkey)

I2 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_sale_growth +", ctrl, "| gvkey + fyear")),
            data = rd, cluster = ~gvkey)

I3 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_downturn +", ctrl, "| gvkey + sic2^fyear")),
            data = rd, cluster = ~gvkey)

I4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:recession + misalign_abs:ind_downturn +", ctrl, "| gvkey + fyear")),
            data = rd, cluster = ~gvkey)

I5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:ind_layoff_rate +", ctrl, "| gvkey + fyear")),
            data = rd, cluster = ~gvkey)

# ── Step 4: Report ──
cat("\n\n")
cat("════════════════════════════════════════════════════════════════════\n")
cat("INDUSTRY DOWNTURN REGRESSION RESULTS\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

etable(I1, I2, I3, I4, I5,
       headers = c("I1:IndDown","I2:ContSale","I3:Ind×YrFE","I4:HorseRace","I5:PeerLayoff"),
       se.below = TRUE, signif.code = c("***"=0.01,"**"=0.05,"*"=0.1))

# ── Net effects and detailed stats ──
cat("\n════════════════════════════════════════════════════════════════════\n")
cat("DETAILED COEFFICIENT SUMMARY\n")
cat("════════════════════════════════════════════════════════════════════\n\n")

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

get_coef_info <- function(mod, var) {
  b <- coef(mod); V <- vcov(mod)
  idx <- match(var, names(b))
  if (is.na(idx)) return(list(b=NA, se=NA, p=NA))
  se <- sqrt(V[idx,idx])
  p <- 2*pt(abs(b[idx]/se), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  list(b=b[idx], se=se, p=p)
}

net_effect <- function(mod, var1, var2) {
  b <- coef(mod); V <- vcov(mod)
  i1 <- match(var1, names(b)); i2 <- match(var2, names(b))
  net <- b[i1] + b[i2]
  se <- sqrt(V[i1,i1] + V[i2,i2] + 2*V[i1,i2])
  t <- net/se
  p <- 2*pt(abs(t), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  list(net=net, se=se, p=p)
}

# I1: Binary industry downturn
cat("I1: Binary Industry Downturn\n")
s_main <- get_coef_info(I1, "misalign_abs")
s_int <- get_coef_info(I1, "misalign_abs:ind_downturn")
s_net <- net_effect(I1, "misalign_abs", "misalign_abs:ind_downturn")
cat(sprintf("  misalign_abs:              %8.5f (%.5f) p=%.4f %s\n", s_main$b, s_main$se, s_main$p, stars(s_main$p)))
cat(sprintf("  misalign_abs:ind_downturn: %8.5f (%.5f) p=%.4f %s\n", s_int$b, s_int$se, s_int$p, stars(s_int$p)))
cat(sprintf("  NET during ind downturn:   %8.5f (%.5f) p=%.4f %s\n\n", s_net$net, s_net$se, s_net$p, stars(s_net$p)))

# I2: Continuous
cat("I2: Continuous Industry Sales Growth\n")
s_main <- get_coef_info(I2, "misalign_abs")
s_int <- get_coef_info(I2, "misalign_abs:ind_sale_growth")
cat(sprintf("  misalign_abs:                   %8.5f (%.5f) p=%.4f %s\n", s_main$b, s_main$se, s_main$p, stars(s_main$p)))
cat(sprintf("  misalign_abs:ind_sale_growth:   %8.5f (%.5f) p=%.4f %s\n", s_int$b, s_int$se, s_int$p, stars(s_int$p)))
cat(sprintf("  Interpretation: 10pp worse ind growth -> misalign effect changes by %.5f\n\n", -0.10 * s_int$b))

# I3: Industry×Year FE
cat("I3: With Industry×Year FE (tough test)\n")
s_main <- get_coef_info(I3, "misalign_abs")
s_int <- get_coef_info(I3, "misalign_abs:ind_downturn")
cat(sprintf("  misalign_abs:              %8.5f (%.5f) p=%.4f %s\n", s_main$b, s_main$se, s_main$p, stars(s_main$p)))
cat(sprintf("  misalign_abs:ind_downturn: %8.5f (%.5f) p=%.4f %s\n\n", s_int$b, s_int$se, s_int$p, stars(s_int$p)))

# I4: Horse race
cat("I4: Horse Race — NBER Recession vs Industry Downturn\n")
s_main <- get_coef_info(I4, "misalign_abs")
s_rec <- get_coef_info(I4, "misalign_abs:recession")
s_ind <- get_coef_info(I4, "misalign_abs:ind_downturn")
cat(sprintf("  misalign_abs:              %8.5f (%.5f) p=%.4f %s\n", s_main$b, s_main$se, s_main$p, stars(s_main$p)))
cat(sprintf("  misalign_abs:recession:    %8.5f (%.5f) p=%.4f %s\n", s_rec$b, s_rec$se, s_rec$p, stars(s_rec$p)))
cat(sprintf("  misalign_abs:ind_downturn: %8.5f (%.5f) p=%.4f %s\n", s_ind$b, s_ind$se, s_ind$p, stars(s_ind$p)))
if (s_rec$p < s_ind$p) {
  cat("  >> NBER recession interaction dominates\n\n")
} else {
  cat("  >> Industry downturn interaction dominates\n\n")
}

# I5: Peer layoffs
cat("I5: Peer Layoff Rate\n")
s_main <- get_coef_info(I5, "misalign_abs")
s_int <- get_coef_info(I5, "misalign_abs:ind_layoff_rate")
cat(sprintf("  misalign_abs:                  %8.5f (%.5f) p=%.4f %s\n", s_main$b, s_main$se, s_main$p, stars(s_main$p)))
cat(sprintf("  misalign_abs:ind_layoff_rate:  %8.5f (%.5f) p=%.4f %s\n", s_int$b, s_int$se, s_int$p, stars(s_int$p)))
cat(sprintf("  At mean layoff rate (%.2f): effect = %.5f\n",
            mean(rd$ind_layoff_rate, na.rm=TRUE),
            s_main$b + s_int$b * mean(rd$ind_layoff_rate, na.rm=TRUE)))

# Save
results <- list(I1=I1, I2=I2, I3=I3, I4=I4, I5=I5, ind_measures=ind)
saveRDS(results, file = "industry_downturn_tests.rds")
cat("\n\nSaved industry_downturn_tests.rds\n")

library(data.table)
load("reg_data_v2.rdata")
load("execucomp_ceos.rdata")
ec <- get(ls(pattern="exec")[1])
if (!is.data.table(ec)) ec <- as.data.table(ec)
ec[, execid := as.character(execid)]

# Sample: pick a few reg_data rows and trace
cat("=== Sample reg_data rows ===\n")
set.seed(42)
samp <- reg_data[sample(.N, 5), .(gvkey, fyear_lead, execid, exec_fullname)]
print(samp)

# For each, check execucomp history
for (i in 1:nrow(samp)) {
  g <- samp$gvkey[i]; e <- samp$execid[i]; y <- samp$fyear_lead[i]
  cat(sprintf("\n--- %s (execid %s) at gvkey %s, fyear_lead %d ---\n", 
      samp$exec_fullname[i], e, g, y))
  hist <- ec[gvkey == g, .(year, execid)][order(year)]
  cat("ExecuComp CEO history for this gvkey:\n")
  print(hist)
  cat("Was this execid CEO at year", y-1, "?", 
      any(ec$gvkey == g & ec$year == (y-1) & ec$execid == e), "\n")
}

# How many reg_data execids are NOT found in execucomp at fyear_lead - 1?
reg_data[, execid := as.character(execid)]
ec_keys <- paste(ec$gvkey, ec$year, ec$execid, sep="_")
reg_data[, prior_key := paste(gvkey, fyear_lead - 1, execid, sep="_")]
cat("\n\nExecid found at fyear_lead-1:", sum(reg_data$prior_key %in% ec_keys), "of", nrow(reg_data), "\n")
cat("NOT found:", sum(!reg_data$prior_key %in% ec_keys), "\n")

# Of those not found, how many have NO gvkey data at fyear_lead-1?
gvkey_yr_keys <- paste(ec$gvkey, ec$year, sep="_")
reg_data[, gvkey_prior := paste(gvkey, fyear_lead - 1, sep="_")]
not_found <- reg_data[!prior_key %in% ec_keys]
cat("  Of those not found:\n")
cat("  - No ExecuComp data at all for prior yr:", sum(!not_found$gvkey_prior %in% gvkey_yr_keys), "\n")
cat("  - ExecuComp exists but DIFFERENT CEO:", sum(not_found$gvkey_prior %in% gvkey_yr_keys), "\n")

# Those are the actual turnovers!
turnovers <- not_found[gvkey_prior %in% gvkey_yr_keys]
cat("\nActual turnovers (sample):\n")
print(turnovers[1:min(5, nrow(turnovers)), .(gvkey, fyear_lead, execid, exec_fullname)])

reg_data[, c("prior_key", "gvkey_prior") := NULL]

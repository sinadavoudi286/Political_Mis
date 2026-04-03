library(data.table)
load("reg_data_v2.rdata")

# Redefine: first observation of this CEO at this firm in the panel
setorder(reg_data, gvkey, execid, cycle)
reg_data[, first_year_ceo := as.integer(cycle == min(cycle)), by = .(gvkey, execid)]

cat("First year CEO (first panel appearance):\n")
print(table(reg_data$first_year_ceo, useNA="ifany"))

# Print final confirmation
cat("\n=== FINAL CONFIRMATION ===\n")
cat("Dimensions:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n")
cat("gvkey+fyear_lead dupes:", sum(duplicated(reg_data[, .(gvkey, fyear_lead)])), "\n")
cat("\nAll new variables:\n")
new_cols <- c("cross_ceo_right","cross_ceo_left","same_side_right","same_side_left",
              "ceo_abs_conservative","log_n_donors","sic2","ceo_turnover",
              "first_year_ceo","donor_coverage","misalign_abs_L2","quadrant")
for (col in new_cols) {
  if (col %in% names(reg_data)) {
    if (is.numeric(reg_data[[col]])) {
      cat(sprintf("  %-22s  non-NA: %d  mean: %.3f\n", col, sum(!is.na(reg_data[[col]])), 
          mean(reg_data[[col]], na.rm=TRUE)))
    } else {
      cat(sprintf("  %-22s  non-NA: %d\n", col, sum(!is.na(reg_data[[col]]))))
    }
  }
}

save(reg_data, file = "reg_data_v2.rdata")
cat("\nSaved reg_data_v2.rdata\n")

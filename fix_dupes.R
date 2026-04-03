library(data.table)
load("reg_data_v2.rdata")
cat("Loaded:", nrow(reg_data), "rows x", ncol(reg_data), "cols\n\n")

# 1. Check duplicates
reg_data[, n := .N, by = .(gvkey, fyear_lead)]
dupes <- reg_data[n > 1]
cat("Duplicate gvkey-fyear pairs:", uniqueN(dupes[, .(gvkey, fyear_lead)]), "\n")
cat("Extra rows from dupes:", nrow(dupes) - uniqueN(dupes[, .(gvkey, fyear_lead)]), "\n\n")

cat("Examples of duplicates:\n")
example_keys <- unique(dupes[, .(gvkey, fyear_lead)])[1:5]
for (i in 1:nrow(example_keys)) {
  g <- example_keys$gvkey[i]; y <- example_keys$fyear_lead[i]
  cat(sprintf("\n--- gvkey=%s, fyear_lead=%d ---\n", g, y))
  print(reg_data[gvkey == g & fyear_lead == y, .(gvkey, fyear_lead, cycle, execid, exec_fullname, ceo_cfscore, ceo_turnover, first_year_ceo)])
}
reg_data[, n := NULL]

# 2. Deduplicate
# Check if ceo_tenure exists
cat("\n\nTenure-related cols:", grep("tenure|becam", names(reg_data), value=TRUE, ignore.case=TRUE), "\n")

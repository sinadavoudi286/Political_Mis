library(data.table)

# 1. Read first 100K rows
dt <- fread("contribDB_2010.csv", nrows = 100000,
            select = c("bonica.cid","contributor.type","contributor.employer",
                        "contributor.cfscore","date","cycle"))

cat("=== FIRST LOOK ===\n")
cat("Rows:", nrow(dt), " Cols:", ncol(dt), "\n")
cat("Column types:\n"); str(dt)
cat("\nSample dates:\n"); print(head(dt$date, 20))
cat("\nCycle values:\n"); print(table(dt$cycle))

# 2. Parse dates
# Try to detect format
dt[, date_parsed := as.Date(date, format = "%m/%d/%Y")]
if (all(is.na(dt$date_parsed))) {
  dt[, date_parsed := as.Date(date, format = "%Y-%m-%d")]
}
if (sum(is.na(dt$date_parsed)) > nrow(dt) * 0.5) {
  cat("\nWARNING: Many dates failed to parse. Trying other formats...\n")
  dt[is.na(date_parsed), date_parsed := as.Date(date, format = "%m-%d-%Y")]
}

cat("\nDate parse success:", sum(!is.na(dt$date_parsed)), "of", nrow(dt), "\n")
dt[, year := year(date_parsed)]

cat("\n=== DONATION YEARS WITHIN CYCLE 2010 FILE ===\n")
print(table(dt$year, useNA = "ifany"))

cat("\nDate range:", as.character(min(dt$date_parsed, na.rm=TRUE)), "to", 
    as.character(max(dt$date_parsed, na.rm=TRUE)), "\n")

# Monthly distribution
dt[, ym := format(date_parsed, "%Y-%m")]
cat("\nMonthly distribution:\n")
print(dt[!is.na(ym), .N, by = ym][order(ym)])

# 3. Unique individual donors per calendar year
indiv <- dt[contributor.type == "I" & !is.na(year)]
cat("\n=== UNIQUE INDIVIDUAL DONORS PER YEAR ===\n")
cat("(Within first 100K rows of cycle 2010 file)\n")
print(indiv[, .(unique_donors = uniqueN(bonica.cid)), by = year][order(year)])

# 4. Known large employers
indiv[, emp_upper := toupper(trimws(contributor.employer))]
targets <- c("MICROSOFT", "GOOGLE", "APPLE", "GOLDMAN SACHS")

cat("\n=== DONORS PER YEAR FOR LARGE EMPLOYERS ===\n")
cat("(Exact substring match in first 100K rows)\n\n")
for (tgt in targets) {
  sub <- indiv[grepl(tgt, emp_upper, fixed = TRUE)]
  if (nrow(sub) > 0) {
    tab <- sub[, .(donors = uniqueN(bonica.cid), contributions = .N), by = year][order(year)]
    cat(tgt, ":\n"); print(tab); cat("\n")
  } else {
    cat(tgt, ": 0 matches in sample\n\n")
  }
}

# Bonus: what fraction of the file is this?
finfo <- file.info("contribDB_2010.csv")
cat("File size:", round(finfo$size / 1e9, 2), "GB\n")
total_est <- as.integer(finfo$size / (object.size(dt) / nrow(dt)))
cat("Estimated total rows (rough):", format(total_est, big.mark=","), "\n")

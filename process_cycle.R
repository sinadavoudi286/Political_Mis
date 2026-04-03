## ============================================================
## process_cycle.R — Reusable employee-side panel builder
## Usage: Rscript process_cycle.R ContribDB_2022.csv.gz
##        Rscript process_cycle.R contribDB_2024.csv
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

## ---- Get filename from command line ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: Rscript process_cycle.R <ContribDB_YYYY.csv[.gz]>")
filename <- args[1]
if (!file.exists(filename)) stop(paste("File not found:", filename))

cat("=== Processing:", filename, "===\n")

## ---- Auto-detect format ----
is_gz <- grepl("\\.gz$", filename)

## ---- Load employer lookup ----
load("employer_lookup_for_panel.rdata")  # all_exact
setDT(all_exact)
cat("Employer lookup:", nrow(all_exact), "rows\n")

## ---- Read first 5 rows to confirm column names ----
if (is_gz) {
  peek <- fread(cmd = paste("gzcat", shQuote(filename)), nrows = 5)
} else {
  peek <- fread(filename, nrows = 5)
}
cat("Columns:", paste(names(peek), collapse = ", "), "\n")

## ---- Identify needed columns ----
cols_needed <- c("cycle", "bonica.cid", "contributor.type",
                 "contributor.employer", "contributor.cfscore")
# Check all exist
missing_cols <- setdiff(cols_needed, names(peek))
if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
rm(peek); gc()

## ---- Read full file ----
cat("Reading full file (select columns)...\n")
t0 <- proc.time()
if (is_gz) {
  dt <- fread(cmd = paste("gzcat", shQuote(filename)), select = cols_needed, showProgress = TRUE)
} else {
  dt <- fread(filename, select = cols_needed, showProgress = TRUE)
}
cat("Read", format(nrow(dt), big.mark = ","), "rows in",
    round((proc.time() - t0)[3], 1), "seconds\n")

## ---- Extract cycle ----
cycle_val <- dt[1, cycle]
cat("Cycle:", cycle_val, "\n")

## ---- Filter to individuals with valid cfscore and employer ----
dt <- dt[contributor.type == "I"]
cat("Individuals:", format(nrow(dt), big.mark = ","), "\n")

dt[, contributor.cfscore := as.numeric(contributor.cfscore)]
dt <- dt[!is.na(contributor.cfscore)]
cat("With cfscore:", format(nrow(dt), big.mark = ","), "\n")

dt <- dt[!is.na(contributor.employer) & contributor.employer != ""]
cat("With employer:", format(nrow(dt), big.mark = ","), "\n")

## ---- Clean employer names (same as lookup) ----
cat("Cleaning employer names...\n")
dt[, employer_clean := toupper(trimws(contributor.employer))]

# Remove suffixes
suffixes <- c("\\s+INC\\.?$", "\\s+INCORPORATED$", "\\s+CORP\\.?$",
              "\\s+CORPORATION$", "\\s+LLC$", "\\s+LLP$", "\\s+LP$",
              "\\s+LTD\\.?$", "\\s+LIMITED$", "\\s+CO\\.?$",
              "\\s+COMPANY$", "\\s+GROUP$", "\\s+HOLDINGS?$",
              "\\s+ENTERPRISES?$", "\\s+INTERNATIONAL$",
              "\\s+& CO\\.?$", "\\s+AND CO\\.?$",
              ",\\s*INC\\.?$", ",\\s*LLC$", ",\\s*LTD\\.?$",
              ",\\s*CORP\\.?$")
for (suf in suffixes) {
  dt[, employer_clean := gsub(suf, "", employer_clean)]
}
dt[, employer_clean := gsub("\\s+", " ", trimws(employer_clean))]

# Remove junk employers
junk <- c("SELF", "SELF-EMPLOYED", "SELF EMPLOYED", "SELFEMPLOYED",
          "RETIRED", "NONE", "N/A", "NA", "NOT EMPLOYED",
          "HOMEMAKER", "HOME MAKER", "STUDENT", "UNEMPLOYED",
          "NOT APPLICABLE", "REFUSED", "INFORMATION REQUESTED",
          "INFORMATION REQUESTED PER BEST EFFORTS")
dt <- dt[!employer_clean %in% junk]
cat("After junk removal:", format(nrow(dt), big.mark = ","), "\n")

## ---- Match to gvkey via employer lookup ----
cat("Matching to gvkeys...\n")
dt_matched <- merge(dt, all_exact[, .(employer_clean, gvkey)],
                    by = "employer_clean")
cat("Matched rows:", format(nrow(dt_matched), big.mark = ","), "\n")
cat("Unique gvkeys:", uniqueN(dt_matched$gvkey), "\n")
cat("Unique donors:", format(uniqueN(dt_matched$bonica.cid), big.mark = ","), "\n")

rm(dt); gc()

## ---- Aggregate per gvkey ----
cat("Aggregating...\n")
emp_panel <- dt_matched[, .(
  emp_mean_cfscore   = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore     = sd(contributor.cfscore, na.rm = TRUE),
  n_emp_donors       = uniqueN(bonica.cid),
  n_contributions    = .N
), by = .(gvkey, cycle)]

rm(dt_matched); gc()

## ---- Save ----
outfile <- paste0("employee_panel_", cycle_val, ".rdata")
save(emp_panel, file = outfile)
cat("\nSaved:", outfile, "\n")

## ---- Summary ----
cat("\n=== Summary for cycle", cycle_val, "===\n")
cat("Firms (gvkey):", nrow(emp_panel), "\n")
cat("\nEmployee mean cfscore:\n"); print(summary(emp_panel$emp_mean_cfscore))
cat("\nDonor count:\n"); print(summary(emp_panel$n_emp_donors))
cat("\nFirms with >= 20 donors:", sum(emp_panel$n_emp_donors >= 20), "\n")
cat("Firms with >= 50 donors:", sum(emp_panel$n_emp_donors >= 50), "\n")
cat("Firms with >= 100 donors:", sum(emp_panel$n_emp_donors >= 100), "\n")
cat("\nDone!\n")

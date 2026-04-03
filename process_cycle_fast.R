## ============================================================
## process_cycle_fast.R — Pre-filter approach for large cycle files
## Usage: Rscript process_cycle_fast.R contribDB_2018.csv
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Usage: Rscript process_cycle_fast.R <contribDB_YYYY.csv>")
filename <- args[1]
if (!file.exists(filename)) stop(paste("File not found:", filename))

cat("=== Processing:", filename, "===\n")
cat("File size:", round(file.size(filename) / 1e9, 1), "GB\n")

## ---- Load employer lookup ----
load("employer_lookup_for_panel.rdata")  # all_exact
setDT(all_exact)
cat("Employer lookup:", nrow(all_exact), "rows\n")

## Suffix-cleaning function (same as lookup)
clean_employer <- function(x) {
  suffixes <- c("\\s+INC\\.?$", "\\s+INCORPORATED$", "\\s+CORP\\.?$",
                "\\s+CORPORATION$", "\\s+LLC$", "\\s+LLP$", "\\s+LP$",
                "\\s+LTD\\.?$", "\\s+LIMITED$", "\\s+CO\\.?$",
                "\\s+COMPANY$", "\\s+GROUP$", "\\s+HOLDINGS?$",
                "\\s+ENTERPRISES?$", "\\s+INTERNATIONAL$",
                "\\s+& CO\\.?$", "\\s+AND CO\\.?$",
                ",\\s*INC\\.?$", ",\\s*LLC$", ",\\s*LTD\\.?$",
                ",\\s*CORP\\.?$")
  for (suf in suffixes) x <- gsub(suf, "", x)
  gsub("\\s+", " ", trimws(x))
}

## Junk employers to exclude
junk <- c("SELF", "SELF-EMPLOYED", "SELF EMPLOYED", "SELFEMPLOYED",
          "RETIRED", "NONE", "N/A", "NA", "NOT EMPLOYED",
          "HOMEMAKER", "HOME MAKER", "STUDENT", "UNEMPLOYED",
          "NOT APPLICABLE", "REFUSED", "INFORMATION REQUESTED",
          "INFORMATION REQUESTED PER BEST EFFORTS")

## ---- Read needed columns ----
cols_needed <- c("cycle", "bonica.cid", "contributor.type",
                 "contributor.employer", "contributor.cfscore")

cat("Reading file...\n")
t0 <- proc.time()
dt <- fread(filename, select = cols_needed, showProgress = TRUE)
elapsed_read <- round((proc.time() - t0)[3], 1)
cat("Read", format(nrow(dt), big.mark = ","), "rows in", elapsed_read, "seconds\n")

cycle_val <- dt[1, cycle]
cat("Cycle:", cycle_val, "\n")

## ---- Filter to individuals with cfscore and employer ----
dt <- dt[contributor.type == "I"]
cat("Individuals:", format(nrow(dt), big.mark = ","), "\n")

dt[, contributor.cfscore := as.numeric(contributor.cfscore)]
dt <- dt[!is.na(contributor.cfscore)]
cat("With cfscore:", format(nrow(dt), big.mark = ","), "\n")

dt <- dt[!is.na(contributor.employer) & contributor.employer != ""]
cat("With employer:", format(nrow(dt), big.mark = ","), "\n")

## ---- STEP 1: Fast pre-match (toupper + trimws only, NO suffix removal) ----
cat("Step 1: Fast pre-match (toupper + trimws only)...\n")
t1 <- proc.time()
dt[, employer_raw := toupper(trimws(contributor.employer))]

# Remove junk
dt <- dt[!employer_raw %in% junk]
cat("After junk removal:", format(nrow(dt), big.mark = ","), "\n")

# Direct match on raw uppercase name
matched1 <- merge(dt, all_exact[, .(employer_clean, gvkey)],
                  by.x = "employer_raw", by.y = "employer_clean")
cat("Step 1 matched rows:", format(nrow(matched1), big.mark = ","),
    "| gvkeys:", uniqueN(matched1$gvkey),
    "| time:", round((proc.time() - t1)[3], 1), "s\n")

## ---- STEP 2: Suffix-clean ONLY unmatched rows ----
matched_raw <- unique(matched1$employer_raw)
unmatched <- dt[!employer_raw %in% matched_raw]
cat("Unmatched rows for suffix cleaning:", format(nrow(unmatched), big.mark = ","), "\n")

rm(dt); gc()

t2 <- proc.time()
unmatched[, employer_clean := clean_employer(employer_raw)]

matched2 <- merge(unmatched, all_exact[, .(employer_clean, gvkey)],
                  by = "employer_clean")
cat("Step 2 matched rows:", format(nrow(matched2), big.mark = ","),
    "| new gvkeys:", uniqueN(matched2$gvkey),
    "| time:", round((proc.time() - t2)[3], 1), "s\n")

rm(unmatched); gc()

## ---- Combine matches ----
# Standardize columns
matched1[, employer_clean := employer_raw]
combined <- rbindlist(list(
  matched1[, .(cycle, bonica.cid, contributor.cfscore, gvkey)],
  matched2[, .(cycle, bonica.cid, contributor.cfscore, gvkey)]
))

rm(matched1, matched2); gc()

cat("Total matched rows:", format(nrow(combined), big.mark = ","), "\n")
cat("Unique gvkeys:", uniqueN(combined$gvkey), "\n")
cat("Unique donors:", format(uniqueN(combined$bonica.cid), big.mark = ","), "\n")

## ---- Aggregate per gvkey ----
cat("Aggregating...\n")
emp_panel <- combined[, .(
  emp_mean_cfscore   = mean(contributor.cfscore, na.rm = TRUE),
  emp_median_cfscore = median(contributor.cfscore, na.rm = TRUE),
  emp_sd_cfscore     = sd(contributor.cfscore, na.rm = TRUE),
  n_emp_donors       = uniqueN(bonica.cid),
  n_contributions    = .N
), by = .(gvkey, cycle)]

rm(combined); gc()

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

total_time <- round((proc.time() - t0)[3], 1)
cat("\nTotal time:", total_time, "seconds\n")
cat("Done!\n")

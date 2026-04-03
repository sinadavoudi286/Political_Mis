## ============================================================
## stack_and_merge.R — Stack all employee panels + merge CEO data
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

cat("=== STEP 1: Stack all employee panels ===\n")
files <- sort(list.files(pattern = "^employee_panel_20[0-9]{2}\\.rdata$"))
cat("Found", length(files), "panel files:", paste(files, collapse = ", "), "\n")

panels <- list()
for (f in files) {
  env <- new.env()
  load(f, envir = env)
  obj_name <- ls(envir = env)[1]
  panels[[f]] <- get(obj_name, envir = env)
}
panel <- rbindlist(panels, fill = TRUE)
rm(panels); gc()

cat("Stacked panel:", format(nrow(panel), big.mark = ","), "firm-cycles\n")
cat("Unique firms:", uniqueN(panel$gvkey), "\n")
cat("Cycles:", paste(sort(unique(panel$cycle)), collapse = ", "), "\n")
cat("Firm-cycles per cycle:\n")
print(panel[, .N, by = cycle][order(cycle)])

## ============================================================
cat("\n=== STEP 2: Merge CEO identity from ExecuComp ===\n")
load("execucomp_ceos.rdata")
setDT(execucomp_ceos)
ceo_slim <- unique(execucomp_ceos[, .(gvkey, year, execid, exec_fullname)])

# Primary merge: cycle == year
panel <- merge(panel, ceo_slim,
               by.x = c("gvkey", "cycle"),
               by.y = c("gvkey", "year"),
               all.x = TRUE)

n_ceo_direct <- sum(!is.na(panel$execid))
cat("CEO identified (direct year match):", format(n_ceo_direct, big.mark = ","),
    "of", format(nrow(panel), big.mark = ","), "\n")

# Fallback: try year-1
missing_idx <- which(is.na(panel$execid))
cat("Missing CEO:", format(length(missing_idx), big.mark = ","), "— trying year-1 fallback\n")

if (length(missing_idx) > 0) {
  missing_dt <- panel[missing_idx, .(gvkey, cycle, row_idx = missing_idx)]
  missing_dt[, year_m1 := cycle - 1L]

  fallback <- merge(missing_dt, ceo_slim,
                    by.x = c("gvkey", "year_m1"),
                    by.y = c("gvkey", "year"),
                    all.x = TRUE)
  fallback <- fallback[!is.na(execid)]
  # Deduplicate: one CEO per firm-cycle
  fallback <- unique(fallback, by = "row_idx")

  if (nrow(fallback) > 0) {
    panel[fallback$row_idx, `:=`(execid = fallback$execid,
                                  exec_fullname = fallback$exec_fullname)]
    cat("Fallback matched:", format(nrow(fallback), big.mark = ","), "\n")
  }
}

n_ceo_total <- sum(!is.na(panel$execid))
cat("Total CEO identified:", format(n_ceo_total, big.mark = ","),
    "of", format(nrow(panel), big.mark = ","),
    "(", round(100 * n_ceo_total / nrow(panel), 1), "%)\n")

## ============================================================
cat("\n=== STEP 3: Merge CEO CFscore from DIME lookup ===\n")
load("ceo_dime_lookup.rdata")
setDT(ceo_dime_lookup)

cat("CEO lookup columns:", paste(names(ceo_dime_lookup), collapse = ", "), "\n")
cat("CEO lookup rows:", nrow(ceo_dime_lookup), "\n")

# Prepare lookup: one row per execid with CFscore
ceo_cf <- unique(ceo_dime_lookup[, .(execid, ceo_cfscore)],
                 by = "execid")
ceo_cf <- ceo_cf[!is.na(ceo_cfscore)]
cat("Unique execids with CFscore:", nrow(ceo_cf), "\n")

panel <- merge(panel, ceo_cf, by = "execid", all.x = TRUE)

n_cfscore <- sum(!is.na(panel$ceo_cfscore))
cat("Firm-cycles with CEO CFscore:", format(n_cfscore, big.mark = ","),
    "of", format(nrow(panel), big.mark = ","),
    "(", round(100 * n_cfscore / nrow(panel), 1), "%)\n")

## ============================================================
cat("\n=== STEP 4: Compute misalignment ===\n")
panel[, misalign_abs := abs(ceo_cfscore - emp_mean_cfscore)]
panel[, misalign_signed := ceo_cfscore - emp_mean_cfscore]

n_misalign <- sum(!is.na(panel$misalign_abs))
cat("Firm-cycles with computable misalignment:", format(n_misalign, big.mark = ","), "\n")

## ============================================================
cat("\n=== STEP 5: Add company names ===\n")
load("employer_lookup_for_panel.rdata")
setDT(all_exact)

# Get unique gvkey → conm mapping
firm_names <- unique(all_exact[, .(gvkey, conm)])
# If multiple names per gvkey, take first
firm_names <- unique(firm_names, by = "gvkey")
cat("Firm name lookup:", nrow(firm_names), "firms\n")

panel <- merge(panel, firm_names, by = "gvkey", all.x = TRUE)
cat("Firms with names:", sum(!is.na(panel$conm)), "of", nrow(panel), "\n")

## ============================================================
cat("\n=== STEP 6: Quality filter and diagnostics ===\n")

panel_complete <- panel[!is.na(misalign_abs)]
panel_qual <- panel_complete[n_emp_donors >= 20]

cat("\n--- Pipeline funnel ---\n")
cat("Total firm-cycles (stacked):", format(nrow(panel), big.mark = ","), "\n")
cat("CEO identified (ExecuComp):", format(n_ceo_total, big.mark = ","),
    "(", round(100 * n_ceo_total / nrow(panel), 1), "%)\n")
cat("CEO CFscore (DIME match):", format(n_cfscore, big.mark = ","),
    "(", round(100 * n_cfscore / nrow(panel), 1), "%)\n")
cat("Computable misalignment:", format(n_misalign, big.mark = ","),
    "(", round(100 * n_misalign / nrow(panel), 1), "%)\n")
cat("Quality filtered (>=20 donors):", format(nrow(panel_qual), big.mark = ","),
    "(", round(100 * nrow(panel_qual) / nrow(panel), 1), "%)\n")
cat("Unique firms (quality):", uniqueN(panel_qual$gvkey), "\n")

cat("\n--- Mean |misalignment| by cycle ---\n")
print(panel_complete[, .(
  n = .N,
  mean_abs_misalign = round(mean(misalign_abs), 3),
  median_abs_misalign = round(median(misalign_abs), 3),
  pct_ceo_more_conservative = round(100 * mean(misalign_signed > 0), 1)
), by = cycle][order(cycle)])

cat("\n--- Overall summary stats (panel_complete) ---\n")
cat("|misalignment|:\n"); print(summary(panel_complete$misalign_abs))
cat("\nSigned misalignment (CEO - Employee):\n"); print(summary(panel_complete$misalign_signed))
cat("\nCEO CFscore:\n"); print(summary(panel_complete$ceo_cfscore))
cat("\nEmployee mean CFscore:\n"); print(summary(panel_complete$emp_mean_cfscore))
cat("\nDonor count:\n"); print(summary(panel_complete$n_emp_donors))

cat("\n--- Quality filtered summary (>=20 donors) ---\n")
cat("|misalignment|:\n"); print(summary(panel_qual$misalign_abs))
cat("\nSigned misalignment:\n"); print(summary(panel_qual$misalign_signed))

cat("\n--- Top 20 firms by number of cycles ---\n")
top_firms <- panel_complete[, .(n_cycles = uniqueN(cycle),
                                 mean_misalign = round(mean(misalign_abs), 3),
                                 mean_donors = round(mean(n_emp_donors))),
                             by = .(gvkey, conm)][order(-n_cycles, -mean_donors)]
print(head(top_firms, 20))

cat("\n--- Firms appearing in all 8 cycles ---\n")
n_cycles_total <- uniqueN(panel_complete$cycle)
all_cycle_firms <- panel_complete[, .(n_cycles = uniqueN(cycle)), by = .(gvkey, conm)][n_cycles == n_cycles_total]
cat("Firms in all", n_cycles_total, "cycles:", nrow(all_cycle_firms), "\n")
if (nrow(all_cycle_firms) > 0) {
  print(all_cycle_firms[order(conm)])
}

## ============================================================
cat("\n=== Saving files ===\n")
save(panel, file = "panel_full.rdata")
cat("Saved: panel_full.rdata (", format(nrow(panel), big.mark = ","), "rows)\n")

save(panel_complete, file = "panel_complete.rdata")
cat("Saved: panel_complete.rdata (", format(nrow(panel_complete), big.mark = ","), "rows)\n")

save(panel_qual, file = "panel_qual.rdata")
cat("Saved: panel_qual.rdata (", format(nrow(panel_qual), big.mark = ","), "rows)\n")

cat("\nDone!\n")

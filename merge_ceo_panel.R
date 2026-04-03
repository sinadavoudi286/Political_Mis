library(data.table)
setwd("/Users/myoffice/Desktop")

# Load all pieces
load('employee_panel_2024.rdata')  # emp_panel
ep_2024 <- copy(emp_panel)
load('employee_panel_2022.rdata')  # emp_panel
ep_2022 <- copy(emp_panel)
rm(emp_panel)
load('execucomp_ceos.rdata')
load('ceo_dime_lookup.rdata')

# Stack employee panels
panel <- rbindlist(list(ep_2024, ep_2022), fill = TRUE)
cat("Stacked panel:", nrow(panel), "firm-cycles\n")
cat("  2024:", nrow(ep_2024), "  2022:", nrow(ep_2022), "\n")
cat("Unique gvkeys:", uniqueN(panel$gvkey), "\n\n")
rm(ep_2024, ep_2022); gc()

# Step 1: Add CEO identity from ExecuComp
ceo_slim <- execucomp_ceos[, .(gvkey, year, execid, exec_fullname)]
ceo_slim[, execid := as.character(execid)]

panel <- merge(panel, ceo_slim,
               by.x = c('gvkey', 'cycle'),
               by.y = c('gvkey', 'year'),
               all.x = TRUE)

cat("After ExecuComp merge — CEO identified:", sum(!is.na(panel$execid)),
    "of", nrow(panel), "\n")

# For rows where cycle year had no CEO, try year-1
n_missing_before <- sum(is.na(panel$execid))
cat("Missing CEO:", n_missing_before, "\n")

if (n_missing_before > 0) {
  missing_idx <- which(is.na(panel$execid))
  missing_gvkeys <- panel[missing_idx, .(gvkey, cycle, year_minus1 = cycle - 1L)]

  fallback <- merge(missing_gvkeys, ceo_slim,
                    by.x = c('gvkey', 'year_minus1'),
                    by.y = c('gvkey', 'year'),
                    all.x = TRUE)
  # Deduplicate: if multiple CEOs in year-1 for same gvkey, take first
  fallback <- fallback[!duplicated(paste(gvkey, cycle))]

  # Update panel rows that got a fallback match
  fb_matched <- fallback[!is.na(execid)]
  cat("Fallback (year-1) recovered:", nrow(fb_matched), "CEO matches\n")

  if (nrow(fb_matched) > 0) {
    # Create key for lookup
    panel[, row_key := paste(gvkey, cycle)]
    fb_matched[, row_key := paste(gvkey, cycle)]
    panel[row_key %in% fb_matched$row_key & is.na(execid),
          `:=`(execid = fb_matched$execid[match(row_key[is.na(execid)],
                                                 fb_matched$row_key)],
               exec_fullname = fb_matched$exec_fullname[match(row_key[is.na(execid)],
                                                               fb_matched$row_key)])]
    # Simpler approach: just do a second merge
    # Actually let me just do it cleanly:
    panel[, row_key := NULL]
  }
}

cat("After fallback — CEO identified:", sum(!is.na(panel$execid)),
    "of", nrow(panel), "\n\n")

# Step 2: Add CEO CFscore from DIME lookup
ceo_dime_lookup[, execid := as.character(execid)]
panel <- merge(panel, ceo_dime_lookup[, .(execid, ceo_cfscore)],
               by = 'execid', all.x = TRUE)

cat("CEO CFscore available:", sum(!is.na(panel$ceo_cfscore)),
    "of", nrow(panel), "\n")

# Breakdown by cycle
cat("\nBy cycle:\n")
print(panel[, .(total = .N,
                has_ceo = sum(!is.na(execid)),
                has_cfscore = sum(!is.na(ceo_cfscore))),
            by = cycle])

# Step 3: Compute misalignment
panel[, misalign_abs := abs(ceo_cfscore - emp_mean_cfscore)]
panel[, misalign_signed := ceo_cfscore - emp_mean_cfscore]

# Step 4: Summary
panel_complete <- panel[!is.na(misalign_abs)]
cat('\nFirm-cycles with misalignment:', nrow(panel_complete), '\n')
cat('Unique firms:', uniqueN(panel_complete$gvkey), '\n')
cat('Mean |misalignment|:', round(mean(panel_complete$misalign_abs), 3), '\n')
cat('Median |misalignment|:', round(median(panel_complete$misalign_abs), 3), '\n')
cat('CEO more conservative:',
    round(100 * mean(panel_complete$misalign_signed > 0), 1), '%\n')

cat("\nBy cycle:\n")
print(panel_complete[, .(n = .N,
                         n_firms = uniqueN(gvkey),
                         mean_abs_mis = round(mean(misalign_abs), 3),
                         mean_signed = round(mean(misalign_signed), 3),
                         pct_ceo_conserv = round(100 * mean(misalign_signed > 0), 1)),
                     by = cycle])

# Quality filtered
panel_qual <- panel_complete[n_emp_donors >= 20]
cat('\nQuality filtered (>=20 donors):\n')
cat('Firm-cycles:', nrow(panel_qual), '\n')
cat('Unique firms:', uniqueN(panel_qual$gvkey), '\n')
cat('Mean |misalignment|:', round(mean(panel_qual$misalign_abs), 3), '\n')
cat('Median |misalignment|:', round(median(panel_qual$misalign_abs), 3), '\n')

cat("\nBy cycle (quality filtered):\n")
print(panel_qual[, .(n = .N,
                     n_firms = uniqueN(gvkey),
                     mean_abs_mis = round(mean(misalign_abs), 3),
                     mean_signed = round(mean(misalign_signed), 3),
                     pct_ceo_conserv = round(100 * mean(misalign_signed > 0), 1)),
                 by = cycle])

save(panel, panel_complete, file = 'panel_stacked.rdata')
cat('\nSaved panel_stacked.rdata\n')
cat('Done!\n')

## ============================================================
## STEP 1: Match 584 missing CEOs to DIME by name
## STEP 2: Validate Christensen ×2 rescaling
## ============================================================

library(data.table)
setwd("/Users/myoffice/Desktop")

cat("=== Loading data ===\n")
load("panel_2024_full.rdata")       # panel
load("dime_contribs_slim.rdata")    # contribs
load("execucomp_ceos.rdata")        # execucomp_ceos
christensen <- fread("POLITICALSCORES.csv")

## ============================================================
## Helper: standardize a single name string
## ============================================================
clean_one <- function(x) {
  x <- toupper(trimws(x))
  # Remove suffixes
  x <- gsub(",?\\s*(JR\\.?|SR\\.?|III|II|IV|V|PH\\.?D\\.?|M\\.?D\\.?|J\\.?D\\.?|ESQ\\.?|CPA|CFA)\\s*", "", x)
  x <- gsub(",?\\s*(JR\\.?|SR\\.?|III|II|IV|V)\\s*$", "", x)
  x <- gsub("[.,]", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

## ============================================================
## Parse ExecuComp: "Firstname M. Lastname" -> "LASTNAME FIRSTNAME"
## ============================================================
parse_exec_names <- function(names_vec) {
  out <- character(length(names_vec))
  for (i in seq_along(names_vec)) {
    std <- clean_one(names_vec[i])
    p <- strsplit(std, "\\s+")[[1]]
    if (length(p) == 0) { out[i] <- NA_character_; next }
    surname <- p[length(p)]
    given <- p[1]
    out[i] <- paste(surname, given)
  }
  out
}

## ============================================================
## Parse DIME: "lastname, firstname [middle]" -> "LASTNAME FIRSTNAME"
## ============================================================
parse_dime_names <- function(names_vec) {
  out <- character(length(names_vec))
  for (i in seq_along(names_vec)) {
    std <- clean_one(names_vec[i])
    parts <- strsplit(std, ",\\s*")[[1]]
    if (length(parts) < 2) {
      # No comma — try space split
      p <- strsplit(std, "\\s+")[[1]]
      out[i] <- if (length(p) >= 2) paste(p[1], p[2]) else std
      next
    }
    surname <- trimws(parts[1])
    given_parts <- strsplit(trimws(parts[2]), "\\s+")[[1]]
    given <- given_parts[1]
    out[i] <- paste(surname, given)
  }
  out
}

## ============================================================
## STEP 1: Match missing CEOs to DIME
## ============================================================
cat("\n=== STEP 1: Match 584 missing CEOs to DIME ===\n")

# Remove any prior DIME columns from panel (from failed run)
panel[, c("ceo_cfscore_dime", "ceo_source", "ceo_cfscore_combined",
          "misalign_abs2", "misalign_signed2") := NULL]

missing_ceos <- panel[!is.na(execid) & is.na(Politics),
                      .(gvkey, execid, exec_fullname)]
cat("Missing CEOs:", nrow(missing_ceos), "\n")

missing_ceos[, match_name := parse_exec_names(exec_fullname)]
cat("\nSample parsed ExecuComp names:\n")
print(head(missing_ceos[, .(exec_fullname, match_name)], 10))

# Get DIME CEOs with reliable scores
dime_ceos <- contribs[is_ceo == TRUE & num.distinct >= 2,
                      .(bonica.cid, most.recent.contributor.name,
                        contributor.cfscore, num.distinct)]
cat("\nParsing", nrow(dime_ceos), "DIME CEO names...\n")
dime_ceos[, match_name := parse_dime_names(most.recent.contributor.name)]

cat("Sample parsed DIME names:\n")
print(head(dime_ceos[, .(most.recent.contributor.name, match_name)], 10))

# Deduplicate: keep highest num.distinct per match_name
dime_ceos_best <- dime_ceos[order(-num.distinct)][!duplicated(match_name)]
cat("Unique DIME CEO match names:", nrow(dime_ceos_best), "\n")

# Exact match
matched <- merge(missing_ceos, dime_ceos_best[, .(match_name, bonica.cid,
                  contributor.cfscore, num.distinct)],
                 by = "match_name")
cat("\nExact name matches:", nrow(matched), "\n")

cat("\nSample matches (top by num.distinct):\n")
print(head(matched[order(-num.distinct),
                   .(exec_fullname, match_name, contributor.cfscore,
                     num.distinct)], 25))

# Update panel
panel[, ceo_cfscore_dime := NA_real_]
panel[, ceo_source := fifelse(!is.na(Politics), "christensen", NA_character_)]
panel[, ceo_cfscore_combined := fifelse(!is.na(Politics),
                                        ceo_cfscore_rescaled, NA_real_)]

matched_lookup <- matched[, .(execid, dime_cfscore = contributor.cfscore)]
for (i in seq_len(nrow(matched_lookup))) {
  eid <- matched_lookup$execid[i]
  dcf <- matched_lookup$dime_cfscore[i]
  panel[execid == eid,
        `:=`(ceo_cfscore_dime = dcf,
             ceo_cfscore_combined = dcf,
             ceo_source = "dime")]
}

cat("\n--- CEO ideology coverage after DIME matching ---\n")
tab <- panel[!is.na(execid), .N, by = ceo_source]
print(tab)
cat("Still missing CEO score:", sum(is.na(panel$ceo_source) & !is.na(panel$execid)), "\n")
cat("No ExecuComp CEO:", sum(is.na(panel$execid)), "\n")

# Recompute misalignment
panel[, misalign_abs2 := abs(ceo_cfscore_combined - emp_mean_cfscore)]
panel[, misalign_signed2 := ceo_cfscore_combined - emp_mean_cfscore]

mis2 <- panel[!is.na(misalign_abs2)]
cat("\n--- Updated misalignment stats (all sources) ---\n")
cat("N obs:", nrow(mis2), "\n")
cat("N unique firms:", uniqueN(mis2$gvkey), "\n")
cat("\nAbsolute misalignment:\n")
print(summary(mis2$misalign_abs2))
cat("\nSigned misalignment:\n")
print(summary(mis2$misalign_signed2))
cat("\nCEO more conservative:", sum(mis2$misalign_signed2 > 0), "/",
    nrow(mis2), "(", round(100*mean(mis2$misalign_signed2 > 0),1), "%)\n")

# By source
cat("\nBy source:\n")
print(mis2[, .(n = .N,
               mean_abs = round(mean(misalign_abs2), 3),
               median_abs = round(median(misalign_abs2), 3),
               mean_signed = round(mean(misalign_signed2), 3)),
           by = ceo_source])

# Quality filtered
mis2q <- mis2[n_emp_donors >= 20]
cat("\n--- Quality-filtered (>=20 donors) ---\n")
cat("N obs:", nrow(mis2q), "\n")
cat("\nBy source:\n")
print(mis2q[, .(n = .N,
                mean_abs = round(mean(misalign_abs2), 3),
                median_abs = round(median(misalign_abs2), 3),
                mean_signed = round(mean(misalign_signed2), 3)),
            by = ceo_source])

# Save updated panel
save(panel, file = "panel_2024_full.rdata")
cat("\nPanel saved.\n")

## ============================================================
## STEP 2: Validate Christensen ×2 rescaling
## ============================================================
cat("\n\n========================================\n")
cat("=== STEP 2: Validate Christensen x2 ===\n")
cat("========================================\n\n")

# Get ALL unique ExecuComp CEOs
all_ceos <- unique(execucomp_ceos[, .(execid, exec_fullname)])
all_ceos[, execid := as.character(execid)]
christensen[, EXECID := as.character(EXECID)]

# Merge with Christensen (non-missing Politics only)
all_ceos_chr <- merge(all_ceos, christensen[!is.na(Politics)],
                      by.x = "execid", by.y = "EXECID")
cat("ExecuComp CEOs with Christensen score:", nrow(all_ceos_chr), "\n")

# Parse their names for DIME matching
all_ceos_chr[, match_name := parse_exec_names(exec_fullname)]

# Match to DIME CEOs
both <- merge(all_ceos_chr, dime_ceos_best[, .(match_name, bonica.cid,
              contributor.cfscore, num.distinct)],
              by = "match_name")
cat("CEOs in BOTH Christensen and DIME (any):", nrow(both), "\n")

# Filter to reliable DIME scores
both_reliable <- both[num.distinct >= 4]
cat("With DIME num.distinct >= 4:", nrow(both_reliable), "\n")

if (nrow(both_reliable) == 0) {
  cat("\nNo overlapping CEOs found — trying looser matching...\n")
  # Also try matching just on DIME is_ceo without restricting to dime_ceos_best
  # Try matching all contribs flagged as CEO
  all_dime_ceos <- contribs[is_ceo == TRUE & num.distinct >= 4,
                            .(bonica.cid, most.recent.contributor.name,
                              contributor.cfscore, num.distinct)]
  all_dime_ceos[, match_name := parse_dime_names(most.recent.contributor.name)]
  all_dime_ceos_best <- all_dime_ceos[order(-num.distinct)][!duplicated(match_name)]
  cat("DIME CEOs with num.distinct>=4:", nrow(all_dime_ceos_best), "\n")

  both2 <- merge(all_ceos_chr, all_dime_ceos_best[, .(match_name, bonica.cid,
                 contributor.cfscore, num.distinct)],
                 by = "match_name")
  cat("Match attempt 2:", nrow(both2), "\n")
  both_reliable <- both2[num.distinct >= 4]
  cat("Reliable:", nrow(both_reliable), "\n")
}

if (nrow(both_reliable) > 0) {
  both_reliable[, christensen_rescaled := Politics * 2]

  r_raw <- cor(both_reliable$Politics, both_reliable$contributor.cfscore)
  r <- cor(both_reliable$christensen_rescaled, both_reliable$contributor.cfscore)
  cat("\nCorrelation (Christensen Politics vs DIME CFscore):", round(r_raw, 4), "\n")
  cat("Correlation (Christensen×2 vs DIME CFscore):", round(r, 4),
      "(same — rescaling is linear)\n")

  # Regression
  reg <- lm(contributor.cfscore ~ christensen_rescaled, data = both_reliable)
  cat("\nRegression: DIME_cfscore ~ Christensen×2\n")
  print(summary(reg))

  # Difference stats
  both_reliable[, diff := christensen_rescaled - contributor.cfscore]
  cat("Mean difference (Christensen×2 - DIME):", round(mean(both_reliable$diff), 4), "\n")
  cat("Mean absolute difference:", round(mean(abs(both_reliable$diff)), 4), "\n")
  cat("SD of difference:", round(sd(both_reliable$diff), 4), "\n")

  # Scale summaries
  cat("\nChristensen×2 summary:\n")
  print(summary(both_reliable$christensen_rescaled))
  cat("\nDIME CFscore summary:\n")
  print(summary(both_reliable$contributor.cfscore))

  # Scatter plot
  cat("\n=== Creating scatter plot ===\n")
  pdf("christensen_vs_dime_validation.pdf", width=8, height=7)

  plot(both_reliable$christensen_rescaled, both_reliable$contributor.cfscore,
       xlab = "Christensen Politics × 2",
       ylab = "DIME CFscore",
       main = paste0("CEO Ideology: Christensen vs DIME (N=",
                     nrow(both_reliable), ", r=", round(r, 3), ")"),
       pch = 16, col = rgb(0.2, 0.4, 0.8, 0.4), cex = 0.8,
       xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
  abline(0, 1, col = "red", lwd = 2, lty = 2)
  abline(reg, col = "blue", lwd = 2)
  abline(h = 0, col = "gray70", lty = 3)
  abline(v = 0, col = "gray70", lty = 3)
  legend("bottomright",
         legend = c(paste0("OLS fit (slope=", round(coef(reg)[2], 3),
                           ", intercept=", round(coef(reg)[1], 3), ")"),
                    "45° line (perfect ×2 rescaling)"),
         col = c("blue", "red"), lwd = 2, lty = c(1, 2), cex = 0.85)
  dev.off()
  cat("Saved: christensen_vs_dime_validation.pdf\n")
} else {
  cat("\nWARNING: Could not find overlapping CEOs for validation.\n")
  cat("This may be because DIME is_ceo flag was based on occupation field\n")
  cat("and many ExecuComp CEOs did not self-report as 'CEO' in DIME.\n\n")
  cat("Trying broader match: match ALL ExecuComp-Christensen CEOs\n")
  cat("to ALL DIME individual contributors (not just is_ceo)...\n")

  # Broader: match to any individual contributor in DIME, not just is_ceo
  all_indiv <- contribs[contributor.type == "I" & num.distinct >= 4,
                        .(bonica.cid, most.recent.contributor.name,
                          contributor.cfscore, num.distinct)]
  cat("DIME individuals with num.distinct>=4:", format(nrow(all_indiv), big.mark=","), "\n")
  all_indiv[, match_name := parse_dime_names(most.recent.contributor.name)]
  all_indiv_best <- all_indiv[order(-num.distinct)][!duplicated(match_name)]
  cat("Unique names:", format(nrow(all_indiv_best), big.mark=","), "\n")

  both3 <- merge(all_ceos_chr, all_indiv_best[, .(match_name, bonica.cid,
                 contributor.cfscore, num.distinct)],
                 by = "match_name")
  cat("Matches:", nrow(both3), "\n")

  if (nrow(both3) > 0) {
    both3[, christensen_rescaled := Politics * 2]
    r <- cor(both3$christensen_rescaled, both3$contributor.cfscore)
    cat("\nCorrelation (Christensen×2 vs DIME CFscore):", round(r, 4), "\n")

    reg <- lm(contributor.cfscore ~ christensen_rescaled, data = both3)
    cat("\nRegression: DIME_cfscore ~ Christensen×2\n")
    print(summary(reg))

    both3[, diff := christensen_rescaled - contributor.cfscore]
    cat("Mean difference:", round(mean(both3$diff), 4), "\n")
    cat("Mean |difference|:", round(mean(abs(both3$diff)), 4), "\n")
    cat("SD of difference:", round(sd(both3$diff), 4), "\n")

    pdf("christensen_vs_dime_validation.pdf", width=8, height=7)
    plot(both3$christensen_rescaled, both3$contributor.cfscore,
         xlab = "Christensen Politics × 2",
         ylab = "DIME CFscore",
         main = paste0("CEO Ideology: Christensen vs DIME (N=",
                       nrow(both3), ", r=", round(r, 3), ")"),
         pch = 16, col = rgb(0.2, 0.4, 0.8, 0.4), cex = 0.8,
         xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
    abline(0, 1, col = "red", lwd = 2, lty = 2)
    abline(reg, col = "blue", lwd = 2)
    abline(h = 0, col = "gray70", lty = 3)
    abline(v = 0, col = "gray70", lty = 3)
    legend("bottomright",
           legend = c(paste0("OLS (slope=", round(coef(reg)[2], 3),
                             ", int=", round(coef(reg)[1], 3), ")"),
                      "45° line"),
           col = c("blue", "red"), lwd = 2, lty = c(1, 2), cex = 0.85)
    dev.off()
    cat("Saved: christensen_vs_dime_validation.pdf\n")
  } else {
    cat("Still no matches. Name parsing may need manual review.\n")
  }
}

cat("\nDone!\n")

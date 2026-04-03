> # CEO-Employee Political Misalignment: Project Context for Claude Code
>
> # ================================
>
> ## Last updated: March 28, 2026
>
> # ================================
>
> ## PROJECT DIRECTORY
>
> /Users/homemac/Library/Mobile Documents/com~apple~CloudDocs/My Research/Accounting Research/Political_Accounting/Political Misalignment/Codes
>
> ## RESEARCHER
>
> -   Sina Davoudi, PhD student, Leeds School of Business, University of Colorado Boulder
> -   Works in R (data.table), has access to WRDS
> -   Research: CEO-employee political ideology misalignment → real corporate outcomes
>
> ## CURRENT STATUS: DATA COMPLETE, TABLES COMPLETE, ALL 13 CYCLES (2000-2024)
>
> -   Full 13-cycle panel built and regression-ready
> -   Final regression dataset: reg_data_v3.rdata (4,269 firm-cycles, 1,031 unique firms, 154 columns)
> -   7 paper tables produced (Tables 1-8 in PM_full.Rmd)
> -   All bug fixes integrated (deduplication, CEO turnover, first-year CEO)
>
> # ================================
>
> # FILE INVENTORY
>
> # ================================

### Rmd Files (the three master notebooks — this is where all code lives now)

# 1. PM_full.Rmd ← FULL replication: Sections 0-8 (data construction eval=FALSE + analysis eval=TRUE)

Sections 0-6: data pipeline documentation (eval=FALSE)

Section 7: loads reg_data_v3.rdata (eval=TRUE)

Section 8: all 7 paper tables (eval=TRUE)

# 2. PM_Data_Cleaning.Rmd ← Data construction only: Sections 0-6 (all eval=FALSE)

Cycle processing, WRDS downloads, employer lookup,

CEO matching, panel assembly, regression data construction

# 3. PM_Measure_Report.Rmd ← Variable construction report with descriptive figures

Misalignment measure description, time-series patterns,

cross-sectional patterns, asymmetry, data quality

Core lookup files (do not delete)

# 4. PROJECT_CONTEXT.md ← this file

# 5. employer_lookup_for_panel.rdata ← 3,862 employer→gvkey pairs (all_exact)

# 6. execucomp_ceos.rdata ← 50,586 CEO-year obs from ExecuComp

# 7. ceo_dime_lookup.rdata ← 4,157 execid→bonica.cid→ceo_cfscore

# 8. compustat_outcomes.rdata ← 238,347 firm-years, 105 columns

Employee panels (one per cycle, 13 files)

# 9. employee_panel_2000.rdata through employee_panel_2024.rdata

Assembled panel and regression data

# 10. panel_full.rdata ← all firm-cycles stacked (all 13 cycles)

# 11. panel_complete.rdata ← firm-cycles with non-missing misalignment

# 12. panel_qual.rdata ← quality filtered (≥20 employee donors)

# 13. reg_data_v3.rdata ← FINAL regression dataset (4,269 rows × 154 cols)

Includes all fixes: dedup, turnover, first_year_ceo,

industry downturn measures, WARN match data

Saved model objects (.rds)

# 14. models_core.rds ← M1-M11 from run_core_final.R

# 15. recession_robustness.rds ← R1-R9 leave-one-recession-out

# 16. industry_downturn_tests.rds ← I1-I5 industry downturn models

# 17. peer_layoff_deep.rds ← P1-P7 peer layoff models

# 18. robustness_additional.rds ← D0-D10 donor threshold, weighted, two-part

# 19. mechanism_tests.rds ← B1-B3, C1-C2 mechanism tests

# 20. placebo_results.rds ← 500-iteration placebo shuffles

Legacy R scripts (kept for reference — code now lives in the 3 Rmd files)

# process_cycle.R, process_cycle_fast.R, process_2020.R, process_contribDB_2024.R

# build_full_panel.R, build_panel_2024.R, build_reg_data_v2.R

# ceo_dime_match_v2.R, ceo_dime_match_and_validate.R

# stack_and_merge.R, merge_ceo_panel.R, merge_and_regress.R

# fix_dupes.R, fix_dupes2.R, fix_dupes3.R, fix_dupes4.R

# fix_turnover.R, fix_turnover2.R, fix_firstyear.R, fix_wald.R

# run_core_models.R, run_core_final.R, run_recession_models.R

# run_recession_robust.R, run_industry_downturn.R, run_peer_layoff.R

# run_addl_robust.R, run_placebo.R, run_mechanism.R

# run_all_tables.R, run_paper_tables.R, print_all_tables.R

# tables_final.Rmd, tables_final_v2.Rmd, tables_paper.Rmd

## Other files

# POLITICALSCORES.csv ← Christensen scores (BACKUP ONLY — not used)

# Misc.R ← Miscellaneous code snippets

# WARN Database Master.csv ← WARN Act layoff data (for validation)

# warn_match_lookup.rdata ← WARN-to-gvkey matching lookup

# =================================

# KEY DESIGN DECISIONS (settled)

# =================================

## CEO ideology source: DIME CFscores via name matching (NOT Christensen)

-   We tested Christensen's executive political orientation scores (-1 to +1 scale)
-   Validation showed the ×2 rescaling to CFscore range is INVALID:
    -   Correlation with DIME CFscores: r = 0.47
    -   OLS slope = 0.257 (should be \~1.0 if rescaling worked)
    -   Mean absolute error = 1.15 CFscore units
-   DECISION: Use DIME CFscores for BOTH CEO and employee ideology
    -   Same scale, same methodology, no rescaling needed
    -   CEO CFscores obtained by matching ExecuComp CEO names to DIME contributor file
    -   Christensen file kept as backup but NOT used in main analysis

## CEO identification: ExecuComp (NOT DIME occupation field)

-   DIME self-reported occupation is noisy and misses many CEOs
-   ExecuComp provides verified CEO identity per gvkey per year
-   CEO-DIME name matching: 4,157 of 8,180 ExecuComp CEOs (50.8%) matched

## Employee ideology: DIME cycle-level files with pre-filter matching

-   Employer names matched to Compustat gvkeys via expanded lookup (3,862 pairs)
-   Pre-filter approach for large files: toupper+trimws first (fast exact match catches \~84%), then suffix removal only on unmatched remainder (avoids gsub on 200M+ rows)

## Panel timing: Biennial (one obs per firm per election cycle)

-   DIME cycles are 2-year windows (e.g., 2012 cycle = donations in 2011-2012)
-   Lagged specification: misalignment at cycle t predicts outcomes at fyear t+1
-   CEO CFscore is TIME-INVARIANT (lifetime DIME estimate) — within-firm variation comes from changes in employee donor composition across cycles

# =====================================================

# DATA CONSTRUCTION PIPELINE (documented in PM_Data_Cleaning.Rmd)

# =====================================================

## Step 1: DIME Contributor File → Employer Lookup (PM_Data_Cleaning.Rmd Section 3)

-   Source: dime_contributors_1979_2024.rdata (44.2M rows)
-   Filtered to: individuals, last_cycle_active ≥ 2000, non-missing cfscore, num.distinct ≥ 2 → 21.8M rows
-   Removed junk employers → 8.66M rows
-   Standardized employer names: uppercase, remove suffixes, collapse whitespace
-   Counted donors per employer, kept those with ≥10 donors
-   Matched to Compustat: 607 verified + 3,216 exact new + 39 manual aliases = 3,862 pairs
-   Manual aliases cover name changes: Google→Alphabet (gvkey 160329), Facebook/Meta (158199), JPMorgan Chase (002968), Goldman Sachs (019249), Bank of America (007647), HP (005606), Merrill Lynch (007267), Lehman Brothers (030128), Walmart (011259), Philip Morris/Altria (008543), Raytheon (008972), RTX (010983), Yahoo (024821)
-   Saved: employer_lookup_for_panel.rdata (contains data.table: all_exact)

## Step 2: CEO Identity from ExecuComp (PM_Data_Cleaning.Rmd Section 2.2)

-   Source: comp_execucomp.anncomp WHERE ceoann = 'CEO' AND year \>= 2000
-   Result: 50,586 CEO-year observations, 3,526 gvkeys, 8,180 execids
-   Saved: execucomp_ceos.rdata

## Step 3: CEO Ideology via DIME Name Matching (PM_Data_Cleaning.Rmd Section 4)

-   Matched ExecuComp exec_fullname to DIME contributor names
-   Name cleaning: uppercase, remove periods/commas, remove suffixes (JR, SR, II, III, IV, V, MD, PHD, MBA, CFA, CPA, ESQ), parse to LASTNAME FIRSTNAME
-   Matching via data.table merge on name key (fast, seconds not hours)
-   Disambiguation: prefer employer match to Compustat firm, then highest num.distinct
-   Result: 4,157 of 8,180 execids matched (50.8%)
-   Saved: ceo_dime_lookup.rdata (columns: execid, bonica.cid, ceo_cfscore, name_key, num_distinct, employer_match, dime_employer)

## Step 4: Processing Cycle-Level Contribution Files (PM_Data_Cleaning.Rmd Section 1)

-   13 cycles processed: 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024
-   Pipeline per cycle:
    1.  fread with select= (only needed columns)
    2.  Filter to individuals with non-missing cfscore
    3.  Pre-filter employer match (fast toupper+trimws, catches \~84%)
    4.  Suffix cleaning on unmatched remainder only
    5.  Compute per-gvkey: emp_mean_cfscore, emp_median_cfscore, emp_sd_cfscore, n_emp_donors
    6.  Save as employee_panel_YYYY.rdata
-   Key pattern: leftward employee shift from -0.22 (2012) to -0.63 (2022)
-   Presidential years (2016, 2020) have highest donor counts

## Step 5: Panel Assembly (PM_Data_Cleaning.Rmd Section 5)

-   Stacked all 13 employee panels
-   Merged CEO identity from ExecuComp (with year-1 fallback for missing matches)
-   Attached CEO CFscore from DIME lookup
-   Computed misalignment: misalign_abs = \|CEO_cfscore - emp_mean_cfscore\|
-   Quality filtered (≥20 employee donors)
-   Saved: panel_full.rdata, panel_complete.rdata, panel_qual.rdata

## Step 6: Compustat Outcomes and Controls (PM_Data_Cleaning.Rmd Section 2.1)

-   Source: comp.funda via WRDS, fyear ≥ 1999, at \> 0, INDL/STD/D/C
-   238,347 firm-years, 24,436 gvkeys, 105 columns
-   Excluded: financials (SIC 6000-6999) and utilities (SIC 4900-4999)
-   Outcome variables:
    -   neg_spi_at = -SPI/AT (PRIMARY DV — positive = more restructuring charges)
    -   emp_change = ΔEMP/EMP (layoff proxy)
    -   layoff = 1 if emp_change \< -0.05
-   Controls (all also in lagged form: lag_varname): size, roa, leverage, tobinq, sale_growth, loss, capx_at, rd_at, acquisition, emp_sale, sga_at, cash_at, oper_margin, asset_growth
-   Fixes applied: sale_growth/asset_growth Inf values → NA, oper_margin capped at -10
-   All continuous variables winsorized at 1%/99%
-   Saved: compustat_outcomes.rdata

## Step 7: Regression Data Construction (PM_Data_Cleaning.Rmd Section 6)

-   Lagged specification: panel_qual[, fyear_lead := cycle + 1]
-   Merged with compustat on gvkey + fyear_lead = gvkey + fyear
-   Excluded financials/utilities
-   BUG FIXES INTEGRATED:
    -   Deduplication (from fix_dupes4.R): one row per gvkey × fyear_lead, preferring non-missing ceo_cfscore
    -   CEO turnover (from fix_turnover2.R): firm-year-level CEO set comparison across consecutive ExecuComp years (not row-level shifts)
    -   First-year CEO (from fix_firstyear.R): first panel appearance of execid at gvkey by cycle order
-   Additional variables constructed:
    -   Quadrant indicators: cross_ceo_right, cross_ceo_left, same_side_right, same_side_left
    -   ceo_abs_conservative, log_n_donors, sic2, donor_coverage
    -   misalign_abs_L2 (prior-cycle misalignment, shift(1) by gvkey)
    -   Industry downturn measures: ind_sale_growth, ind_downturn, ind_layoff_rate
-   FINAL: reg_data_v3.rdata (4,269 rows × 154 cols, 1,031 unique firms, 13 cycles)

# =================================

# REGRESSION RESULTS (13 cycles: 2000-2024)

# =================================

## Main findings (from PM_full.Rmd Section 8, Tables 3-8):

### Table 3: Baseline — Misalignment predicts LESS restructuring

-   Firm + Year FE: misalign_abs coefficient is negative and significant
-   Effect survives adding employee ideology, CEO turnover, and donor count controls

### Table 4: Recession moderation (MAIN RESULT)

-   \|Misalignment\| × Recession interaction is positive and significant
-   During non-recessions: misalignment constrains restructuring (negative coef)
-   During recessions: the constraint loosens (positive interaction offsets base effect)
-   Story: recessions provide political cover for misaligned CEOs to restructure

### Table 5: Donor threshold sensitivity

-   Results stable across 20+, 30+, 50+, 100+ donor thresholds

### Table 6: CEO ideology level test

-   Adding CEO CFscore directly: misalignment effect unchanged, CEO level insignificant
-   It's the GAP that matters, not the CEO's ideology per se

### Table 7: Asymmetry test

-   Conservative CEO misalignment and liberal CEO misalignment decomposed
-   Wald test for differential recession effects

### Table 8: Leave-one-recession-out robustness

-   Sequentially exclude 2001, 2008-09, 2020, 2020-21
-   Results hold across all exclusions

## Additional analyses (in saved .rds model objects):

-   Industry downturn interactions (industry_downturn_tests.rds)
-   Peer layoff rate interactions with marginal effect plot (peer_layoff_deep.rds)
-   Donor threshold sensitivity, weighted regressions, two-part model (robustness_additional.rds)
-   Mechanism tests: labor intensity 3-way, employee cohesion, peer restructuring, cumulative charges (mechanism_tests.rds)
-   Placebo tests: 500-iteration shuffled ind_layoff_rate (placebo_results.rds)

# =====================================================

# EMPLOYER NAME CLEANING PROCEDURE (must be identical for lookup and cycle files)

# =====================================================

``` r
# This EXACT cleaning must be applied to both the employer lookup and
# the ContribDB employer fields for matching to work:
employer_clean <- toupper(trimws(raw_employer))
suffixes <- c("\\s+INC\\.?$", "\\s+INCORPORATED$", "\\s+CORP\\.?$",
              "\\s+CORPORATION$", "\\s+LLC$", "\\s+LLP$", "\\s+LP$",
              "\\s+LTD\\.?$", "\\s+LIMITED$", "\\s+CO\\.?$",
              "\\s+COMPANY$", "\\s+GROUP$", "\\s+HOLDINGS?$",
              "\\s+ENTERPRISES?$", "\\s+INTERNATIONAL$",
              "\\s+& CO\\.?$", "\\s+AND CO\\.?$",
              ",\\s*INC\\.?$", ",\\s*LLC$", ",\\s*LTD\\.?$",
              ",\\s*CORP\\.?$")
for (suf in suffixes) employer_clean <- gsub(suf, "", employer_clean)
employer_clean <- gsub("\\s+", " ", trimws(employer_clean))
```

## PRE-FILTER APPROACH FOR LARGE CYCLE FILES

For files with 50M+ rows, do NOT run gsub on all rows: 1. toupper(trimws(employer)) — fast, no regex 2. Exact match against all_exact\$employer_clean — catches \~84% of matches 3. Run suffix gsub ONLY on unmatched remainder 4. Re-match cleaned remainder 5. Combine

# =====================================================

# TECHNICAL NOTES

# =====================================================

## Memory/RAM constraints

-   Machine: Mac Mini, 64 GB RAM
-   Process one cycle at a time, save result, rm() and gc() before next
-   ContribDB files can be deleted/moved after processing — only employee_panel_YYYY.rdata needed
-   Use fread with select= to read only needed columns

## R packages used

-   data.table (primary data manipulation)
-   fixest (regressions with high-dimensional fixed effects)
-   kableExtra (publication-quality LaTeX tables)
-   stringdist (fuzzy matching — used sparingly in employer lookup)
-   RPostgres (WRDS — Claude Code cannot access WRDS directly)

## Key variable definitions

-   neg_spi_at = -SPI/AT (positive = more restructuring charges)
-   misalign_abs = \|ceo_cfscore - emp_mean_cfscore\|
-   misalign_signed = ceo_cfscore - emp_mean_cfscore (positive = CEO more conservative)
-   ceo_right = max(misalign_signed, 0)
-   ceo_left = max(-misalign_signed, 0)
-   recession = nber_recession (fiscal years 2001, 2008, 2009, 2020)
-   fyear = fyear_lead = cycle + 1 (lagged specification)
-   Controls: lag_size, lag_roa, lag_leverage, lag_tobinq, lag_sale_growth, lag_loss, lag_capx_at, lag_rd_at, lag_aqc_indicator (= lag_acquisition)

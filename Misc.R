# Load the .RData file
load("/Users/homemac/Desktop/compustat_outcomes.rdata")
models_core <- readRDS("/Users/homemac/Desktop/models_core.rds")
# See what objects were loaded
ls()
load("misalignment_compustat_final.rdata")
suspicious <- matched_final[match_distance >= 0.05, 
                            .(employer_clean, conm, match_distance, n_employees)]
print(suspicious[order(-match_distance)], nrow = nrow(suspicious))

# ============================================================
# KEEP: These are correct matches despite fuzzy distance
# ============================================================
good_matches <- c(
  "EXETER FINANCE",           # → Exeter Finance Corp (correct)
  "GROCERY OUTLET",           # → Grocery Outlet Hldng Corp (correct)
  "PATTERN ENERGY",           # → Pattern Energy Group Inc (correct)
  "SIEMENS",                  # → Siemens AG (correct)
  "WASHINGTON GAS",           # → Washington Gas Light Co (correct)
  "BOOZ ALLEN HAMILTON",      # → Booz Allen Hamilton Hldg Cp (correct)
  "AMERICAN TIRE DISTRIBUTORS",# → American Tire Distrbtr Hldgs (correct)
  "ANHEUSER BUSCH COS",       # → Anheuser-Busch Cos Inc (correct)
  "ANHEUSER-BUSCH",           # → Anheuser-Busch Cos Inc (correct)
  "CONTAINER STORE",          # → Container Store Group (correct)
  "CLEARWAY ENERGY GROUP",    # → Clearway Energy Inc (correct)
  "APPLIED MEDICAL",          # → Applied Medical Corp (correct)
  "UNITED GUARANTY",          # → United Guaranty Corp (correct)
  "FIFTH THIRD BANK",         # → Fifth Third Bancorp (correct)
  "CAREMARK",                 # → Caremark Rx Inc (correct)
  "NOVARTIS",                 # → Novartis AG (correct)
  "SAFETY INSURANCE",         # → Safety Insurance Group Inc (correct)
  "RAYMOND JAMES FINANCIAL SERVICES", # → Raymond James Financial Inc (correct)
  "CITY NATIONAL BANK",       # → City National Corp (correct)
  "7 ELEVEN",                 # → 7-Eleven Inc (correct)
  "CRACKER BARREL OLD COUNTRY STORE", # → Cracker Barrel Old Ctry Stor (correct)
  "FIRST FINANCIAL BANK",     # → First Financial Corp (correct)
  "OLLIE'S BARGAIN OUTLET",   # correct variant
  "OLLIES BARGAIN OUTLET",    # → Ollie's Bargain Outlet Hldgs (correct)
  "TENET HEALTH",             # → Tenet Healthcare Corp (correct)
  "UNITED HEALTH GROUP",      # → UnitedHealth Group Inc (correct)
  "CB RICHARD ELLIS",         # → CB Richard Ellis Svcs (correct)
  "COCA COLA",                # → Coca-Cola Co (correct)
  "GENERAL ELECTRIC",         # → General Electric Cap Corp (close enough)
  "OLD DOMINION FREIGHT LINE",# → Old Dominion Freight (correct)
  "PUBLICIS GROUPE",          # → Publicis Groupe SA (correct)
  "UNIVERSAL HEALTH SERVICES", # → Universal Health Svcs Inc (correct)
  "WELLS FARGO",              # → Wells Fargo & Co (correct)
  "AMERICAN EAGLE OUTFITTERS",# → Amern Eagle Outfitters Inc (correct)
  "WISCONSIN ELECTRIC",       # → Wisconsin Electric Power Co (correct)
  "BROCADE COMMUNICATIONS",   # → Brocade Communications Sys (correct)
  "DUKE ENERGY FIELD SERVICES",# → Duke Energy Field Svcs LLC (correct)
  "UNION PLANTERS BANK",      # → Union Planters Corp (correct)
  "OLD NATIONAL BANK",        # → Old National Bancorp (correct)
  "FIRST BANK & TRUST",       # → First Banctrust Corp (plausible)
  "COMMUNITY TRUST BANK",     # → Community Trust Bancorp Inc (correct)
  "FIRST NATIONAL BANK",      # → First National Corp/VA (plausible)
  "INDEPENDENT BANK",         # → Independent Bank Corp/MA (correct)
  "AGL RESOURCES",            # → close enough (was acquired by Southern Co)
  "CONNS",                    # → Conn's Inc (correct)
  "PRINCIPAL FINANCIAL",      # → Principal Financial Grp Inc (correct)
  "HORIZON THERAPEUTICS",     # → Horizon Therapeutics Pub Ltd (correct)
  "LEIDOS",                   # → Eidos PLC (actually WRONG — keep in drop list)
  "CARNIVAL",                 # → Carnival Corporation & PLC (correct)
  "EMCOR",                    # → Emcore Corp (actually different — drop)
  "HEALTH MANAGEMENT ASSOCIATES", # → Health Management Assoc (correct)
  "FOSTER WHEELER",           # → Foster Wheeler AG (correct)
  "BROOKS AUTOMATION",        # → Brooks Automation CDA Inc (correct)
  "AMERICAN BANK",            # → American Bank Inc/PA (plausible)
  "COMMERCIAL BANK",          # → Commercial Bank/NY (plausible)
  "SUMMIT BANK",              # → Summit Bank Corp/GA (plausible)
  "SOUTHERN BANK",            # → Southern Banc Co Inc (plausible)
  "MEDIA NEWS GROUP",         # → MediaNews Group Inc (correct)
  "REYNOLDS & REYNOLDS",      # → Reynolds & Reynolds (correct)
  "LOGITECH",                 # → Logistec Corp (WRONG — drop)
  "ERICSSON",                 # → Erickson Inc (WRONG — drop)
  "ADOBE SYSTEMS",            # → BAE Systems PLC (WRONG — drop)
  "ENDEAVOR"                  # → Endeavor IP Inc (plausible but risky)
)

# ============================================================
# DROP: These are incorrect matches
# ============================================================
drop_matches <- c(
  # Generic single words matched to wrong companies
  "ACCEL", "AGILITY", "BARTECH", "ELITE", "EMPOWER", "EQUINOX",
  "ESKATON", "FRONT", "GARDNER", "GARLAND", "GRIFFIN", "IMATION",
  "INMAR", "INTEGRO", "MARCO", "MAXIM", "MEDCO", "NEXUS",
  "OPTIMUS", "ORION", "SIMPLEX", "SKOOKUM", "SPIREON",
  "AVATAR", "AXON", "CARD", "CATS", "CDCR", "CONVERGE",
  "COSTAR", "EDCO", "EMERUS", "ENVISION", "IMVU",
  "INTEGRIS", "INTERSYSTEMS", "MEDICA", "NAIC",
  "PCSD", "PSSI", "ROBINSON", "SERA", "TOMS", "TSMC", "TSRI",
  "VELOCITY", "VERIFONE", "AMCI", "AMERILIFE",
  "ADNET", "AEGIS", "ASPIRE", "ASTEC", "EXCEL", "GENEX",
  "INTEC", "MASON", "NEWCO", "NEXGEN", "RESTEK", "TELOS",
  "VISTA", "MANA", "PACE", "PASS", "PATH", "SAMS", "SCORE",
  "STAR", "TROON", "TYCO", "IRIS", "CASE", "EDGE",
  "ACES", "ALLY", "AWAY", "EVANS",
  
  # Clearly wrong fuzzy matches
  "CONSOL ENERGY",          # → Loon Energy (wrong)
  "COMPUTER SOLUTIONS",     # → Computer Motion (wrong)
  "DIGNITY HEALTH",         # → Signify Health (wrong)
  "POLARIS INDUSTRIES",     # → Norris Industries (wrong)
  "BASIN ELECTRIC",         # → Harbin Electric (wrong)
  "CYBERONICS",             # → CyberOptics (wrong)
  "FOCUS GROUP",            # → Corus Group (wrong)
  "HEALTH EAST",            # → Health Net (wrong)
  "HELP GROUP",             # → Chell Group (wrong)
  "LCMC HEALTH",            # → CRC Health (wrong)
  "LEONA GROUP",            # → Lenox Group (wrong)
  "MAXIM GROUP",            # → UMAX Group (wrong)
  "SOL SYSTEMS",            # → OSI Systems (wrong)
  "SBC MANAGEMENT SERVICES",# → BPO Management (wrong)
  "CRITICAL MASS",          # → Critical Metals (wrong)
  "TEC EQUIPMENT",          # → Texas Equipment (wrong)
  "ALLIANCE",               # → FX Alliance (different)
  "LS POWER",               # → Altus Power (wrong)
  "HAWORTH",                # → Ashworth (wrong)
  "REGENCE",                # → Mergence (wrong)
  "COX ENTERPRISES",        # → DXP Enterprises (wrong)
  "LIBERTY RESOURCES",      # → Tyler Resources (wrong)
  "XO COMMUNICATIONS",      # → GEOS Communications (wrong)
  "COUNTRY FINANCIAL",      # → Century Financial (wrong)
  "INTERNATIONAL PAPER",    # → International Power (WRONG)
  "UNITED TECHNOLOGIES",    # → Wanted Technologies (WRONG)
  "WALTERS MANAGEMENT",     # → Waste Management (wrong)
  "ROCHE DIAGNOSTICS",      # → Home Diagnostics (wrong)
  "FLEET FINANCIAL GROUP",  # → FG Financial Group (wrong)
  "ADOBE SYSTEMS",          # → BAE Systems (WRONG)
  "LOGITECH",               # → Logistec Corp (WRONG)
  "ERICSSON",               # → Erickson Inc (WRONG)
  "LEIDOS",                 # → Eidos PLC (WRONG)
  "EMCOR",                  # → Emcore Corp (different company)
  "NEXT GENERATION",        # → Next Generation Mgmt (different)
  "PLANET HOLLYWOOD",       # → Planet Hollywood Intl (actually ok but tiny)
  "CHILDREN INTERNATIONAL", # → Shiner International (wrong)
  "ADVENTHEALTH",           # → Ardent Health (wrong)
  "ATRIUS HEALTH",          # → Radius Health (wrong)
  "CENTRAL BANK",           # → Centura Banks (wrong)
  "PHOENIX GROUP",          # → Pernix Group (wrong)
  "TRC COMPANIES",          # → GC Companies (wrong)
  "INTERNET",               # → Interdent (wrong — generic word)
  "LOWE ENTERPRISES",       # → Wellco Enterprises (wrong)
  "DHR INTERNATIONAL",      # → RPM International (wrong)
  "ZEBRA TECHNOLOGIES",     # → Uber Technologies (WRONG)
  "URBAN STRATEGIES",       # → Uranium Strategies (wrong)
  "HONOR HEALTH",           # → Horizon Health (wrong)
  "PRIME THERAPEUTICS",     # → Spyre Therapeutics (wrong)
  "DECISION SYSTEMS",       # → Isilon Systems (wrong)
  "CSG SYSTEMS",            # → Cash Systems (wrong)
  "LINDE GROUP",            # → Alpine Group (wrong)
  "HOLTEC INTERNATIONAL",   # → Ostex International (wrong)
  "ALLEN GROUP",            # → Mullen Group (wrong)
  "BET NETWORKS",           # → FTE Networks (wrong)
  "INVESTORS CAPITAL",      # → plausible but risky
  "NATIONAL EXPRESS",       # → Nations Express (wrong)
  "ALBANY INTERNATIONAL",   # → Kanbay International (wrong)
  "ACCESS INDUSTRIES",      # → CSS Industries (wrong)
  "AVI SYSTEMS",            # → Diva Systems (wrong)
  "BEACON GROUP",           # → Aecon Group (wrong)
  "ICON INTERNATIONAL",     # → Rino International (wrong)
  "M FINANCIAL GROUP",      # → KB Financial Group (wrong)
  "SRI INTERNATIONAL",      # → SRA International (different)
  "CONCENTRIC",             # → Concentrix (different)
  "IGS ENERGY",             # → IAS Energy (wrong)
  "DPS HOLDINGS",           # → GDS Holdings (wrong)
  "CHR SOLUTIONS",          # → Charah Solutions (wrong)
  "COHEN GROUP",            # → Coe Group (wrong)
  "S&T BANK",               # → M&T Bank Corp (WRONG)
  "WHEELS UP",              # → Wheels Group (wrong)
  "VSP GLOBAL",             # → S&P Global (WRONG)
  "UST GLOBAL",             # → HST Global (wrong)
  "INTERSTATE BATTERIES",   # → Interstate Bakeries (WRONG)
  "ALLINA",                 # → Allin Corp (wrong — Allina is a health system)
  "FIRST UNITED BANK",      # → First United Corp (plausible but risky)
  "ENCOMPASS",              # → Compass Inc (wrong)
  "DIVERSANT",              # → Versant Corp (wrong)
  "ST FRANCIS MEDICAL CENTER" # → St Francis Medical Tech (different)
)

# ============================================================
# Apply the drops
# ============================================================
before <- nrow(matched_final)
matched_final <- matched_final[!employer_clean %in% drop_matches]
after <- nrow(matched_final)
cat("Dropped", before - after, "bad fuzzy matches\n")
cat("Remaining firms:", after, "\n")

# Save cleaned version
save(matched_final, file = "misalignment_compustat_final.rdata")
cat("Saved cleaned misalignment_compustat_final.rdata\n")

# Rebuild employer lookup for panel construction
employer_lookup_clean <- matched_final[, .(employer_clean, gvkey, conm)]
save(employer_lookup_clean, file = "employer_lookup_panel.rdata")
cat("Saved cleaned employer_lookup_panel.rdata\n")

cat("\nFinal sample:", nrow(matched_final), "firms,", 
    uniqueN(matched_final$gvkey), "unique gvkeys\n")


load("dime_contributors_1979_2024.rdata")
load("employer_lookup_panel.rdata")
library(data.table)

# -------------------------------------------------------------------
# 1. Check: do we have known name-change pairs?
# -------------------------------------------------------------------
known_changes <- data.table(
  old_name = c("GOOGLE", "FACEBOOK", "PHILIP MORRIS", "RAYTHEON",
               "TIME WARNER", "MONSANTO", "KRAFT FOODS", "TRIBUNE",
               "HEWLETT PACKARD", "HEWLETT-PACKARD", "HP",
               "YAHOO", "MOTOROLA", "SUN MICROSYSTEMS",
               "COMPAQ", "WORLDCOM", "MCI", "LUCENT",
               "COUNTRYWIDE", "WACHOVIA", "WASHINGTON MUTUAL",
               "BEAR STEARNS", "LEHMAN BROTHERS", "MERRILL LYNCH",
               "JPMORGAN", "JP MORGAN", "JPMORGAN CHASE",
               "GOLDMAN SACHS", "MORGAN STANLEY",
               "BANK OF AMERICA", "CITIGROUP", "CITIBANK",
               "META", "ALPHABET", "RTX", "ALTRIA"),
  current_name = c("ALPHABET", "META", "ALTRIA GROUP", "RTX",
                   "WARNER BROS DISCOVERY", "BAYER", "MONDELEZ", "TRIBUNE MEDIA",
                   "HP", "HP", "HP",
                   "YAHOO/ALTABA", "MOTOROLA SOLUTIONS", "ORACLE",
                   "HP", "MCI", "VERIZON", "NOKIA",
                   "BANK OF AMERICA", "WELLS FARGO", "JPMORGAN CHASE",
                   "JPMORGAN CHASE", "BARCLAYS", "BANK OF AMERICA",
                   "JPMORGAN CHASE", "JPMORGAN CHASE", "JPMORGAN CHASE",
                   "GOLDMAN SACHS", "MORGAN STANLEY",
                   "BANK OF AMERICA", "CITIGROUP", "CITIGROUP",
                   "META", "ALPHABET", "RTX", "ALTRIA GROUP")
)

cat("=== Which known names are in our lookup? ===\n")
for (i in 1:nrow(known_changes)) {
  old <- known_changes$old_name[i]
  in_lookup <- old %in% employer_lookup_clean$employer_clean
  in_matched <- old %in% matched_final$employer_clean
  if (in_matched) {
    gv <- matched_final[employer_clean == old, .(gvkey, conm)]
    cat("YES:", old, "→ gvkey", gv$gvkey[1], "(", gv$conm[1], ")\n")
  } else if (in_lookup) {
    cat("LOOKUP ONLY:", old, "\n")
  } else {
    cat("MISSING:", old, "\n")
  }
}

# -------------------------------------------------------------------
# 2. Check: multiple employer_clean values per gvkey?
#    This tells us if name variants are properly consolidated
# -------------------------------------------------------------------
cat("\n=== gvkeys with multiple employer names ===\n")
multi <- matched_final[, .N, by = gvkey][N > 1]
cat("gvkeys appearing more than once:", nrow(multi), "\n")
if (nrow(multi) > 0) {
  print(matched_final[gvkey %in% multi$gvkey, 
                      .(gvkey, employer_clean, conm, match_distance)][order(gvkey)])
}

# -------------------------------------------------------------------
# 3. Pull historical Compustat names for our gvkeys
#    This shows ALL names a firm has used in Compustat
# -------------------------------------------------------------------

library(RPostgres)
library(data.table)

wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu", port = 9737,
                  dbname = "wrds", user = "sinadavoudi",
                  password = rstudioapi::askForPassword("AccountingCUWRDS@2024"),
                  sslmode = "require")

hist_names <- dbGetQuery(wrds, "
  SELECT gvkey, conm, fyear
  FROM comp.funda
  WHERE indfmt = 'INDL' AND datafmt = 'STD' AND popsrc = 'D' AND consol = 'C'
    AND fyear >= 2000
  ORDER BY gvkey, fyear
")

dbDisconnect(wrds)
setDT(hist_names)

hist_names[, name_upper := toupper(conm)]
changes <- hist_names[, .(
  n_names = uniqueN(name_upper),
  names_used = paste(unique(name_upper), collapse = " | ")
), by = gvkey][n_names > 1]

cat("gvkeys with name changes:", nrow(changes), "\n")
print(changes[order(-n_names)][1:30])

save(hist_names, changes, file = "compustat_historical_names.rdata")
cat("Saved compustat_historical_names.rdata\n")

load("compustat_historical_names.rdata")
load("misalignment_compustat_final.rdata")

# Get ALL historical names for ALL Compustat firms (not just our 600)
# Clean them the same way we cleaned DIME names
all_hist <- unique(hist_names[, .(gvkey, name_upper)])

suffixes <- c(
  "\\bINC\\.?\\b", "\\bCORP\\.?\\b", "\\bCORPORATION\\b",
  "\\bCO\\.?\\b", "\\bLLC\\b", "\\bLTD\\.?\\b", "\\bLIMITED\\b",
  "\\bLP\\b", "\\bL\\.P\\.\\b", "\\bPLC\\b", "\\bNA\\b",
  "\\bTHE\\b", ",", "\\.", "\\/"
)
all_hist[, comp_clean := name_upper]
for (p in suffixes) {
  all_hist[, comp_clean := gsub(p, " ", comp_clean)]
}
all_hist[, comp_clean := gsub("\\s+", " ", trimws(comp_clean))]

cat("Total historical name-gvkey pairs:", nrow(all_hist), "\n")
cat("Unique gvkeys:", uniqueN(all_hist$gvkey), "\n")

# Now check: which of our missing firms would be found 
# if we searched historical names?
missing_firms <- c("GOOGLE", "FACEBOOK", "PHILIP MORRIS", "TIME WARNER",
                   "KRAFT FOODS", "TRIBUNE", "HEWLETT PACKARD", "HP",
                   "YAHOO", "MOTOROLA", "COMPAQ", "WORLDCOM", "LUCENT",
                   "COUNTRYWIDE", "BEAR STEARNS", "LEHMAN BROTHERS", 
                   "MERRILL LYNCH", "JPMORGAN", "JP MORGAN", "JPMORGAN CHASE",
                   "GOLDMAN SACHS", "MORGAN STANLEY", "BANK OF AMERICA",
                   "CITIBANK", "META", "ALPHABET", "RTX", "ALTRIA")

cat("\n=== Searching historical Compustat names for missing firms ===\n")
for (f in missing_firms) {
  matches <- all_hist[grepl(f, comp_clean, fixed = TRUE)]
  if (nrow(matches) > 0) {
    gvkeys <- unique(matches$gvkey)
    names <- unique(matches$comp_clean)
    cat("FOUND:", f, "→ gvkey(s):", paste(gvkeys, collapse = ","), 
        "| names:", paste(names[1:min(3, length(names))], collapse = " | "), "\n")
  } else {
    cat("NOT FOUND:", f, "\n")
  }
}

library(data.table)
library(RPostgres)

# -------------------------------------------------------------------
# 1. Load our existing good matches (the 600+ we already verified)
# -------------------------------------------------------------------
load("misalignment_compustat_final.rdata")
existing <- unique(matched_final[, .(employer_clean, gvkey, conm)])
cat("Existing verified matches:", nrow(existing), "\n")

# -------------------------------------------------------------------
# 2. Load slimmed DIME and count donors per employer
# -------------------------------------------------------------------
load("dime_contribs_slim.rdata")
setDT(contribs)

contribs[, employer_clean := toupper(trimws(most.recent.contributor.employer))]
suffixes <- c("\\bINC\\.?\\b", "\\bCORP\\.?\\b", "\\bCORPORATION\\b",
              "\\bCO\\.?\\b", "\\bLLC\\b", "\\bLTD\\.?\\b", "\\bLIMITED\\b",
              "\\bLP\\b", "\\bL\\.P\\.\\b", "\\bPLC\\b", "\\bNA\\b",
              "\\bTHE\\b", ",", "\\.", "\\/")
for (p in suffixes) {
  contribs[, employer_clean := gsub(p, " ", employer_clean)]
}
contribs[, employer_clean := gsub("\\s+", " ", trimws(employer_clean))]

emp_counts <- contribs[, .(n_donors = .N), by = employer_clean]
emp_counts <- emp_counts[n_donors >= 10]
cat("Employers with >= 10 donors:", nrow(emp_counts), "\n")

rm(contribs)
gc()

# -------------------------------------------------------------------
# 3. Pull Compustat names
# -------------------------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu", port = 9737,
                  dbname = "wrds", user = "sinadavoudi",
                  password = rstudioapi::askForPassword("WRDS Password"),
                  sslmode = "require")

comp_names <- dbGetQuery(wrds, "
  SELECT DISTINCT gvkey, conm FROM comp.funda 
  WHERE indfmt='INDL' AND datafmt='STD' AND popsrc='D' AND consol='C' AND fyear >= 2000
")
setDT(comp_names)
comp_names <- unique(comp_names, by = "gvkey")

comp_names[, comp_clean := toupper(trimws(conm))]
for (p in suffixes) {
  comp_names[, comp_clean := gsub(p, " ", comp_clean)]
}
comp_names[, comp_clean := gsub("\\s+", " ", trimws(comp_clean))]

# -------------------------------------------------------------------
# 4. Also pull ExecuComp CEOs while connected
# -------------------------------------------------------------------

# Check what ExecuComp tables are available
tables <- dbGetQuery(wrds, "
  SELECT table_schema, table_name 
  FROM information_schema.tables 
  WHERE table_schema LIKE '%exec%' 
  ORDER BY table_schema, table_name
")
print(tables)
# Check columns

cols <- dbGetQuery(wrds, "
  SELECT column_name 
  FROM information_schema.columns 
  WHERE table_schema = 'comp_execucomp' AND table_name = 'anncomp'
  ORDER BY ordinal_position
")
print(cols)

execucomp_ceos <- dbGetQuery(wrds, "
  SELECT gvkey, year, execid, exec_fullname, ceoann, becameceo, titleann
  FROM comp_execucomp.anncomp
  WHERE ceoann = 'CEO'
    AND year >= 2000
")

setDT(execucomp_ceos)
cat("ExecuComp CEO-years:", nrow(execucomp_ceos), "\n")
cat("Unique gvkeys:", uniqueN(execucomp_ceos$gvkey), "\n")
cat("Unique execids:", uniqueN(execucomp_ceos$execid), "\n")
cat("Year range:", range(execucomp_ceos$year), "\n")

# Preview
print(head(execucomp_ceos, 20))

# Save
save(execucomp_ceos, file = "execucomp_ceos.rdata")
cat("Saved execucomp_ceos.rdata\n")

# Now disconnect
dbDisconnect(wrds)
# -------------------------------------------------------------------
# 5. Exact match: DIME employers → Compustat
# -------------------------------------------------------------------
emp_counts[, merge_name := employer_clean]
comp_names[, merge_name := comp_clean]

new_exact <- merge(emp_counts[!employer_clean %in% existing$employer_clean],
                   comp_names[, .(gvkey, conm, merge_name)],
                   by = "merge_name", all.x = FALSE)
cat("New exact matches:", nrow(new_exact), "\n")

# -------------------------------------------------------------------
# 6. Manual aliases for major missing firms
# -------------------------------------------------------------------
manual <- data.table(
  employer_clean = c(
    "GOOGLE", "ALPHABET", "FACEBOOK", "META", "META PLATFORMS",
    "HEWLETT PACKARD", "HEWLETT-PACKARD",
    "JPMORGAN CHASE", "JP MORGAN", "JP MORGAN CHASE", "JPMORGAN",
    "CHASE MANHATTAN", "CHASE",
    "GOLDMAN SACHS", "MORGAN STANLEY", "BANK OF AMERICA",
    "MERRILL LYNCH", "BEAR STEARNS", "LEHMAN BROTHERS",
    "COUNTRYWIDE", "COUNTRYWIDE FINANCIAL",
    "CITIBANK", "CITICORP",
    "PHILIP MORRIS", "ALTRIA", "ALTRIA GROUP",
    "KRAFT FOODS", "KRAFT",
    "TIME WARNER", "WARNER BROS",
    "RAYTHEON TECHNOLOGIES", "RTX",
    "WORLDCOM", "MCI", "MCI WORLDCOM",
    "LUCENT", "LUCENT TECHNOLOGIES",
    "TRIBUNE", "TRIBUNE MEDIA",
    "ANDERSEN CONSULTING", "ACCENTURE",
    "YAHOO",
    "WAL MART", "WAL-MART",
    "SUNTRUST BANK", "SUNTRUST",
    "UNITEDHEALTH", "UNITED HEALTH GROUP", "UNITED HEALTHCARE",
    "ANHEUSER BUSCH", "ANHEUSER-BUSCH", "ANHEUSER BUSCH COS"
  ),
  gvkey = c(
    "160329", "160329", "158199", "158199", "158199",
    "005606", "005606",
    "002968", "002968", "002968", "002968",
    "002968", "002968",
    "019249", "012124", "007647",
    "007267", "011818", "030128",
    "003555", "003555",
    "003243", "003243",
    "008543", "008543", "008543",
    "012978", "012978",
    "025056", "025056",
    "008972", "008972",
    "143971", "143971", "143971",
    "043610", "043610",
    "010726", "010726",
    "176893", "176893",
    "024821",
    "011259", "011259",
    "010187", "010187",
    "010903", "010903", "010903",
    "001663", "001663", "001663"
  )
)

# Add conm from comp_names
manual <- merge(manual, comp_names[, .(gvkey, conm)], by = "gvkey", all.x = TRUE)
# Keep only those that actually have donors in DIME
manual <- manual[employer_clean %in% emp_counts$employer_clean]
cat("Manual aliases matched to DIME employers:", nrow(manual), "\n")

# -------------------------------------------------------------------
# 7. Combine everything
# -------------------------------------------------------------------
all_exact <- rbind(
  existing[, .(employer_clean, gvkey, conm, match_type = "verified")],
  new_exact[, .(employer_clean, gvkey, conm, match_type = "exact_new")],
  manual[, .(employer_clean, gvkey, conm, match_type = "manual_alias")]
)

# Deduplicate
all_exact <- all_exact[order(match_type)]
all_exact <- unique(all_exact, by = "employer_clean")

cat("\n=== EXPANDED EMPLOYER LOOKUP (exact + manual only) ===\n")
cat("Total employer-gvkey pairs:", nrow(all_exact), "\n")
cat("Unique gvkeys:", uniqueN(all_exact$gvkey), "\n")
cat("By match type:\n")
print(all_exact[, .N, by = match_type])

# Verify key firms
cat("\n=== Key firms check ===\n")
check <- c("GOOGLE", "FACEBOOK", "ALPHABET", "META",
           "GOLDMAN SACHS", "JPMORGAN CHASE", "JP MORGAN",
           "BANK OF AMERICA", "HEWLETT PACKARD",
           "PHILIP MORRIS", "ALTRIA", "WALMART", "WAL MART",
           "RAYTHEON", "RTX", "YAHOO", "MERRILL LYNCH",
           "MORGAN STANLEY", "CITIBANK", "LEHMAN BROTHERS")
for (f in check) {
  row <- all_exact[employer_clean == f]
  if (nrow(row) > 0) {
    cat("OK:", f, "→", row$gvkey[1], "(", row$conm[1], ")\n")
  } else {
    cat("MISSING:", f, "\n")
  }
}

# -------------------------------------------------------------------
# 8. Save
# -------------------------------------------------------------------
save(all_exact, file = "employer_lookup_for_panel.rdata")
save(execucomp_ceos, file = "execucomp_ceos.rdata")
cat("\nSaved employer_lookup_for_panel.rdata\n")
cat("Saved execucomp_ceos.rdata\n")
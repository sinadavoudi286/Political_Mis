library(data.table)
library(fixest)
setFixest_notes(FALSE)

load("reg_data_v2.rdata")
reg_data <- reg_data[!sic2 %in% c(49, 60:69)]
setnames(reg_data, "fyear_lead", "fyear")
reg_data[, recession := nber_recession]
reg_data[, lag_aqc_indicator := lag_acquisition]
reg_data[, firm_distress := as.integer(lag_sale_growth < 0)]

ctrl <- "lag_size + lag_roa + lag_leverage + lag_tobinq + lag_sale_growth + lag_loss + lag_capx_at + lag_rd_at + lag_aqc_indicator"
base_fml <- paste("neg_spi_at ~ misalign_abs + misalign_abs:recession +", ctrl, "| gvkey + fyear")

# Helper: extract key stats
get_stats <- function(mod, main_var="misalign_abs", inter_var="misalign_abs:recession") {
  b <- coef(mod); V <- vcov(mod)
  im <- match(main_var, names(b)); ii <- match(inter_var, names(b))
  
  # Main effect
  main_b <- b[im]; main_se <- sqrt(V[im,im])
  main_p <- 2*pt(abs(main_b/main_se), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  
  # Interaction
  int_b <- b[ii]; int_se <- sqrt(V[ii,ii])
  int_p <- 2*pt(abs(int_b/int_se), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  
  # Net effect
  net <- main_b + int_b
  net_se <- sqrt(V[im,im] + V[ii,ii] + 2*V[im,ii])
  net_t <- net/net_se
  net_p <- 2*pt(abs(net_t), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
  
  list(main_b=main_b, main_se=main_se, main_p=main_p,
       int_b=int_b, int_se=int_se, int_p=int_p,
       net=net, net_se=net_se, net_p=net_p, N=mod$nobs)
}

stars <- function(p) ifelse(p<0.01,"***",ifelse(p<0.05,"**",ifelse(p<0.1,"*","")))

# ═══════════════════════════════════════════════
# R1: Exclude 2020-2021
# ═══════════════════════════════════════════════
R1 <- feols(as.formula(base_fml), data=reg_data[!fyear %in% c(2020,2021)], cluster=~gvkey)

# ═══════════════════════════════════════════════
# R2a-c: Leave-one-recession-out
# ═══════════════════════════════════════════════
R2a <- feols(as.formula(base_fml), data=reg_data[fyear != 2001], cluster=~gvkey)
R2b <- feols(as.formula(base_fml), data=reg_data[!fyear %in% c(2008,2009)], cluster=~gvkey)
R2c <- feols(as.formula(base_fml), data=reg_data[fyear != 2020], cluster=~gvkey)

# ═══════════════════════════════════════════════
# R3: Double-lagged misalignment
# ═══════════════════════════════════════════════
R3 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs_L2 + misalign_abs_L2:recession +", ctrl, "| gvkey + fyear")),
            data=reg_data, cluster=~gvkey)

# ═══════════════════════════════════════════════
# R4-R5: Firm-level distress
# ═══════════════════════════════════════════════
R4 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:firm_distress +", ctrl, "| gvkey + fyear")),
            data=reg_data, cluster=~gvkey)
R5 <- feols(as.formula(paste("neg_spi_at ~ misalign_abs + misalign_abs:lag_loss +", ctrl, "| gvkey + fyear")),
            data=reg_data, cluster=~gvkey)

# ═══════════════════════════════════════════════
# R6-R9: Quadrant subsamples
# ═══════════════════════════════════════════════
R6 <- feols(as.formula(base_fml), data=reg_data[cross_ceo_right==1], cluster=~gvkey)
R7 <- feols(as.formula(base_fml), data=reg_data[cross_ceo_left==1], cluster=~gvkey)
R8 <- feols(as.formula(base_fml), data=reg_data[same_side_right==1], cluster=~gvkey)
R9 <- feols(as.formula(base_fml), data=reg_data[same_side_left==1], cluster=~gvkey)

# ═══════════════════════════════════════════════
# COMPACT SUMMARY TABLE
# ═══════════════════════════════════════════════

cat("═══════════════════════════════════════════════════════════════════════════════════════════\n")
cat("RECESSION ROBUSTNESS: COMPACT SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════════════════════════════\n\n")
cat(sprintf("%-28s %8s %8s %8s %8s %8s %8s %6s\n",
            "Model", "Main_b", "Main_p", "Inter_b", "Inter_p", "Net_b", "Net_p", "N"))
cat(paste(rep("─",100), collapse=""), "\n")

# R1-R5
labels <- c("R1: Excl 2020-21", "R2a: Excl 2001", "R2b: Excl 2008-09", "R2c: Excl 2020",
            "R3: Double-lag misalign")
mods_15 <- list(R1, R2a, R2b, R2c, R3)
inter_vars <- c(rep("misalign_abs:recession",4), "misalign_abs_L2:recession")
main_vars <- c(rep("misalign_abs",4), "misalign_abs_L2")

for (i in 1:5) {
  s <- get_stats(mods_15[[i]], main_var=main_vars[i], inter_var=inter_vars[i])
  cat(sprintf("%-28s %8.4f%s %7.4f %8.4f%s %7.4f %8.4f%s %7.4f %6d\n",
              labels[i], s$main_b, stars(s$main_p), s$main_p,
              s$int_b, stars(s$int_p), s$int_p,
              s$net, stars(s$net_p), s$net_p, s$N))
}

# R4-R5 (different interaction vars)
s4 <- get_stats(R4, inter_var="misalign_abs:firm_distress")
cat(sprintf("%-28s %8.4f%s %7.4f %8.4f%s %7.4f %8.4f%s %7.4f %6d\n",
            "R4: Firm distress inter", s4$main_b, stars(s4$main_p), s4$main_p,
            s4$int_b, stars(s4$int_p), s4$int_p,
            s4$net, stars(s4$net_p), s4$net_p, s4$N))

s5 <- get_stats(R5, inter_var="misalign_abs:lag_loss")
cat(sprintf("%-28s %8.4f%s %7.4f %8.4f%s %7.4f %8.4f%s %7.4f %6d\n",
            "R5: Loss firm inter", s5$main_b, stars(s5$main_p), s5$main_p,
            s5$int_b, stars(s5$int_p), s5$int_p,
            s5$net, stars(s5$net_p), s5$net_p, s5$N))

cat(paste(rep("─",100), collapse=""), "\n")

# R6-R9 quadrant subsamples
cat(sprintf("\n%-28s %8s %8s %8s %8s %6s %s\n",
            "Quadrant Subsample", "Main_b", "Main_p", "Inter_b", "Inter_p", "N", "Note"))
cat(paste(rep("─",85), collapse=""), "\n")

quad_labels <- c("R6: CEO Right / Emp Left", "R7: CEO Left / Emp Right",
                 "R8: Same side, CEO more R", "R9: Same side, CEO more L")
quad_mods <- list(R6, R7, R8, R9)
for (i in 1:4) {
  s <- get_stats(quad_mods[[i]])
  note <- ifelse(s$N < 200, " *** SMALL N ***", "")
  cat(sprintf("%-28s %8.4f%s %7.4f %8.4f%s %7.4f %6d%s\n",
              quad_labels[i], s$main_b, stars(s$main_p), s$main_p,
              s$int_b, stars(s$int_p), s$int_p, s$N, note))
}

# ═══════════════════════════════════════════════
# FULL ETABLES
# ═══════════════════════════════════════════════
cat("\n\n═══════════════════════════════════════════════════════════════════════════════════════════\n")
cat("R1–R5: FULL ETABLE\n")
cat("═══════════════════════════════════════════════════════════════════════════════════════════\n\n")
etable(R1, R2a, R2b, R2c, R3,
       headers=c("R1:No20-21","R2a:No01","R2b:No08-09","R2c:No20","R3:DblLag"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

cat("\n═══════════════════════════════════════════════════════════════════════════════════════════\n")
cat("R4–R5: FIRM DISTRESS\n")
cat("═══════════════════════════════════════════════════════════════════════════════════════════\n\n")
etable(R4, R5,
       headers=c("R4:FirmDistress","R5:LagLoss"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

cat("\n═══════════════════════════════════════════════════════════════════════════════════════════\n")
cat("R6–R9: QUADRANT SUBSAMPLES\n")
cat("═══════════════════════════════════════════════════════════════════════════════════════════\n\n")
etable(R6, R7, R8, R9,
       headers=c("R6:CeoR/EmpL","R7:CeoL/EmpR","R8:SameR","R9:SameL"),
       se.below=TRUE, signif.code=c("***"=0.01,"**"=0.05,"*"=0.1))

# Save
robustness <- list(R1=R1,R2a=R2a,R2b=R2b,R2c=R2c,R3=R3,R4=R4,R5=R5,R6=R6,R7=R7,R8=R8,R9=R9)
saveRDS(robustness, file="recession_robustness.rds")
cat("\nSaved recession_robustness.rds\n")

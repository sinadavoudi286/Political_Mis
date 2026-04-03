library(data.table)
library(fixest)
models_core <- readRDS("models_core.rds")
M10 <- models_core$M10

# Wald test: H0: ceo_right:recession == ceo_left:recession
cat("=== WALD TEST (M10) ===\n")
cat("H0: coef(ceo_right:recession) == coef(ceo_left:recession)\n\n")

# Try wald
wt <- wald(M10, "ceo_right::recession = ceo_left::recession")
cat("\n")

# Also manual delta method
b <- coef(M10)
V <- vcov(M10)
nm_r <- "ceo_right:recession"
nm_l <- "ceo_left:recession"
diff <- b[nm_r] - b[nm_l]
se_diff <- sqrt(V[nm_r,nm_r] + V[nm_l,nm_l] - 2*V[nm_r,nm_l])
t_diff <- diff / se_diff
p_diff <- 2 * pt(abs(t_diff), df = degrees_freedom(M10, type="t"), lower.tail=FALSE)
cat(sprintf("Manual: diff = %.5f, SE = %.5f, t = %.3f, p = %.4f\n", diff, se_diff, t_diff, p_diff))

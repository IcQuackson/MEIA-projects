###############################################
# PART 1 — Preliminary multivariate analysis
# Soils data (carData): total/generalized variance
# + Mahalanobis distances + outlier screening
###############################################

# 0) Load data
install.packages("carData") 
library(carData)
data("Soils")

# 1) Preparation of the data (select the last 9 variables)
X <- Soils[, c("pH","N","Dens","P","Ca","Mg","K","Na","Conduc")]
X <- na.omit(X)   # removes rows with missing values (if any)

p <- ncol(X)      # number of variables (should be 9)
n <- nrow(X)      # number of observations

cat("n (observations) =", n, "\n")
cat("p (variables)     =", p, "\n\n")


# 2) Total variance and generalized variance
S <- cov(X)

total_var <- sum(diag(S))     # total variance = trace(S)
gen_var   <- det(S)           # generalized variance = det(S)

Z  <- scale(X)                # standardized data (mean 0, sd 1)
Sz <- cov(Z)

total_var_z <- sum(diag(Sz))
gen_var_z   <- det(Sz)

cat("Total variance (original scale):      ", total_var, "\n")
cat("Generalized variance (original scale):", gen_var, "\n\n")
cat("Total variance (standardized):        ", total_var_z, "\n")
cat("Generalized variance (standardized):  ", gen_var_z, "\n\n")


# 3) Mahalanobis distances and graphics
# Mahalanobis squared distances (D2)
D2 <- mahalanobis(X, center = colMeans(X), cov = S)

# Chi-square cutoffs (df = p)
cut_975  <- qchisq(0.975, df = p)
cut_99   <- qchisq(0.99,  df = p)
cut_bonf <- qchisq(1 - 0.05/n, df = p)   # Bonferroni-style cutoff

# Identify potential multivariate outliers
out_975  <- which(D2 > cut_975)
out_99   <- which(D2 > cut_99)
out_bonf <- which(D2 > cut_bonf)

cat("Outliers (97.5% cutoff): ", out_975, "\n")
cat("Outliers (99% cutoff):   ", out_99, "\n")
cat("Outliers (Bonferroni):   ", out_bonf, "\n\n")

# Table with the 10 largest distances
top <- order(D2, decreasing = TRUE)[1:10]
top_table <- data.frame(index = top, D2 = D2[top])
print(top_table)
cat("\n")

# Summary table (one row per observation)
results <- data.frame(
  obs = 1:n,
  D2 = D2,
  flag_975  = D2 > cut_975,
  flag_99   = D2 > cut_99,
  flag_bonf = D2 > cut_bonf
)

# Plot 1: D2 by observation + cutoffs
plot(D2,
     ylab = "Mahalanobis squared distance (D2)",
     xlab = "Observation",
     pch = 19)

abline(h = cut_975,  lty = 2)
abline(h = cut_99,   lty = 2)
abline(h = cut_bonf, lty = 3)

# Label the 99% outliers
if (length(out_99) > 0) {
  text(out_99, D2[out_99], labels = out_99, pos = 3, cex = 0.8)
}

# Plot 2 (optional): Chi-square Q-Q plot of D2
qqplot(qchisq(ppoints(n), df = p), sort(D2),
       xlab = paste0("Theoretical Chi-square quantiles (df = ", p, ")"),
       ylab = "Ordered Mahalanobis squared distances (D2)",
       pch = 19)
abline(0, 1, lty = 2)




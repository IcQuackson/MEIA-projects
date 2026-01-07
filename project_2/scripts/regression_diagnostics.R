source("project_2/scripts/utils.R")
x <- load_soils_subset()

stopifnot(is.data.frame(x), "pH" %in% names(x))

# Log console output to a file in scripts
log_file <- file.path("project_2", "scripts", "regression_diagnostics.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

# Install/load required packages for modeling and plotting
install_if_missing("MASS")
install_if_missing("leaps")
install_if_missing("ggplot2")
library(MASS)
library(leaps)
library(ggplot2)

# Prepare complete-case data and response/predictors
df <- x
df <- df[complete.cases(df), , drop = FALSE]

y <- "pH"
preds <- setdiff(names(df), y)
stopifnot(length(preds) >= 2)

# ---------- quick data visuals ----------
# Correlation heatmap to spot collinearity and strong pairwise links
num_df <- df[, c(y, preds), drop = FALSE]
cor_mat <- cor(num_df)

cor_long <- as.data.frame(as.table(cor_mat))
names(cor_long) <- c("Var1", "Var2", "cor")

p_cor <- ggplot(cor_long, aes(Var1, Var2, fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  coord_equal() +
  theme_minimal(base_size = 11) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

print(p_cor)

# ---------- full model (raw scale) ----------
# Fit the baseline linear model on raw units
form_full <- as.formula(paste(y, "~", paste(preds, collapse = " + ")))
m_full <- lm(form_full, data = df)
s_full <- summary(m_full)

cat("\nFULL MODEL (raw scale)\n")
cat("Overall F-test p-value:", pf(s_full$fstatistic[1], s_full$fstatistic[2], s_full$fstatistic[3], lower.tail = FALSE), "\n")
cat("R-squared:", s_full$r.squared, "\n")
cat("Adj R-squared:", s_full$adj.r.squared, "\n")

# ---------- standardized predictors model (for exclusion evidence + effect comparison) ----------
# Standardize predictors to compare effect sizes and find weak signals
df_std <- df
df_std[, preds] <- scale(df_std[, preds, drop = FALSE])

m_std_full <- lm(form_full, data = df_std)
s_std_full <- summary(m_std_full)

cat("\nFULL MODEL (standardized predictors)\n")
cat("Overall F-test p-value:", pf(s_std_full$fstatistic[1], s_std_full$fstatistic[2], s_std_full$fstatistic[3], lower.tail = FALSE), "\n")
cat("R-squared:", s_std_full$r.squared, "\n")
cat("Adj R-squared:", s_std_full$adj.r.squared, "\n")

insig <- coef(s_std_full)[, "Pr(>|t|)"]
insig <- insig[names(insig) != "(Intercept)"]
cat("\nPredictors with p-value > 0.05 in standardized full model:\n")
print(sort(insig[insig > 0.05]))

# ---------- best subset search on standardized predictors ----------
# Exhaustive subset selection using BIC/AdjR2
n <- nrow(df_std)
p <- length(preds)

regsub <- regsubsets(form_full, data = df_std, nvmax = p, method = "exhaustive")
rs <- summary(regsub)

pick_bic <- which.min(rs$bic)
pick_adjr2 <- which.max(rs$adjr2)

cat("\nBEST SUBSET (standardized predictors)\n")
cat("BIC-min subset size:", pick_bic, " BIC:", rs$bic[pick_bic], "\n")
cat("AdjR2-max subset size:", pick_adjr2, " AdjR2:", rs$adjr2[pick_adjr2], "\n")

coef_bic <- coef(regsub, id = pick_bic)
vars_bic <- setdiff(names(coef_bic), "(Intercept)")
cat("Selected by BIC:\n")
print(vars_bic)

form_bic <- as.formula(paste(y, "~", paste(vars_bic, collapse = " + ")))
m_bic_raw <- lm(form_bic, data = df)
m_bic_std <- lm(form_bic, data = df_std)

# ---------- compare against stepwise AIC (optional cross-check) ----------
# Stepwise as a sanity check against exhaustive search
m_step <- stepAIC(m_full, direction = "both", trace = FALSE)
cat("\nSTEPWISE AIC selected formula:\n")
print(formula(m_step))

# ---------- final model choice: BIC subset (usually best for reports: parsimonious) ----------
# Use BIC-selected subset for reporting
m_final <- m_bic_raw
s_final <- summary(m_final)

cat("\nFINAL MODEL (raw scale; BIC subset)\n")
cat("Formula: "); print(formula(m_final))
cat("Overall F-test p-value:", pf(s_final$fstatistic[1], s_final$fstatistic[2], s_final$fstatistic[3], lower.tail = FALSE), "\n")
cat("R-squared:", s_final$r.squared, "\n")
cat("Adj R-squared:", s_final$adj.r.squared, "\n\n")
print(coef(s_final))

# ---------- useful plots for the report ----------
# Summary plots to communicate fit and effect sizes
# 1) Observed vs Predicted
plot_df <- data.frame(
  observed = df[[y]],
  predicted = fitted(m_final)
)

p_ovp <- ggplot(plot_df, aes(predicted, observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  theme_minimal(base_size = 11) +
  labs(x = "Predicted pH", y = "Observed pH", title = "Observed vs Predicted (final model)")
print(p_ovp)

# 2) Coefficient plot (standardized predictors, final subset)
s_final_std <- summary(m_bic_std)
coefs <- coef(s_final_std)
coefs <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]

coef_df <- data.frame(
  term = rownames(coefs),
  estimate = coefs[, "Estimate"],
  se = coefs[, "Std. Error"]
)
coef_df$lo <- coef_df$estimate - 1.96 * coef_df$se
coef_df$hi <- coef_df$estimate + 1.96 * coef_df$se
coef_df$term <- factor(coef_df$term, levels = coef_df$term[order(coef_df$estimate)])

p_coef <- ggplot(coef_df, aes(term, estimate)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "Standardized effect on pH", title = "Final model effects (standardized predictors)")
print(p_coef)

# 3) Diagnostics (assumptions)
plot_dir <- file.path("project_2", "plots", "regression_diagnostics")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

png(file.path(plot_dir, "diagnostics_plots.png"), width = 800, height = 800, res = 120)
par(mfrow = c(2, 2))
plot(m_final)
par(mfrow = c(1, 1))
dev.off()

# ---------- compact model comparison table (for the report text) ----------
# Table to compare full/stepwise/final models
mods <- list(full = m_full, stepAIC = m_step, final_BIC = m_final)
cmp <- do.call(rbind, lapply(names(mods), function(nm) {
  m <- mods[[nm]]
  s <- summary(m)
  pval <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
  data.frame(
    model = nm,
    k = length(coef(m)) - 1,
    R2 = s$r.squared,
    adjR2 = s$adj.r.squared,
    F_pvalue = pval,
    AIC = AIC(m),
    BIC = BIC(m),
    row.names = NULL
  )
}))
print(cmp)

# Optional: save plots for the report
ggsave(file.path(plot_dir, "correlation_heatmap.png"), p_cor, width = 7, height = 6, dpi = 200)
ggsave(file.path(plot_dir, "observed_vs_predicted.png"), p_ovp, width = 6, height = 4, dpi = 200)
ggsave(file.path(plot_dir, "coef_plot_standardized.png"), p_coef, width = 6, height = 4, dpi = 200)

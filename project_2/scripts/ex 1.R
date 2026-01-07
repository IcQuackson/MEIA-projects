###############################################################
# 0. Carregar subset das máquinas
###############################################################

source("project_2/scripts/utils.R")
install_if_missing("carData")
data("Soils")

###############################################################
# 1. Exploratory Data Analysis (EDA)
###############################################################

install_if_missing("carData")
install_if_missing("ggplot2")
install_if_missing("GGally")
install_if_missing("psych")
install_if_missing("corrplot")

df <- Soils[, 6:14]
df$Machine <- rownames(df)
rownames(df) <- NULL

###############################################################
# 1.1 Estatísticas descritivas
###############################################################

# Summary
summary(df)

# Média
means <- colMeans(df[ , -ncol(df)])
means

# Mediana
medians <- apply(df[ , -ncol(df)], 2, median)
medians

# Trimmed mean (10%)
trimmed_means <- apply(df[ , -ncol(df)], 2, mean, trim = 0.1)
trimmed_means

# Winsorized mean (10%)
winsor_means <- apply(df[ , -ncol(df)], 2, winsor.mean)
winsor_means

# Variâncias
variances <- apply(df[ , -ncol(df)], 2, var)
variances

# MAD (Median Absolute Deviation)
mads <- apply(df[ , -ncol(df)], 2, mad)
mads

###############################################################
# 1.2 Covariância, Variância Total, Generalized Variance
###############################################################

S <- cov(df[, -ncol(df)])  # matriz de covariância

# Variância Total = soma das variâncias
total_variance <- sum(diag(S))
total_variance

# Generalized Variance (determinante)
generalized_variance <- det(S)
generalized_variance

###############################################################
# 1.3 Distâncias de Mahalanobis
###############################################################

center <- colMeans(df[ , -ncol(df)])
md <- mahalanobis(df[ , -ncol(df)], center, S)

df$Mahalanobis <- md
md

# Outliers potenciais (nível 97.5%)
cutoff <- qchisq(0.975, df = ncol(df)-1)
which(md > cutoff)

###############################################################
# 1.4 Gráficos
###############################################################

# Histogramas
par(mfrow=c(3,3))
for(col in names(df)[-c(ncol(df), length(names(df)))]){
  hist(df[[col]], main=paste("Histograma de", col), xlab=col)
}

# Boxplots
par(mfrow=c(3,3))
for(col in names(df)[-c(ncol(df), length(names(df)))]){
  boxplot(df[[col]], main=paste("Boxplot de", col))
}

install_if_missing("GGally")
install_if_missing("ggplot2")

my_cor_tile <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  r <- cor(x, y, use = "pairwise.complete.obs")
  
  ggplot() +
    geom_tile(aes(x = 1, y = 1, fill = r)) +
    geom_text(aes(x = 1, y = 1, label = sprintf("%.2f", r)), size = 5) +
    scale_fill_gradient2(limits = c(-1, 1), midpoint = 0) +
    theme_void() +
    theme(legend.position = "none")
}

ggpairs(
  df[, sapply(df, is.numeric)],
  upper = list(continuous = my_cor_tile),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 0.7)),
  diag  = list(continuous = "densityDiag")
) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Carregar os dados (supondo que já tens o ficheiro "load_machines_subset.R" que carrega os dados)
source("scripts/load_machines_subset.R")  # carrega a FUNÇÃO (não os dados)
X <- load_machines_subset()               # Carrega os dados (subset) para X

# Garantir rownames (igual ao teu template)
if (is.null(rownames(X)) || any(rownames(X) == "")) {
  rownames(X) <- paste0("Obs", seq_len(nrow(X)))
}

# -------------------------------------------------
# 1) Introduzir o outlier (conforme enunciado)
# -------------------------------------------------
X_new <- as.matrix(X)
X_new["hp-3000/64", ] <- c(75, 2000, 0.8, 80000, 300, 24, 62, 47)

# -------------------------------------------------
# 2) PCA robusta baseada em MCD (SEM standardization)
# -------------------------------------------------
rpca <- PcaHubert(
  X_new,
  scale = FALSE,                 # Sem estandardizar os dados
  k = ncol(X_new)                # Número de componentes principais
)

# Variância explicada (usando os eigenvalues do objeto robusto)
eig  <- rpca@eigenvalues
pve  <- eig / sum(eig)          # Proporção de variância explicada
cpve <- cumsum(pve)             # Proporção acumulada de variância explicada

cat("\n==================== ROBUST PCA (MCD) ====================\n")
cat("p =", ncol(X_new), "\n")
cat("n =", nrow(X_new), "\n\n")

cat("PVE por PC:\n")
print(pve)

cat("\nCPVE (Proporção acumulada da variância explicada):\n")
print(cpve)

cat("\nLoadings (MCD PCA):\n")
print(getLoadings(rpca))

# -------------------------------------------------
# 3) Gráficos úteis para discussão do efeito do outlier
# -------------------------------------------------
out_dir <- "plots/plots_pca_robusta_mcd"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Gráfico de Scree plot (para verificar a variância explicada por cada componente)
png(file.path(out_dir, "01_screeplot_mcd.png"), width=1200, height=800, res=150)
screeplot(rpca, main="Screeplot - Robust PCA (MCD)")
dev.off()

# Mapa de outliers (diagnóstico da PCA robusta)
png(file.path(out_dir, "02_outliermap_mcd_zoom.png"), width=1200, height=800, res=150)
# Ajustar a escala para zoom, removendo o argumento ylim de 'plot' e utilizando as funções adequadas
plot(rpca@sd, main="Outlier map / Diagnostic plot - Robust PCA (MCD)", 
     xlab="Index", ylab="Score distance", pch=19)
# Limitar a escala do eixo Y para zoom
ylim <- c(0, 5000)  # Definindo o intervalo de Y
# Adicionar pontos dentro do limite do eixo Y
points(which(rpca@sd <= ylim[2]), rpca@sd[rpca@sd <= ylim[2]], pch=19, col='black')
dev.off()

# Gráfico dos Scores (PC1 vs PC2)
scores <- getScores(rpca)
png(file.path(out_dir, "03_scores_pc1_pc2_mcd_zoom.png"), width=1200, height=800, res=150)
# Ajustar os limites (zoom) para visualização clara
plot(scores[,1], scores[,2], pch=19, xlab="PC1 score", ylab="PC2 score", 
     main="Scores - Robust PCA (MCD) (PC1 vs PC2)", xlim=c(-5000, 5000), ylim=c(-5000, 5000))
abline(h=0, v=0, lty=2)
dev.off()

cat("\nGráficos salvos em:", out_dir, "\n")

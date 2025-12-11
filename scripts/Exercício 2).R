source("scripts/load_machines_subset.R")  # carrega a FUNÇÃO (não os dados)
X <- load_machines_subset()               # aqui é que os dados (subset) são carregados para X

# garantir rownames (igual ao teu template)
if (is.null(rownames(X)) || any(rownames(X) == "")) {
  rownames(X) <- paste0("Obs", seq_len(nrow(X)))
}

# -------------------------------------------------
# 1) Introduzir o outlier (enunciado)
# -------------------------------------------------
X_new <- as.matrix(X)
X_new["hp-3000/64", ] <- c(75, 2000, 0.8, 80000, 300, 24, 62, 47)


# -------------------------------------------------
# 2) PCA robusta baseada em MCD (SEM standardization)
# -------------------------------------------------
rpca <- PcaCov(
  X_new,
  cov.control = CovControlMcd(),  # MCD
  scale = FALSE,                 # sem estandardizar
  k = ncol(X_new)
)

# Variância explicada (usa eigenvalues do objeto robusto)
eig  <- rpca@eigenvalues
pve  <- eig / sum(eig)
cpve <- cumsum(pve)

cat("\n==================== ROBUST PCA (MCD) ====================\n")
cat("p =", ncol(X_new), "\n")
cat("n =", nrow(X_new), "\n\n")

cat("PVE per PC:\n")
print(pve)

cat("\nCPVE:\n")
print(cpve)

cat("\nLoadings (MCD PCA):\n")
print(getLoadings(rpca))

# -------------------------------------------------
# 3) Plots úteis para a discussão do efeito do outlier
# -------------------------------------------------
out_dir <- "plots/plots_pca_robusta_mcd"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

png(file.path(out_dir, "01_screeplot_mcd.png"), width=1200, height=800, res=150)
screeplot(rpca, main="Screeplot - Robust PCA (MCD)")
dev.off()

png(file.path(out_dir, "02_outliermap_mcd.png"), width=1200, height=800, res=150)
plot(rpca, main="Outlier map / diagnostic plot - Robust PCA (MCD)")
dev.off()

scores <- getScores(rpca)

png(file.path(out_dir, "03_scores_pc1_pc2_mcd.png"), width=1200, height=800, res=150)
plot(scores[,1], scores[,2], pch=19, xlab="PC1 score", ylab="PC2 score",
     main="Scores - Robust PCA (MCD) (PC1 vs PC2)")
abline(h=0, v=0, lty=2)
dev.off()

cat("\nSaved plots in:", out_dir, "\n")

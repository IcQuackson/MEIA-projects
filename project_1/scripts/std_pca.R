source("scripts/pca_original_scale.R")

X = load_machines_subset()

compute_pca(X, "plots/plots_pca_std", TRUE)
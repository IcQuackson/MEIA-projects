install_if_missing <- function(pkg) {
	if (!requireNamespace(pkg, quietly = TRUE)) {
		install.packages(pkg)
	}
}

load_soils_subset <- function() {
	install_if_missing("carData")
	library(carData)
	data("Soils")

	soils_subset <- Soils[, (ncol(Soils) - 8):ncol(Soils)]
	soils_subset
}
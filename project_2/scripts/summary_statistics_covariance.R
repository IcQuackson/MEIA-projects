source("project_2/scripts/utils.R")

datascale <- load_soils_subset()

round(cov(datascale), 3)

round(cor(datascale),3)

pairs(datascale, main = "Scatterplot matrix – Soils data")

library(corrplot)
cor_matrix <- cor(datascale)
corrplot::corrplot(cor_matrix, method="color", type="upper")
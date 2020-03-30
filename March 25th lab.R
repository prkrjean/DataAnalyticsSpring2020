wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)

nrow(wine_data)
dim(wine_data)

colnames(wine_data) <- c("Cvs", "Alcohol", "Malic_Acid", "Ash", "Alkalinity_of_Ash", "Magnesium", "Total_Phenols",
                         "Flavaniods", "NonFlavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280?OD315_of_Diluted_Wine", "Proline")
head(wine_data)
help(heatmap)
heatmap(cor(wine_data), Rowv = NA, Colv = NA)

help(factor)

cultivar_classes <- factor(wine_data$Csv)
cultivar_classes

help(prcomp)

wine_data_pca <- prcomp(scale(wine_data[,-1]))
summary(wine_data_pca)

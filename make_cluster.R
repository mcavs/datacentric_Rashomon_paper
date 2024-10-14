library(ggplot2)   
library(cluster)   
library(factoextra) 
library(readr)
library(tidyverse)
comp_data <- read_csv("comp_data.csv")
metrics <- comp_data[, c("T2", "T3", "T4", "L1", "L2", "L3", "N1", "N2", "N3", "N4", "T1", "LSC", "F1", "F1v", "F2", "F3", "F4")]

metrics <- metrics |>
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

set.seed(123)
kmeans_result <- kmeans(metrics, centers = 3, nstart = 25)

print(kmeans_result)

comp_data$cluster <- as.factor(kmeans_result$cluster)

fviz_cluster(kmeans_result, 
             data = metrics,
             ellipse.type = "convex", 
             geom = "point",
             main = "K-means clusters of data complexities",
             xlab = "Dimension 1", 
             ylab = "Dimension 2") + 
  theme_bw()

comp_data_with_clusters <- data.frame(data = comp_data$data, 
                                      cluster = kmeans_result$cluster,
                                      comp_data[,2:19])


#
# PCA

x <- comp_data[, 2:12]
mx <- as.data.frame(x)
mx <- as.matrix(mx)
rownames(mx) <- comp_data$data

pca <- prcomp(mx, scale = TRUE)

nx <- as.data.frame(pca$x)
nx$cluster <- comp_data$cluster

fviz_pca_biplot(pca) +
  geom_point(data=nx, aes(PC1, PC2, color=cluster), size=3)

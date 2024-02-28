library(ggplot2)
library(tidyverse)
library(ggfortify)
library(readxl)
library(factoextra)

td=typesofdiseases
str(td)
td_clust= select(td,c(2:12))
View(td_clust)
# Perform k-means clustering
fviz_nbclust(td_clust, kmeans, method = "wss") + labs(subtitle = "ElbowPlot ")  # Elbow plot

# Fit k-means with 3 clusters
km.out <- kmeans(td_clust, centers = 3, nstart = 67)
print(km.out)

# Extract cluster assignments
km.cluster <- km.out$cluster
km.cluster

#index for clustering
td_clust$Cluster <- km.cluster
td_clust

# Convert tibble to data frame
td_clust <- as.data.frame(td_clust)

# Add points name as row names in the dataset
rownames(td_clust) <- paste(td$"Infectious Diseases", nrow(td), sep = "_")

# Visualize clustering with points name
fviz_cluster(list(data = td_clust, cluster = km.cluster))

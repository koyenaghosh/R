library(ggplot2)
library(tidyverse)
library(ggfortify)
library(readxl)
library(factoextra)
#0-28 days
ban=bangladesh
str(ban)
cban = ban[,c(2:19)]

# Perform k-means clustering
fviz_nbclust(cban, kmeans, method = "wss") + labs(subtitle = "ElbowPlot 1")  # Elbow plot

# Fit k-means with 3 clusters
kmeansban <- kmeans(cban, centers = 3, nstart = 100)
print(kmeansban)

# Extract cluster assignments
km.cluster <- kmeansban$cluster
km.cluster

#index for clustering
cban$Cluster <- km.cluster
cban
View(cban)
# Convert tibble to data frame
cban <- as.data.frame(cban)

# Add points name as row names in the dataset
rownames(cban) <- paste(ban$Diseases, nrow(ban), sep = ".")

# Visualize clustering with points name
fviz_cluster(list(data = cban, cluster = km.cluster))

#1-59 months
ban1=bangladesh1
str(ban1)
cban1 = ban1[,c(2:19)]

# Perform k-means clustering
fviz_nbclust(cban1, kmeans, method = "wss") + labs(subtitle = "ElbowPlot 1")  # Elbow plot

# Fit k-means with 5 clusters
kmeansban1 <- kmeans(cban1, centers = 5, nstart = 100)
print(kmeansban1)

# Extract cluster assignments
km.cluster1 <- kmeansban1$cluster
km.cluster1

#index for clustering
cban1$Cluster <- km.cluster1
cban
View(cban)
# Convert tibble to data frame
cban1 <- as.data.frame(cban1)

# Add points name as row names in the dataset
rownames(cban1) <- paste(ban1$Diseases, nrow(ban1), sep = ".")

# Visualize clustering with points name
fviz_cluster(list(data = cban1, cluster = km.cluster1))

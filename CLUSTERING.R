library(ggplot2)
library(tidyverse)
library(ggfortify)
library(readxl)
library(factoextra)

#COUNTRY WISE CLUSTERING

df<-clustering2251
df = na.omit(df)

df$`HIV/AIDS` = as.numeric(df$`HIV/AIDS`)
df$`Diarrhoeal diseases` = as.numeric(df$`Diarrhoeal diseases`)
df$Tetanus = as.numeric(df$Tetanus)
df$`Acute lower respiratory infections` = as.numeric(df$`Acute lower respiratory infections`)
df$Prematurity = as.numeric(df$Prematurity)
df$`Birth asphyxia and birth trauma` = as.numeric(df$`Birth asphyxia and birth trauma`)
df$`Sepsis and other infectious conditions of the newborn` = 
  as.numeric(df$`Sepsis and other infectious conditions of the newborn`)
str(df)
df.km = select(df,c(2:11))

# Perform k-means clustering
fviz_nbclust(df.km, kmeans, method = "wss") + labs(subtitle = "ElbowPlot 1")  # Elbow plot

# Fit k-means with 3 clusters
km.out <- kmeans(df.km, centers = 3, nstart = 67)
print(km.out)
#SILHOUETTE SCORE CALCULATION
library(cluster)

silhouette_score <- function(k){
  km <- kmeans(df.km, centers = k, nstart=10)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
avg_sil[1] #silhouette score for 2 clusters
# Extract cluster assignments
km.cluster <- km.out$cluster
km.cluster

#index for clustering
df.km$Cluster <- km.cluster
df.km

# Convert tibble to data frame
df.km <- as.data.frame(df.km)

# Add points name as row names in the dataset
rownames(df.km) <- paste(df$Countries, 1:10, sep = "_")

# Visualize clustering with points name
fviz_cluster(list(data = df.km, cluster = km.cluster))

#DISEASEWISE CLUSTERING

df1 <-diseasewise
str(df1)
df1.km = select(df1,c(2:12))

# Perform k-means clustering
fviz_nbclust(df1.km, kmeans, method = "wss") + labs(subtitle = "ElbowPlot 2")  # Elbow plot

# Fit k-means with 3 clusters
km1.out <- kmeans(df1.km, centers = 3, nstart = 100)
print(km1.out)

# Extract cluster assignments
km1.cluster <- km1.out$cluster
km1.cluster
#index for clustering
df1.km$Cluster <- km1.cluster
df1.km
# Convert tibble to data frame
df1.km <- as.data.frame(df1.km)
# Add points name as row names in the dataset
rownames(df1.km) <- paste(df1$DISEASES, 1:nrow(df1), sep = ".")
# Visualize clustering with points name
fviz_cluster(km1.out, ellipse = TRUE, ggtheme = theme_classic(), pointsize = 0.1,labelsize = 7 , data = df1.km)

#SILHOUETTE SCORE CALCULATION
silhouette_score1 <- function(k1){
  km1 <- kmeans(df1.km, centers = k1, nstart=50)
  ss1 <- silhouette(km1$cluster, dist(df1))
  mean(ss1[, 3])
}
k1 <- 2:10
avg_sil1 <- sapply(k1, silhouette_score1)
plot(k1, type='b', avg_sil1, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
avg_sil1 

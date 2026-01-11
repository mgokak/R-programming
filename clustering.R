library(tidyverse)
library(caret)
library(ggplot2)
library(rpart)
library(rattle)
library(kknn)

library(stats)
#install.packages("factoextra")
library(factoextra)

storms <- read.csv("M:/MVG/FALL CLASSES/FundamentalsOfDS_R/week8/Storms.csv")
head(storms)
str(storms)

# deleting any kind of label (class labels)
predictors <- storms %>% select(-c(name, status, ts_diameter , hu_diameter))
predictors$category <- as.integer((predictors$category))
head(predictors)

set.seed(123)

# Normalise data before clustering
preproc <- preProcess(predictors, method = c("center", "scale"))
predictors <- predict(preproc, predictors)

# K MEANS

# find the knee, the k value is where the plot changes. its either 4 or 5
fviz_nbclust(predictors, kmeans, method = "wss")

# for sillhoutte score, the highest peak in the graph
fviz_nbclust(predictors, kmeans, method = "silhouette")

# Fit the data
fit <- kmeans(predictors, centers = 4, nstart = 25)
fit

# Display the plot
fviz_cluster(fit, data = predictors)

# Calculate PCA and plot
pca = prcomp(predictors)
# save dataframe
rotated_data = as.data.frame(pca$x)
rotated_data$Color <- df$status

#ggplot(data = rotated_data, aes(x = PC1, y = PC2, col = Color)) + geom_point(alpha = 0.3)

# Cluster plot
rotated_data$Clusters = as.factor(fit$cluster)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col=Clusters)) + geom_point()

# HAC (Hierarchical Agglomerative Clustering)
dist_mat <- dist(predictors, method = "euclidean")
hift <- hclust(dist_mat, method = "complete")
plot(hift)

# Distance matrix with both numerical and categorical data 
# (gower matrix has the ability to take both): Iris dataset
library(cluster)
dist_mat2 <- daisy(iris, metric = "gower")
summary(dist_mat2)

# knee plot and silhouette plot
fviz_nbclust(predictors, FUN = hcut, method = "wss")
fviz_nbclust(predictors, FUN = hcut, method = "silhouette")

# Build a new model
h3 <- cutree(hift, k = 3) # cutting the dendrogram

# Visualize a 2-cluster HAC
fviz_cluster(list(data = predictors, cluster = h3))

# Assign cluster as new column, plot and color by labels
rotated_data$Clusters = as.factor(h3)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point()

# create a dataframe
result <- data.frame(Status = df$status, HAC3 = h3, kmeans = fit$cluster) #status not available, use below
# result <- data.frame(Status = storms$status, HAC3 = h3, kmeans = fit$cluster)
head(result, n=100)

# Crosstab for HAC
result %>% group_by(HAC3) %>% select(HAC3, Status) %>% table()

# Crosstab for K-means
result %>% group_by(kmeans) %>% select(kmeans, Status) %>% table()

# HClust Agglomeration Methods

# MEAN (average) method
hfit <- hclust(dist_mat, method = "average") 
# distance btw 2 clusters is the dist between mean of cluster 1 and mean of cluster 2
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5)

# MEADIAN method
# distance btw 2 clusters is the dist between median of cluster 1 and median of cluster 2
hfit <- hclust(dist_mat, method = "median") 
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5)

# CENTROID method
# distance btw 2 clusters is the dist between centroid of cluster 1 and centroid of cluster 2
hfit <- hclust(dist_mat, method = "centroid") 
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5)

# SINGLE method
# distance btw 2 clusters is the minimum value of all pairwise distance for elements 
# in cluster 1 and all elements in cluster 2
hfit <- hclust(dist_mat, method = "single") 
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5) 

# COMPLETE method
# distance between two clusters is defi ned as the maximum value of all pairwise distances 
# between the elements in cluster 1 and the elements in cluster 2.
hfit <- hclust(dist_mat, method = "complete") 
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5) 

# WARD D method
# minimizes the total within-cluster variance
# At each step the pair of clusters with minimum between-cluster distance are merged
hfit <- hclust(dist_mat, method = "ward.D") 
h8 <- cutree(hfit, k=8)
# Assign clusters as new column
rotated_data$Clusters = as.factor(h8)
ggplot(data = rotated_data, aes(x=PC1, y=PC2, col = Clusters)) + geom_point(alpha=0.5) 



library(tidyverse)
library(RVAideMemoire)
library(fpc)
library(dbscan)
library(gridExtra)
library(grid)

# in this project I want conduct a cluster analysis on the "crime dataset"
# This dataset was provided by the university of Muenster (Germany) to make us do some practice with k-means clustering
# I will also test the other clustering techniques (Hierarchical clustering and DBSCAN) and evaluate their performance
# in the end, I will try to interpret the results

data <- read.table("Crime.txt", sep = " ")

mqqnorm(data, main = "Multivariate QQ-plot")
# DC is an outlier

data["DC",] <- NA
data <- data[complete.cases(data),]
# the outlier has been removed

# calculating the variance for each column and decide whether to standardize the data or not
varcol <- sapply(data, var)
varcol

# stadardize the data

st.data <- sapply(data, function(x) (x - mean(x)) / sqrt(var(x)))

# I will apply k-means algorithm with different values for k, then I will compare the Within sum of squares in the elbow plot

set.seed(22)
dataCluster1 <- kmeans(st.data, 1, nstart = 20)
dataCluster1

set.seed(21)
dataCluster2 <- kmeans(st.data, 2, nstart = 20)
dataCluster2

set.seed(20)
dataCluster3 <- kmeans(st.data, 3, nstart = 20)
dataCluster3

set.seed(24)
dataCluster4 <- kmeans(st.data, 4, nstart = 20)
dataCluster4

set.seed(25)
dataCluster5 <- kmeans(st.data, 5, nstart = 20)
dataCluster5

set.seed(26)
dataCluster6 <- kmeans(st.data, 6, nstart = 20)
dataCluster6

# creating the elbowpot

SSW1 <- mean(dataCluster1$withinss)
SSW2 <- mean(dataCluster2$withinss)
SSW3 <- mean(dataCluster3$withinss)
SSW4 <- mean(dataCluster4$withinss)
SSW5 <- mean(dataCluster5$withinss)
SSW6 <- mean(dataCluster6$withinss)

SSWc <- c(SSW1, SSW2, SSW3, SSW4, SSW5, SSW6)
k <- 1:6

elbow_df <- cbind(k, SSWc)
elbow_df <- data.frame(elbow_df)

p <- ggplot(data = elbow_df, aes(x = k, y = log(SSWc)))
p + 
  geom_point(size = 2.5) + 
  geom_line() + 
  ggtitle("Elbow Plot") +
  ylab("Ln of mean sum of squares") +
  xlab("Number of clusters")

# Two or three clusters is the correct number

# --------------------------------------------------------------- Data visualization (k-Means)

# I have to reduce the dimensions to enable data visualization.
# I choose the correlation matrix, the magnitude of the variance is too different across the features

pca <- princomp(x = data, cor = TRUE)
  
summary(pca, loadings = T)
# with 2 pc, we can almost explain the 80% of the variance of the original features

scores <- pca$scores
scores2pcas <- scores[,1:2]

data_c3_pca <- as.data.frame(cbind(dataCluster3$cluster, scores2pcas))
colnames(data_c3_pca) <- c("Clusters", "Comp.1", "Comp.2")
data_c3_pca$Clusters <- as.factor(data_c3_pca$Clusters)


pca3C_plot <- ggplot(data = data_c3_pca, aes(x = Comp.1, y = Comp.2)) +
  geom_point(aes(colour = Clusters), size = 3) + 
  ggtitle("pca plot with 3 clusters")
pca3C_plot  


data_c2_pca <- as.data.frame(cbind(dataCluster2$cluster, scores2pcas))
colnames(data_c2_pca) <- c("Clusters", "Comp.1", "Comp.2")
data_c2_pca$Clusters <- as.factor(data_c2_pca$Clusters)

pca2C_plot <- ggplot(data = data_c2_pca, aes(x = Comp.1, y = Comp.2)) +
  geom_point(aes(colour = Clusters), size = 3) + 
  ggtitle("pca plot with 2 clusters")
pca2C_plot

# the clusters seem to be well separated. dimesionality reduction helped visualization.
# Before Interpreting the data, I want to try other clustering techniques to see if I can get better results
# (elements belonging to the same clusters similar to each other, 
#  and elements belonging to different clusters similar to each other)


# hierarhical clustering trial

rownames(st.data) <- rownames(data)
h_clusters <- hclust(dist(st.data))
plot(h_clusters)

# cut the tree and plot the results

hier.clusters <- cutree(h_clusters, k = 2)

data_hierarchical_pca <- data.frame(Clusters = hier.clusters, scores2pcas)
data_hierarchical_pca$Clusters <- as.factor(data_hierarchical_pca$Clusters)

hierarchical_plot <- ggplot(data = data_hierarchical_pca, aes(x = Comp.1, y = Comp.2)) +
  geom_point(aes(colour = Clusters), size = 3) + 
  ggtitle("pca plot, 2 clusters (hierarchical)")
hierarchical_plot

# two is the best number of clusters if we consider the hierarchical clustering, but the plots are better with three clusters


# DBSCAN. minPts = 4 (convension). K-dist graph to find the right size of the e-neighbourhood

kNNdistplot(st.data, k=4)
# eps is approx 1.4

DBSCANc <- dbscan(st.data, eps = 1.4, minPts = 4)
DBSCANc$cluster <- as.character(DBSCANc$cluster)
DBSCANc$cluster <- as.factor(DBSCANc$cluster)

data_dbscan_pca <- cbind(pca$scores[,1:2], DBSCANc$cluster)
colnames(data_dbscan_pca)[3] <- c("Clusters") 

data_dbscan_pca <- as.data.frame(data_dbscan_pca)

typeof(data_dbscan_pca$Comp.1)
typeof(data_dbscan_pca$Comp.2)
typeof(data_dbscan_pca$Clusters)

data_dbscan_pca$Clusters <- as.character(data_dbscan_pca$Clusters)
data_dbscan_pca$Clusters <- as.factor(data_dbscan_pca$Clusters)

data_dbscan_pca

pcaDBSCAN_plot <- ggplot(data = data_dbscan_pca, aes(x = Comp.1, y = Comp.2)) +
  geom_point(aes(colour = Clusters), size = 3) + 
  ggtitle("pca plot with DBSCAN")
pcaDBSCAN_plot

# apperently, what really determines the clusters is pca1 (pca2 has a minor effect).
# this could be explain by the fact that all the loadings in PC1 are positive
# the higher PC1 is, the more dangerous the city is
# PC2 is also significant in determining the level of safety of a city
pca$loadings

biplot(pca) # all the features have a positive loading over PCA1

grid.arrange(pcaDBSCAN_plot, pca2C_plot, pca3C_plot, hierarchical_plot, ncol = 2)

# ---------------------------------------------------------------------- Evaluation metrics

# since the data is unlabeled (the real cluster assgnment is unknown) I can only use internal evaluation metrics

# silouhette

# kmeans with three clusters
assignment1 <- data_c3_pca$Clusters

assignment1 <- as.numeric(as.character(assignment1))
silh.kmeans.3 <- cluster::silhouette(assignment1, dist(st.data))[,"sil_width"]
mean(silh.kmeans.3)

# kmeans with two clusters

assignment2 <- data_c2_pca$Clusters

assignment2 <- as.numeric(as.character(assignment2))
silh.kmeans.2 <- cluster::silhouette(assignment2, dist(st.data))[,"sil_width"]
mean(silh.kmeans.2)

# hierarchical clustering

assignment3 <- as.numeric(as.character(hier.clusters))
silh.hierarchical <- cluster::silhouette(assignment3, dist(st.data))[,"sil_width"]
mean(silh.hierarchical)

# DBSCAN

assignment4 <- as.numeric(as.character(DBSCANc$cluster))
silh.DBSCAN <- cluster::silhouette(assignment4, dist(st.data))[,"sil_width"]
mean(silh.DBSCAN)


barplot(c(mean(silh.kmeans.3), mean(silh.kmeans.2), mean(silh.hierarchical), mean(silh.DBSCAN)),
        main = "mean Silhouette comparison",
        ylab = "mean Silhouette",
        names.arg = c("Kmeans3", "Kmeans2", "Hierarchical", "DBSCAN"))


# hierarchical clustering is the one with the highest mean Silhouette, so I will choose its results for interpeting the data

# ------------------------------------------------------------------ INTERPRETATION

data$Cluster <- NA
data$Cluster <- as.character(hier.clusters)

df1 <- data[data$Cluster == 1,]
df2 <- data[data$Cluster == 2,]

colMeans(df1[sapply(df1, is.numeric)])
colMeans(df2[sapply(df2, is.numeric)]) 

# the results are very easy to interpret: group 1 contains the safest cities, while grouo 2 contains the dangerous ones

v1 <- colMeans(df1[sapply(df1, is.numeric)])
v2 <- colMeans(df2[sapply(df2, is.numeric)]) 

plot1 <- data.frame(Crimes= names(v1), Mean_Value=v1)
plot2 <- data.frame(Crimes= names(v2), Mean_Value=v2)

rownames(plot1) <- 1:nrow(plot1)
plot1$Crimes <- as.factor(plot1$Crimes )
plot1

p1 <- ggplot(data=plot1, aes(x=Crimes, y=Mean_Value)) +
  geom_bar(stat="identity", color = "black", fill = "green") + 
  ggtitle("Cluster 1: safe cities") +
  coord_cartesian(ylim=c(0, 3700)) +
  ylab("Mean Value")
p1

rownames(plot2) <- 1:nrow(plot2)
plot2$Crimes <- as.factor(plot2$Crimes)
plot2

p2 <- ggplot(data=plot2, aes(x=Crimes, y=Mean_Value)) +
  geom_bar(stat="identity", color = "black", fill = "red") +
  ggtitle("Cluster 2: dangerous cities") +
  coord_cartesian(ylim=c(0, 3700)) +
  ylab("Mean Value")
p2

grid.arrange(p1, p2 , ncol = 2) 

# it can also be done with the scaled dataset




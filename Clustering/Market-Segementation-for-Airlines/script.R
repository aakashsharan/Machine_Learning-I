# dataset is taken from datamining.com

# read the dataset
airlines <- read.csv('AirlinesCluster.csv')
head(airlines)
summary(airlines)

# 7 different variables in the dataset.
  # Balance = number of miles eligible for award travel
  # QualMiles = number of miles qualifying for TopFlight status
  # BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
  # BonusTrans = number of non-flight bonus transactions in the past 12 months
  # FlightMiles = number of flight miles in the past 12 months
  # FlightTrans = number of flight transactions in the past 12 months
  # DaysSinceEnroll = number of days since enrolled in the frequent flyer program

# lets look for na values.
table(is.na(airlines))

# lets normalize the data.
library(caret)
preProc <- preProcess(airlines)
airlinesNorm <- predict(preProc, airlines)
summary(airlinesNorm)

# lets create a hierarchical cluster
distances <- dist(airlinesNorm, method = 'euclidean')
clusters <- hclust(distances, method = "ward.D")
#lets plot the cluster, cluster dendogram.
plot(clusters)
#lets divide the data points into 5 cluster.
cluster_groups <- cutree(clusters, 5)
table(cluster_groups)

hier_cluster <- split(airlinesNorm, cluster_groups)
#lets compare average values for each variables in the 5 clusters.
tapply(airlinesNorm$Balance, cluster_groups, mean)
tapply(airlinesNorm$QualMiles, cluster_groups, mean)
tapply(airlinesNorm$BonusMiles, cluster_groups, mean)
tapply(airlinesNorm$BonusTrans, cluster_groups, mean)
tapply(airlinesNorm$FlightMiles, cluster_groups, mean)
tapply(airlinesNorm$FlightTrans, cluster_groups, mean)
tapply(airlinesNorm$DaysSinceEnroll, cluster_groups, mean)

# after observing we can infer that:
  # 1. Cluster 1 has infrequent but loyal customers.
  # 2. Cluster 2 has customers with large amount of miles and largest number of flight transactions.
  # 3. Cluster 3 has customers with large amount of miles through non-flight transactions.
  # 4. Cluster 4 has customers who are new and accumulating miles through non-flight transactions.
  # 5. Cluster 5 has relatively new customers who don't use airline often.


# lets design a kmeans clustering.
set.seed(88)
k <- 5

KMC <- kmeans(airlinesNorm, k, iter.max = 1000)
summary(KMC)

kmc_cluster <- KMC$cluster
table(kmc_cluster)
k_cust <- split(airlinesNorm, kmc_cluster)
#lets compare average values for each variables in the 5 clusters.
tapply(airlinesNorm$Balance, kmc_cluster, mean)
tapply(airlinesNorm$QualMiles, kmc_cluster, mean)
tapply(airlinesNorm$BonusMiles, kmc_cluster, mean)
tapply(airlinesNorm$BonusTrans, kmc_cluster, mean)
tapply(airlinesNorm$FlightMiles, kmc_cluster, mean)
tapply(airlinesNorm$FlightTrans, kmc_cluster, mean)
tapply(airlinesNorm$DaysSinceEnroll, kmc_cluster, mean)

# using the information from the clusters from kmeans or hierarchical we can target different customer segments with different types of mileage offers.


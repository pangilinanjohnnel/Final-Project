
#Can labor force observations be grouped into
#Low, Medium, and High labor levels using clustering?

library(dplyr)
library(tidyr)
library(cluster)
library(clusterSim)

#LOAD
path <- "C:/Users/Johnnel/Desktop/TIP folder/2nd year 1st sem/business intellegence/FINAL/LaborForce.csv"
labor <- read.csv(path, skip = 1, header = TRUE, fill = TRUE)

#CHECK
head(labor)
str(labor)

#CLEAN AND PREPARE DATA
labor[labor == "."] <- NA
labor[3:23] <- lapply(labor[3:23], as.numeric)

labor_cluster <- labor %>%
  filter(Month != "Annual") %>%
  dplyr::select(Year, Month,
                starts_with("Male."),
                starts_with("Female.")) %>%
  pivot_longer(
    cols = starts_with(c("Male.", "Female.")),
    names_to = "Group",
    values_to = "Labor_Value"
  ) %>%
  filter(!grepl("Total", Group)) %>%
  mutate(
    Sex = ifelse(grepl("^Male", Group), "Male", "Female"),
    Age_Group = sub("^(Male|Female)\\.", "", Group)
  ) %>%
  dplyr::select(Year, Month, Sex, Age_Group, Labor_Value) %>%
  na.omit()


#SCALE
demographics <- model.matrix(
  ~ Sex + Age_Group - 1,
  data = labor_cluster
)

labor_scaled <- scale(labor_cluster$Labor_Value)

cluster_data <- cbind(demographics, labor_scaled)

#DISTANCE
dist_matrix <- dist(cluster_data, method = "euclidean")

#ELBOW: CHOOSE K
#Elbow Method-helps choose the number of clusters
#look for where the line stops improving
set.seed(417)
wss <- sapply(1:10, function(k) {
  kmeans(cluster_data, centers = k, nstart = 25)$tot.withinss
})

plot(
  1:10, wss,
  type = "b",
  main = "Elbow Method",
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares"
)

#MODEL: K-MEANS
#Choose K after inspecting the elbow plot
set.seed(417)
kmeans_model <- kmeans(
  cluster_data,
  centers = 3,#3 is where the drop begins
  nstart = 25
)

#VIEW CLUSTERS
labor_cluster$Cluster <- factor(kmeans_model$cluster)
table(labor_cluster$Cluster)
#cluster are divided now into 3 with 520 each 

#AVERAGE LABOR VALUE PER CLUSTER
cluster_means <- aggregate(
  Labor_Value ~ Cluster,
  data = labor_cluster,
  FUN = mean
)
cluster_means
#Cluster Labor_Value
#   1    1734.989
#   2    5798.721
#   3    3798.099
#cluster 2 being the highest labor value
#cluster 1 being the lowest labor value


#SEX DISTRIBUTION
table(labor_cluster$Sex, labor_cluster$Cluster)
#         1   2   3
#Female 260   0 520
#Male   260 520   0
#cluster 1: mixed gender
#cluster 2: all males
#cluster 3: all females

#AGE DISTRIBUTION
table(labor_cluster$Age_Group, labor_cluster$Cluster)
#                         1   2   3
#15...24.Years.Old       0 130 130
#25...34.Years.Old       0 130 130
#35...44.Years.Old       0 130 130
#45...54.Years.Old       0 130 130
#55...64.Years.Old     260   0   0
#65.Years.Old.and.Over 260   0   0
#Cluster 1: Composed of ages 55 and above
#Cluster 2 and 3: Composed of mixed age groups from 15 to 54

#EVAL
#SILHOUETTE SCORE
#Measures how well-separated the clusters are
sil <- silhouette(kmeans_model$cluster, dist(cluster_data))
sil
mean(sil[, 3])
#Range: -1 to 1; higher = better
#Result: 0.3535 = moderate cluster separation

#CALINSKI-HARABASZ INDEX
#Measures cluster separation and compactness
index.G1(cluster_data, kmeans_model$cluster)
#Higher = better; compare with other K values or models
#Result: 886.0005

#DAVIES-BOULDIN INDEX
#Measures cluster similarity and overlap
index.DB(cluster_data, kmeans_model$cluster)
#Lower = better
#Result: 1.2594 = some cluster overlap


#loading all the required packages


library(tidyverse)
library(factoextra)
library(ISLR)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cluster)

#importing the cvs file

pharma = read.csv("C:\\Users\\desineni\\Downloads\\Pharmaceuticals.csv")
View(pharma)

#task 1
#removing the missing values that might be present in the data
pharma = na.omit(pharma)

#collectig the numerical varibles from 1-9 columns
row.names(pharma) = pharma[,1]
p1 = pharma[,3:11]
head(p1)

#using scale function to scale the data
df = scale(p1)
head(df)

#computing the k-means custering for different values of k
kmeans = kmeans(df,centers = 2,nstart = 30)
kmeans = kmeans(df,centers = 5,nstart = 30)
kmeans = kmeans(df,centers = 6,nstart = 30)
plot1 = fviz_cluster(kmeans,data = df)+ggtitle("k = 2")
plot2 = fviz_cluster(kmeans,data = df)+ggtitle("k = 5")
plot3 = fviz_cluster(kmeans,data = df)+ggtitle("k = 6")
grid.arrange(plot1,plot2,plot3,nrow=2)

#using the eblow method to decide the optimal value of k
distance = dist(df, method = "euclidean")
fviz_dist(distance)

#
set.seed(64060)
wss = function(k){kmeans(df, k, nstart =10)$tot.withinss}
k.values = 1:10
wss_clusters = map_dbl(k.values, wss)
plot(k.values, wss_clusters,type="b", pch = 16, frame = TRUE,
     xlab="Number of clusters",ylab="Total within-clusters sum of squares")
fviz_nbclust(df,kmeans,method = "silhouette")+labs(subtitle = "silhouette method")

#Extracting results using 5 clusters and Visualize the results


set.seed(64060)
final = kmeans(df, 5, nstart = 25)
print(final)

fviz_cluster(final, data = df)
p1%>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster)%>% summarise_all("mean")
clusplot(df,final$cluster, color = TRUE, labels = 2,lines = 0)

#Task b

cluster1-AHM,ABT,AZN,BMY,LLY,NVS,SGP,WYE
cluster2-GSK,MRK,PFE,
cluster3-AVE,CHTT,ELN,IVX,MRX,WPI
cluster4-AGN,BAY,PHA
ClusterForm<- pharma[,c(12,13,14)]%>% mutate(clusters = final$cluster)%>%
  arrange(clusters, ascending = TRUE)
ClusterForm

#task c



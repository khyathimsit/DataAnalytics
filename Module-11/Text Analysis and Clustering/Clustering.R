#Reading data
Movies = read.table("Movies.txt",  header = FALSE, sep = "|", quote = "\"" )
colnames(Movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western" )
Movies[1:4] = NULL
nrow(subset(Movies, Action == 1 & Horror == 1))
#Ans: 13

#For building hierarchial clustering

Distances = dist(Movies[2:20] , method = "euclidean")
clusterMovies = hclust(Distances, method = "ward.D")
plot(clusterMovies)
#Ans: 4

#For cluster grouping
Clustergroups = cutree(clusterMovies, k = 7)
table(Clustergroups)
#Ans: Min: 7 and Max: 2

#Subsetting cluster groups
cluster1 = subset(Movies, Clustergroups == 1)
nrow(subset(cluster1, Adventure == 1))
#Ans: 56

#Children Category
tapply(Movies$Childrens, Clustergroups, sum)
#Ans: 118 cluster 1

#Fantasy Category
tapply(Movies$Fantasy, Clustergroups, sum)
#Ans: 3,4,5,6,7 with 0

#K-Means Clustering
k = 7
set.seed(1000)
KMC = kmeans(Movies[2:20], centers = k, iter.max = 1000)
str(KMC)
KMCclusters = KMC$cluster
table(KMCclusters)
#Ans: Max: 6 and Min: 1

#Best corresponding
table(Clustergroups,KMCclusters)
#Ans: for K-Means 6 it is CLuster 3

#Q11
tapply(Movies$Action, KMCclusters, sum)
#Ans: Cluster 3

#Q12
tapply(Movies$War, KMCclusters, sum)
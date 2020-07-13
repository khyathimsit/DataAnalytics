airlines = read.csv("AirlinesCluster.csv")

Q1.1:
summary(airlines)

Q1.3:
install.packages("caret")
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

Q2.1:
airlinesDist = dist(airlinesNorm, method = "euclidean")
airlinesclust = hclust(airlinesDist, method = "ward.D")
plot(airlinesclust)

Q2.2:
airline = cutree(airlinesclust, k=5)
table(airline)

Q2.3:
lapply(split(airlines, airline), colMeans)

Q2.4:
 set.seed(88)
 kmeansclust = kmeans(airlinesNorm, centers=5, iter.max=1000)
 table(kmeansclust$cluster)

wine <- read.csv("E:/ExcelR/R/PCA/PCA Assignment/wine.csv")
View(wine)
str(wine)
names(wine)
class(wine)
summary(wine)
attach(wine)


boxplot(Alcohol,horizontal = T,xlab="Alcohol")
boxplot(Malic,horizontal = T,xlab="Malic")
boxplot(Ash,horizontal = T,xlab="Ash")
boxplot(Alcalinity,horizontal = T,xlab="Alcalinity")
boxplot(Magnesium,horizontal = T,xlab="Magnesium")
boxplot(Phenols,horizontal = T,xlab="Phenols")
boxplot(Flavanoids,horizontal = T,xlab="Flavanoids")
boxplot(Nonflavanoids,horizontal = T,xlab="Nonflavanoids")
boxplot(Proanthocyanins,horizontal = T,xlab="Proanthocyanins")
boxplot(Color,horizontal = T,xlab="Color")
boxplot(Hue,horizontal = T,xlab="Hue")
boxplot(Dilution,horizontal = T,xlab="Dilution")
boxplot(Proline,horizontal = T,xlab="Proline")

mydata <- wine[,-1]
cor(mydata)

pcaobj <- princomp(mydata,cor = T,scores = T,covmat = NULL)
str(pcaobj)
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
plot(cumsum(pcaobj$sdev*pcaobj$sdev)*100/(sum(pcaobj$sdev*pcaobj$sdev)),type="b")
pcaobj$scores[,1:3]
mydata1<-cbind(wine,pcaobj$scores[,1:3])
View(mydata1)

# Hierarchial Clustering

clus_data<-wine[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) 
distance<-dist(norm_clus,method = "euclidean") 

fit<-hclust(distance,method="complete") 

plot(fit) 

rect.hclust(fit, k=7, border="red")
groups <- cutree(fit,5)
clust_1 <- as.matrix(groups)
View(clust_1)
final <- cbind(clust_1,mydata1)
View(final)
aggregate(final[,-c(2,16:18)],by=list(clust_1),FUN = mean)
aggregate(final[,-c(2,16:18)],by=list(clust_1),FUN = max)
aggregate(final[,-c(2,16:18)],by=list(clust_1),FUN = min)
write.csv(final,file = "data_clust.txt",row.names = F,col.names = F)
write.csv(final,file = "data_clust.csv",row.names = F,col.names = F)
getwd()

# K-Means Clustering :
library(plyr)
mydata2 <- final


normalized_data<-scale(mydata2[,15:17])
kmeans_clust <- kmeans(normalized_data,7)
str(kmeans_clust)

final1<- cbind(kmeans_clust$cluster,mydata2) 
View(final1)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

aggregate(mydata2[,2:12],by=list(kmeans_clust$cluster),FUN = mean)
aggregate(mydata2[,2:12],by=list(kmeans_clust$cluster),FUN = min)
aggregate(mydata2[,2:12],by=list(kmeans_clust$cluster),FUN = max)
kmeans_clust$centers
table(kmeans_clust$cluster)
write.csv(final1,file = "data_clust1.csv",row.names = F,col.names = F)
getwd()

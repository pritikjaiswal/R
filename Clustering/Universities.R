#Data load
mydata1<-read.csv("E:/ExcelR/R/clustering/Universities.csv", header=TRUE)

################################

mydata <- scale(mydata1[,2:7])
d <- dist(mydata, method = "euclidean") #Computing the distance natrix
fit <- hclust(d, method="average") # Building the algorithm # try with 'centroid'
plot(fit) # display dendogram
clusters <- cutree(fit, k=5) # cut tree into 4 clusters
  
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

#Attach the cluster numbers to Uni
clusters=data.frame('Uni'=mydata1[,1],'Cluster' =clusters)
View(clusters)

# Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(mydata1[,-1], kmeans, method = "wss") +
    labs(subtitle = "Elbow method")
  
###Cluster algorithm building
  km <- kmeans(mydata1[,-1],4) 
  km$centers
  km$cluster
  ##Animation
  install.packages("animation")
  library(animation)
  windows()
  km <- kmeans.ani(mydata1[,-1], 4)
  
  
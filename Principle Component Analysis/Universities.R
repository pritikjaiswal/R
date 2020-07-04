Universities <- read.csv("E:/ExcelR/R/PCA/Universities.csv", header=TRUE)
View(Universities)
pca <- princomp(Universities[,2:7],cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca)
pca$scores
new_data <- pca$scores[,1:4]
#pca loading
plot(pca$scores[,1:2], col="Blue", cex=0.2)
text(pca$scores[,1:2], labels = c(1:25), cex = 0.7)

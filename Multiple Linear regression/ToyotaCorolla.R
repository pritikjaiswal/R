ToyotaCorolla <- read.csv("E:/ExcelR/R/Multiple Linear Regression/Multiple linear Regr Assgn/ToyotaCorolla.csv", header=TRUE)
View(ToyotaCorolla)
toyota<-ToyotaCorolla[,c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(toyota)
attach(toyota)
summary(toyota)

boxplot(Price)
boxplot(Age_08_04)
boxplot(KM)
boxplot(HP)
boxplot(cc)
boxplot(Doors)
boxplot(Gears)
boxplot(Quarterly_Tax)
boxplot(Weight)

boxplot(Price,plot = FALSE)$out
outliers <- boxplot(Price,plot = FALSE)$out
print(outliers)
toyota[which(toyota$Price %in% outliers),]
toyota <-toyota[-which(Price %in% outliers),]

boxplot(KM,plot = FALSE)$out
outlier1 <- boxplot(KM,plot = FALSE)$out
print(outlier1)
toyota[which(KM %in% outliers),]
toyota <-toyota[-which(KM %in% outliers),]

boxplot(cc,plot = FALSE)$out
outliers <- boxplot(cc,plot = FALSE)$out
print(outliers)
toyota[which(cc %in% outliers),]
toyota <-toyota[-which(toyota$cc %in% outliers),]

boxplot(Gears,plot = FALSE)$out
outliers <- boxplot(Gears,plot = FALSE)$out
print(outliers)
toyota[which(toyota$Gears %in% outliers),]
toyota <-toyota[-which(toyota$Gears %in% outliers),]

boxplot(Quarterly_Tax,plot = FALSE)$out
outliers <- boxplot(Quarterly_Tax,plot = FALSE)$out
print(outliers)
toyota[which(Quarterly_Tax %in% outliers),]
toyota <-toyota[-which(Quarterly_Tax %in% outliers),]

pairs(toyota)
cor(toyota)

toyota_model1 <- lm(Price~ Age_08_04+KM+HP+Gears+cc+Doors+Quarterly_Tax+Weight)
summary(toyota_model1)

layout(matrix(c(1,2,3,4),2,2))
plot(toyota_model1)

# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc) # Its significat to output

model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor)

model.car <- lm(Price ~ cc + Doors)
summary(model.car)

library(car)
influenceIndexPlot(toyota_model1,id.n=3)
influencePlot(toyota_model1,id.n=3)

toyota_model2 <- lm(Price~Age_08_04+KM+HP+cc+Gears+Doors+Quarterly_Tax+Weight,data = toyota[-c(81),])
summary(toyota_model2)

influencePlot(toyota_model2)
influenceIndexPlot(toyota_model2) 
predict(toyota_model2)
vif(toyota_model1)

avPlots(toyota_model2)
avPlots(toyota_model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = toyota[-c(81),])
summary(finalmodel)            
plot(finalmodel)


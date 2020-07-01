ComputerData <- read.csv("E:/ExcelR/R/Multiple Linear Regression/Multiple linear Regr Assgn/Computer_Data.csv")

ComputerData <- ComputerData[,-1]
orgdata <- ComputerData
View(ComputerData)
attach(ComputerData)
summary(ComputerData)

install.packages("plyr")
library(plyr)
ComputerData$cd <- as.numeric(revalue(ComputerData$cd,c("yes"=1, "no"=0)))
ComputerData$multi <- as.numeric(revalue(ComputerData$multi,c("yes"=1, "no"=0)))
ComputerData$premium <- as.numeric(revalue(ComputerData$premium,c("yes"=1, "no"=0)))
View(ComputerData)

pairs(ComputerData)
cor(ComputerData)

boxplot(price,horizontal = TRUE)
boxplot(speed)
boxplot(hd)
boxplot(ram)
boxplot(screen)
boxplot(ads)

boxplot(price,plot = FALSE)$out
outliers<-boxplot(price,plot = FALSE)$out 
print(outliers)
ComputerData[which(price %in% outliers),]
ComputerData <-ComputerData[-which(price %in% outliers),]
boxplot(price)


boxplot(hd,plot = FALSE)$out
outliers<-boxplot(hd,plot = FALSE)$out 
print(outliers)
ComputerData[which(hd %in% outliers),]
ComputerData <-ComputerData[-which(hd %in% outliers),]
boxplot(hd)

boxplot(ram,plot = FALSE)$out
outliers<-boxplot(ram,plot = FALSE)$out 
print(outliers)
ComputerData[which(ram %in% outliers),]
ComputerData <-ComputerData[-which(ram %in% outliers),]

pairs(ComputerData)
cor(ComputerData)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(ComputerData))
computer_model<- lm(price~speed+hd+ram+screen+trend+multi+ads+premium+cd)
summary(computer_model)

influenceIndexPlot(computer_model)

computer_model1 <- lm(price~speed+hd+ram+screen+trend+multi+ads+premium+cd,data = ComputerData[-c(1441,1701),])
summary(computer_model1)
vif(computer_model)

computer_model2 <- lm(price ~ speed + hd + ram + screen + ads + trend + premium, data = ComputerData[-c(1441, 1701),])
summary(computer_model2)
avPlots(computer_model2)
plot(computer_model2)
qqPlot(computer_model2)
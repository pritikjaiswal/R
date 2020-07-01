
#Load data
WC_AT<-read.csv("E:/ExcelR/R/Simple Linear Regression/WC_AT.csv")
View(WC_AT)

# Visualization
install.packages("lattice")
library(lattice)
dotplot(WC_AT$AT, main="Dot Plot of Adipose Tissue data",col="dodgerblue4")
dotplot(WC_AT$Waist, main="Dot Plot of Waist Circumference", col="dodgerblue4")
boxplot(WC_AT$AT,col="dodgerblue4")
boxplot(WC_AT$Waist,col="dodgerblue4")

#Regression equation
#Syntax  model<-lm(y~x,data=data set name)
colnames(WC_AT)
model<- lm(AT~Waist,data =WC_AT)
summary(model)

newdata=data.frame(Waist=c(40,70,100))
AT=predict(model,newdata=newdata)
AT   #-77.6   26.13  129.90

newdata=data.frame(Waist=75)
AT=predict(model,newdata=newdata)
AT     #43.43



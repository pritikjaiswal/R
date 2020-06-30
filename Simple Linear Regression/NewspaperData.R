#linear Regression Problem

#load data ()
Nd<-read.csv("E:/ExcelR/R/Simple Linear Regression/NewspaperData.csv")

#Visualization
install.packages("lattice")
library(lattice)
dotplot(Nd$sunday, main="Dot Plot of Sunday Circulations",col="dodgerblue4")
dotplot(Nd$daily, main="Dot Plot of Daily Circulations", col="dodgerblue4")
boxplot(Nd$sunday,col="dodgerblue4")
boxplot(Nd$daily,col="dodgerblue4")

#Regression equation
#Syntax  model<-lm(y~x,data=data set name)
colnames(Nd)
model<- lm(sunday~daily,data =Nd)
summary(model)
sun= 13.84 + (1.34*200)
sun  #281.84


pred<-predict(model)
pred
finaldata<-data.frame(Nd, pred, "Error"=Nd$sunday-pred)
finaldata

newdata=data.frame(daily=200)
newdata
sun1=predict(model, newdata=newdata)
sun1 #281.77

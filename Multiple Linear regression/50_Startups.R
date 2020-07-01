Startups <- read.csv("E:/ExcelR/R/Multiple Linear Regression/Multiple linear Regr Assgn/50_Startups.csv")
View(Startups)

Startups$State = factor(Startups$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
Startups <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
Startups <- as.data.frame(Startups)
View(Startups)

#Scatter Plot Matrix:
pairs(Startups)

#Correlation Matrix:
cor(Startups)

Startups_model <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Startups_model)

#Multi-colinearity
install.packages("car")
library(car)
car::vif(Startups_model)

##Subset selection
library(MASS)
stepAIC(Startups_model)

#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(Startups_model) 

#Residuals vs Regressors
library(car)
residualPlots(Startups_model)

#Added Variable Plots
avPlots(Startups_model)

#QQ plots of studentized residuals
qqPlot(Startups_model)

#Deletion Diagnostics
influenceIndexPlot(Startups_model) # Index Plots of the influence measures

Startups1<-Startups[-c(46,50),]
Startups1<-lm(Profit~.,data =Startups1)
summary(Startups1)


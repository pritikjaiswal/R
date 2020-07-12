library(readxl)
Cocacola <- read_excel("E:/ExcelR/R/Forecasting/Forecasting Assignment/CocaCola_Sales_Rawdata.xlsx") 
View(Cocacola)
attach(Cocacola)
class(Cocacola)
summary(Cocacola)
plot(Cocacola$Sales,type="o")

Q1 <-  ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

CocacolaData<-cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

CocacolaData["n"]<- 1:42
View(CocacolaData)
#CocacolaData["log_Sales"]<-log(CocacolaData["Sales"])
#CocacolaData["n_square"]<-CocacolaData["n"]*CocacolaData["n"]
attach(CocacolaData)

train<-CocacolaData[1:36,]
test<-CocacolaData[37:40,]

# Linear Model 

linear_model<-lm(Sales~n,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 

# Exponential 

expo_model<-lm(log(Sales)~n,data=train)
summary(expo_model)#Adjusted R-squared:  0.8017 

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo #524.7351

# Quadratic 

Quad_model<-lm(Sales~n+I(n^2),data=train)
summary(Quad_model)#Adjusted R2 - 85.96 %
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 434.7185 

# Additive Seasonality 

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.03346
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 1785.135

# Additive Seasonality with Linear

Add_sea_Linear_model<-lm(Sales~n+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.8761 
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 534.6979 

#Additive Seasonality with Quadratic

Add_sea_Quad_model<-lm(Sales~n+I(n^2)+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)#Adjusted R-squared:  0.9549
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 236.7075 

# Multiplicative Seasonality

multi_sea_model<-lm(log(Sales)~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.05006
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 1871.203

# Multiplicative Seasonality Linear trend

multi_add_sea_model<-lm(log(Sales)~n+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.8986
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 335.1026 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Additive Seasonality with Quadratic trend  has least RMSE value

new_model<-lm(Sales~n+I(n^2)+Q1+Q2+Q3+Q4,data=CocacolaData)
new_model_pred<-data.frame(predict(new_model,newdata=CocacolaData,interval='predict'))
new_model_fin <- new_model$fitted.values

View(new_model_fin)

# pred_res<- predict(arima(log_Passenger,order=c(1,0,0)),n.ahead = 12)
Quarter <- as.data.frame(CocacolaData$Quarter)

Final <- as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Quarter",type="o")
View(Final)

##----Cocacola Sales Prediction using Auto Arima Forecast---##
# Using Arima Model - 
Cocacola1<-read_excel(file.choose()) # read the Cocacola data
Cocacola1 <- Cocacola1$Sales
Cocacola1 <- as.ts(Cocacola1)
View(Cocacola1)
class(Cocacola1)

Cocacola2 <- ts(Cocacola1,start=c(1986,1),end=c(1995,6),frequency=4)

start(Cocacola2)

end(Cocacola2)
class(Cocacola2)
sum(is.na(Cocacola2))
summary(Cocacola2)
View(Cocacola2)

decomdata<- decompose(Cocacola2, "multiplicative")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Cocacola2)
abline(reg=lm(Cocacola2~time(Cocacola2)))
cycle(Cocacola2)
# Boxplot by Cycle
boxplot(Cocacola2~cycle(Cocacola1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))
install.packages("forecast")
library(forecast)
library(tseries)
# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Cocacola2)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.

auto.arima(Cocacola2, ic = "aic", trace = TRUE)
plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
plot(Pass_Forecast)

# Test your final model

Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")
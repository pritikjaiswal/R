
Plastics<-read.csv("E:/ExcelR/R/Forecasting/Forecasting Assignment/PlasticSales.csv")
View(Plastics)  
plot(Plastics$Sales,type="o")

# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
Plasticsdata<-cbind(Plastics,X)
View(Plastics)
colnames(Plastics)
Plasticsdata["t"]<- 1:60

attach(Plasticsdata)

train<-Plasticsdata[1:48,]

test<-Plasticsdata[49:60,]

# Linear Model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)#Adjusted R-squared:  0.3159 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 260.9378 

# Exponential
expo_model<-lm(log(Sales)~t,data=train)
summary(expo_model)#Adjusted R-squared:  0.3025
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.6938  

# Quadratic
Quad_model<-lm(Sales~t+I(t^2),data=train)
summary(Quad_model)#Adjusted R-squared:  0.3048
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297.4067 

# Additive Seasonality 
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.6985
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.6027 

#Additive Seasonality with Linear 
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.9645 
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.5536 

# Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+I(t^2)+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)#Adjusted R-squared:  0.9768
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 218.1939 

# Multiplicative Seasonality
multi_sea_model<-lm(log(Sales)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.728
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.6543

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.9751 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 160.6833 

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
new_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticsdata)
new_model_pred<-data.frame(predict(new_model,newdata=Plasticsdata,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

Month <- as.data.frame(Plasticsdata$Month)

Final <- as.data.frame(cbind(Month,Plasticsdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",type="s")
View(Final)

####Plastics Sales Prediction using Auto Arima Forecast

Plastics<-read.csv(file.choose()) # read the Plastics data
Plastics <- Plastics$Sales
Plastics <- as.ts(Plastics)
View(Plastics)
class(Plastics)
library(tseries)
Plastics1 <- ts(Plastics,start=c(1986,1),end=c(1995,6),frequency=4)
start(Plastics1)
end(Plastics1)
class(Plastics1)
sum(is.na(Plastics1))
summary(Plastics1)
View(Plastics1)

decomdata<- decompose(Plastics1, "multiplicative")# decomdata<- decompose(Plastics1, "additive")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Plastics1)
abline(reg=lm(Plastics1~time(Plastics1)))
cycle(Plastics1)

# Boxplot by Cycle
boxplot(Plastics1~cycle(Plastics1,xlab = "Date", ylab = "Passenger Number(100's)",
                        main = "Monthly Boxplot of passengers from 1995 to 2002"))
library(tseries)
library(forecast)

# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Plastics1)
Newmodel

# Use the trace function to understand the determine the best p,d,q values that were selected.
auto.arima(Plastics1, ic = "aic", trace = TRUE)

# tseries evaluation
plot.ts(Newmodel$residuals)
acf(ts(Newmodel$residuals),main = 'ACF Residual')
pacf(ts(Newmodel$residuals),main = 'PACF Residual')
# Forecast for next 2 year
Pass_Forecast <- forecast(Newmodel,Level=c(95),h=10*12)
plot(Pass_Forecast)

# Test your final model
Box.test(Newmodel$resid, lag = 5, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 15, type = "Ljung-Box")
Box.test(Newmodel$resid, lag = 10, type = "Ljung-Box")
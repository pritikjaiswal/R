Airlines<-read_excel("E:/ExcelR/R/Forecasting/Forecasting Assignment/Airlines+Data.xlsx") 
View(Airlines)
plot(Airlines$Passengers,type="o")

# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)
AirlinesData["t"]<- 1:96
View(AirlinesData)
attach(AirlinesData)

train<-AirlinesData[1:84,]
test<-AirlinesData[85:96,]

# Linear Mode
linear_model<-lm(Passengers~t,data=train)
summary(linear_model) #Adjusted R-squared:  0.7898 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924

# Exponential 
expo_model<-lm(log(Passengers)~t,data=train)
summary(expo_model)#Adjusted R-squared:  0.8218 
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736  

# Quadratic 
Quad_model<-lm(Passengers~t+I(t^2),data=train)
summary(Quad_model)#Adjusted R-squared:  0.7912
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189 

# Additive Seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)#Adjusted R-squared:  0.04015
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

# Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)#Adjusted R-squared:  0.9475
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 35.34896 

# Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Passengers~t+I(t^2)+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model) #Adjusted R-squared:  0.9524 
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082 

#Multiplicative Seasonality 
multi_sea_model<-lm(log(Passengers)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)#Adjusted R-squared:  0.02568
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

# Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log(Passengers)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) #Adjusted R-squared:  0.9723
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917 

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
new_model<-lm(log(Passengers)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = AirlinesData)
new_model_pred<-data.frame(predict(new_model,newdata=AirlinesData,interval='predict'))
new_model_fin <- exp(new_model$fitted.values)

View(new_model_fin)

pred_res<- predict(arima(log(Passengers),order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))
colnames(Final) <-c("Month","Passengers","New_Pred_Value")
Final <- as.data.frame(Final)
View(Final)

###Airline Passengers using Auto Arima Forecast

Airlines1<-read_excel(file.choose()) # read the Airlines data
Airlines1 <- Airlines1$Passengers
Airlines1 <- as.ts(Airlines)
View(Airlines1)
class(Airlines1)

Airlines2 <- ts(Airlines1,start=c(1995,1),end=c(2002,12),frequency=12)

start(Airlines2)
end(Airlines2)
class(Airlines2)
sum(is.na(Airlines2))
summary(Airlines2)
View(Airlines2)

decomdata<- decompose(Airlines2, "multiplicative") ##decomdata<- decompose(Airlines2, "additive")
plot(decomdata)
plot(decomdata$seasonal)
plot(decomdata$trend)
plot(decomdata$random)
# EDA on the Original Data
plot(Airlines2)
abline(reg=lm(Airlines2~time(Airlines2)))
cycle(Airlines2)
# Boxplot by Cycle
boxplot(Airlines2~cycle(Airlines1,xlab = "Date", ylab = "Passenger Number(100's)",main = "Monthly Boxplot of passengers from 1995 to 2002"))
library(forecast)
# Use Auto Arima for the Best Model 
Newmodel <- auto.arima(Airlines2)
Newmodel
auto.arima(Airlines2, ic = "aic", trace = TRUE)

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

Calories_consumed <- read.csv("E:/ExcelR/R/Simple Linear Regression/SimpleLinearRegressionAssgn/calories_consumed.csv")
View(Calories_consumed)
attach(Calories_consumed)
plot(Calories_consumed)
summary(Calories_consumed)
cor(Weight.gained..grams.,Calories.Consumed)


reg <- lm(Weight.gained..grams.~Calories.Consumed,data = Calories_consumed)
summary(reg)
#Hence the P-value is less than 0.05. So X varibale is significance and also Multiple R-Square value is 0.8968. That's mean this model will predict the output 89.68% time correct
confint(reg,level = 0.95)
predict(reg,interval = "predict")
plot(reg)

# transform the variables to check whether the predicted values are better
reg_log <- lm(Weight.gained..grams.~log(Calories.Consumed),data = Calories_consumed)
summary(reg_log)
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")
plot(reg_log)

reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed),data = Calories_consumed)
summary(reg_sqrt)
confint(reg_sqrt,level = 0.95)
predict(reg_sqrt,interval = "predict")

reg_exp <- lm(log(Weight.gained..grams.)~Calories.Consumed,data = Calories_consumed)
summary(reg_exp)
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "predict")

a <- sqrt(log(Calories.Consumed))
reg_1 <- lm(log(Weight.gained..grams.)~a,data = Calories_consumed)
summary(reg_1)
confint(reg_1,level = 0.95)
predict(reg_1,interval = "predict")

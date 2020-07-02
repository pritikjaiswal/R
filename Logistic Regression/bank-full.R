bank.full <- read_xls("E:/ExcelR/R/Logistic Regression/Logistic Regr Assgn/bank-full.csv", sep=";")
View(bank.full)
colnames(bank.full)
class(bank.full)
summary(bank.full)
str(bank.full)
dim(bank.full)

bank <- bank.full[,-9]
View(bank)
bank <- bank[,-15]
View(bank)

library(plyr)
bank$default <- as.numeric(revalue(bank$default,c("yes"=1, "no"=0)))
bank$housing <- as.numeric(revalue(bank$housing,c("yes"=1,"no"=0)))
bank$loan <- as.numeric(revalue(bank$loan,c("yes"=1,"no"=0)))
bank$y <- as.numeric(revalue(bank$y,c("yes"=1, "no"=0)))
View(bank)

fit1 <- glm(loan~housing+default+campaign+pdays+y+balance+duration,data = bank,family = "binomial")
summary(fit1)
prob1 <- predict(fit1,type = "response")
logit1 <- glm(loan~factor(housing)+factor(default)+factor(campaign)+factor(pdays)+balance+duration,family= binomial,data=bank)
summary(logit1)
exp(coef(logit1))
library(MASS)
library(car)

stepAIC(fit1)

prob <- predict(logit1,type = c("response"),bank)                          
prob
final_y <- cbind(bank, prob)
confusion_y <- table(prob>0.5, bank$y)
table(prob>0.5)
confusion_y
Accuracy1 <- sum(diag(confusion_y)/sum(confusion_y))
Accuracy1 

threshold=0.5
predicted_values<-ifelse(predict(fit1,type="response")>threshold,1,0)
actual_values<-fit1$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

library(caret)
specificity(conf_matrix)
sensitivity(conf_matrix)
Accuracy <- sum(diag(conf_matrix)/sum(conf_matrix))
Accuracy
#changing threshold
threshold=0.8
predicted_values<-ifelse(predict(fit1,type="response")>threshold,1,0)
actual_values<-fit1$y
conf_matrix1<-table(predicted_values,actual_values)
conf_matrix1

Accuracy2<- sum(diag(conf_matrix1)/sum(conf_matrix1))
Accuracy2
##Roc curve
library(ROCR)
rocpred <- prediction(prob1,bank$loan)
rocpref <- performance(rocpred,"tpr","fpr")
plot(rocpref,colorize=T,text.adj=c(-0.2,1.7))
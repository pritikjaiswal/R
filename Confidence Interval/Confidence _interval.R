#calculating CI
install.packages("gmodels")
library(gmodels)
install.packages('nycflights13')
library(nycflights13)
data<-nycflights13::flights

dep_delay<-data$dep_delay
ar_delay<-data$arr_delay
dep_delay1<-dep_delay[!is.na(dep_delay)] #Removing NA value from dep_delay and storing in dep_delay1
ci(dep_delay1)   #default confidence is 0.95
#ci(dep_delay1, confidence=0.80) # confidence can be changed
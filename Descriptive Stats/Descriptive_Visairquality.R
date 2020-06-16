
library(datasets)
data("airquality")

#airquality = read.csv('path/airquality.csv',header=TRUE, sep=",")

airquality <- datasets::airquality

head(airquality)   #gives first 6 row and col
tail(airquality)   #gives last 6 row and col
airquality[,c(1,2)]  #all row , 1 and 2 col
airquality$Ozone    #only ozone col
####################
summary(airquality$Temp) # summary of only temp
summary(airquality)      # summary of the col variable


#####################
plot(airquality$Ozone)   # plots only ozone

plot(airquality$Ozone, airquality$Temp) #plot ozone against temp

plot(airquality)     # plots everything

# points and lines 
plot(airquality$Ozone, type= "p") # p: points, l: lines,b: both

plot(airquality$Ozone, xlab = 'ozone Concentration', 
     ylab = 'No of Instances', main = 'Ozone levels in NY city',
     col = 'blue')


# Horizontal bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',
        xlab = 'ozone levels', col= 'blue',horiz = F)



#Histogram
hist(airquality$Solar.R)
hist(airquality$Solar.R, 
     main = 'Solar Radiation values in air',
     xlab = 'Solar rad.', col='blue')

#Single box plot
boxplot(airquality$Solar.R)

# Multiple box plots
boxplot(airquality[,1:4],main='Multiple')

#margin of the grid(mar), 
#no of rows and columns(mfrow), 
#whether a border is to be included(bty) 
#and position of the 
#labels(las: 1 for horizontal, las: 0 for vertical)
#bty - box around the plot

par(mfrow=c(3,3),mar=c(2,5,2,1),  las=1, bty="o")

plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type= "l")
plot(airquality$Ozone, type= "l")
plot(airquality$Ozone, type= "l")
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',
        xlab = 'ozone levels', col='green',horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')

sd(airquality$Ozone,na.rm = T)















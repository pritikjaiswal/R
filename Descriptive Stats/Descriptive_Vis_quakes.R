

library(help='datasets')
data("quakes")


quakes<-datasets::quakes

head(quakes)   #gives first 6 row and col
tail(quakes)   #gives last 6 row and col
quakes[,c(1,2)]  #all row , 1 and 2 col
quakes$lat  #only ozone col
####################
summary(quakes$lat) # summary of only lat
summary(quakes)      # summary of the col variable


#####################
plot(quakes$lat)  

plot(quakes$lat, quakes$long)

plot(quakes)     # plots everything

# points and lines 
plot(quakes$lat, type= "p") # p: points, l: lines,b: both

plot(quakes$lat, xlab = 'lattiude', 
     ylab = 'No of Instances', main = 'earthquakes of fiji',
     col = 'blue')


# Horizontal bar plot
barplot(quakes$lat, main = 'earthquakes of fiji',
        xlab = 'lattitude', col= 'blue',horiz = F)



#Histogram
hist(quakes$long)
hist(quakes$long, 
     main = 'earthquakes off fiji',
     xlab = 'Longitude', col='blue')

#Single box plot
boxplot(quakes$long)

# Multiple box plots
boxplot(quakes[,1:4],main='Multiple')

#margin of the grid(mar), 
#no of rows and columns(mfrow), 
#whether a border is to be included(bty) 
#and position of the 
#labels(las: 1 for horizontal, las: 0 for vertical)
#bty - box around the plot

par(mfrow=c(3,3),mar=c(2,5,2,1),  las=1, bty="n")

plot(quakes$lat)
plot(quakes$lat, quakes$long)
plot(quakes$lat, type= "l")
plot(quakes$lat, type= "l")
plot(quakes$lat, type= "l")
barplot(quakes$lat, main = 'latitude off fiji',
        xlab = 'latitude', col='green',horiz = TRUE)
hist(quakes$long)
boxplot(quakes$long)
boxplot(quakes[,0:4], main='Multiple Box plots')

sd(quakes,na.rm = T)
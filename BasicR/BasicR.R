# Defining vectors
# 
rep(1,5) # repeats 1 , 5 times

seq(2,7) # gives numbers from 2 to 7

seq(4,26,4) # gives numbers from 4 to 26 with a gap of 4

# Accessing vector elements
x <- c(2,0,0,4)
x[1]
x[-1] # excludes the first element

x[1] <- 3 ;x

x[-1] <- 5; x

x[c(1,2)] <- c(4,9)
x

y <- c(1,9,9,9)
y < 9

y[4] <- 1; y
y < 9

y[y<9] =2 # edits elements marked as True with 2
y

# Data frames
df <- data.frame(x=1:3, y =c('a','b','c'))
df

df_new = data.frame(height=c(150,160,110,130),weight=c(60,70,50,65))
df_new
length(df_new)
ncol(df_new)
nrow(df_new)
names(df_new)
colnames(df_new)
rownames(df_new)

df_new1 <- data.frame(height=c(150,160),weight=c(65,72))
df_new1

# accessing dataframe elements: slicing 
df
df[c(1,3),2]

df[1,] #  1,a
df[c(1,3),1] # 1,3
df[-2,1]     # 1,3
df[c(1,3),]   # 1,a , 3,c
df[-2,]       # 1,a , 3,c
df[3,2]  # c
df[c(2,3),1] # 2,3
df[-1,1]     # 2,3
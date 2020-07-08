wbcd<-read.csv("E:/ExcelR/R/KNN/wbcd_new.csv")

# table of diagnosis
prop.table(table(wbcd$diagnosis))*100

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
View(wbcd)

# create training and test data
wbcd_train <- wbcd[1:469, ]
wbcd_test <- wbcd[470:569, ]

# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#---- Training a model on the data ----

# load the "class" library
library(class)
wbcd_test_pred <- knn(train = wbcd_train[,-1], 
                      test = wbcd_test[,-1],
                      cl = wbcd_train_labels, k=25)

##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

## Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=25)

# Create the cross tabulation of predicted vs. actual
  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
             prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                                                        cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

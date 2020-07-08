# load the library
library(caret)

# load the dataset
data(iris)

ctrl <- trainControl(method="repeatedcv",repeats = 3) 

my_knn_model <- train(Species ~ .,
                      method = "knn",
                      data = iris,
                      trControl=ctrl,
                      tuneGrid = expand.grid(k = c(9, 13, 19)))
my_knn_model
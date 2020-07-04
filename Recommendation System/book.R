library(recommenderlab)
library(caTools)
book <- read.csv("E:/ExcelR/R/Recommendation System/Recommendation system Assignment/book.csv")
str(book)
names(book)
book <- book[,-1]
book <- book[,-1]
View(book)

hist(book$Book.Rating)
plot(book$Book.Rating)
head(book$Book.Rating)

book_matrix <- as(book,'realRatingMatrix')
head(book_matrix)

book_model1 <- Recommender(book_matrix,method="POPULAR")
Recommend_book1 <- predict(book_model1,book_matrix[100:101],n=5)
as(Recommend_book1,"list")

book_model2 <- Recommender(book_matrix,method="IBCF")
Recommend_book2 <- predict(book_model2,book_matrix[100:101],n=5)
as(Recommend_book2,"list")

book_model3 <- Recommender(book_matrix,method="UBCF")
Recommend_book3 <- predict(book_model3,book_matrix[1000:1001],n=5)
as(Recommend_book1,"list")

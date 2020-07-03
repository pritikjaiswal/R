
#Different set of rule values for book Dataset using apriori algorithm.
book <- read.csv("E:/ExcelR/R/Association rule/Association rule Assignment/book.csv")
View(book)
summary(book)
str(book)
class(book)
names(book)
attach(book)

rules1 <- apriori(as.matrix(book,parameter=list(support=0.02, confidence = 0.5,minlen=5)))
rules1
rules_confidence <- sort (rules1, by="confidence", decreasing=T)
inspect(head(rules_confidence))
rules_lift <- sort(rules1,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules1, method = "grouped",control = list(cex=0.90))
plot(rules1,method = "scatterplot",control = list(cex=0.90))
plot(rules1,method = "graph",control = list(cex=0.90))

rules2 <- apriori(as.matrix(book,parameter=list(support=0.05, confidence = 0.7,minlen=6)))
rules2
rules_confidence <- sort (rules2, by="confidence", decreasing=T)
inspect(head(rules_confidence))
rules_lift <- sort(rules5,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules2, method = "grouped",control = list(cex=0.90))
plot(rules2,method = "scatterplot",control = list(cex=0.90))
plot(rules2,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules1, rules1)) > 1) 
length(subsetRules)  
rules1 <- rules1[-subsetRules] 
rules1

subsetRules <- which(colSums(is.subset(rules2, rules2)) > 1) 
length(subsetRules)  
rules2 <- rules2[-subsetRules] 
rules2
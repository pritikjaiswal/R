#Different set of rule values for my_movies Dataset using apriori algorithm.
mymovies <- read.transactions("E:/ExcelR/R/Association rule/Association rule Assignment/my_movies.csv")
View(mymovies)
summary(mymovies)
str(mymovies)
class(mymovies)

#to see most frequent items
FrequentItem <- eclat(mymovies,parameter = list(support=0.07,maxlen=15))
inspect(FrequentItem)
itemFrequencyPlot(mymovies, topN=5, type="absolute", main="Item Frequency")
dev.off()
rules1 <- apriori(mymovies,parameter = list(support=0.002,confidence=0.5,minlen=2,maxlen=5))
rules1
inspect(head(sort(rules1,by="lift"),n=15))
inspect(tail(sort(rules1,by="lift"),n=15))
plot(rules1)
quality(head(rules1))
plot(rules1, method = "grouped",control = list(cex=0.90))
plot(rules1,method = "scatterplot",control = list(cex=0.90))
plot(rules1,method = "graph",control = list(cex=0.90))

mymovies1 <- read.csv("E:/ExcelR/R/Association rule/Association rule Assignment/my_movies.csv")

rules2 <- apriori(as.matrix(mymovies1[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules2
rules_conf <- sort (rules2, by="confidence", decreasing=T)
inspect(head(rules_conf))
rules_lift <- sort(rules2,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules2, method = "grouped",control = list(cex=0.90))
plot(rules2,method = "scatterplot",control = list(cex=0.90))
plot(rules2,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules1, rules1)) > 1) 
length(subsetRules)  
rules1 <- rules1[-subsetRules] 
rules2

subsetRules1 <- which(colSums(is.subset(rules2, rules2)) > 1) 
length(subsetRules2)  
rules2 <- rules2[-subsetRules1] 
rules2

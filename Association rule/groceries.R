#Different set of rule values for Groceries Dataset using apriori algorithm.

groceries_data <- read.transactions("E:/ExcelR/R/Association rule/Association rule Assignment/groceries.csv")
View(groceries_data)
summary(groceries_data)
str(groceries_data)
class(groceries_data)

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#to see most frequent items
FrequentItem <- eclat(groceries_data,parameter = list(support=0.07,maxlen=15))
inspect(FrequentItem)
itemFrequencyPlot(groceries_data, topN=10, type="absolute", main="Item Frequency")

rules <- apriori(groceries_data,parameter = list(support=0.002,confidence=0.5,minlen=2,maxlen=5))
rules
inspect(head(sort(rules,by="lift"),n=15))
inspect(tail(sort(rules,by="lift"),n=15))
plot(rules)
quality(head(rules))
plot(rules, method = "grouped",control = list(cex=0.90))
plot(rules,method = "scatterplot",control = list(cex=0.90))
plot(rules,method = "graph",control = list(cex=0.90))

rules1 <- apriori(groceries_data,parameter = list(support=0.001,confidence=0.5,minlen=3,maxlen=5))
rules1
rules_conf <- sort (rules1, by="confidence", decreasing=T)
inspect(head(rules_conf))
rules_lift <- sort(rules1,by="lift",decreasing = T)
inspect(head(rules_lift))
plot(rules1, method = "grouped",control = list(cex=0.90))
plot(rules1,method = "scatterplot",control = list(cex=0.90))
plot(rules1,method = "graph",control = list(cex=0.90))

#remove redundant rules
subsetRules <- which(colSums(is.subset(rules1, rules1)) > 1) 
length(subsetRules)  
rules1 <- rules1[-subsetRules] 
rules1

subsetRules1 <- which(colSums(is.subset(rules, rules)) > 1) 
length(subsetRules1)  
rules <- rules[-subsetRules1] 
rules


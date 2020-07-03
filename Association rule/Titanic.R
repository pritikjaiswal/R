install.packages("arules")
library(arules)
Titanic<-read.csv("E:/ExcelR/R/Association rule/Titanic.csv")


Titanic<-Titanic[,-c(1)]
rules <- apriori(Titanic)
arules::inspect(rules)
rules.sorted <- sort(rules, by="lift")
arules::inspect(rules.sorted)

# rules with rhs containing "Survived" only (rhs=right hand side) (verbose is the extra details)
rules <- apriori(Titanic,parameter = list(supp=0.1, conf=0.1, minlen= 3)
                 ,appearance = list(rhs=c("Survived=No", "Survived=Yes")
                 ),control = list(verbose=F))
arules::inspect(rules)

install.packages("arulesViz")
library(arulesViz)
plot(rules, method = "scatterplot", jitter = 0)
plot(rules, method = "grouped")
plot(rules, method = "graph")

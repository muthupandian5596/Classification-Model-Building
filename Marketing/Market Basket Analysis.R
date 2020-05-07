setwd("D:/Great Lakes/Projects/Domain/Marketing")
getwd()
MBA <- read.csv("Cafe Coffee Night-1.csv",header = TRUE)
MBA <- MBA[,-11]
MBA.Agg <- split(MBA$Item.Desc,MBA$Bill.Number)
head(MBA.Agg)

MBA.Agg1 <- list()
for(i in 1: length(MBA.Agg)){
  MBA.Agg1[[i]] = unique(MBA.Agg[[i]])
}
head(MBA.Agg1)

library(arules)
Txns = as(MBA.Agg1,"transactions")
summary(Txns)
inspect(Txns[10])
freq <- itemFrequency(Txns)
freq <- freq[order(-freq)]
barplot(freq[1:20])
itemFrequencyPlot(Txns,support=0.1)
itemFrequencyPlot(Txns,topN = 10)
arules <- apriori(data = Txns,parameter = list(support = 0.05, confidence = 0.5))
inspect(sort(arules,by="lift"))

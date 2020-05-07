setwd("D:/Great Lakes/R/Data Sets")
MCD <- read.csv("Mcdonalds.csv",header=TRUE)
dim(MCD)
str(MCD)
summary(MCD)
attach(MCD)
library(rpivotTable)
rpivotTable(MCD)
# Plot graphically which food categories have the highest and lowest varieties

library(ggplot2)
library(tidyverse)

ggplot(MCD,aes(Category))+geom_bar(fill= "blue")+ ggtitle ("Food Categories with Varieties")+theme(plot.title= element_text(hjust = 0.5,face = "bold"),axis.text.x = element_text(color = "black",face = "bold"),axis.text.y = element_text(color = "black",face = "bold"))

# Which all variables have an outlier?
library(cowplot)
boxplot(MCD[,2:7],col=rainbow(length(MCD)),las= 1)
boxplot(MCD[,8:12],col=rainbow(length(MCD)),las= 1)
boxplot(MCD[,13:16],col=rainbow(length(MCD)),las= 1)
boxplot(MCD[,17:21],col=rainbow(length(MCD)),las= 1)
boxplot(MCD[,21:24],col=rainbow(length(MCD)),las= 1)
# Which variables have the Highest correlation. Plot them and find out the value ?
library(corrplot)
cor <- cor(MCD[4:24])
corrplot(cor,method = "circle",type="upper",tl.cex = 0.7)
cor(MCD$Calories,MCD$Calories.from.Fat)

# Which category contributes to the maximum % of Cholesterol in a diet (% daily value)?
MCD$Chol.percent.daily.value <- (Cholesterol....Daily.Value./Cholesterol)*100
MCD$Chol.percent.daily.value[is.na(Chol.percent.daily.value)]<-0
aggregate(Chol.percent.daily.value,by=list(Category),max)
boxplot(Chol.percent.daily.value~Category,srt=45,las=1.5)
# Which item contributes maximum to the Sodium intake?

# Which 4 food items contains the most amount of Saturated Fat ?
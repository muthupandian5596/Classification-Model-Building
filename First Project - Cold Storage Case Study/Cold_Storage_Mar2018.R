setwd("D:/Great Lakes/Projects/First Project - Cold Storage Case Study")
# Problem 2 
Cold_Storage <- read.csv("Cold_Storage_Mar2018.csv")
dim(Cold_Storage)
View(Cold_Storage)
attach(Cold_Storage)
# Total number of rows or sample size
n <- 35
# Mean of Temperature
sample_mean <- mean(Temperature)
Mu<- 3.9 
# 3.974286
# SD of Temperature
sd<- sd(Temperature) # For dataset std deviation
# 0.159674
# alpha = 0.1 which means 90% significance level
# State the Hypothesis, do the calculation using z test
# h0 mu>= 3.9 , h1 mu < 3.9 # Right Side Test / one tail test
hist(Temperature)
sample_error <- sample_mean - Mu
standard_error <- sd/(sqrt(n))
z <- sample_error/standard_error
# z <- 2.752 
pnorm(-abs(z))*100
#0.2958%  probability which means we are 90 % confidence in rejecting the null hypothesis
# which means we have enough evidence in proving alternate hypothesis i.e., mu< 3.9 
# State the Hypothesis, do the calculation using t-test
t.test(Temperature,mu=3.9,alternative = "less",conf.level = 0.90)
# 	One Sample t-test

# data:  Temperature
# t = 2.7524, df = 34, p-value = 0.9953
# alternative hypothesis: true mean is less than 3.9
# 90 percent confidence interval:
#-Inf 4.00956
#sample estimates:
#mean of x 
# 3.974286
# mean is less than 3.9 which means there is no problem in our process temperature



# 0.508589 for previous Problem
sd1 <- 0.508589
sample_error <- sample_mean - Mu
standard_error1 <- sd1/(sqrt(n))
z1 <- sample_error/standard_error1
# 0.8641166
pnorm(-abs(z1))*100
# 19.37619 % probability which means we are 90 % confidence in rejecting the null hypothesis
# which means we have enough evidence in proving alternate hypothesis i.e., mu< 3.9 
# State the Hypothesis, do the calculation using t-test
t.test(Temperature,mu=3.9,alternative = "less",conf.level = 0.90)


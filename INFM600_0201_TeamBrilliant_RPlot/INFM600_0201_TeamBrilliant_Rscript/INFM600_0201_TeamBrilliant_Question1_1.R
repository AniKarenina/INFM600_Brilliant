###################R Script INFM600 Team Brilliant##########################

#Read the data file as a csv format
codersdata = read.csv(file.choose(), header = T, ",")

###Performing statistical Test for each question
#Question 1.1: Correlation test
cor.test(codersdata$MoneyForLearning,codersdata$age)

##Question 1.3
#Correlation test
cor.test(codersdata$MonthsProgramming,codersdata$MoneyForLearning)

#Calculating the descriptive stats for columns having ratio scale
#Age 
hist(codersdata$age) #plot a histogram to check normality
mean(codersdata$age) #calculate mean for age
sd(codersdata$age)   #calculate standard deviation for age
qqnorm(codersdata$age) #plot a normal Quantile-Quantile plot
qqline(codersdata$age) #add a line to the QQ plot

#Question 1.1: Linear Regression
plot(codersdata$age, codersdata$MoneyForLearning, main="Linear Regression", xlab="Age", ylab="Money Spent on Learning(USD)")
m = lm(codersdata$MoneyForLearning~codersdata$age) 
summary(m)
abline(m)
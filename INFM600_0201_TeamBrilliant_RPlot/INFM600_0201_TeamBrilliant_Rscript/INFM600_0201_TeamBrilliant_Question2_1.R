###################R Script INFM600 Team Brilliant##########################

#Read the data file as a csv format
codersdata = read.csv(file.choose(), header = T, ",")

#Commute Time
hist(codersdata$CommuteTime) #plot a histogram to check normality
mean(codersdata$CommuteTime) #calculate mean for Commute time
sd(codersdata$CommuteTime)   #calculate standard deviation for Commute time
qqnorm(codersdata$CommuteTime) #plot a normal Quantile-Quantile plot
qqline(codersdata$CommuteTime) #add a line to the QQ plot

##Question 2.1
#Correlation test
cor.test(codersdata$CommuteTime,codersdata$HoursLearning)

#Linear Regression
plot(codersdata$CommuteTime, codersdata$HoursLearning)
o = lm(codersdata$HoursLearning~codersdata$CommuteTime) 
summary(o)
abline(o)
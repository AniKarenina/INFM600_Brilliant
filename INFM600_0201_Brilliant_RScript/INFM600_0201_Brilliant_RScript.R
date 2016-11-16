###################R Script INFM600 Team Brilliant##########################

#Read the data file as a csv format
codersdata = read.csv(file.choose(), header = T, ",")

#Calculating the descriptive stats for columns having ratio scale
#Age 
hist(codersdata$age) #plot a histogram to check normality
mean(codersdata$age) #calculate mean for age
sd(codersdata$age)   #calculate standard deviation for age
qqnorm(codersdata$age) #plot a normal Quantile-Quantile plot
qqline(codersdata$age) #add a line to the QQ plot

#Commute Time
hist(codersdata$CommuteTime)
mean(codersdata$CommuteTime)
sd(codersdata$CommuteTime)
qqnorm(codersdata$CommuteTime)
qqline(codersdata$CommuteTime)

##Money for learning aka investments
hist(codersdata$MoneyForLearning)
mean(codersdata$MoneyForLearning)
sd(codersdata$MoneyForLearning)
qqnorm(codersdata$MoneyForLearning)
qqline(codersdata$MoneyForLearning)

##Hours Learning
mean(codersdata$HoursLearning) 
sd(codersdata$HoursLearning)
hist(codersdata$HoursLearning) 
qqnorm(codersdata$HoursLearning) 
qqline(codersdata$HoursLearning)



###Performing statistical Test for each question
#Question 1.1: Correlation test
cor.test(codersdata$MoneyForLearning,codersdata$age)

#Question 1.1: Linear Regression
plot(codersdata$age, codersdata$MoneyForLearning)
m = lm(codersdata$MoneyForLearning~codersdata$age) 
summary(m)
abline(m)


#Question 1.2: Test for Assumptions of ANOVA test
#Normality test for all employment fields, which are the different levels of the IV
d.m1 = subset(codersdata,codersdata$EmploymentField=="arts, entertainment, sports, or media") 
qqnorm(d.m1$MoneyForLearning)
qqline(d.m1$MoneyForLearning)
hist(d.m1$MoneyForLearning)

d.m2 = subset(codersdata,codersdata$EmploymentField=="software development") 
qqnorm(d.m2$MoneyForLearning)
qqline(d.m2$MoneyForLearning)
hist(d.m2$MoneyForLearning)

d.m3 = subset(codersdata,codersdata$EmploymentField=="law enforcement and fire and rescue") 
qqnorm(d.m3$MoneyForLearning)
qqline(d.m3$MoneyForLearning)
hist(d.m3$MoneyForLearning)

d.m4 = subset(codersdata,codersdata$EmploymentField=="education") 
qqnorm(d.m4$MoneyForLearning)
qqline(d.m4$MoneyForLearning)
hist(d.m4$MoneyForLearning)

d.m5 = subset(codersdata,codersdata$EmploymentField=="architecture or physical engineering") 
qqnorm(d.m5$MoneyForLearning)
qqline(d.m5$MoneyForLearning)
hist(d.m5$MoneyForLearning)

d.m6 = subset(codersdata,codersdata$EmploymentField=="transportation") 
qqnorm(d.m6$MoneyForLearning)
qqline(d.m6$MoneyForLearning)
hist(d.m6$MoneyForLearning)

d.m7 = subset(codersdata,codersdata$EmploymentField=="finance") 
qqnorm(d.m7$MoneyForLearning)
qqline(d.m7$MoneyForLearning)
hist(d.m7$MoneyForLearning)

d.m8 = subset(codersdata,codersdata$EmploymentField=="office and administrative support") 
qqnorm(d.m8$MoneyForLearning)
qqline(d.m8$MoneyForLearning)
hist(d.m8$MoneyForLearning)

d.m9 = subset(codersdata,codersdata$EmploymentField=="food and beverage") 
qqnorm(d.m9$MoneyForLearning)
qqline(d.m9$MoneyForLearning)
hist(d.m9$MoneyForLearning)

d.mm1 = subset(codersdata,codersdata$EmploymentField=="health care") 
qqnorm(d.mm1$MoneyForLearning)
qqline(d.mm1$MoneyForLearning)
hist(d.mm1$MoneyForLearning)

d.mm2 = subset(codersdata,codersdata$EmploymentField=="sales") 
qqnorm(d.mm2$MoneyForLearning)
qqline(d.mm2$MoneyForLearning)
hist(d.mm2$MoneyForLearning)

d.mm3 = subset(codersdata,codersdata$EmploymentField=="software development and IT") 
qqnorm(d.mm3$MoneyForLearning)
qqline(d.mm3$MoneyForLearning)
hist(d.mm3$MoneyForLearning)

d.mm4 = subset(codersdata,codersdata$EmploymentField=="farming, fishing, and forestry") 
qqnorm(d.mm4$MoneyForLearning)
qqline(d.mm4$MoneyForLearning)
hist(d.mm4$MoneyForLearning)

d.mm5 = subset(codersdata,codersdata$EmploymentField=="construction and extraction") 
qqnorm(d.m1$MoneyForLearning)
qqline(d.m1$MoneyForLearning)
hist(d.m1$MoneyForLearning)

d.mm6 = subset(codersdata,codersdata$EmploymentField=="legal") 
qqnorm(d.mm6$MoneyForLearning)
qqline(d.mm6$MoneyForLearning)
hist(d.mm6$MoneyForLearning)

#Assumption test two - Homogeneity of variance using Levene test
library(car)
leveneTest(codersdata$MoneyForLearning~codersdata$EmploymentField,codersdata)

#Perform ANOVA test
a = aov(codersdata$MoneyForLearning~codersdata$EmploymentField,data=codersdata)
summary(a)

#ggplot to display various Employment Fields
library(ggplot2)
codersdata$result <- factor(codersdata$EmploymentField, levels = c("arts, entertainment, sports, or media","software development","law enforcement and fire and rescue","education","architecture or physical engineering","transportation","finance","office and administrative support","food and beverage","health care","sales","software development and IT","farming, fishing, and forestry","construction and extraction","legal")) 
k <- ggplot(codersdata, aes(codersdata$EmploymentField, fill=result)) 
p = k +geom_bar(stat = "count") 
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##Question 1.3
#Correlation test
cor.test(codersdata$MonthsProgramming,codersdata$MoneyForLearning)

#Linear Regression
plot(codersdata$MonthsProgramming, codersdata$MoneyForLearning)
n = lm(codersdata$MoneyForLearning~codersdata$MonthsProgramming) 
summary(n)
abline(n)

##Question 2.1
#Correlation test
cor.test(codersdata$CommuteTime,codersdata$HoursLearning)

#Linear Regression
plot(codersdata$CommuteTime, codersdata$HoursLearning)
o = lm(codersdata$HoursLearning~codersdata$CommuteTime) 
summary(o)
abline(o)


#Question 2.2: Test for Assumptions of ANOVA test
#Normality test for all the school majors, which are the different levels of the IV
d.s1 = subset(codersdata,codersdata$SchoolMajor=="Computer Science") 
qqnorm(d.s1$HoursLearning)
qqline(d.s1$HoursLearning)
hist(d.s1$HoursLearning)

d.s2 = subset(codersdata,codersdata$SchoolMajor=="Computer Aided Design (CAD)")
qqnorm(d.s2$HoursLearning)
qqline(d.s2$HoursLearning)
hist(d.s2$HoursLearning)

d.s3 = subset(codersdata,codersdata$SchoolMajor=="Computer Programming")
qqnorm(d.s3$HoursLearning)
qqline(d.s3$HoursLearning)
hist(d.s3$HoursLearning)

d.s4 = subset(codersdata,codersdata$SchoolMajor=="Computer Graphics")
qqnorm(d.s4$HoursLearning)
qqline(d.s4$HoursLearning)
hist(d.s4$HoursLearning)

d.s5 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Networking and Telecommunications")
qqnorm(d.s5$HoursLearning)
qqline(d.s5$HoursLearning)
hist(d.s5$HoursLearning)

d.s6 = subset(codersdata,codersdata$SchoolMajor=="Computer Engineering Technician")
qqnorm(d.s6$HoursLearning)
qqline(d.s6$HoursLearning)
hist(d.s6$HoursLearning)

d.s7 = subset(codersdata,codersdata$SchoolMajor=="Computer and Information Studies")
qqnorm(d.s7$HoursLearning)
qqline(d.s7$HoursLearning)
hist(d.s7$HoursLearning)

d.s8 = subset(codersdata,codersdata$SchoolMajor=="Computer Software Engineering")
qqnorm(d.s8$HoursLearning)
qqline(d.s8$HoursLearning)
hist(d.s8$HoursLearning)

d.s9 = subset(codersdata,codersdata$SchoolMajor=="Computer Networking")
qqnorm(d.s9$HoursLearning)
qqline(d.s9$HoursLearning)
hist(d.s9$HoursLearning)

d.ss1 = subset(codersdata,codersdata$SchoolMajor=="Computer Teacher Education")
qqnorm(d.ss1$HoursLearning)
qqline(d.ss1$HoursLearning)
hist(d.ss1$HoursLearning)

d.ss2 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Analysis")
qqnorm(d.ss2$HoursLearning)
qqline(d.ss2$HoursLearning)
hist(d.ss2$HoursLearning)

d.ss3 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Technician")
qqnorm(d.ss3$HoursLearning)
qqline(d.ss3$HoursLearning)
hist(d.ss3$HoursLearning)

d.ss4 = subset(codersdata,codersdata$SchoolMajor=="Computer Hardware Engineering")
qqnorm(d.ss4$HoursLearning)
qqline(d.ss4$HoursLearning)
hist(d.ss4$HoursLearning)

d.ss5 = subset(codersdata,codersdata$SchoolMajor=="Computer and Information Systems Security")
qqnorm(d.ss5$HoursLearning)
qqline(d.ss5$HoursLearning)
hist(d.ss5$HoursLearning)

#Assumption test two - Homogeneity of variance using Levene test
library(car)
leveneTest(codersdata$HoursLearning~codersdata$SchoolMajor,codersdata)

#Perform ANOVA test
b = aov(codersdata$HoursLearning~codersdata$SchoolMajor,data=codersdata)
summary(b)
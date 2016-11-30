###################R Script INFM600 Team Brilliant##########################

#Read the data file as a csv format
codersdata = read.csv(file.choose(), header = T, ",")
View(codersdata)

#Question 2.2: Test for Assumptions of ANOVA test
#Normality test for all the school majors, which are the different levels of the IV
d.s1 = subset(codersdata,codersdata$SchoolMajor=="Computer Science") 
qqnorm(d.s1$HoursLearning)
qqline(d.s1$HoursLearning)
hist(d.s1$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s2 = subset(codersdata,codersdata$SchoolMajor=="Computer Aided Design (CAD)")
qqnorm(d.s2$HoursLearning)
qqline(d.s2$HoursLearning)
hist(d.s2$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s3 = subset(codersdata,codersdata$SchoolMajor=="Computer Programming")
qqnorm(d.s3$HoursLearning)
qqline(d.s3$HoursLearning)
hist(d.s3$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s4 = subset(codersdata,codersdata$SchoolMajor=="Computer Graphics")
qqnorm(d.s4$HoursLearning)
qqline(d.s4$HoursLearning)
hist(d.s4$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s5 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Networking and Telecommunications")
qqnorm(d.s5$HoursLearning)
qqline(d.s5$HoursLearning)
hist(d.s5$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s6 = subset(codersdata,codersdata$SchoolMajor=="Computer Engineering Technician")
qqnorm(d.s6$HoursLearning)
qqline(d.s6$HoursLearning)
hist(d.s6$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s7 = subset(codersdata,codersdata$SchoolMajor=="Computer and Information Studies")
qqnorm(d.s7$HoursLearning)
qqline(d.s7$HoursLearning)
hist(d.s7$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s8 = subset(codersdata,codersdata$SchoolMajor=="Computer Software Engineering")
qqnorm(d.s8$HoursLearning)
qqline(d.s8$HoursLearning)
hist(d.s8$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.s9 = subset(codersdata,codersdata$SchoolMajor=="Computer Networking")
qqnorm(d.s9$HoursLearning)
qqline(d.s9$HoursLearning)
hist(d.s9$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.ss1 = subset(codersdata,codersdata$SchoolMajor=="Computer Teacher Education")
qqnorm(d.ss1$HoursLearning)
qqline(d.ss1$HoursLearning)
hist(d.ss1$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.ss2 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Analysis")
qqnorm(d.ss2$HoursLearning)
qqline(d.ss2$HoursLearning)
hist(d.ss2$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.ss3 = subset(codersdata,codersdata$SchoolMajor=="Computer Systems Technician")
qqnorm(d.ss3$HoursLearning)
qqline(d.ss3$HoursLearning)
hist(d.ss3$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.ss4 = subset(codersdata,codersdata$SchoolMajor=="Computer Hardware Engineering")
qqnorm(d.ss4$HoursLearning)
qqline(d.ss4$HoursLearning)
hist(d.ss4$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

d.ss5 = subset(codersdata,codersdata$SchoolMajor=="Computer and Information Systems Security")
qqnorm(d.ss5$HoursLearning)
qqline(d.ss5$HoursLearning)
hist(d.ss5$HoursLearning, main="Histogram to check Normality", xlab = "Hours Spent on learning")

#Assumption test two - Homogeneity of variance using Levene test
library(car)
leveneTest(codersdata$HoursLearning~codersdata$SchoolMajor,codersdata)

#Perform ANOVA test
b = aov(codersdata$HoursLearning~codersdata$SchoolMajor,data=codersdata)
summary(b)

#create a subset with major related to computers
d.finalSett = subset(codersdata, codersdata$SchoolMajor=="Computer Science" | codersdata$SchoolMajor == "Computer Aided Design (CAD)" | codersdata$SchoolMajor == "Computer Programming" | codersdata$SchoolMajor == "Computer Graphics" | codersdata$SchoolMajor == "Computer Systems Networking and Telecommunications" | codersdata$SchoolMajor == "Computer Engineering Technician" | codersdata$SchoolMajor == "Computer Hardware Engineering" | codersdata$SchoolMajor == "Computer and Information Systems Security")

#import required libraries for xkcd style
library(xkcd)
library(extrafont)

#import font style
font_import(pattern="[C/c]omic")

#Verify the font loaded on right device
fonts()
loadfonts(device="win")

### XKCD theme
theme_xkcd <- theme(
  panel.background = element_rect(fill="whitesmoke"),
  panel.grid = element_line(colour="white"),
  axis.text.x = element_text(colour="black"),
  text = element_text(size=10, family="Comic Sans MS")
)

#plot a colourful ggplot with Legend and comic sans font 
library(ggplot2)
Legend <- d.finalSett$SchoolMajor
ggplot(d.finalSett, aes(x=d.finalSett$SchoolMajor, y=d.finalSett$HoursLearning, fill=Legend))+stat_summary(geom="bar") +
   labs(y = "Hours Spent in Learning Each Week") + labs(title = "Bar Graph: Hours spent in Learning as per different School Majors") +scale_x_discrete("School Majors", waiver(), NULL)+
theme_xkcd


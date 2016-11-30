###################R Script INFM600 Team Brilliant##########################

#Read the data file as a csv format
codersdata = read.csv(file.choose(), header = T, ",")

#Question 1.2: Test for Assumptions of ANOVA test
#Normality test for all employment fields, which are the different levels of the IV
d.m1 = subset(codersdata,codersdata$EmploymentField=="arts, entertainment, sports, or media") 
qqnorm(d.m1$MoneyForLearning)
qqline(d.m1$MoneyForLearning)
hist(d.m1$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m2 = subset(codersdata,codersdata$EmploymentField=="software development") 
qqnorm(d.m2$MoneyForLearning)
qqline(d.m2$MoneyForLearning)
hist(d.m2$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m3 = subset(codersdata,codersdata$EmploymentField=="law enforcement and fire and rescue") 
qqnorm(d.m3$MoneyForLearning)
qqline(d.m3$MoneyForLearning)
hist(d.m3$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m4 = subset(codersdata,codersdata$EmploymentField=="education") 
qqnorm(d.m4$MoneyForLearning)
qqline(d.m4$MoneyForLearning)
hist(d.m4$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m5 = subset(codersdata,codersdata$EmploymentField=="architecture or physical engineering") 
qqnorm(d.m5$MoneyForLearning)
qqline(d.m5$MoneyForLearning)
hist(d.m5$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning by architecture or physical engineering folks")

d.m6 = subset(codersdata,codersdata$EmploymentField=="transportation") 
qqnorm(d.m6$MoneyForLearning)
qqline(d.m6$MoneyForLearning)
hist(d.m6$MoneyForLearning, main="Histogram to check Normality for Transportation", xlab = "Money Spent for learning")

d.m7 = subset(codersdata,codersdata$EmploymentField=="finance") 
qqnorm(d.m7$MoneyForLearning)
qqline(d.m7$MoneyForLearning)
hist(d.m7$MoneyForLearning,main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m8 = subset(codersdata,codersdata$EmploymentField=="office and administrative support") 
qqnorm(d.m8$MoneyForLearning)
qqline(d.m8$MoneyForLearning)
hist(d.m8$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.m9 = subset(codersdata,codersdata$EmploymentField=="food and beverage") 
qqnorm(d.m9$MoneyForLearning)
qqline(d.m9$MoneyForLearning)
hist(d.m9$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm1 = subset(codersdata,codersdata$EmploymentField=="health care") 
qqnorm(d.mm1$MoneyForLearning)
qqline(d.mm1$MoneyForLearning)
hist(d.mm1$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm2 = subset(codersdata,codersdata$EmploymentField=="sales") 
qqnorm(d.mm2$MoneyForLearning)
qqline(d.mm2$MoneyForLearning)
hist(d.mm2$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm3 = subset(codersdata,codersdata$EmploymentField=="software development and IT") 
qqnorm(d.mm3$MoneyForLearning)
qqline(d.mm3$MoneyForLearning)
hist(d.mm3$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm4 = subset(codersdata,codersdata$EmploymentField=="farming, fishing, and forestry") 
qqnorm(d.mm4$MoneyForLearning)
qqline(d.mm4$MoneyForLearning)
hist(d.mm4$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm5 = subset(codersdata,codersdata$EmploymentField=="construction and extraction") 
qqnorm(d.m1$MoneyForLearning)
qqline(d.m1$MoneyForLearning)
hist(d.m1$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

d.mm6 = subset(codersdata,codersdata$EmploymentField=="legal") 
qqnorm(d.mm6$MoneyForLearning)
qqline(d.mm6$MoneyForLearning)
hist(d.mm6$MoneyForLearning, main="Histogram to check Normality", xlab = "Money Spent for learning")

#Make a subset to filter the variables that violate the normality assumption
d.finalSet = subset(codersdata,codersdata$EmploymentField!="farming, fishing, and forestry" & codersdata$EmploymentField!="transportation" & codersdata$EmploymentField!="law enforcement and fire and rescue") 

#Assumption test two - Homogeneity of variance using Levene test
library(car)
leveneTest(d.finalSet$MoneyForLearning~d.finalSet$EmploymentField,codersdata)

#Perform ANOVA test
a = aov(d.finalSet$MoneyForLearning~d.finalSet$EmploymentField)
summary(a)


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


#ggplot2 to display various Employment Fields
library(ggplot2)
codersdata$Legend <- factor(codersdata$EmploymentField, levels = c("arts, entertainment, sports, or media","software development","law enforcement and fire and rescue","education","architecture or physical engineering","transportation","finance","office and administrative support","food and beverage","health care","sales","software development and IT","farming, fishing, and forestry","construction and extraction","legal")) 
k <- ggplot(codersdata, aes(codersdata$EmploymentField, fill=Legend)) 
p = k +geom_bar(stat = "Count") +  labs(title = "Bar Graph for various Employment Fields") + scale_x_discrete("Employment Fields", waiver(), NULL) 
p +  labs(x = "Employment Fields") + theme_xkcd


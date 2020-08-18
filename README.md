# Suicide-Death-records---Probability-and-stactistics

setwd("D:/") #set working directory
math_ass=read.csv("mathass.csv")  #read file
attach(math_ass)
class(math_ass)


View(math_ass)      #view data
print(math_ass)     #print data in console
rbind(math_ass)
is.data.frame(math_ass)

print(math_ass$college)   #print college
cbind(math_ass$college)

summary(math_ass)
cor(math_ass)       #Correlation

#summary stats for selected var
math_ass.sub0 <- subset(math_ass, select = c("college", "gender"))
summary(math_ass.sub0)
cor(math_ass.sub0)


math_ass.subgender <- subset(math_ass, gender=2, select = ("college"))
summary(math_ass.subgender)
print(math_ass.subgender)
sum(math_ass$gender==1)

#table for gender like no of female or male
table(math_ass$gender)


#install.packages("plyr")
library(plyr)
mean(math_ass$college)
mean(math_ass$gender)
mean(math_ass$att1)
mean(math_ass$att2)
mean(math_ass$q1)
mean(math_ass$q2)
mean(math_ass$q3)

median(math_ass$college)
median(math_ass$gneder)
median(math_ass$att1)
median(math_ass$att2)
median(math_ass$q1)
median(math_ass$q2)
median(math_ass$q3)

var(math_ass$college)
var(math_ass$gender)
var(math_ass$att1)
var(math_ass$att2)
var(math_ass$q1)
var(math_ass$q2)
var(math_ass$q3)

sd(math_ass$college)
sd(math_ass$gender)
sd(math_ass$att1)
sd(math_ass$att2)
sd(math_ass$q1)
sd(math_ass$q2)
sd(math_ass$q3)

min(math_ass$att1)
min(math_ass$att2)
min(math_ass$q1)
min(math_ass$q2)
min(math_ass$q3)


max(math_ass$att1)
max(math_ass$att2)
max(math_ass$q1)
max(math_ass$q2)
max(math_ass$q3)


#cofficient of variation
sd(math_ass$college)/mean(math_ass$college)*100
sd(math_ass$gender)/mean(math_ass$gender)*100
sd(math_ass$att1)/mean(math_ass$att1)*100
sd(math_ass$att2)/mean(math_ass$att2)*100
sd(math_ass$q1)/mean(math_ass$q1)*100
sd(math_ass$q2)/mean(math_ass$q2)*100
sd(math_ass$q3)/mean(math_ass$q3)*100

#histrogram
hist(math_ass$college,col = c("RED","GREEN","BLUE","BLACK","ORANGE","PURPLE"))
hist(math_ass$gender,col = c("RED", "BLUE"), )
hist(math_ass$att1)
hist(math_ass$att2)
hist(math_ass$q1)
hist(math_ass$q2)
hist(math_ass$q3)


#Stem or leaf Graph
stem(math_ass$college)
stem(math_ass$gender)
stem(math_ass$att1)
stem(math_ass$att2)
stem(math_ass$q1)
stem(math_ass$q2)
stem(math_ass$q3)
#################################################
dotchart(math_ass$college,color = "RED")
####################################################
plot(math_ass$college)
####################################################
plot(gender,college,main = "ScattterPlot",pch=2, col="RED")
abline(lm(college,gender))
abline(lm(college~gender),col="RED")


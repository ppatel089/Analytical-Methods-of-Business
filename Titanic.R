rm(list=ls())
library(readxl)
library(ResourceSelection)
titanic=read_excel("Module 8 Data.xlsx",sheet="Sheet1")
colnames(titanic) = tolower(make.names(colnames(titanic)))
attach(titanic)
set.seed(27738410)
my.titanic = titanic[sample(1:nrow(titanic),900,replace=FALSE),]
titanic.out1 = glm(survived~class.name, family = "binomial",data = my.titanic)
summary(titanic.out1)
titanic.out2 = glm(survived~adult, family = "binomial",data = my.titanic)
summary(titanic.out2)
titanic.out3 = glm(survived~gender, family = "binomial",data = my.titanic)
summary(titanic.out3)
titanic.out4 = glm(survived~class.name+adult, family = "binomial",data = my.titanic)
summary(titanic.out4)
titanic.out5 = glm(survived~class.name+gender, family = "binomial",data = my.titanic)
summary(titanic.out5)
titanic.out6 = glm(survived~adult+gender, family = "binomial",data = my.titanic)
summary(titanic.out6)
titanic.out7 = glm(survived~class.name+adult+gender, family = "binomial",data = my.titanic)
summary(titanic.out7)

hl=hoslem.test(titanic.out1$y,titanic.out1$fitted.values,g=10)
hl
h2=hoslem.test(titanic.out2$y,titanic.out2$fitted.values,g=10)
h2
h3=hoslem.test(titanic.out3$y,titanic.out3$fitted.values,g=10)
h3
h4=hoslem.test(titanic.out4$y,titanic.out4$fitted.values,g=10)
h4
h5=hoslem.test(titanic.out5$y,titanic.out5$fitted.values,g=10)
h5
h6=hoslem.test(titanic.out6$y,titanic.out6$fitted.values,g=10)
h6
h7=hoslem.test(titanic.out7$y,titanic.out7$fitted.values,g=10)
h7

yy=1/(1+(exp(-1*(titanic.out7$coefficients[1]+titanic.out7$coefficients[2]+titanic.out7$coefficients[5]))))
yy
yy1=1/(1+(exp(-1*(titanic.out7$coefficients[1]+titanic.out7$coefficients[2]+titanic.out7$coefficients[5]+titanic.out7$coefficients[6]))))
yy1
yy2=1/(1+(exp(-1*(titanic.out7$coefficients[1]+titanic.out7$coefficients[4]+titanic.out7$coefficients[5]))))
yy2


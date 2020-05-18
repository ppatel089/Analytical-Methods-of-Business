
rm(list=ls())
library(readxl)
library(corrplot)

#Loading the Data
setwd("D:/AMB/RegressionProject")
getwd()
input=read_excel("6304 Regression Project Data.xlsx", sheet="midwest")
colnames(input)=tolower(make.names(colnames(input)))
attach(input)

#Creating the poptotal variable
input$popcollege<-(percollege*poptotal)/100
input$popprof<-(perprof*poptotal)/100

input$CToA<-(popchild/popadult)
input$popchildpoverty<-(perchildpoverty*popchild)/100


input_rural<-subset(input, inmetro==0)
input_metro<-subset(input, inmetro==1)


set.seed(67798770)
some.rural.poverty=input[sample(1:nrow(input_rural),60,replace=FALSE),]
colnames(some.rural.poverty)=tolower(make.names(colnames(some.rural.poverty)))
attach(some.rural.poverty)



some.metro.poverty=input[sample(1:nrow(input_metro),30,replace=FALSE),]
colnames(some.metro.poverty)=tolower(make.names(colnames(some.metro.poverty)))
attach(some.metro.poverty)

#################################################################################

#Copy the continuous variables to a new data object for the rural dataset.

some.rural.poverty.continuous= some.rural.poverty[,-c(1,2,3,16)]

#Checking the co-relation among all the continuous variables

xx=cor(some.rural.poverty.continuous)
xx
corrplot(xx,method="number")


#put interpretation

#Performing the Linear Reg. by including all the continuous variables for check
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous)
summary(continuous.regout)
#Paste the output
#Imporatnce is only for perchildpoverty and R2 is 67% . We can also see the same relationship
#from the correlation graph


#Performing the Linear Reg.by considering only the percentage variables
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(9:12)])
summary(continuous.regout)
#R square is 54%. 2 columns are significant. 

#Adding area
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(1,9:12)])
summary(continuous.regout)
#even though the Rsq. is increasing slightly, it looks like the area variable has not 
#much contribution to this . So will ignore area. 

#Removing area, adding poptotal along with percentage fields
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(2,9:12)])
summary(continuous.regout)
#intrepretation
#ignoring poptotal

#Removing area, adding CtoA along with percentage fields
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(9:12,15)])
summary(continuous.regout)
#Rsq- 56% and we can see the percollege and perprof has some effect.

#So, adding popcollege and popprof
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(9:12,13:14)])
summary(continuous.regout)
#still no significance for popcollege and popprof


#So, trying to add the percentage contro part of these variables( percollege and perprof)to check
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(9:10)])
summary(continuous.regout)
#ending up in decreasing the R2 sq value significantly.

#Finally, removing the Perprof and selecting only the percollege and perchildpoverty
continuous.regout=lm(some.rural.poverty.continuous$perelderlypoverty~.,data=some.rural.poverty.continuous[,c(9,11,12)])
summary(continuous.regout)





#Question 2> LINE Assumption for the best fit model selected above
#Linearity
#plot(some.rural.poverty.continuous$perelderlypoverty,some.rural.poverty.continuous$percollege,pch=19)
#plot(some.rural.poverty.continuous$perelderlypoverty,some.rural.poverty.continuous$perchildpoverty,pch=19)

subset_continuous=subset(some.rural.poverty.continuous,select=c("perelderlypoverty","percollege","perchildpoverty"))
plot(subset_continuous)
#fairly linear


#Independence:
cor(some.rural.poverty.continuous$percollege,some.rural.poverty.continuous$perchildpoverty)
-0.3460324
#From the variables in the model we can see that there is no correlation between the independent variables
  
#Normality and Equality of Variance

par(mfrow=c(2,2))
plot(continuous.regout) 
#interpret results

#Question 3>

xx=cor(some.rural.poverty.continuous)
xx
corrplot(xx,method="number")
corrplot(xx,method="ellipse")

#Correlation matrix with p values.

library(Hmisc)
yy=rcorr(as.matrix(some.rural.poverty.continuous))
yy


#Variance Inflation Factors (VIF)83 
#Measure of Multicollinearity -- correlation of independents  
#How much the variance of a beta coefficient is being inflated by multicollinearity.
library(car)
vif(continuous.regout)

#if(continuous.regout)
 # percollege perchildpoverty 
#1.136026        1.136026


#we can see no collinearity exists between percollege and perchildpoverty. Hence, 
#multicollinaery doesnt exist between the dependent variables


#Question 4>
#evaluating the standardized residuals.
stdresids=rstandard(continuous.regout)
plot(continuous.regout$fitted.values,stdresids)
abline(0,0,col="red",lwd=3)
#Paste the output
#From this we can see there is one outlier


#We have an outlier 

boxplot(some.rural.poverty.continuous$perelderlypoverty)
max(some.rural.poverty.continuous$perelderlypoverty)

#19.2

mean(some.rural.poverty.continuous$perelderlypoverty)
#11.24143

# This statement finds the data frame row that's the max value.
 which(some.rural.poverty$perelderlypoverty==19.2)
 some.rural.poverty[42,]
 #BROWN
 
 #Question 5>
 #Creating a new dataset with only continuous variables for metro dataset
 some.metro.poverty.continuous= some.metro.poverty[,-c(1,2,3,16)]
 
 predict(continuous.regout,some.metro.poverty.continuous,interval="predict")
 predict(continuous.regout,some.metro.poverty.continuous,interval="confidence")
 a=data.frame(predict(continuous.regout,some.metro.poverty.continuous))
names(Accuracy)=c("Actual","Fitted","Error")
Accuracy=data.frame(some.metro.poverty.continuous$perelderlypoverty,a$Fit,some.metro.poverty.continuous$perelderlypoverty-a$Fit)
Accuracy$Percentage=abs((1-Accuracy$Actual/Accuracy$Fitted)*100)


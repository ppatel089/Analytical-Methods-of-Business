rm(list=ls())
library(readxl)
countries_population=read_excel("6304 Regression Project Data.xlsx",sheet="midwest")
colnames(countries_population)=tolower(make.names(colnames(countries_population)))
attach(countries_population)
countries_population["popcollege"] <- NA 
countries_population$popcollege <- countries_population$poptotal * countries_population$percollege/100
countries_population["popprof"] <- countries_population$poptotal * countries_population$perprof/100
countries_population["childadultratio"] <- countries_population$popchild/countries_population$popadult
countries_population["popchildpoverty"] <- countries_population$popchild * countries_population$perchildpoverty/100
#countries_population["popelderlypoverty"] <- countries_population$popchild * countries_population$perelderlypoverty/100
rural_countries<-subset(countries_population, inmetro == 0)
metro_countries<-subset(countries_population, inmetro == 1)
View(rural_countries)
set.seed(55424397)
some.rural.poverty=rural_countries[sample(1:nrow(rural_countries),60,replace=FALSE),]
colnames(some.rural.poverty)=tolower(make.names(colnames(some.rural.poverty)))
attach(some.rural.poverty)
some.metro.poverty=metro_countries[sample(1:nrow(metro_countries),30,replace=FALSE),]
colnames(some.metro.poverty)=tolower(make.names(colnames(some.metro.poverty)))
attach(some.metro.poverty)


#Taking continuous variables of the rural dataset by creating a new data object

continuous.some.rural.poverty= some.rural.poverty[,-c(1,2,3,16)]

#Checking the co-relation among all the continuous variables

xx=cor(continuous.some.rural.poverty)
par(mfrow(1,1))
corrplot(xx,method="circle")

#put interpretation

#Performing the Linear Reg by including all the continuous variables for check
cont.regout1=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty)
summary(cont.regout1)
#Paste the output
#No importance of any dependent variable and R-squared value is 58% . We can also see the same relationship
#from the correlation graph

#Performing the Linear Reg.by considering only the percentage variables
cont.regout2=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty[,c(9:12)])
summary(cont.regout2)
#Imporatnce is only for perchildpoverty. We can also see the same relationship
#from the correlation graph
#R square value is 43%. One vairable is are significant. 

#Adding area
cont.regout3=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty[,c(1,9:12)])
summary(cont.regout3)
#Since there is no change in the R-square value, it looks like the area variable has not 
#much contribution to this.So will ignore area. 

#Removing area, adding poptotal along with percentage fields
cont.regout4=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty[,c("poptotal","perchildpoverty")])
summary(cont.regout4)
# poptotal and perchildpoverty are significant. 
# R-square value is 51%. (BEST FIT)

#So, trying to add the percentage parts of these variables( percollege and perprof)to check
cont.regout5=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty[,c(9:10)])
summary(cont.regout5)
# ending up in decreasing the R2 sq value significantly.
# significance of percollege

# Finally, removing the Perprof and selecting only the percollege and perchildpoverty
cont.regout6=lm(continuous.some.rural.poverty$perelderlypoverty~.,data=continuous.some.rural.poverty[,c(9,11,12)])
summary(cont.regout6)
# R-square is 43%,percollege and perchildpoverty is considered to be significant.

#Question 2> LINE Assumption for the best fit model selected above
#Linearity
#plot(some.rural.poverty.continuous$perelderlypoverty,some.rural.poverty.continuous$percollege,pch=19)
#plot(some.rural.poverty.continuous$perelderlypoverty,some.rural.poverty.continuous$perchildpoverty,pch=19)

subset_s1=subset(continuous.some.rural.poverty,select=c("perelderlypoverty","poptotal","perchildpoverty"))
plot(subset_s1)
#fairly linear


#Independence:
cor(continuous.some.rural.poverty$poptotal,continuous.some.rural.poverty$perchildpoverty)
-0.04262536
#From the variables in the model we can see that there is no correlation between the independent variables

#Normality and Equality of Variance

par(mfrow=c(2,2))
plot(cont.regout4) 
#interpret results

#Question 3>

xx=cor(subset_s1)
View(xx)
xx
corrplot(xx,method="circle")
par(mfrow(1,1))
corrplot(xx,method="number")

#Correlation matrix with p values.
install.packages("Hmisc")
library(Hmisc)
yy=rcorr(as.matrix(subset_s1))
View(yy)
yy


#Variance Inflation Factors (VIF)
#Measure of Multicollinearity -- correlation of independents  
#How much the variance of a beta coefficient is being inflated by multicollinearity.
#Evidence of Multicollinearity
library(car)
vif(cont.regout4)

#multicollinaery doesnt exist between the dependent variables.

#Question 4>
#evaluating the standardized residuals.
standard_residuals=rstandard(cont.regout4)
par(mfrow(1,1))
plot(cont.regout4$fitted.values,standard_residuals)
abline(0,0,col="red",lwd=3)
#From this we can see there is one outlier


#We have an outlier 

boxplot(continuous.some.rural.poverty$perelderlypoverty)
max(continuous.some.rural.poverty$perelderlypoverty)

#19.87753

mean(continuous.some.rural.poverty$perelderlypoverty)
#12.50

#Max value.
which(some.rural.poverty$perelderlypoverty==19.87)
#Leverage of Points
lev=hat(model.matrix(cont.regout4))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
a=some.rural.poverty[lev>(3*mean(lev)),]
a
#Question 5>
#Creating a new dataset with only continuous variables for metro dataset
continuous.some.metro.poverty= some.metro.poverty[,-c(1,2,3,16)]
View(continuous.some.metro.poverty)
p=data.frame(predict(cont.regout4,continuous.some.metro.poverty,interval="predict"))
p
predict(cont.regout4,continuous.some.metro.poverty,interval="confidence")

##Running mutiple regression on some.metro.poverty
regout_metro=lm(perelderlypoverty~.,data=continuous.some.metro.poverty[,c("poptotal","perchildpoverty")])
summary(regout_metro)



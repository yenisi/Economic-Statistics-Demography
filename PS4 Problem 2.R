install.packages("DescTools")
library(dineq)
library(ineq)
rm(list=ls())
household=c(9.67,34.36,38.78,8.45,5.18,1.64,1.07,0.85)
income=c(2.44,16.86,33.87,13.18,12.98,6.32,5.72,8.63)
#it is ordered according to income levels so we don't need to order it
#o=order(income);o
#cumpropop=cumsum(household[o]/100);cumpropop
#cumpropincome=cumsum(income[o]/100);cumpropincome
#data=data.frame(cumpropop,cumpropincome)
#plot(cumpropop,cumpropincome)

x=c(0,1)
y=c(0,1)
plot(cumsum(household/100),cumsum(income/100), type='l', main="Lorenz curve",ylab="Cumulative proportion of income", xlab="Cumulative proportion of population")
LorenzArea=gini.wtd(income,weight=NULL)/2;LorenzArea
library(DescTools)
Gini(income, unbiased=FALSE)/2

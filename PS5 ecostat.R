rm(list=ls())
p=c(11.38,8.35,15.49,16.09,18.31,15.40,6.24,4.83,3.9)
prop=p/sum(p);prop
px=rev(cumsum(rev(p)))/100;px
x=c(0,500,1000,2500, 5000,10000,20000,30000,50000)
data=data.frame(x,p,prop,px);data
x1=log(x[-1]);x1
y1=log(px[-1]);y1
l=lm( y1~ x1);l
nu=-0.6508
matplot(x1,y1,type='l',col="red")
abline(l)

#After truncating
x0.hat=7500 #(estimated by the mode)
pstar=c(0.18311831/2,prop[6:9]);pstar
prop1=pstar/sum(pstar);prop1
px1=rev(cumsum(rev(pstar)))/sum(pstar);px1
x1=c(7500,10000,20000,30000,50000)
data=data.frame(x1,pstar,prop1,px1);data
l1=lm(log(px1)~log(x1));l1
matplot(log(x1),log(px1),type='l',col="red")
abline(l1)

nu.hat=1.20178
#nu*ln(x0)=10.797
#x0=exp(10.797/nu);x0

#when there is no estimation of x0 using the mode during trujcation we can use the estimate from the fitting (lm)

#fitted values
predicted=(x0.hat/x1)^(nu.hat);predicted

Gini.Coefficient=1/(2*nu.hat-1);Gini.Coefficient

#2
rm(list=ls())
x=c(0,11,15,21,28,43);x
p=c(23.76,21.20,22.53,15.45,11.51,5.55);p
prop=p/sum(p);prop
px=rev(cumsum(rev(p)))/sum(p);px
eta=qnorm(1-px);eta
l=lm(eta[-1]~log(x[-1]));l
sigma=1/1.695;sigma
mu=4.736/1.695;mu

#fitted values
?pnorm
px.hat=1-pnorm((log(x)-mu)/sigma);px.hat

data=data.frame(x,p,prop,px,eta,px.hat);data
Gini.Coefficient=2*pnorm(sigma/sqrt(2))-1
Gini.Coefficient

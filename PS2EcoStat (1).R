rm(list=ls())

q1=c(8.69,1.51,7.66)
q2=c(7.94,2.73,8.19)
q3=c(7.58,2.43,7.75)
q4=c(8.28,2.78,7.71)

p1=c(37,37,32)
p2=c(39,32,20)
p3=c(52,41,31)
p4=c(52,44,36)

l12=sum(q1*p2)/sum(q1*p1);l12
l23=sum(q2*p3)/sum(q2*p2)
l34=sum(q3*p4)/sum(q3*p3)

l14=l12*l23*l34;l14 #chain based index number

I14_fixed=sum(p4*q1)/sum(p1*q1); I14_fixed # fixed-base index number

#2

rm(list=ls())

q1=c(871,199)
q2=c(706,179)
q3=c(724,214)
q4=c(627,288)

p1=c(202,577)
p2=c(320,767)
p3=c(311,884)
p4=c(302,799)

#Laspeyres'=Lasp


data=cbind(q1,q2,q3,q4,p1,p2,p3,p4);data
data[,5]
Lasp=0
for(i in 1:3)
{
Lasp[i]=sum(data[,i]*data[,i+5])/sum(data[,i]*data[,i+4])
}
ChainLasp=prod(Lasp)*100;ChainLasp

#paas=paasches
Paas=0
for(i in 1:3)
{
Paas[i]=sum(data[,i+1]*data[,i+5])/sum(data[,i+1]*data[,i+4])
}
ChainPaas=prod(Paas)*100;ChainPaas

#Fishers=Fish
FishChain=(ChainLasp*ChainPaas)^(0.5);FishChain

#EM

EM=0
for(i in 1:3)
{
EM[i]=sum(data[,i+5]*(data[,i]+data[,i+1]))/sum(data[,i+4]*(data[,i]+data[,i+1]))
}
EM
prod(EM)




rm(list=ls())
#2
year=1:18
population=c(9.738,12.966,17.169,23.292,31.543,38.658,50.256,63.048,75.095,92.072,105.311,122.875,131.769,150.797,179.423,182.826,189.213,193.816)
n=length(population)
x=1/population[-n];x
y=1/population[-1];y

l=lm(y~x);l

A=0.001228
B= 0.734933 
r=-log(B);r
L=(1-B)/A;L

N=18
t=0:(N-1)
t
Beta=((N-1)/2)+((1/(N*r))*sum(log((L/population)-1)));Beta

fitted.pop=L/(1+exp(r*(Beta-t)));fitted.pop

matplot(t,cbind(population,fitted.pop),type='l')

#3
fem.pop=c(152.7,164.2,176.1,177.6,185.2,167.1,150.3)
births=c(2971,22773,28245,19548,11462,3235,217)
yearslived=c(4813,4790,4761,4726,4685,4632,4552)

#i: GFR

GFR= (sum(births)/(sum(fem.pop)*1000))*1000; GFR

#II: ASFR

ASFR=(births/fem.pop*1000)*1000;ASFR

#III: grr
maletofemale=1.05
fem.birth=(1/2.05)*births;fem.birth
GRR=sum(5*fem.birth/fem.pop);GRR #we multiply by 5 since quinquennial age groups are given

#IV
f_l0=1000
#f_Lx=years lived by Lx persons=yearslived

f_ix=fem.birth/fem.pop
NRR=sum(f_ix*yearslived)/f_l0;NRR

#since 1184/1000>1, there's a growth in the population
#If f_lx is given we just replace f_Lx by f_lx, rest is same
#If pi_x is given then we multiply by 5
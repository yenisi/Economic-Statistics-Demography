#1
rm(list=ls())
p0=439234771
p1=548159652
#linear growth 
p1-p0
t=c(2,3)
p=p0+(p1-p0)*t;p

#exponential growth
b=p1/p0;b
P=p0*b^t;P
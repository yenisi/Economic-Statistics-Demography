#1
rm(list=ls())
age=seq(17,47,5);age
fempop=c(84.79,70.01,72.66,75.92,75.10,71.62,66.66)
birth=c(2343,14541,16736,10210,5134,1422,93)
totalpop=2285800
cbr=sum(birth)*1000/totalpop;cbr
gfr=sum(birth)*1000/(sum(fempop)*1000);gfr
asfr=birth*1000/(fempop*1000);asfr
sum(asfr)
tfr=5*sum(asfr);tfr
plot(age,asfr,type='h')

#2
rm(list=ls())
fempop=c(9000,9200,8900,8600,8400,8500)
birth=c(140,1312,1067,771,468,160)
survivalfactor=c(0.92,0.914,0.908,0.891,0.878,0.869)
asfr=(birth)*1000/fempop;asfr
tfr=sum(asfr)*5;tfr
grr=tfr*48.7/100;grr
nrr=sum(asfr*survivalfactor)*48.7/100;nrr

#3
rm(list=ls())
fempop=c(3940607,2996164,3093865,3061298,2344824,1986587,1498082,1302680)
ASDR=c(1,2.3,2.9,3.4,4.3,4.5,4,5.5);ASDR
ASFR=c(0,68.6,214.9,164,85.3,41.3,22.2,7.2)/1000;ASFR
TFR=sum(ASFR)*5;TFR
k=1.05; #k=f_B/B
f=1/(1+k);f #proportion of female birth
GRR=(f*TFR);GRR #round()
fbx=f  
nPIx=(1-ASDR/1000); nPIx #survival factor 
NRR=(sum(ASFR*(1/2.05)*nPIx));NRR #round()
#barplot? part iv
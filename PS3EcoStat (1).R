 rm(list=ls())
#a
wt=c(350,156,187,108,76,123)
q1_2004=c(117,113,118,112,102,121) #GI2004
q1_2005=c(120,118,0,117,111,125)    #GI2005
CLI_2004=sum(q1_2004*wt)/sum(wt)

#b
GroupIndexHousing=(119.5*sum(wt)-sum(q1_2005*wt))/187;GroupIndexHousing

#c
IncreaseinExp=5000*120/117 - 5000;IncreaseinExp

#d
Relativechange=(1.1-0.8*119.5/115.375-0.2)/0.2

#2
#a
rm(list=ls())
wt=c(65.3,4.8,8.5,7.6,13.8)
GI=c(212.45,328.06,345.89,173.41,201.35)
CLI=sum(GI*wt)/sum(wt)

#c
increase_in_price=2400*CLI/100 - 4950
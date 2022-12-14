items=c("rice", "wheat", "pulses", "sugar", "salt", "milk", "oil", "fish", "egg", "tea")
price1961=c(0.75,0.46,1.03,1.25,0.15,1.20,3.85,3.55,2.05,0.06)
price1971=c(1.4,0.98,2.1,1.8,0.25,1.85,6.2,6.5,3.65,0.1)
avgexp=c(40.2,15.8,12.6,7.2,0.6,19.4,14.2,80.8,8.2,10.3)
simp.agg.p.in.no=sum(price1971)*100/sum(price1961)

wt=avgexp/price1971

wtd.agg=sum(price1971*wt)*100/sum(price1961*wt)

commodity=c("A","B","C","D","E")
price1978=c(2,5,1.5,10,8)
price1982=c(2.5,4,2,12,5.5)
qty1978=c(4000,500,1500,0,2500)
qty1982=c(4500,800,900,260,5000)

#a

sum(price1982/price1978)*100/5

#b

laspyresprice=sum(price1982*qty1978)*100/sum(price1978*qty1978)
laspyres.qty=sum(qty1982*price1978)*100/sum(qty1978*price1978)


#c

paascheprice=sum(price1982*qty1982)*100/sum(price1978*qty1982)
paascheqty=sum(qty1982*price1982)*100/sum(qty1978*price1982)

#d
fisherprice=sqrt(laspyresprice*paascheprice)
fisherqty=sqrt(laspyres.qty*paascheqty)

#e

edgeworthmarshallsprice  =sum(price1982*(qty1982+qty1978))/sum(price1978*(qty1982+qty1978))

#f

valueindexno=sum(price1982*qty1982)*100/sum(price1978*qty1978)
fisherprice*fisherqty/100
 



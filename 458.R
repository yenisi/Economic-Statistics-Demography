#Problem set - 3
#Question - 2, part d

rm(list = ls())

#Given data
group=c("Food", "Clothing", "Fuel & Light", "House Rent", "Miscellaneous")
group_index = c(212.45, 328.06, 345.89, 173.41, 201.35)
weight = c(65.3, 4.8, 8.5, 7.6, 13.8)
#intial salary in the year 1981 = rupees 2400
initial_salary = 2400
#salary in the current year 1995 = rupees 4950
current_salary = 4950

#Computing cost of living index number of 1995
CLI_1995 = sum(group_index*weight)/sum(weight); CLI_1995
CLI_1981 = 100

#Spendings on the first four groups
S = (weight[-5] * CLI_1995 * initial_salary) / (sum(weight) * CLI_1981); S

#Spendings on miscellaneous
M = current_salary - sum(S); M

#Weights of first four groups
W = (S / (sum(S) + M)) * 100; W

#Weight of miscellaneous
W_M = 100 - sum(W); W_M

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Problem set - 4
#Question - 2

rm(list = ls())

#Given data
annual_disposable_income = c("-500", "500-1000", "1000-2000", "2000-3000", "3000-5000", "5000-7000", "7000-10000", "10000-")
percent_household = c(9.67, 34.36, 38.78, 8.45, 5.18, 1.64, 1.07, 0.85)
percent_share = c(2.44, 16.86, 33.87, 13.18, 12.98, 6.32, 5.72, 8.63)

#Calculating proportion of household
proportion_household = percent_household/100;proportion_household

#Calculating proportion of share in disposable income
proportion_share = percent_share/100;proportion_share

#Calculating cumulative proportion of household taking z_0 = 0
cumulative_proportion_household = c(0, cumsum(proportion_household)) ;cumulative_proportion_household

#Calculating cumulative proportion of share in disposable income taking z_0 = 0
cumulative_proportion_share = c(0, cumsum(proportion_share));cumulative_proportion_share

#Plotting the Lorenz Curve taking z_0 = 0
plot(cumulative_proportion_household,cumulative_proportion_share,type='l',col="darkred",lwd=2,main="Lorenz Curve",ylab="Cumulative proportion of share in disposable income", xlab="Cumulative proportion of household (from lowest to highest income group)")
lines(cumulative_proportion_household,cumulative_proportion_household, lwd=2, col="darkgreen")
legend("topleft", legend = c("Line of Equality","Lorenz Curve"), col = c( "darkgreen","darkred"),lty = 1:1, lwd=2:2, cex = 0.8)
axis(side = 1)

#Calculating Gini's Coefficient and Area of Concentration 
S = 0

for (i in 1:8) 
{
  S[i] = cumulative_proportion_share[i] + cumulative_proportion_share[i+1]
}

gini_coefficient = 1 - sum(S*proportion_household)
gini_coefficient

area_of_concentration = 0.5*gini_coefficient 
area_of_concentration

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



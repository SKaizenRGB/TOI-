#                 BOND AMORTIZATION SCHEDULE: PREMIUM

#  The Tesla Powers Company issued Php 200,000.00 of a five year par bond
#  paying a coupon rate of 12% convertible semi-annually and a yield effective 
#  rate of 10%.Generate and complete a Bond Premium Amortization Schedule for 
#  Tesla Powers Company.


#Given:

F <- 200000 #par value
C <- 200000 #payment at the date of redemption
r <- 0.12   #coupon rate
i <- 0.10   #yield to maturity
n <- 5      #no. of coupon payments

#Formulas:

m <- n*2 #since semi-annually
j <- i/2 #convertible semi-annually
s <- r/2 #semi-annually coupon rate

B_t <- F*s*((1-((1+j)^-m))/j) + C*((1+j)^-m) #Book Value formula

cp <- C*s #coupon payment

options(scipen=999) #to disable scientific notation

#Bond_Premium_Amortization_Schedule:

r_1 <- c(0,0,0,B_t) #row one

Bond_Premium_Amortization_Schedule <- matrix(ncol=4, nrow=1+m)

Bond_Premium_Amortization_Schedule[1,] <- r_1

for(period in 1: m) 
{cp           #coupon payment
  ie <- B_t*j  #interest expense
  pav <- cp-ie #premium amortized value
  
  B_t <- B_t-pav
  
  Bond_Premium_Amortization_Schedule[period+1,] <- c(cp,ie,pav,B_t)}

#Bond Premium Amortization Schedule legends:

colnames (Bond_Premium_Amortization_Schedule) = 
  c( "Coupon Payment","Interest Expense","Premium Amortized","Book Value (Php)")

rownames (Bond_Premium_Amortization_Schedule) = c(0,1:m)

library(pander)  
pandoc.table(Bond_Premium_Amortization_Schedule, style = "grid", split.tables = Inf)


#                         SINKING FUND SCHEDULE

#   Consider a loan for Php 300,000.00 to be paid in 5 annual payments at 
#   6% rate per period. The lender charges an interest at 8% rate per period. 
#   Generate and complete a Sinking Fund Schedule.


#Given:

L <- 300000 #loan
j <- 0.06 #sinking fund interest rate
i <- 0.08 #interest rate 
n <- 5

#Formulas:
I <- L*i  
D <- L/(((((1+j)^n) - 1))/j) #Sinking Fund Deposit Formula

options(scipen=999) #to disable scientific notation

#Sinking_Fund_Schedule:

r_1 <- c(0,0,0,0,L) #row one

Sinking_Fund_Schedule <- matrix(ncol=5, nrow=1+n)

Sinking_Fund_Schedule[1,] <- r_1

A <- 0

for(period in 1: n) 
{ D                   #Sinking Fund Deposit
  I                   #Interest Payment
  IE <- A*j           #Interest on the Sinking Fund 
  A <- A + D + IE     #Amount in the Sinking Fund
  L <- L - D - IE     #Net Loan Amount
  
  Sinking_Fund_Schedule[period+1,] <- c(D,I,IE,A,L)}

#Sinking_Fund_Schedule legends:

colnames (Sinking_Fund_Schedule) = 
  c("SF Deposit","Interest Payment","IESF","Amount in SF","Net Loan Amount(Php)")

rownames (Sinking_Fund_Schedule) = c(0,1:n)

library(pander)  
pandoc.table(Sinking_Fund_Schedule, style = "grid", split.tables = Inf)

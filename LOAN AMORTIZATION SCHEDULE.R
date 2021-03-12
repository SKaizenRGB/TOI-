#   LOAN AMORTIZATION SCHEDULE

#   Consider a PAG-IBIG Home mortgage loan for Php 100,000.00 at 6% nominal annual
#   rate with equal monthly payments for 30 years. What are the
#   characteristics of this loan? Generate and complete a Loan Amortization Schedule.

#Given:

L <- 100000 #loan
i <- 0.06 #nominal annual rate
n <- 30 #no. of payments

#Formulas:

m <- n*12 #total number of payments
j <- i/12 #convertible monthly
PA <- L/((1-((1+j)^-m))/j)  #Amortization Formula for each monthly payment

options(scipen=999) #to disable scientific notation

#Loan_Amortization_Schedule:

r_1 <- c(0,0,0,L) #row one

Loan_Amortization_Schedule <- matrix(ncol=4, nrow=1+m)

Loan_Amortization_Schedule[1,] <- r_1

for(period in 1: m) 
{ PA
  IP <- L*j #interest paid
  PR <- PA-IP #Principal Repaid
  L <- L-PR #Outstanding Balance
  Loan_Amortization_Schedule[period+1,] <- c(PA,IP,PR,L)}

#Loan_Amortization_Schedule legends:

colnames (Loan_Amortization_Schedule) = 
  c( "Payment Amount","Interest Paid","Principal Repaid","Outstanding Balance(Php)")

rownames (Loan_Amortization_Schedule) = c(0,1:m)

library(pander)  
pandoc.table(Loan_Amortization_Schedule, style = "grid", split.tables = Inf)
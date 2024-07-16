# Project exercises in Quantitative Finance course

# Monte Carlo Simulation of option prices

# Importing libraries with functions for options, summaries, visualization and tools for importing data
library('derivmkts')
library('data.table')
library('ggplot2')

#Importing stock price data 
StockPrices<-read.table(file="StockPrices.txt",header=TRUE)
str(StockPrices)

# Creating a new dataframe of only returns during 2023
yearlyprices <- StockPrices[StockPrices$yyyymmdd >= 20230101 & StockPrices$yyyymmdd <= 20231231,]
head(yearlyprices)

# Stock price volatility (Ticker VZ)
v <- sd(diff(log(yearlyprices$VZ)))*sqrt(252) #volatility estimate sigma_hat. Prices are first converted to log returns
v <- round(v, 4)
d <- 0 # In this example we will use a non-dividend paying stock as the underlying asset
K <- 39 #Strike price
Price <- 40.52 #Last price of the stock


library('quantmod')
rate <- getSymbols(Symbols='DGS3MO',src='FRED',auto.assign =FALSE) # Importing the 3-month risk-free rate. Today = 16.2.2024
rfree <- as.numeric(rate[time(rate) =='2024-02-16'])/100 #Annualized, unit-less

#Expiration in March
TTE <- round((3-2)/12,2) #Time-to-maturity in years (simplified by only accounting for months)


#Monte Carlo Simulation
RR <- 10000 # Number of replicas
set.seed(1)

myST<-function(S0,mu,sigma,TT,RR)
{
  # The function for MCS re-written in R
  part1<-S0*exp((mu-sigma^2/2)*TT) 
  part2<-sigma*sqrt(TT)
  ST<-part1*exp(part2*rnorm(n=RR))
  
  return(ST)
}

ST<-myST(S0=Price,mu=rfree,sigma=v,TT=TTE,RR=RR) # Using the created function for the simulation
OPMCS <- round(ST, 4) # Option price
OPMCS
str(OPMCS)










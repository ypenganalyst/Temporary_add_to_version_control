#load required package
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)

# Use 2-stage DDM model to calculate a given stock, in today's 10-year interest rate,
# change Number_of_g1 years, which is future cash flow, g2 is terminal growth
# MRP is 100 years average stock risk premium

Stock_Reco <- function(Givenstock_ticker, Number_of_g1_years, g2, MRP){
  dividend_startdate = as.Date("2016-03-01") 
  dividend_last6 <- getDividends(Givenstock_ticker, 
                                 from = dividend_startdate,
                                 to = Sys.Date(), 
                                 env = .GlobalEnv, 
                                 src = "yahoo", 
                                 auto.assign = TRUE, 
                                 auto.update = TRUE, 
                                 verbose = FALSE)
  dm <- matrix(data = dividend_last6 , nrow= 4)
  colnames(dm) <- c(2016:2021)
  rownames(dm) <- c(1:4)
  totalD <- c(sum(dm[1:4,1]), sum(dm[1:4,2]), sum(dm[1:4,3]), sum(dm[1:4,4]), sum(dm[1:4,5]), sum(dm[1:4,6]))
  dm <- rbind(dm, totalD)
  percentage.change <- Delt(c(dm[5,1:6]))
  g1 <- mean(percentage.change[2:6,1])
  mostrecent.dividend <-  dm[5,6]
  future4.dividend <- c(dm[5,6]* (1+g1)^(1:4))
  last.cf <- future4.dividend[4]
  start = as.Date("2012-03-09") 
  end = as.Date("2022-03-09")
  A_stock <- getSymbols(Givenstock_ticker, src = "yahoo", from = start, to = end, verbose = FALSE, auto.assign = FALSE)
  sp500_stock <- getSymbols("^GSPC", src = "yahoo", from = start, to = end, verbose = FALSE, auto.assign = FALSE)
  Prices <- cbind( A_stock , sp500_stock)
  My_prices <- cbind(Prices[,6], Prices[,12])
  next_year <- tail(data.frame(My_prices), -1)
  base_year <- head(data.frame(My_prices), -1)
  return <- next_year/base_year
  real_return <- as.xts(return - 1)
  beta_10 <- lm(real_return[,1] ~ real_return[,2] -1, data.frame(real_return))
  realbeta_10 <- beta_10[["coefficients"]]
  t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
  risk_free <- t10yr[length(t10yr)]/100
  r = risk_free + realbeta_10 * MRP
  terminal_value <- last.cf * (1+g2)/(r-g2)
  Total_futurecashflow <- sum(future4.dividend)+terminal_value
  NPV_givenstock <- Total_futurecashflow/(1+r)^ Number_of_g1_years
  dt <- getQuote(Givenstock_ticker,src = "yahoo", what = standardQuote())
  stockprice_today <- dt[1,5]
  My_recommendation <- ifelse(NPV_givenstock <= stockprice_today, "Sell","Buy")
  print(My_recommendation)
}

Stock_Reco("XOM",4,0.02,0.06)
Stock_Reco("AAPL",5,0.03,0.08)  


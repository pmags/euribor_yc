### Parameter
##Imports and cleans data for analysis

#Load necessary packages
library(YieldCurve)
library(xts)
library(Quandl)

#Imports Euribor information using Quandl API and package
#(attention: more than 50 calls a day requires a token)
search_date <- as.Date(Sys.Date()-2)
 
bundesbank_codes <- c("BUNDESBANK/BBK01_ST0310","BUNDESBANK/BBK01_ST0316","BUNDESBANK/BBK01_ST0325","BUNDESBANK/BBK01_ST0334","BUNDESBANK/BBK01_ST0343")
names(bundesbank_codes) <- c("Euribor 1M", "Euribor 3M", "Euribor 6M", "Euribor 9M", "Euribor 12M")
spot_euribor <- Quandl (bundesbank_codes, 
                        api_key="uEV_9ozUK1EEBXnDaXYy",
                        type = "xts",
                        start_date= search_date, 
                        end_date= search_date)

## Creates the spot yield curve for euribor
#Svensson parameters calculation
maturity_months <- c(1,3,6,9,12)
svensson_parameters <- Svensson(rate= spot_euribor, 
                                maturity = maturity_months)

#Calculation of the actual yield curve
maturity_previsions <- seq(1:12)
euribor_yc_linear_spot <- Srates(Coeff = svensson_parameters, 
                                 maturity = maturity_previsions, 
                                 whichRate = "Spot")

colnames(euribor_yc_linear_spot) <- maturity_previsions
plot(maturity_previsions,euribor_yc_linear_spot)

## In order to know the future evolution of the Euribor curve we bootstrp the forward values from the spot. 
##We fill the gaps in the spot curve using the linear function created above
# The bootstrap is based on the concept of non arbitrage in investment. 
#This means that investing for 2 years at a spot rate of S2 is equal to invest for one year at S1 and reinvest for one year at a Fwrd 1 rate 

# creates a Spot curve for 4 years for the given period
fwrd_maturity <- seq(1:48)
spot_euribor_curve <- Srates(Coeff = svensson_parameters, 
                             maturity = fwrd_maturity, 
                             whichRate = "Spot")

colnames(spot_euribor_curve) <- fwrd_maturity

# Bootstrap function

bootstrap<- function(future_period, interval = 1) {
  
  if(future_period > interval){
  
  (((1+spot_euribor_curve[,future_period])^(future_period))/((1+spot_euribor_curve[,future_period-interval])^((future_period-interval)))-1)
  
  } else {
      message("Not enough information!")
    }
}

# Results for 4 years (1:48) months
results<- c()
for (i in 1:48) {
    results[i] <- bootstrap(i)
      
}
results<- t(as.data.frame(results))
results<- xts(results, order.by = search_date)

# Extract values to create a linear forward yield curve 
fwrd_time_moments <- results[,c(6,12,18,24,30,36,42,48)]

# Applies Svensson to create the future euribor(interval)
fwrd_svensson_parameters <- Svensson(rate= fwrd_time_moments, 
                                maturity = c(6,12,18,24,30,36,42,48))


fwrd_maturity_previsions <- seq(1:48)
euribor_yc_linear_fwrd <- Srates(Coeff = fwrd_svensson_parameters, 
                                 maturity = fwrd_maturity_previsions, 
                                 whichRate = "Spot")

colnames(euribor_yc_linear_fwrd) <- fwrd_maturity_previsions
 




library(bsts) # Need our local install for this one
.libPaths(paste0(getwd(), "/packages"))
library(gtrendsR)
library(tidyquant)
library(gridExtra)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(forecast)
library(dplyr)
source("packages/helpers.R")



ticker <- "W"



# Grid Search-------------------------------------------------------------------



state.sigmas <- seq(0.1,1,.1)
observation.sigmas <- seq(1,10, 1)

hypers <- data.frame()

for (state.sigma in state.sigmas) {
  for (observation.sigma in observation.sigmas) {
    days <- seq(Sys.Date() - 90, Sys.Date(), 1)
    days <- days[wday(days) %in% 2:6]
    buySellMarks <- data.frame()
    funds <- 10000
    stocks <- 0
    for (day in days) {
      dailyPrice <- getSymbols(
        ticker,
        src = "yahoo",
        from = "2018-01-01",
        to = day,
        auto.assign = FALSE
      )
      names(dailyPrice) <- gsub(paste0(ticker, "."), "", names(dailyPrice))
      end <- tail(dailyPrice, 1)
      dailyPrice <- dailyPrice[1:nrow(dailyPrice) - 1]
      
      ss <- AddLocalLevel(list(), log(dailyPrice$Low))
      model <- bsts(log(dailyPrice$Low),
                    state.specification = ss,
                    niter = 1000,
                    sigma.prior = SdPrior(sigma.guess = state.sigma),
                    y.sigma.prior = SdPrior(sigma.guess = observation.sigma)
                    )
      
      # Prediction
      
      pred <- predict(model, horizon = 1)
      
      buy <- unname(exp(apply(pred$distribution, 2, summary))[, 1][2])
      sell <- unname(exp(pred$mean))
      
      print(paste0("Sell at ", sell))
      print(paste0("Buy at ", buy))
      
      if (unname(end$High)[[1]] > sell && stocks > 0) {
        if (unname(end$Low)[[1]] > sell)
        {
          funds <- funds + (stocks * unname(end$Low)[[1]])
          stocks <- stocks - stocks
        } else {
          funds <- funds + (stocks * sell)
          stocks <- stocks - stocks
        }
      }
      if (buy > unname(end$Low)[[1]] && funds > 0) {
        bought <- floor(funds / 2 / buy)
        stocks <- stocks + bought
        funds <- funds - bought * buy
      }
      print(paste0("Funds at ", funds))
      print(paste0("Stocks at ", stocks))
      print(paste("End of day", day))
      buySellMarks <- rbind(
        buySellMarks,
        data.frame(
          date = day,
          buy = buy,
          sell = sell,
          funds = funds,
          stocks = stocks
        )
      )
    }
    results <- data.frame(state.sigma = state.sigma,
                          observation.sigma = observation.sigma,
                          funds = funds,
                          stocks = stocks)
    hypers <- rbind(hypers, results)
  }
}

dailyPrice <- getSymbols(ticker, 
                         src = "yahoo", 
                         from = "2018-01-01", to = Sys.Date(), 
                         auto.assign = FALSE)
buySellMarks$date <- as.Date(buySellMarks$date)
combined <- merge(dailyPrice, buySellMarks)
combined <- na.omit(combined)
combined <- combined[as.Date((Sys.Date() - 90):Sys.Date())]
names(combined) <- gsub(paste0(ticker, "."), "", names(combined))
columns.we.want <- c("High", "Low", "buy", "sell")
trends <- combined[, columns.we.want]
fortune <- combined[,c("stocks", "funds")]

par(mfrow = c(2,1),
    xpd = T)
plot(fortune, type = "l", legend.loc = "bottomright")
plot(trends, type = "l", legend.loc = "top")
grid.arrange(plot1, plot2, nrow = 2, ncol = 1)



# Ignore


# state.sigma <- .1
# observation.sigma <- 1
# bsts(log(dailyPrice$NVDA.Low),
#      state.specification = ss,
#      niter = 1000,
#      state.sigma.prior = SdPrior(sigma.guess = state.sigma),
#      y.sigma.prior = SdPrior(sigma.guess = observation.sigma)
#      )

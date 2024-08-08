# Wayfair Investigation/Visualization

# Exploration for later feature engineering/model configuration



# Setup-------------------------------------------------------------------------



options(scipen = 999)

.libPaths(paste0(getwd(), "/packages"))
library(tidyverse) # Includes ggplot2 & dplyr
library(tidyquant)
library(forecast)
library(statespacer)
library(tseries) # for adf.test
source("helpers/helpers.R")

# Notes

# Lag discussion
# https://github.com/tidyverse/dplyr/issues/1586
# https://github.com/joshuaulrich/xts/issues/131
# https://stackoverflow.com/questions/74222141/lag-not-working-correctly-on-an-r-dataframe



# Declare Global Settings-------------------------------------------------------



tickers <- c("W")
window <- 365 # Sorta spitballing



# Get Daily Pricing Data--------------------------------------------------------



dailyPrice <- xts()
for (ticker in tickers) {
  temp <- getSymbols(ticker,                # Stock symbol
                     src = "yahoo",         # Source of data
                     from = "2015-12-10",   # Start date
                     to = Sys.Date(),       # End date
                     auto.assign = FALSE)   # Assign to temp
  # rather than global environment
  dailyPrice <- merge(dailyPrice, temp)
}
index(dailyPrice) <- as.Date(index(dailyPrice))



# Simple Buy and Hold-----------------------------------------------------------



for(ticker in tickers) {
  
  # dailyReturn issues warnings if it gets NA values 
  # which we expect because we have different lengths. 
  # This is acceptable because dailyReturn trims leading NA values
  returns <- 
    suppressWarnings(dailyReturn(dailyPrice[,paste0(ticker, ".Close")])) 
  returns <- c(rep(NA, nrow(dailyPrice)-nrow(returns)), returns)          
  dailyPrice <- merge(dailyPrice, 
                      matrix(returns, # Returns for the stock
                             ncol=1,  # One column b/c one stock
                             dimnames=list(NULL, # Set rownames to null
                                           paste0(ticker, ".Return"))))   # Set the column name to Ticker.Return
}

results <- getResults(stocks = tickers, # Our vector of ticker symbols
                      period = window, # How may trading days to calculate for
                      data = dailyPrice, # Dataset we want results for
                      strategyReturn = "Return", # String name of the returns for the strategy we're getting results for
                      findings = NULL) # No results exists for first one



# Basic Visualization-----------------------------------------------------------



# Lots of autocorrelation?
# Highs same story as closes (almost identical)

df.dailyPrice <- fortify(dailyPrice)
g <- df.dailyPrice %>%
  ggplot(aes(x = Index)) +
  geom_line(aes(y = W.Close, color = "W")) +
  scale_color_manual(values = c("W" = "magenta")) +
  theme_minimal()
plot(g)



# Close ACF/PACF----------------------------------------------------------------



df_long <- df.dailyPrice %>%
  pivot_longer(cols = starts_with(c("W.Close")),
               names_to = "Ticker", values_to = "Close")

# Compute ACF for each Ticker
acf_data <- df_long %>%
  group_by(Ticker) %>%
  summarise(acf = list(acf(Close, plot = FALSE)$acf)) %>%
  unnest(acf) %>%
  group_by(Ticker) %>%
  mutate(lag = row_number() - 1)

pacf_data <- df_long %>%
  group_by(Ticker) %>%
  summarise(pacf = list(pacf(Close, plot = FALSE)$acf)) %>%
  unnest(pacf) %>%
  group_by(Ticker) %>%
  mutate(lag = row_number() - 1)

# Plot ACF
acf_plot <- acf_data %>%
  ggplot(aes(x = lag, y = acf, color = Ticker)) +
  geom_line() +
  facet_wrap(~ Ticker, scales = "free_y") +
  scale_color_manual(values = c("W.Close" = "magenta")) +
  theme_minimal() +
  labs(title = "ACF Plots of Closing Prices", x = "Lag", y = "ACF") +
  theme(legend.position = "none")

pacf_plot <- pacf_data %>%
  ggplot(aes(x = lag, y = pacf, color = Ticker)) +
  geom_line() +
  facet_wrap(~ Ticker, scales = "free_y") +
  scale_color_manual(values = c("W.Close" = "magenta")) +
  theme_minimal() +
  labs(title = "PACF Plots of Closing Prices", x = "Lag", y = "PACF") +
  theme(legend.position = "none")


# Plot the ACF
plot(acf_plot)

# Plot the ACF
plot(pacf_plot)



# First Models ---------------------------------------------------------------------



arima <- arima(dailyPrice$W.Close, order = c(0, 1, 1))
arima$aic
# 5938.266
residuals <- residuals(arima)
Box.test(residuals, lag = 12, type = "Ljung-Box")
# Box-Ljung test
# 
# data:  residuals
# X-squared = 22.575, df = 12, p-value = 0.03156

# Not stationary yet

# residuals look to be larger in the beginning than the end
plot(residuals(arima))

y <- as.matrix(Cl(dailyPrice))
ss <- statespacer(y = y,
                  local_level_ind = TRUE,
                  initial = 0,#.5 *log(var(head(y, 200))),
                  arima_list = list(c(0, 0, 1)),
                  method = "BFGS",
                  verbose = T)
ss$diagnostics$AIC
# 6.521453 # Way better?

preds <- ss$predicted$yfit + ss$predicted$ARIMA1
residuals <- preds - dailyPrice$W.Close
plot(residuals)
Box.test(residuals, lag = 12, type = "Ljung-Box")
# Box-Ljung test
# 
# data:  residuals
# X-squared = 11.682, df = 12, p-value = 0.4715



# Returns-----------------------------------------------------------------------



# Looks more stationary, but not sure if there's any memory there

g <- df.dailyPrice %>%
  pivot_longer(cols = starts_with(c("W.Return")),
               names_to = "Ticker", values_to = "Return") %>%
  ggplot(aes(x = Index, y = Return, color = Ticker)) +
  geom_line() +
  facet_wrap(~ Ticker, scales = "free_y") +
  scale_color_manual(values = c("W.Return" = "magenta")) +
  theme_minimal() +
  theme(legend.position = "none")
plot(g)

df_long <- df.dailyPrice %>%
  pivot_longer(cols = starts_with(c("W.Close")),
               names_to = "Ticker", values_to = "Return")

# Compute ACF for each Ticker
acf_data <- df_long %>%
  group_by(Ticker) %>%
  summarise(acf = list(acf(Return, plot = FALSE)$acf)) %>%
  unnest(acf) %>%
  group_by(Ticker) %>%
  mutate(lag = row_number() - 1)

pacf_data <- df_long %>%
  group_by(Ticker) %>%
  summarise(pacf = list(pacf(Return, plot = FALSE)$acf)) %>%
  unnest(pacf) %>%
  group_by(Ticker) %>%
  mutate(lag = row_number() - 1)

# Plot ACF
acf_plot <- acf_data %>%
  ggplot(aes(x = lag, y = acf, color = Ticker)) +
  geom_line() +
  facet_wrap(~ Ticker, scales = "free_y") +
  scale_color_manual(values = c("W.Close" = "magenta")) +
  theme_minimal() +
  labs(title = "ACF Plots of Wayfair Returns", x = "Lag", y = "ACF") +
  theme(legend.position = "none")

pacf_plot <- pacf_data %>%
  ggplot(aes(x = lag, y = pacf, color = Ticker)) +
  geom_line() +
  facet_wrap(~ Ticker, scales = "free_y") +
  scale_color_manual(values = c("W.Close" = "magenta")) +
  theme_minimal() +
  labs(title = "PACF Plots of Wayfair Returns", x = "Lag", y = "PACF") +
  theme(legend.position = "none")


# Plot the ACF
plot(acf_plot)

# Plot the ACF
plot(pacf_plot)



# Try again with returns--------------------------------------------------------



returnArima <- arima(dailyPrice$W.Return, order = c(0, 1, 1))
returnArima$aic
# -2804.43
residuals <- residuals(returnArima)
Box.test(residuals, lag = 12, type = "Ljung-Box")
# Box-Ljung test
# 
# data:  residuals
# X-squared = 22.575, df = 12, p-value = 0.03156

# not stationary

# residuals look to be larger in the middle
plot(residuals(returnArima))


# https://miro.medium.com/v2/resize:fit:1100/format:webp/1*ITw1jLNRnTNz-SLL6O-D5A.png

# Try the parados stationarity thing
# Not really working
adfs <- c()
for (d in seq(.1,1,.01)) {
  kmax <- 50
  dailyPrice$Parados <- NULL
  for (k in 0:kmax) {
    if (k == 0){
      dailyPrice$Parados <- 1 - d * lag(dailyPrice$W.Close, 1)
    } else {
      dailyPrice$Parados <- dailyPrice$Parados + (d * (d-1))/factorial(k) * -lag(dailyPrice$W.Close, 1) ^ 2
    }
  }
  adf <- adf.test(na.omit(dailyPrice$Parados))
  adfs <- c(adfs, adf$p.value)
}

adfs <- c()
for (d in seq(.1,1,.1)) {
  dailyPrice$Parados <- NULL
  dailyPrice$Parados <- (dailyPrice$W.Close - lag(dailyPrice$W.Close, 1)) ^ d
  adf <- adf.test(na.omit(dailyPrice$Parados))
  adfs <- c(adfs, adf$p.value)
}

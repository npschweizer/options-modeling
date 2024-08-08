# Setup-------------------------------------------------------------------------



options(scipen = 999)

library(bsts) # Need our local install for this one
.libPaths(paste0(getwd(), "/packages"))
library(gtrendsR)
library(tidyquant)
library(quantmod)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(forecast)
library(dplyr)
library(jsonlite)
source("helpers/helpers.R")


apikey <- readLines("secret-treasure.txt")



ticker <- "FFIE"



# Get Daily Pricing Data--------------------------------------------------------



dailyPrice <- getSymbols(ticker, 
                   src = "yahoo", 
                   from = "2018-01-01", to = Sys.Date(), 
                   auto.assign = FALSE)
names(dailyPrice) <- gsub(paste0(ticker, "."), "", names(dailyPrice))
dailyPrice <- log(dailyPrice)



# Simple Daily Model------------------------------------------------------------



ss <- AddLocalLevel(list(), dailyPrice$Low)
model <- bsts(dailyPrice$Low,
               state.specification = ss,
               niter = 1000)
plot(model)
plot(model, "components")
tail(exp(apply(model$one.step.prediction.errors, 2, mean)))
mean(abs(exp(apply(model$one.step.prediction.errors, 2, mean))))
summary(exp(model$final.state))
exp(tail(dailyPrice, 1))


# Prediction

pred <- predict(model, horizon = 5)
plot(pred, plot.original = 5)
exp(apply(pred$distribution, 2, summary))
apply(pred$distribution, 2, sd)



# With Insider Data-------------------------------------------------------------



insider <- fromJSON(paste0(
  "https://api.gurufocus.com/public/user/", apikey, "/stock/", ticker, "/insider"
))

insider <- insider[[ticker]]
insider <- insider %>% select(date, type, trans_share)
insider <- insider %>% 
  mutate(trans_share = as.numeric(gsub(",", "", trans_share)),
         date = as.Date(date)) %>%
  mutate(insiderSharesSold =  if_else(type == "P", -trans_share, trans_share)) %>%
  select(date, insiderSharesSold) %>%
  group_by(date) %>%
  summarize(insiderSharesSold = sum(insiderSharesSold))
insider <- as.xts(insider)
dailyPrice <- merge(dailyPrice, insider, join = 'left', fill = 0)



# With Insider Data-------------------------------------------------------------



ss1 <- AddLocalLevel(list(), dailyPrice$Low)
model1 <- bsts(Low ~ insiderSharesSold,
              state.specification = ss1,
              data = dailyPrice,
              niter = 1000)
plot(model1)
plot(model1, "components")
plot(model1, "coefficients")
tail(exp(apply(model$one.step.prediction.errors, 2, mean)))
mean(abs(exp(apply(model$one.step.prediction.errors, 2, mean))))
summary(exp(model$final.state))
exp(tail(dailyPrice, 1))


# Prediction

pred <- predict(model1, horizon = 10, newdata = rep(0, 10))
plot(pred, plot.original = 5)
exp(apply(pred$distribution, 2, summary))
apply(pred$distribution, 2, sd)



# With Holiday Data-------------------------------------------------------------



lastTradingDayBeforeTheFourth <-  as.Date(c("2018-07-03", 
                                            "2019-07-03", 
                                            "2020-07-03", 
                                            "2021-07-02", 
                                            "2022-07-01", 
                                            "2023-07-03"))
dailyPrice$dayBeforeFourth <- if_else(index(dailyPrice) %in% lastTradingDayBeforeTheFourth, 1, 0)

ss2 <- AddLocalLevel(list(), dailyPrice$Low)
model2 <- bsts(Low ~ insiderSharesSold + dayBeforeFourth,
              state.specification = ss2,
              data = dailyPrice,
              niter = 1000)
plot(model2)
plot(model2, "components")
plot(model2, "coefficients")
tail(exp(apply(model2$one.step.prediction.errors, 2, mean)))
mean(abs(exp(apply(model2$one.step.prediction.errors, 2, mean))))
summary(exp(model2$final.state))
exp(tail(dailyPrice, 1))


# Prediction

pred <- predict(model2, 
                horizon = 10, 
                newdata = data.frame(insiderSharesSold = c(0, 0, 0,0,0,0,0,0,0,0),
                                     dayBeforeFourth = c(0,0,0,0,0,0,0,0,0,0)))
plot(pred, plot.original = 5)
exp(apply(pred$distribution, 2, summary))
apply(pred$distribution, 2, sd)



# Get Monthly Price Data---------------------------------------------------------



FFIE <- getSymbols("FFIE", 
                   src = "yahoo", 
                   from = "2018-01-01", to = Sys.Date(), 
                   auto.assign = FALSE)
FFIE.weekly <- FFIE %>%
  fortify() %>%
  rename_with(~sub('FFIE.', "", .))%>%
  select(Open, High, Low, Close) %>%
  as.xts(order.by = as.Date(index(FFIE))) %>%
  to.weekly(indexAt = "endof") 

index(FFIE.weekly) <- # Data ends on Fridays, but we move the end to Sundays to match google trends
  ceiling_date(index(FFIE.weekly), unit = "week")

# Get weekly volume

volumes <- apply.weekly(FFIE, colSums)
index(volumes) <- 
  ceiling_date(index(volumes), unit = "week")
FFIE.weekly$avg.price <- (FFIE.weekly$..Open + FFIE.weekly$..High + FFIE.weekly$..Low + FFIE.weekly$..Close) / 4
FFIE.weekly$total.value <- FFIE.weekly$avg.price * volumes$FFIE.Volume
FFIE.weekly <- FFIE.weekly[, colnames(FFIE.weekly) %in% c(#"total.value", # For non-mixed model
                                                          "..Low")]

FFIE <- FFIE %>%
  fortify() %>%
  rename_with(~sub('FFIE.', "", .))%>%
  select(Open, High, Low, Close) %>%
  as.xts(order.by = as.Date(index(FFIE))) %>%
  to.monthly(indexAt = "lastof")



# Get Faraday Financials--------------------------------------------------------



financials.path <- "../ffie-financial-nowcasting/data"
financials.pred <- readRDS(paste(financials.path, "20240614-ffie-financials-pred.rds", sep = "/"))
filingDates <- readRDS(paste(financials.path, "20240614-ffie-financials-filingDates.rds", sep = "/"))
financials.model <- readRDS(paste(financials.path, "20240614-ffie-financials-model.rds", sep = "/"))

FFIE.weekly$daysSinceLastRelease <- sapply(index(FFIE.weekly), days.since.last.release, release.dates = filingDates$Filing.Date)

financials <- financials.pred$original.series
filingDates <- filingDates[rev(rownames(filingDates)), ]
filingDates$Filing.Date <- as.Date(filingDates$Filing.Date)
index(financials) <- filingDates$Filing.Date
index(financials) <- 
  ceiling_date(index(financials), 
               unit = "week")
FFIE.weekly <- merge(FFIE.weekly, financials)
FFIE.weekly <- na.locf(FFIE.weekly)    # financials effectively become last published value
FFIE.weekly <- na.fill(FFIE.weekly, 0) # And we do this to account for the initial period
                                       # after the company was founded when there were
                                       # no financials



# Get yfinance sentiment--------------------------------------------------------



sentiment.path <- 
  "data"
sentimentFiles <- list.files(sentiment.path, 
                              pattern = "yfinance-encoded",
                              full.names = TRUE)
sentimentFileTimestamps <- file.info(sentimentFiles)$ctime
most.recent.sentiment.file <- sentimentFiles[which.max(sentimentFileTimestamps)]
print(paste0("Most recent yfinance sentiment files is: ", most.recent.sentiment.file))
df.sentiment <-
  read.csv(most.recent.sentiment.file)
df.sentiment <- df.sentiment[df.sentiment$Ticker == "FFIE", ]
df.sentiment <- df.sentiment %>% 
  select(!c(Scrape.Time, Date, Link, Title, Subtext, Author, Ticker, Combined.Text))
df.sentiment$Absolute.Date <-
  as.Date(df.sentiment$Absolute.Date, format="%B %d, %Y")
df.sentiment <- na.omit(df.sentiment)
sentiment <- as.xts(df.sentiment)
sentiment <- apply.weekly(sentiment, colMeans)
index(sentiment) <- 
  ceiling_date(index(sentiment), 
               unit = "week",
               week_start = 1) - days(1) # we get duplicated weeks if we don't do it this way
                                         # because XTS starts days on Monday
                                         # but lubridate starts days on Sunday
                                         # if period.apply will create separate entries 
                                         # for Sunday and Monday
                                         # and ceiling_date will round them to the same day,
                                         # creating, duplicates
FFIE.weekly <- merge(FFIE.weekly, sentiment)
FFIE.weekly <- na.fill(FFIE.weekly, 0)



# Regression Model, No Trends---------------------------------------------------



ss <- AddLocalLevel(list(), log(FFIE.weekly$..Low))
model1 <- bsts(log(FFIE.weekly$..Low) ~ .,
               data = FFIE.weekly,
               state.specification = ss,
               niter = 1000)
plot(model1)
plot(model1, "components")
plot(model1, "residuals")
tail(exp(apply(model1$one.step.prediction.errors, 2, mean)))
mean(abs(exp(apply(model1$one.step.prediction.errors, 2, mean))))
summary(exp(model1$final.state))

# Create new prediction data

predData <- tail(FFIE.weekly, 1) # very temporary copy paste to get values
predData[,10:ncol(predData)] <- 0
predData$daysSinceLastRelease <- predData$daysSinceLastRelease + 7

pred1 <- predict(model1, horizon = 1, newdata = predData)
plot(pred1, plot.original = 5)
exp(apply(pred1$distribution, 2, summary))



# Get Gtrends Data--------------------------------------------------------------



terms <- c("Faraday Future", 
           "ffie", 
           "Tesla",
           "ff 91 futurist",
           "Electric vehicle",
           #"ffie stock", 
           "reddit ffie", 
           "ffie short",
           "法拉第 未来 股票",
           "贾跃宁 ff",
           "ff 汽车",
           "法拉第 未来 股价",
           "法拉第 未来",
           "ff",
           "法拉第"
           )
places <- c("all",
            "CN",
            "US",
            "HK",
            "CA",
            "SG")
           
trends <- list()

for (term in terms) {
  for (place in places) {
    trend <- try(gtrends(
      keyword = term,
      geo = place,
      gprop = c("web"),
      category = 0,
      hl = "en-US",
      compared_breakdown = FALSE,
      low_search_volume = FALSE,
      cookie_url = "http://trends.google.com/Cookies/NID",
      tz = 0,
      onlyInterest = T
    )$interest_over_time,
    silent = T)
    
    if (class(trend) != "try-error") {
      trends[[paste(term, place, sep = ".")]] <- zoo(trend$hits, order.by = as.Date(trend$date))
      ffie <- do.call(merge, trends)
    }
    Sys.sleep(2)
  }
}


predictors <- ffie

google.core <- coredata(predictors)
google.core <- apply(google.core, 2, as.numeric) # this will create NA values because there are some 
                                                 # <1 values in the dataset, which we impute with .5 two lines below
predictors <- zoo(google.core, order.by = index(predictors))
predictors <- na.fill(predictors, .5) # chosen at random

predictors.index <- index(predictors)
predictors.index <- ceiling_date(predictors.index, unit = "week")
predictors <- zoo(coredata(predictors), order.by = predictors.index)

# Join predictors w/ weekly data

predictors <- merge(predictors, as.zoo(FFIE.weekly)) # This will result in na values 
                                                     # In the last row on certain days because
                                                     # Early on monday there is trends data for 
                                                     # The week but no new stock data

# Figure out our seams

the.end <- end(FFIE)
the.beginning <- start(FFIE)

# Date matches NOT guaranteed

predictors.end <- predictors[index(predictors) >= ceiling_date(the.end, unit = "week") - days(1)]
predictors <- predictors[index(predictors) <= ceiling_date(the.end, unit = "week") - days(1)]
predictors <- predictors[index(predictors) >= the.beginning - days(21)] # I guess we only need 21 days because it's the end of the period

variances <- apply(coredata(predictors), 2, var)
varDrops <- names(variances[variances == 0])                    # If there happens to be a unique value in the prediction window only
predictors <- predictors[, !colnames(predictors) %in% varDrops] # then this will let a zero variance column through

FFIE <- as.zoo(FFIE)


# Log transform the y variable b/c it's nothing like normal

FFIE$..Low <- log(FFIE$..Low)

horizon <- 1
prediction.window <- tail(FFIE, horizon)
FFIE <- FFIE[1:nrow(FFIE) - 1]
predictors.pred <- predictors[index(predictors) > max(index(FFIE))] # Admittedly some fuzziness at this seam
predictors <- predictors[index(predictors) <= max(index(FFIE))] # Admittedly some fuzziness at this seam



# Model


## Setting an upper limit on the standard deviations can help keep the 
## MCMC from flying off to infinity.
sd.limit <- sd(FFIE$..Low)
state.specification <-
  AddLocalLevel(list(),
                      FFIE$..Low#,
                      #level.sigma.prior = SdPrior(1.0, 5, upper.limit = sd.limit),
                      #slope.sigma.prior = SdPrior(.5, 5, upper.limit = sd.limit)
                      )
weeks <- index(predictors)
months <- index(FFIE)
which.month <- MatchWeekToMonth(weeks, months[1])
membership.fraction <- GetFractionOfDaysInInitialMonth(weeks)
contains.end <- WeekEndsMonth(weeks)
model <- bsts.mixed(target.series = t(FFIE$..Low),
                    predictors = predictors,
                    membership.fraction = membership.fraction,
                    contains.end = contains.end,
                    which.coarse = which.month,
                    state.specification = state.specification,
                    niter = 1000
                    )

PlotBstsMixedComponents(model)
sort(apply(model2$coefficients, 2, mean)[apply(model2$coefficients, 2, mean) != 0])
plot(model, "coefficients")
plot(model, "size")

pred <- predict(model, 
                horizon = 1,
                newdata = predictors.pred)

my_function <- function(target.series, predictors) {
  # Capture the expression of target.series
  target.series_expr <- deparse(substitute(target.series))
  
  # Construct the formula dynamically
  formula_str <- paste(target.series_expr, "~", paste(names(predictors), collapse = " + "))
  
  # Create a list containing your data
  my_data <- list(target.series = target.series,
                  predictors = predictors)
  
  # Use match.call to capture the original call
  my.model.frame <- match.call(expand.dots = FALSE)
  
  # Modify the call to include model.frame and data
  my.model.frame[[1L]] <- as.name("model.frame")
  my.model.frame$formula <- as.formula(formula_str)  # Convert to formula object
  my.model.frame$data <- my_data
  
  # Evaluate the modified call
  result <- eval(my.model.frame, parent.frame())
  
  # Return the result or perform further operations
  return(result)
}

# Example usage
# Assuming FFIE$..Low is your unlabeled object
FFIE <- list(..Low = c(1, 2, 3, 4, 5))
result <- my_function(target.series = t(FFIE$..Low), predictors = data.frame(x1 = 1:5, x2 = 6:10))

# Display the result or use it further
print(result)
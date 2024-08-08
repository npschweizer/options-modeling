days.since.last.release <- function(date, release.dates) {
  past.releases <- release.dates[release.dates <= date]
  if (length(past.releases) == 0) {
    return(0)
  } else {
    last.release <- max(past.releases)
    return(as.numeric(difftime(date, last.release, units = "days")))
  }
}

# Calculate Stochastic Oscillator
calculate_stochastic_oscillator <- function(data) {
  high <- runMax(Hi(data), n = 14)
  low <- runMin(Lo(data), n = 14)
  k <- (Cl(data) - low) / (high - low) * 100
  d <- SMA(k, n = 3)
  k <- as.vector(k) # We return these to vectors so they can
  d <- as.vector(d) # be given names with the $ operator
  return(list(k = k, d = d))
}

honestLoess <- function(data, 
                        ticker, 
                        columns,
                        span = .2,
                        degree = 2) {
  y <- paste0(ticker, ".Close")
  y <- data[,y]
  if(sum(is.na(y))==length(y) | length(y) - sum(is.na(y)) < 5){ # 5 is pure guess
    return(NA)
  }
  formula <- paste(columns, collapse = " + ")
  if (length(columns) > 0) {
    formula <- paste0(formula, " + ")
  }
  formula <- paste0(ticker,".Close ~ ", formula, "as.numeric(index(data))")
  formula <- as.formula(formula)
  loess_fit <- loess(formula, 
                     data = data, 
                     span = span,
                     degree = degree)$fitted
  return(loess_fit[length(loess_fit)])  # Using the last value of the fitted values
}

honestLoessPredict <- function(data, 
                               ticker, 
                               columns,
                               span = .2,
                               degree = 2) {
  y <- paste0(ticker, ".Close")
  y <- data[,y]
  if(sum(is.na(y))==length(y) | length(y) - sum(is.na(y)) < 5){ # 5 is pure guess
    return(NA)
  }
  formula <- paste(columns, collapse = " + ")
  if (length(columns) > 0) {
    formula <- paste0(formula, " + ")
  }
  formula <- paste0(ticker,".Close ~ ", formula, "as.numeric(index(data))")
  formula <- as.formula(formula)
  
  loess_model <- loess(formula, 
                       data = data, 
                       span = span,
                       degree = degree)
  
  # Create a new data point for the next time step
  new_data <- data[nrow(data),]
  new_data$as.numeric.index.data.. <- as.numeric(index(data)[nrow(data)]) + 1
  
  # Predict the next value
  next_predicted <- predict(loess_model, newdata = new_data)
  
  return(next_predicted)
}
mean.reversion <- function(data, smoothed_data, deviation_threshold = .1) {
  buy_signals <- if_else(data < (1-deviation_threshold) * smoothed_data, 1, 0)
  return(buy_signals)
}

mean.reversion.hold <- function(data, smoothed_data, deviation_threshold = .1) {
  buy_signals <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    if (is.na(smoothed_data[i])) {
      buy_signals[i] <- 0
    } else if (data[i] < (1-deviation_threshold) * smoothed_data) {
      buy_signals[i] <- 1
    } else if (data[i] > (1-deviation_threshold) * smoothed_data & buy_signals[i - 1] == 1) {
      buy_signals[i] <- 1
    } else {
      buy_signals[i] <- 0
    }
  }
  return(buy_signals)
}

R <- function (x) 
{
  if (length(grep("Return", names(temp))) > 0) 
    return(x[, grep("Return", colnames(x), ignore.case = TRUE)])
  stop("subscript out of bounds: no column name containing \"Return\"")
}

reverseReverse <- function(column.name, tickers) {
  for (ticker in tickers) {
    if (grepl(ticker, column.name)) {
      column.name <- sub(paste0("_", ticker), "", column.name)
      column.name <- paste(ticker, column.name, sep = ".")
    }
  }
  return(column.name)
}


calculate.cumulative.returns <- function(x) {
  x <- na.fill(x, 0) # Doing this rather than na.omit because there are gaps for 
                     # holidays, weekends, and the beginning of the timeseries
                     # It also means that returns for the weekend for companies with 24 hour trading
                     # are effectively imputed to monday, I think
  cum_returns <- apply(x, 2, function(x) cumprod(1 + x))
  cum_returns <- xts(cum_returns, order.by = index(x))
  return(cum_returns)
}

# For peak end memory
allTimeBestOrWorst <- function(x) {
  idx <- which.max(abs(x))
  x[,idx]
}



# Backtests and Results--------------------------------------------------------



# This function is used to calculate the cumulative return for a given strategy
# from the beginning of the window to the present time

getResults <- function(stocks = tickers,
                       period = window,
                       data = NULL,
                       strategyReturn = NULL,
                       findings = results) {
  resultVec <- c()
  strategy <- gsub("StrategyReturn", "", strategyReturn)
  
  if (!is.null(findings) && strategy %in% colnames(findings)) {
    stop(paste("Error: Strategy", strategy, "already exists in the findings data frame."))
  }
  
  for(stock in stocks) {
    if(is.numeric(period)){
      period <- Sys.Date() - period
      temp <- data[(index(data) > period), paste0(stock, ".", strategyReturn)]
    } else if(is.Date(period)){
      temp <- data[(index(data) > period), paste0(stock, ".", strategyReturn)]
    } else {
      stop("window must be either a number or a date range")
    }
    
    if(sum(is.na(temp)) > 0) {
      print(paste("Detected", sum(is.na(temp)), "NA values for", stock))
    }
    
    cumulative.return <-
      cumprod(1 + na.omit(temp))[length(na.omit(temp))] - 1
    
    print(paste("Cumulative Return (",
                strategy,
                ") for",
                stock,
                "since",
                period,
                ":",
                round(cumulative.return, 4)))
    resultVec <- c(resultVec, cumulative.return)
    
    rm(temp)
    rm(cumulative.return)
  }
  
  portfolio.return <- mean(resultVec)
  resultVec <- c(resultVec, portfolio.return)
  
  if (is.null(findings)) {
    findings <- data.frame(ticker = c(stocks, "Portfolio"),
                           Buy.And.Hold = resultVec)
  } else {
    findings[,strategy] <- resultVec
  }
  
  print(paste("Cumulative Return (",
              strategy,
              ") for evenly-weighted portfolio",
              "since",
              period,
              ":",
              round(portfolio.return, 4)))
  
  rm(resultVec)
  
  return(findings)
}

# This function is used to calculate the cumulative return for a given strategy
# for a list of predefined test cases

getTestCaseResults <- function(stock_period_pairs, # A list object containing tickers and test case start/stop dates
                            data = NULL, # An xts object containing the periodized returns for each ticker
                            strategyReturn = NULL, # The strategy we want returns for
                            findings = testCaseResults) # The pre-existing results object. 
                                                        # If NULL, a new one will be created
  { 
  
  resultList <- list()
  strategy <- gsub("StrategyReturn", "", strategyReturn)
  
  if (!is.null(findings) && strategy %in% colnames(findings)) {
    stop(paste("Error: Strategy", strategy, "already exists in the findings data frame."))
  }
  
  for (pair in stock_period_pairs) {
    stock <- pair$stock
    start_date <- pair$start_date
    end_date <- pair$end_date
    
    if (!is.Date(start_date) || !is.Date(end_date)) {
      stop("Start and end dates must be Date objects.")
    }
    
    temp <- data[(index(data) >= start_date & index(data) <= end_date), paste0(stock, ".", strategyReturn)]
    
    if (sum(is.na(temp)) > 0) {
      print(paste("Detected", sum(is.na(temp)), "NA values for", stock))
    }
    
    cumulative.return <- cumprod(1 + na.omit(temp))[length(na.omit(temp))] - 1
    
    print(paste("Cumulative Return (",
                strategy,
                ") for",
                stock,
                "from",
                start_date,
                "to",
                end_date,
                ":",
                round(cumulative.return, 4)))
    
    resultList[[length(resultList) + 1]] <- list(
      ticker = stock, # The stock name
      start_date = start_date, # The start date of the test case
      end_date = end_date, # The end date of the test case
      return = cumulative.return[[1]] # The return for that period
    )
  }
  if (is.null(findings)) {
    print("Creating New Findings Dataframe")
    findings <- do.call(rbind, lapply(resultList, as.data.frame))
    colnames(findings)[ncol(findings)] <- strategy
  } else {
    new_findings <- do.call(rbind, lapply(resultList, as.data.frame))
    colnames(new_findings)[ncol(new_findings)] <- strategy
    findings[,strategy] <- new_findings[,strategy]
  }
  
  return(findings)
}

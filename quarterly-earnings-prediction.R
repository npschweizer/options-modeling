# Remember to set working wd to source file location



# Setup



.libPaths(paste0(getwd(),"/packages"))
library(jsonlite)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(bsts)
library(car)



# Get Faraday Financials



apikey <- readLines("secret-treasure.txt")
doc <- fromJSON(paste0(
  "https://api.gurufocus.com/public/user/", apikey, "/stock/FFIE/financials"
))



# Get y



numQuarters <- 
  length(doc$financials$quarterly$balance_sheet$`Cash and Cash Equivalents`)
quarters <- seq(as.Date("2020-09-30"), 
                by="quarter", 
                length.out = numQuarters)

financials <- data.frame(Cash = as.numeric(doc$financials$quarterly$balance_sheet$`Cash and Cash Equivalents`),
                         Revenue = as.numeric(doc$financials$quarterly$income_statement$Revenue),
                         Interest.Expense = as.numeric(doc$financials$quarterly$income_statement$`Interest Expense`),
                         EPS.Basic = as.numeric(doc$financials$quarterly$income_statement$`EPS (Basic)`),
                         Inventories.Raw = as.numeric(doc$financials$quarterly$balance_sheet$`Inventories, Raw Materials & Components`),
                         Inventories.InProgress =  as.numeric(doc$financials$quarterly$balance_sheet$`Inventories, Work In Process`)
                         )
financials$carsDelivered <- c( 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 4, 6)
financials <- as.matrix(financials)



# Timestamp Info----------------------------------------------------------------



filing.dates <- data.frame(
  "Filing Date" = as.Date(c(
    "2024-05-30", "2024-05-28", "2023-11-13", "2023-08-21", "2023-08-21",
    "2023-08-21", "2023-08-21", "2023-05-12", "2023-03-09", "2022-11-21",
    "2022-08-15", "2022-05-23", "2022-05-13", "2022-05-06", "2021-08-13",
    "2021-05-28", "2021-05-27", "2021-03-31", "2020-11-13"
  )),
  "Document Date" = as.Date(c(
    "2023-12-31", "2023-12-31", "2023-09-30", "2023-06-30", "2023-03-31",
    "2022-12-31", "2022-09-30", "2023-03-31", "2022-12-31", "2022-09-30",
    "2022-06-30", "2022-03-31", "2021-12-31", "2021-09-30", "2021-06-30",
    "2021-03-31", "2020-12-31", "2020-12-31", "2020-09-30"
  )),
  "Form" = c(
    "10-K/A", "10-K", "10-Q", "10-Q", "10-Q/A", "10-K/A", "10-Q/A", "10-Q",
    "10-K", "10-Q", "10-Q", "10-Q", "10-K", "10-Q", "10-Q", "10-Q", "10-K/A",
    "10-K", "10-Q"
  )
)
filing.dates <- filing.dates[!grepl("/A",
                                    filing.dates$Form), ]

filing.dates$Quarters  <- doc$financials$quarterly$`Fiscal Year`



# Get supplier financials



Myoung.doc <- fromJSON(paste0(
  "https://api.gurufocus.com/public/user/", apikey, "/stock/XKRX/financials"
))

# Myoung

Myoung <- data.frame(Myoung.doc$financials$quarterly$`Fiscal Year`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Accounts Receivable`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Inventories, Raw Materials & Components`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Inventories, Work In Process`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Inventories, Inventories Adjustments`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Inventories, Finished Goods`,
                     Myoung.doc$financials$quarterly$balance_sheet$`Inventories, Other`)
Myoung <- Myoung[Myoung$Myoung.doc.financials.quarterly..Fiscal.Year. %in% format(quarters, format = "%Y-%m"),]
Myoung$Myoung.doc.financials.quarterly..Fiscal.Year. <- NULL
Myoung <- apply(Myoung, 2, as.numeric)
  
# Palantir

Palantir.doc <- fromJSON(paste0(
  "https://api.gurufocus.com/public/user/", apikey, "/stock/PLTR/financials"
))
Palantir <- data.frame(Palantir.doc$financials$quarterly$`Fiscal Year`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Accounts Receivable`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Inventories, Raw Materials & Components`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Inventories, Work In Process`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Inventories, Inventories Adjustments`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Inventories, Finished Goods`,
                     Palantir.doc$financials$quarterly$balance_sheet$`Inventories, Other`)
Palantir <- Palantir[Palantir$Palantir.doc.financials.quarterly..Fiscal.Year. %in% format(quarters, format = "%Y-%m"),]
Palantir$Palantir.doc.financials.quarterly..Fiscal.Year. <- NULL
Palantir <- apply(Palantir, 2, as.numeric)




# Get Interest Rates



data <- read.table("data/Quarterly_Loan_Stuff_Quarterly.txt", 
           sep = "\t",
           header = T)
data <- data[-1, ]
data$Cash <- as.matrix(y)
data <- cbind(data, Myoung, Palantir)
data <- data[ , colSums(is.na(data)) == 0]



# Model



matrix(rnorm(ntimes * nfactors, 0, state.innovation.sd), nrow = ntimes)


ss <- AddSharedLocalLevel(state.specification = list(),
                          response = financials,
                          nfactors = 7,
                          #timestamps = quarters,
                          )
model <- mbsts(financials, shared.state.specification = ss, 
               niter = 1000,
               data.format = "wide", 
               seed = 99999)

plot(model)
plot(model, "means")
pred <- predict(model, horizon = 1, burn = SuggestBurn(.1, model))
pred$mean


saveRDS(pred, 
        file = paste("data", 
                     "20240614-ffie-financials-pred.rds", 
                     sep = "/"))
saveRDS(model, 
        file = paste("data", 
                     "20240614-ffie-financials-model.rds", 
                     sep = "/"))
saveRDS(filing.dates, 
        file = paste("data", 
                     "20240614-ffie-financials-filingDates.rds", 
                     sep = "/"))


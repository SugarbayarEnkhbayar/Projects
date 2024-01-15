
library(xts)
library(quantmod)
library(pryr)
library(dplyr)

setwd('C:/Users/se456296/Desktop/data-20231023T131621Z-001/data')


###########################################################################
# Exercises 1                                                             #
###########################################################################
# Exercise 1.1

# Import quotations for three different companies (eg. MSFT, KO, PEP)
# from yahoo finance. Merge together all close prices into one xts
# object and plot them on one graph.
getSymbols("MSFT",
           from = "2010-01-01")
getSymbols("KO",
           from = "2010-01-01")
getSymbols("PEP",
           from = "2010-01-01")

MSFT <- MSFT$MSFT.Close
KO <- KO$KO.Close
PEP <- PEP$PEP.Close
close_prices <-merge.xts(MSFT, KO, PEP)
plot(close_prices)

# Exercise 1.2
# Load truefx data for EURGBP from file named EURGBP-2023-08.csv.
# Data were downloaded by the lecturer from https://www.truefx.com.
# Signing up is required to get them directly.
# CAUTION! There are no column names in the first row.
# Assign column names: symbol, date_time, bid, ask.
# Create a correct date-time index.
# Convert to xts.
# Compare the size of a data.frame and xts object.
# Play with different plots of the data.
column_names <- c("symbol", "date_time", "bid", "ask")
EURGBP_ticks <- read.csv("EURGBP-2023-08.csv", col.names = column_names)
head(EURGBP_ticks)
str(EURGBP_ticks)
EURGBP_ticks=EURGBP_ticks[1:25000,]
EURGBP_ticks=EURGBP_ticks %>% select(date_time,bid,ask)
EURGBP_ticks$date_time <- strptime(EURGBP_ticks$date_time,
                                   format = "%Y%m%d %H:%M:%OS")
str(EURGBP_ticks)
EURGBP_ticks.xts <- xts(EURGBP_ticks[,-1], 
                        EURGBP_ticks$date_time, 
                        tzone = "GMT")
head(EURGBP_ticks.xts)
object_size(EURGBP_ticks)
object_size(EURGBP_ticks.xts)
plot(EURGBP_ticks.xts,
     multi.panel = 3,
     yaxis.same = FALSE)

# Exercise 1.3
# Aggregate the data for EURGBP (from Exercise 1.2) to:
# - 15 sec data
EURGBP_ticks_15sec.ask <- to.period(EURGBP_ticks.xts$ask, period = 'seconds',
                                k = 15, OHLC = FALSE)
EURGBP_ticks_15sec.bid <- to.period(EURGBP_ticks.xts$bid, period = 'seconds',
                                    k = 15, OHLC = FALSE)
EURGBP_ticks_15sec <- merge(EURGBP_ticks_15sec.ask,EURGBP_ticks_15sec.bid)

# - 3 min data
EURGBP_ticks_3min.ask <- to.minutes(EURGBP_ticks.xts$ask,k=3)
EURGBP_ticks_3min.bid <- to.minutes(EURGBP_ticks.xts$bid,k=3)
EURGBP_ticks_3min <- merge(EURGBP_ticks_3min.ask, EURGBP_ticks_3min.bid)

# - 2 hourly data
EURGBP_ticks_2hour.ask <- to.period(EURGBP_ticks.xts$ask, period = 'hours',
                                k = 2, OHLC = FALSE)
EURGBP_ticks_2hour.bid <- to.period(EURGBP_ticks.xts$bid, period = 'hours',
                                    k = 2, OHLC = FALSE)
EURGBP_ticks_2hour <- merge(EURGBP_ticks_2hour.ask, EURGBP_ticks_2hour.bid)

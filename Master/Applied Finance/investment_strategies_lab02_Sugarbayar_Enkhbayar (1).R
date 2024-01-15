library(xts)
library(quantmod)
library(tseries) 
library(chron)
library(TTR)
library(caTools)
library(roll)
library(lubridate)
library(dplyr)
library(lattice)
library(grDevices)
library(ggplot2)

Sys.setlocale("LC_TIME", "English")

#annual sharpe ratio function

mySR <- function(x, # x = series of returns
                 scale) # scaling parameter = Nt if we have daily data = 252
{
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 
positionR <- function(signal, lower, upper, pos_flat, strategy)
{
  # lets check thevalue of the strategy parameter
  if (! strategy %in% c("mom", "mr"))
  {  print("Strategy parameter incorrect. Please use 'mom' or 'mr'!")
    stop
  }
  
  # lets first create a vector of 0s
  position <- rep(0, length(signal))
  
  for (i in 2:length(signal))
  {
    if ( pos_flat[i] == 1 ) position[i] <- 0 
    else
    { # check if values are nonmissing (otherwise calculations not possible)
      if (!is.na(signal[i-1]) & 
          !is.na(upper[i-1]) & 
          !is.na(lower[i-1]))
      { 
        # what if previous position was 0
        if (position[i-1] == 0){
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==-1){
          # what if previous position was -1
          if (signal[i-1] > lower[i-1]){position[i] <- -1}
          if (signal[i-1] < lower[i-1]){position[i] <- 1}
        } else if (position[i-1]==1){
          # what if previous position was 1
          if (signal[i-1] < upper[i-1]){position[i] <- 1}
          if (signal[i-1] > upper[i-1]){position[i] <- -1}
        }
      } else position[i] <- position[i-1]
      # if anything is missing, keep previous position
    }
  }
  # reverse the position if we use a momentum ("mom") strategy
  if(strategy == "mom") position <- (-position)
  
  # return() function clearly indicates 
  # what the function should return
  return(position)
}

#####################################################################################################################
# Exercises 2

# Exercise 2.1
# Check the performance of the same strategy on the test data.
# Are 5 top performing combinations still profitable?
# How test net SR differs from train net SR?
# Use plots to verify sensitivity of results to parameters.
load("data/currencies.RData")
currencies_train <- currencies[day(currencies) < 17,]
currencies_test <- currencies[day(currencies) >= 17,]
E6_train<- currencies_train$E6
E6_test<- currencies_test$E6
E6<- currencies$E6
plot(E6)
E6_train["T18:01/T18:05"] <- NA
E6_train["T16:56/T17:00"] <- NA
pos_flat_train <- xts(rep(0, nrow(E6_train)), index(E6_train))
pos_flat_train["T16:46/T18:15"] <- 1
dweek_train <- wday(E6_train)
time_train <- substr(index(E6_train), 12, 19)
pos_flat_train[(dweek_train == 6 & times(E6_train) > times("17:00:00")) |   # end of Friday
                 (dweek_train == 7) |                                      # whole Saturday
                 (dweek_train == 1 & times(E6_train) <= times("18:00:00")),] <- 1 # beginning of Sunday

for(signalEMA in c(10, 15, 20, 30, 45)) {
  for(slowEMA in c(60, 90, 120, 150, 180)) {
    for(volat.sd in c(60, 90, 120)) {
      for(m_ in c(1, 2, 3)) {
        
        message(paste0("signalEMA = ", signalEMA,
                       ", slowEMA = ", slowEMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        
        # calculating elements of the strategy
        
        # here calculation on coredata() makes a difference
        # signal
        signalEMA.values <- EMA(na.locf(coredata(E6_train$E6)), 
                                signalEMA)
        # basis for volatility bands
        slowEMA.values <- EMA(na.locf(coredata(E6_train$E6)), 
                              slowEMA)
        # size of volatility bands
        volat.sd.values <- roll_sd(na.locf(coredata(E6_train$E6)), 
                                   volat.sd)
        
        # position for momentum strategy
        pos.mom <- positionR(signal = signalEMA.values,
                             lower = slowEMA.values - m_ * volat.sd.values,
                             upper = slowEMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat_train),
                             strategy = "mom" # important !!!
        )
        
        # gross pnl
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(E6_train$E6)),
                                0, pos.mom * diff.xts(E6_train$E6) * 125000 # point value for E6
        )
        
        # nr of transactions - the same for mom and mr
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        
        # net pnl
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of E6
        
        # aggregate pnls and # transactions to daily
        
        # lets find endpoints of days
        ends_ <- endpoints(E6_train, "days")
        
        # aggregating gross pnl 
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        # aggregating net pnl 
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        
        # calculate summary measures
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        
        # summary of a particular strategy
        summary_train_e6 <- data.frame(signalEMA = signalEMA,
                                       slowEMA = slowEMA,
                                       volat.sd = volat.sd,
                                       m = m_,
                                       period = "2013-08",
                                       gross.SR.mom,
                                       net.SR.mom,
                                       gross.PnL.mom,
                                       net.PnL.mom,
                                       stringsAsFactors = FALSE
        )
        
        # putting all summaries together
        if(!exists("summary.all.breakout1")) summary.all.breakout1 <- summary_train_e6 else
          summary.all.breakout1 <- rbind(summary.all.breakout1, summary_train_e6)
        
        # deleting working files not needed any more
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_train_e6,
           signalEMA.values, slowEMA.values, volat.sd.values
        )
        
      } # end of loop for m_
    } # end of loop for volatility  
  } # end of loop for slowEMA
} # end of loop for signal
train_e6<-summary.all.breakout1 %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(5)
train_e6
E6_test["T18:01/T18:05"] <- NA
E6_test["T16:56/T17:00"] <- NA
pos_flat_test <- xts(rep(0, nrow(E6_test)), index(E6_test))
pos_flat_test["T16:46/T18:15"] <- 1
dweek_test <- wday(E6_test)
time_test <- substr(index(E6_test), 12, 19)
pos_flat_test[(dweek_test == 6 & times(E6_test) > times("17:00:00")) |   # end of Friday
                (dweek_test == 7) |                                      # whole Saturday
                (dweek_test == 1 & times(E6_test) <= times("18:00:00")),] <- 1 # beginning of Sunday
model1<-c(10, 150, 120, 3)
model2<-c(10, 120, 120, 3)
model3<-c(10, 60, 120, 2)
model4<-c(10, 90, 90, 2)
model5<-c(10, 180, 120, 3)

best5<-data.frame(model1, model2, model3, model4, model5)
index_ <- index(E6_test)
i=1
###########
for (i in 1:5) {
  signalEMA <- as.numeric(best5[1,i])
  slowEMA <- as.numeric(best5[2,i])
  volat.sd <- as.numeric(best5[3,i])
  m_ <- as.numeric(best5[4,i])
  
  signalEMA.values <- EMA(na.locf(coredata(E6_test$E6)), signalEMA)
  slowEMA.values <- EMA(na.locf(coredata(E6_test$E6)), slowEMA)
  volat.sd.values <- roll_sd(na.locf(coredata(E6_test$E6)), volat.sd)
  
  
  pos.mom <- positionR(signal = signalEMA.values,
                       lower = slowEMA.values - m_ * volat.sd.values,
                       upper = slowEMA.values + m_ * volat.sd.values,
                       pos_flat = coredata(pos_flat_test),
                       strategy = "mom" )# important !!!
  
  pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(E6_test$E6)),
                          0, pos.mom * diff.xts(E6_test$E6) * 125000 # point value for E6
  ) # point value for E6
  
  # gross pnl
  
  # nr of transactions - the same for mom and mr
  ntrans <- abs(diff.xts(pos.mom))
  ntrans[1] <- 0
  
  # net pnl
  pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of E6
  
  
  # aggregate pnls and # transactions to daily
  
  # lets find endpoints of days
  ends_ <- endpoints(E6_test, "days")
  
  # aggregating gross pnl 
  pnl.gross.mom.d <- period.apply(pnl.gross.mom, INDEX = ends_, FUN = function(x) sum(x, na.rm = TRUE))
  # aggregating net pnl 
  pnl.net.mom.d <- period.apply(pnl.net.mom, INDEX = ends_, FUN = function(x) sum(x, na.rm = TRUE))
  
  # calculate summary measures
  gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
  net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
  gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
  
  net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
  
  # summary of a particular strategy
  summary_test_e6 <- data.frame(model1=i,
                                signalEMA = signalEMA,
                                slowEMA = slowEMA,
                                volat.sd = volat.sd,
                                m = m_,
                                period = "2013-08",
                                gross.SR.mom,
                                net.SR.mom,
                                gross.PnL.mom,
                                net.PnL.mom,
                                stringsAsFactors = FALSE)
  
  
  if(!exists("summary.all.breakout2")) summary.all.breakout2 <- summary_test_e6 else
    summary.all.breakout2 <- rbind(summary.all.breakout2, summary_test_e6)
  
  # deleting working files not needed any more
  rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
     pnl.gross.mom.d, pnl.net.mom.d, 
     pnl.gross.mom, pnl.net.mom,
     pos.mom, ends_, summary_test_e6,
     signalEMA.values, slowEMA.values, volat.sd.values
  )
}
test_e6<-summary.all.breakout2 %>% 
  arrange(model1) %>% 
  head(5)
test_e6


# Exercise 2.2
# Perform similar analyses of a momentum strategy for another currency
# (other than E6) use volatility breakout model -- check different 
# SMAs and EMAs (function SMA() has the same syntax and requirements 
# as EMA()); use MORE combinations of parameters.
# Are the conclusions on train and test data similar as for E6?

A6_train<- currencies_train$A6
A6_test<- currencies_test$A6

A6<-currencies$A6
plot(A6)
A6_train["T18:01/T18:05"] <- NA
A6_train["T16:56/T17:00"] <- NA

pos_flat_train <- xts(rep(0, nrow(A6_train)), index(A6_train))

pos_flat_train["T16:46/T18:15"] <- 1

dweek_train <- wday(A6_train)

time_train <- substr(index(A6_train), 12, 19)

pos_flat_train[(dweek_train == 6 & times(A6_train) > times("17:00:00")) |   # end of Friday
                 (dweek_train == 7) |                                      # whole Saturday
                 (dweek_train == 1 & times(A6_train) <= times("18:00:00")),] <- 1 # beginning of Sunday
for(signalEMA in c(10, 15, 20, 25, 30, 35, 45)) {
  for(slowEMA in c(60, 90, 100, 120, 150, 180, 210, 240)) {
    for(volat.sd in c(60, 90, 120, 150)) {
      for(m_ in c(1, 2, 3, 4, 5)) {
        
        message(paste0("signalEMA = ", signalEMA,
                       ", slowEMA = ", slowEMA,
                       ", volat.sd = ", volat.sd,
                       ", m_ = ", m_)) 
        
        # calculating elements of the strategy
        
        # here calculation on coredata() makes a difference
        # signal
        signalEMA.values <- EMA(na.locf(coredata(A6_train$A6)), 
                                signalEMA)
        # basis for volatility bands
        slowEMA.values <- EMA(na.locf(coredata(A6_train$A6)), 
                              slowEMA)
        # size of volatility bands
        volat.sd.values <- roll_sd(na.locf(coredata(A6_train$A6)), 
                                   volat.sd)
        
        # position for momentum strategy
        pos.mom <- positionR(signal = signalEMA.values,
                             lower = slowEMA.values - m_ * volat.sd.values,
                             upper = slowEMA.values + m_ * volat.sd.values,
                             pos_flat = coredata(pos_flat_train),
                             strategy = "mom" # important !!!
        )
        
        # gross pnl
        pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(A6_train$A6)),
                                0, pos.mom * diff.xts(A6_train$A6) * 100000 # point value for A6
        )
        
        # nr of transactions - the same for mom and mr
        ntrans <- abs(diff.xts(pos.mom))
        ntrans[1] <- 0
        
        # net pnl
        pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of A6
        
        # aggregate pnls and # transactions to daily
        
        # lets find endpoints of days
        ends_ <- endpoints(A6_train, "days")
        
        # aggregating gross pnl 
        pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                        INDEX = ends_, 
                                        FUN = function(x) sum(x, na.rm = TRUE))
        # aggregating net pnl 
        pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                      INDEX = ends_,
                                      FUN = function(x) sum(x, na.rm = TRUE))
        
        # calculate summary measures
        gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
        net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
        gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
        net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
        
        # summary of a particular strategy
        summary_train_a6 <- data.frame(signalEMA = signalEMA,
                                       slowEMA = slowEMA,
                                       volat.sd = volat.sd,
                                       m = m_,
                                       period = "2013-08",
                                       gross.SR.mom,
                                       net.SR.mom,
                                       gross.PnL.mom,
                                       net.PnL.mom,
                                       stringsAsFactors = FALSE
        )
        
        # putting all summaries together
        if(!exists("summary.all.breakout3")) summary.all.breakout3 <- summary_train_a6 else
          summary.all.breakout3 <- rbind(summary.all.breakout3, summary_train_a6)
        
        # deleting working files not needed any more
        rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
           pnl.gross.mom.d, pnl.net.mom.d, 
           pnl.gross.mom, pnl.net.mom,
           pos.mom, ends_, summary_train_a6,
           signalEMA.values, slowEMA.values, volat.sd.values
        )
        
      } # end of loop for m_
    } # end of loop for volatility  
  } # end of loop for slowEMA
} # end of loop for signal
train_a6<-summary.all.breakout3 %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(5)
train_a6
A6_test["T18:01/T18:05"] <- NA
A6_test["T16:56/T17:00"] <- NA
pos_flat_test <- xts(rep(0, nrow(A6_test)), index(A6_test))
pos_flat_test["T16:46/T18:15"] <- 1
dweek_test <- wday(A6_test)
time_test <- substr(index(A6_test), 12, 19)
pos_flat_test[(dweek_test == 6 & times(A6_test) > times("17:00:00")) |   # end of Friday
                (dweek_test == 7) |                                      # whole Saturday
                (dweek_test == 1 & times(A6_test) <= times("18:00:00")),] <- 1 # beginning of Sunday


model1<-c(35, 210, 60, 4)
model2<-c(30, 900, 60, 2)
model3<-c(25, 900, 60, 2)
model4<-c(35, 90, 60, 2)
model5<-c(10, 210, 60, 4)

best5_<-data.frame(model1, model2, model3, model4, model5)

for (i in 1:5) {
  signalEMA <- as.numeric(best5[1,i])
  slowEMA <- as.numeric(best5[2,i])
  volat.sd <- as.numeric(best5[3,i])
  m_ <- as.numeric(best5[4,i])
  
  
  signalEMA.values <- EMA(na.locf(coredata(A6_test$A6)), 
                          signalEMA)
  # basis for volatility bands
  slowEMA.values <- EMA(na.locf(coredata(A6_test$A6)), 
                        slowEMA)
  # size of volatility bands
  volat.sd.values <- roll_sd(na.locf(coredata(A6_test$A6)), 
                             volat.sd)
  
  # position for momentum strategy
  pos.mom <- positionR(signal = signalEMA.values,
                       lower = slowEMA.values - m_ * volat.sd.values,
                       upper = slowEMA.values + m_ * volat.sd.values,
                       pos_flat = coredata(pos_flat_train),
                       strategy = "mom" # important !!!
  )
  
  # gross pnl
  pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(A6_test$A6)),
                          0, pos.mom * diff.xts(A6_test$A6) * 100000 # point value for A6
  )
  
  # nr of transactions - the same for mom and mr
  ntrans <- abs(diff.xts(pos.mom))
  ntrans[1] <- 0
  
  # net pnl
  pnl.net.mom <- pnl.gross.mom - ntrans * 15 # 15$ per transaction of A6
  
  # aggregate pnls and # transactions to daily
  
  # lets find endpoints of days
  ends_ <- endpoints(A6_test, "days")
  
  # aggregating gross pnl 
  pnl.gross.mom.d <- period.apply(pnl.gross.mom, 
                                  INDEX = ends_, 
                                  FUN = function(x) sum(x, na.rm = TRUE))
  # aggregating net pnl 
  pnl.net.mom.d <- period.apply(pnl.net.mom, 
                                INDEX = ends_,
                                FUN = function(x) sum(x, na.rm = TRUE))
  
  # calculate summary measures
  gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
  net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
  gross.PnL.mom <- sum(pnl.gross.mom.d, na.rm = T)
  net.PnL.mom <- sum(pnl.net.mom.d, na.rm = T)
  
  # summary of a particular strategy
  summary_test_a6 <- data.frame(signalEMA = signalEMA,
                                slowEMA = slowEMA,
                                volat.sd = volat.sd,
                                m = m_,
                                period = "2013-08",
                                gross.SR.mom,
                                net.SR.mom,
                                gross.PnL.mom,
                                net.PnL.mom,
                                stringsAsFactors = FALSE
  )
  
  # putting all summaries together
  if(!exists("summary.all.breakout4")) summary.all.breakout4 <- summary_test_a6 else
    summary.all.breakout4 <- rbind(summary.all.breakout4, summary_test_a6)
  
  # deleting working files not needed any more
  rm(gross.SR.mom, net.SR.mom, gross.PnL.mom, net.PnL.mom, 
     pnl.gross.mom.d, pnl.net.mom.d, 
     pnl.gross.mom, pnl.net.mom,
     pos.mom, ends_, summary_test_a6,
     signalEMA.values, slowEMA.values, volat.sd.values
  )
} # end of loop for signal
test_a6<-summary.all.breakout4 %>% 
  arrange(desc(net.SR.mom)) %>% 
  head(5)
test_a6

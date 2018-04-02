library(dplyr)
library(plyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

setwd("C:/btc/Orderbook/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/OMGBTC")
OMGBTC <- read.csv("BITF_OMGBTC_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/OMGUSD")
OMGUSD <- read.csv("BITF_OMGUSD_ob_20170810_20180211.csv", header = TRUE)

BTCUSD$date <- as.POSIXct(BTCUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
BTCUSD$day <- as.Date(BTCUSD$date, 'GMT')
BTCUSD$H <- hour(BTCUSD$date)
BTCUSD$M <- minute(BTCUSD$date)
OMGBTC$date <- as.POSIXct(OMGBTC$date %/% 1000, origin="1970-01-01", tz="GMT")
OMGBTC$day <- as.Date(OMGBTC$date, 'GMT')
OMGBTC$H <- hour(OMGBTC$date)
OMGBTC$M <- minute(OMGBTC$date)
OMGUSD$date <- as.POSIXct(OMGUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
OMGUSD$day <- as.Date(OMGUSD$date, 'GMT')
OMGUSD$H <- hour(OMGUSD$date)
OMGUSD$M <- minute(OMGUSD$date)
BTCUSD <- data.table(BTCUSD)
OMGBTC <- data.table(OMGBTC)
OMGUSD <- data.table(OMGUSD)
require(data.table)
OMG <- BTCUSD[OMGBTC[OMGUSD, mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L],
              mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L]
OMG <- OMG[,-c(7:10,15)]
colnames(OMG) <- c("Date","Index","BTC/USD_bid","BTC/USD_ask","BTC/USD_am_bid",
                   "BTC/USD_am_ask","OMG/BTC_bid","OMG/BTC_ask","OMG/BTC_am_bid",
                   "OMG/BTC_am_ask","OMG/USD_bid","OMG/USD_ask","OMG/USD_am_bid",
                   "OMG/USD_am_ask")
OMG$Date <- as.numeric(OMG$Date)
OMG <- na.locf(OMG)
OMG$"OMG/BTC/USD_bid" <- OMG$"BTC/USD_bid" * OMG$"OMG/BTC_bid"
OMG$"OMG/BTC/USD_ask" <- OMG$"BTC/USD_ask" * OMG$"OMG/BTC_ask"
OMG$S1 <- round((OMG$"OMG/USD_bid" / OMG$"OMG/BTC/USD_ask" - 1) * 100, 2)
OMG$S2 <- round((OMG$"OMG/USD_ask" / OMG$"OMG/BTC/USD_bid" - 1) * 100, 2)
OMG <- OMG[order(OMG[,1],OMG[,2]),]
OMG <- unique(OMG, by=c("Date", "Index"))
OMG <- OMG[!duplicated(OMG),]

head(OMG,10)
str(OMG)

m <- data.matrix(OMG)
rm(BTCUSD,OMGBTC,OMGUSD,OMG)
gc()
cat("\014")

hb <- 0.4
hn <- -0.6
lb <- -0.4
ln <- 0.6

t1 <- Sys.time()

trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_btcOMG <- 0
balance_usdOMG <- 0
# balance_btc <- 0
# balance_OMG <- 0
leverage <- 2
PL <- 0
commission <- 0
balance_avail <- balance_usd * leverage
balance_used <- 0
deal <- 0
profit_sum <- c()
profit_perc <- c()
duration <- c()
step_entry <- 0.1
step_continue <- 0.3
comm <- c()
scaling <- 1.0

for(i in seq(11, nrow(m), by = 5)){
  if(trade == 0){
    if(m[i,17] < 20 & m[i,17] > -20 & m[i,18] < 20 & m[i,18] > -20){
      if(m[i,17] > hb){#min(m[i,17],m[i-5,17],m[i-10,17])
        trade <- 1
        trade_positive_difference <- 1
        
        #SELL OMG/USD && BUY OMG/BTC/USD
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcOMG <- deal / m[i,16]
        balance_usdOMG <- -deal / m[i,11]
        # balance_btc <- deal / m[i,3]
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdOMG <- m[i,11]
        avg_price_btcOMG <- m[i,16]
        
        start <- m[i,1]
        s <- m[i,17]
        cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Equity USD:",format(round(start_balance,2), nsmall = 2),
            "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
        cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
            "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
            "Commission:", format(round(commission,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else {
        if(m[i,18] < lb){#max(m[i,18],m[i-5,18],m[i-10,18])
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/OMG && SELL USD/BTC/OMG
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcOMG <- -deal / m[i,15]
          balance_usdOMG <- deal / m[i,12]
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdOMG <- m[i,12]
          avg_price_btcOMG <- m[i,15]
          
          start <- m[i,1]
          s <- m[i,18]
          cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              "Equity USD:",format(round(start_balance,2), nsmall = 2),
              "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
          cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
              "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
              "Commission:", format(round(commission,2), nsmall = 2),"\n")
          cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if(m[i,17] > s + scaling && trade_positive_difference == 1 && balance_avail > 100){
      
      if(balance_avail < 200){
        deal <- balance_avail - 100
      }
      else{
        deal <- balance_avail * step_continue
      }
      
      temp_commission <- commission
      commission <- temp_commission + deal * 0.002 * 2
      temp_usd <- balance_usd
      balance_usd <- temp_usd - deal * 0.002 * 2
      temp_btcOMG <- balance_btcOMG
      balance_btcOMG <- temp_btcOMG + deal / m[i,16]
      temp_usdOMG <- balance_usdOMG
      balance_usdOMG <- temp_usdOMG - deal / m[i,11]
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
      avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
      
      s <- m[i,17]
      
      cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
          "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
      cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
          "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
          "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
          "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
          "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
      cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
          "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
          "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
      cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
    }
    else{
      if(m[i,18] < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
        
        if(balance_avail < 200){
          deal <- balance_avail - 100
        }
        else{
          deal <- balance_avail * step_continue
        }
        temp_commission <- commission
        commission <- temp_commission + deal * 0.002 * 2
        temp_usd <- balance_usd
        balance_usd <- temp_usd - deal * 0.002 * 2
        temp_btcOMG <- balance_btcOMG
        balance_btcOMG <- temp_btcOMG - deal / m[i,15]
        temp_usdOMG <- balance_usdOMG
        balance_usdOMG <- temp_usdOMG + deal / m[i,12]
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
        avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
        
        s <- m[i,18]
        
        cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
            "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
        cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
        cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
            "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
            "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
          #Close positions:
          ##BUY OMG/USD && SELL OMG/BTC/USD
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcOMG * m[i,15] * 0.002) +
            abs(balance_usdOMG * m[i,12] * 0.002)
          temp_commission <- abs(balance_btcOMG * m[i,15] * 0.002) + 
            abs(balance_usdOMG * m[i,12] * 0.002)
          PL_btcOMG <- (m[i,15] - avg_price_btcOMG)*balance_btcOMG
          PL_usdOMG <- (m[i,12] - avg_price_usdOMG)*balance_usdOMG
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_btcOMG <- 0
            PL_usdOMG <- 0
          }
          PL <- PL_usdOMG + PL_btcOMG
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,15] * 0.002) +
                                            abs(balance_usdOMG * m[i,12] * 0.002))
          balance_btcOMG <- 0
          balance_usdOMG <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcOMG * m[i,15] * 0.002) +
                      abs(balance_usdOMG * m[i,12] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcOMG <- 0
          temp_usdOMG <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcOMG <- 0
          PL_usdOMG <- 0
          
        }
        else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
          #Close positions:
          ##SELL OMG/USD && BUY OMG/BTC/USD
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcOMG * m[i,16] * 0.002) +
            abs(balance_usdOMG * m[i,11] * 0.002)
          temp_commission <- abs(balance_btcOMG * m[i,15] * 0.002) +
            abs(balance_usdOMG * m[i,4] * 0.002)
          PL_btcOMG <- (m[i,16] - avg_price_btcOMG)*balance_btcOMG
          PL_usdOMG <- (m[i,11] - avg_price_usdOMG)*balance_usdOMG
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_btcOMG <- 0
            PL_usdOMG <- 0
          }
          PL <- PL_usdOMG + PL_btcOMG
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,16] * 0.002) +
                                            abs(balance_usdOMG * m[i,11] * 0.002))
          balance_btcOMG <- 0
          balance_usdOMG <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcOMG * m[i,16] * 0.002) +
                      abs(balance_usdOMG * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcOMG <- 0
          temp_usdOMG <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcOMG <- 0
          PL_usdOMG <- 0
          
        }
      }
    }
  }
  if (i == nrow(m)-4){
    cat("Start testing:", as.character(as.POSIXct(m[1,1], origin = "1970-01-1")),
        "End testing",as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
        "Estimating period (Days):",format(round((m[i,1]-m[1,1])/86400,0), nsmall = 0),"\n")
    cat("Start balance USD:",4000,"\n")
    cat("End balance USD:",balance_usd,"\n")
    cat("Total deals:",length(profit_sum),"\n")
    cat("Total profit:",sum(profit_sum),"\n")
    cat("Profit per deal:",mean(profit_sum),"\n")
    cat("Average duration (H):",mean(duration),"\n")
    cat("Average duration (M):",mean(duration) * 60,"\n")
    cat("Average duration (S):",mean(duration) * 3600,"\n")
    cat("Average profit percent:",format(round(mean(profit_perc)*100,2), nsmall = 2),"%\n")
    cat("Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%\n")
    cat("Max profit:",max(profit_sum),"\n")
    cat("Min profit:",min(profit_sum),"\n")
    cat("Paid commission:",sum(comm),"\n")
  }
}

t2 <- Sys.time()
t2-t1

############################################################################
#############TEST######################
hb <- 100
hn <- 99
lb <- -100
ln <- -99

resH <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("hb", "hn", "profit", "deals")
colnames(resH) <- x
for(hb in seq(-15.0,15.0,by=0.2)){
  for(hn in seq(hb-10,hb-1,by=0.2)){
    
    t1 <- Sys.time()
    
    trade <- 0
    trade_positive_difference <- 0
    trade_negative_difference <- 0
    start <- 0
    end <- 0
    s <- 0
    start_balance <- 4000
    balance_usd <- 4000
    balance_btcOMG <- 0
    balance_usdOMG <- 0
    # balance_btc <- 0
    # balance_OMG <- 0
    leverage <- 2
    PL <- 0
    commission <- 0
    balance_avail <- balance_usd * leverage
    balance_used <- 0
    deal <- 0
    profit_sum <- c()
    profit_perc <- c()
    duration <- c()
    step_entry <- 0.1
    step_continue <- 0.3
    comm <- c()
    scaling <- 1.1
    
    for(i in seq(11, nrow(m), by = 5)){
      if(trade == 0){
        if(m[i,17] < 20 & m[i,17] > -20 & m[i,18] < 20 & m[i,18] > -20){
          if(m[i,17] > hb){#min(m[i,17],m[i-5,17],m[i-10,17])
            trade <- 1
            trade_positive_difference <- 1
            
            #SELL OMG/USD && BUY OMG/BTC/USD
            deal <- balance_avail * step_entry
            commission <- deal * 0.002 * 2
            start_balance <- balance_usd
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_btcOMG <- deal / m[i,16]
            balance_usdOMG <- -deal / m[i,11]
            # balance_btc <- deal / m[i,3]
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdOMG <- m[i,11]
            avg_price_btcOMG <- m[i,16]
            
            start <- m[i,1]
            s <- m[i,17]
            # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
            #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
            # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
            # cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
            #     "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
            #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else {
            if(m[i,18] < lb){#max(m[i,18],m[i-5,18],m[i-10,18])
              trade <- 1
              trade_negative_difference <- 1
              
              #BUY USD/OMG && SELL USD/BTC/OMG
              deal <- balance_avail * step_entry
              commission <- deal * 0.002 * 2
              start_balance <- balance_usd
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_btcOMG <- -deal / m[i,15]
              balance_usdOMG <- deal / m[i,12]
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_usdOMG <- m[i,12]
              avg_price_btcOMG <- m[i,15]
              
              start <- m[i,1]
              s <- m[i,18]
              # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
              #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
              # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
            }
          }
        }
        else next
      }
      else{
        if(m[i,17] > s + scaling && trade_positive_difference == 1 && balance_avail > 100){
          
          if(balance_avail < 200){
            deal <- balance_avail - 100
          }
          else{
            deal <- balance_avail * step_continue
          }
          
          temp_commission <- commission
          commission <- temp_commission + deal * 0.002 * 2
          temp_usd <- balance_usd
          balance_usd <- temp_usd - deal * 0.002 * 2
          temp_btcOMG <- balance_btcOMG
          balance_btcOMG <- temp_btcOMG + deal / m[i,16]
          temp_usdOMG <- balance_usdOMG
          balance_usdOMG <- temp_usdOMG - deal / m[i,11]
          temp_used <- balance_used
          balance_used <- temp_used + deal * 2
          temp_avail <- balance_avail
          balance_avail <- balance_usd * leverage - balance_used
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
          avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
          
          s <- m[i,17]
          
          # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          #     "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
          #     "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
          # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
          #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
          #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
          #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          # cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
          #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
          #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
          # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
        else{
          if(m[i,18] < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
            
            if(balance_avail < 200){
              deal <- balance_avail - 100
            }
            else{
              deal <- balance_avail * step_continue
            }
            temp_commission <- commission
            commission <- temp_commission + deal * 0.002 * 2
            temp_usd <- balance_usd
            balance_usd <- temp_usd - deal * 0.002 * 2
            temp_btcOMG <- balance_btcOMG
            balance_btcOMG <- temp_btcOMG - deal / m[i,15]
            temp_usdOMG <- balance_usdOMG
            balance_usdOMG <- temp_usdOMG + deal / m[i,12]
            temp_used <- balance_used
            balance_used <- temp_used + deal * 2
            temp_avail <- balance_avail
            balance_avail <- balance_usd * leverage - balance_used
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
            avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
            
            s <- m[i,18]
            
            # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            #     "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
            #     "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
            # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
            # cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
            #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
            #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else{
            if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
              #Close positions:
              ##BUY OMG/USD && SELL OMG/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcOMG * m[i,15] * 0.002) +
                abs(balance_usdOMG * m[i,12] * 0.002)
              temp_commission <- abs(balance_btcOMG * m[i,15] * 0.002) + 
                abs(balance_usdOMG * m[i,12] * 0.002)
              PL_btcOMG <- (m[i,15] - avg_price_btcOMG)*balance_btcOMG
              PL_usdOMG <- (m[i,12] - avg_price_usdOMG)*balance_usdOMG
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcOMG <- 0
                PL_usdOMG <- 0
              }
              PL <- PL_usdOMG + PL_btcOMG
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,15] * 0.002) +
                                                abs(balance_usdOMG * m[i,12] * 0.002))
              balance_btcOMG <- 0
              balance_usdOMG <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcOMG * m[i,15] * 0.002) +
                          abs(balance_usdOMG * m[i,12] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              #     "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_positive_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcOMG <- 0
              temp_usdOMG <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcOMG <- 0
              PL_usdOMG <- 0
              
            }
            else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
              #Close positions:
              ##SELL OMG/USD && BUY OMG/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcOMG * m[i,16] * 0.002) +
                abs(balance_usdOMG * m[i,11] * 0.002)
              temp_commission <- abs(balance_btcOMG * m[i,16] * 0.002) +
                abs(balance_usdOMG * m[i,11] * 0.002)
              PL_btcOMG <- (m[i,16] - avg_price_btcOMG)*balance_btcOMG
              PL_usdOMG <- (m[i,11] - avg_price_usdOMG)*balance_usdOMG
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcOMG <- 0
                PL_usdOMG <- 0
              }
              PL <- PL_usdOMG + PL_btcOMG
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,16] * 0.002) +
                                                abs(balance_usdOMG * m[i,11] * 0.002))
              balance_btcOMG <- 0
              balance_usdOMG <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcOMG * m[i,16] * 0.002) +
                          abs(balance_usdOMG * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              #     "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcOMG <- 0
              temp_usdOMG <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcOMG <- 0
              PL_usdOMG <- 0
              
            }
          }
        }
      }
      if (i == nrow(m)-4){
        # cat("Start testing:", as.character(as.POSIXct(m[1,1], origin = "1970-01-1")),
        #     "End testing",as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
        #     "Estimating period (Days):",format(round((m[i,1]-m[1,1])/86400,0), nsmall = 0),"\n")
        # cat("Start balance USD:",4000,"\n")
        # cat("End balance USD:",balance_usd,"\n")
        # cat("Total deals:",length(profit_sum),"\n")
        # cat("Total profit:",sum(profit_sum),"\n")
        # cat("Profit per deal:",mean(profit_sum),"\n")
        # cat("Average duration (H):",mean(duration),"\n")
        # cat("Average duration (M):",mean(duration) * 60,"\n")
        # cat("Average duration (S):",mean(duration) * 3600,"\n")
        # cat("Average profit percent:",format(round(mean(profit_perc)*100,2), nsmall = 2),"%\n")
        # cat("Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%\n")
        # cat("Max profit:",max(profit_sum),"\n")
        # cat("Min profit:",min(profit_sum),"\n")
        # cat("Paid commission:",sum(comm),"\n")
      }
    }
    
    t2 <- Sys.time()
    cat("hb=",hb,"hn=",hn,"Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),
        "% Total deals:",length(profit_sum),"time difference of",t2-t1,"secs\n")
    df <- data.frame(hb,hn,format(round((balance_usd/4000-1)*100,2), nsmall = 2),length(profit_sum))
    x <- c("hb", "hn", "profit", "deals")
    colnames(df) <- x
    resH <- rbind(resH,df)
  }
}
setwd("C:/Users/AlgoTrader/Downloads")
write.csv(resH, "OMG_ResH.csv", row.names = FALSE)

#####################################################################

hb <- 100
hn <- 99
lb <- -100
ln <- -99

resL <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("lb", "ln", "profit", "deals")
colnames(resL) <- x
for(lb in seq(-15.0,15.0,by=0.2)){
  for(ln in seq(lb+1,lb+10,by=0.2)){
    
    t1 <- Sys.time()
    
    trade <- 0
    trade_positive_difference <- 0
    trade_negative_difference <- 0
    start <- 0
    end <- 0
    s <- 0
    start_balance <- 4000
    balance_usd <- 4000
    balance_btcOMG <- 0
    balance_usdOMG <- 0
    # balance_btc <- 0
    # balance_OMG <- 0
    leverage <- 2
    PL <- 0
    commission <- 0
    balance_avail <- balance_usd * leverage
    balance_used <- 0
    deal <- 0
    profit_sum <- c()
    profit_perc <- c()
    duration <- c()
    step_entry <- 0.1
    step_continue <- 0.3
    comm <- c()
    scaling <- 1.1
    
    for(i in seq(11, nrow(m), by = 5)){
      if(trade == 0){
        if(m[i,17] < 20 & m[i,17] > -20 & m[i,18] < 20 & m[i,18] > -20){
          if(m[i,17] > hb){#min(m[i,17],m[i-5,17],m[i-10,17])
            trade <- 1
            trade_positive_difference <- 1
            
            #SELL OMG/USD && BUY OMG/BTC/USD
            deal <- balance_avail * step_entry
            commission <- deal * 0.002 * 2
            start_balance <- balance_usd
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_btcOMG <- deal / m[i,16]
            balance_usdOMG <- -deal / m[i,11]
            # balance_btc <- deal / m[i,3]
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdOMG <- m[i,11]
            avg_price_btcOMG <- m[i,16]
            
            start <- m[i,1]
            s <- m[i,17]
            # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
            #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
            # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
            # cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
            #     "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
            #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else {
            if(m[i,18] < lb){#max(m[i,18],m[i-5,18],m[i-10,18])
              trade <- 1
              trade_negative_difference <- 1
              
              #BUY USD/OMG && SELL USD/BTC/OMG
              deal <- balance_avail * step_entry
              commission <- deal * 0.002 * 2
              start_balance <- balance_usd
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_btcOMG <- -deal / m[i,15]
              balance_usdOMG <- deal / m[i,12]
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_usdOMG <- m[i,12]
              avg_price_btcOMG <- m[i,15]
              
              start <- m[i,1]
              s <- m[i,18]
              # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
              #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance BTC/OMG:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance USD/OMG:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
              # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
            }
          }
        }
        else next
      }
      else{
        if(m[i,17] > s + scaling && trade_positive_difference == 1 && balance_avail > 100){
          
          if(balance_avail < 200){
            deal <- balance_avail - 100
          }
          else{
            deal <- balance_avail * step_continue
          }
          
          temp_commission <- commission
          commission <- temp_commission + deal * 0.002 * 2
          temp_usd <- balance_usd
          balance_usd <- temp_usd - deal * 0.002 * 2
          temp_btcOMG <- balance_btcOMG
          balance_btcOMG <- temp_btcOMG + deal / m[i,16]
          temp_usdOMG <- balance_usdOMG
          balance_usdOMG <- temp_usdOMG - deal / m[i,11]
          temp_used <- balance_used
          balance_used <- temp_used + deal * 2
          temp_avail <- balance_avail
          balance_avail <- balance_usd * leverage - balance_used
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
          avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
          
          s <- m[i,17]
          
          # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          #     "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
          #     "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
          # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
          #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
          #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
          #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          # cat("SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
          #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
          #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
          # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
        else{
          if(m[i,18] < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
            
            if(balance_avail < 200){
              deal <- balance_avail - 100
            }
            else{
              deal <- balance_avail * step_continue
            }
            temp_commission <- commission
            commission <- temp_commission + deal * 0.002 * 2
            temp_usd <- balance_usd
            balance_usd <- temp_usd - deal * 0.002 * 2
            temp_btcOMG <- balance_btcOMG
            balance_btcOMG <- temp_btcOMG - deal / m[i,15]
            temp_usdOMG <- balance_usdOMG
            balance_usdOMG <- temp_usdOMG + deal / m[i,12]
            temp_used <- balance_used
            balance_used <- temp_used + deal * 2
            temp_avail <- balance_avail
            balance_avail <- balance_usd * leverage - balance_used
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdOMG <- (balance_used / 2) / abs(balance_usdOMG)
            avg_price_btcOMG <- (balance_used / 2) / abs(balance_btcOMG)
            
            s <- m[i,18]
            
            # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            #     "Balance OMG/BTC:",format(round(temp_btcOMG,2), nsmall = 2),
            #     "Balance OMG/USD:",format(round(temp_usdOMG,2), nsmall = 2),"\n")
            # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
            # cat("BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
            #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
            #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else{
            if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
              #Close positions:
              ##BUY OMG/USD && SELL OMG/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcOMG * m[i,15] * 0.002) +
                abs(balance_usdOMG * m[i,12] * 0.002)
              temp_commission <- abs(balance_btcOMG * m[i,15] * 0.002) + 
                abs(balance_usdOMG * m[i,12] * 0.002)
              PL_btcOMG <- (m[i,15] - avg_price_btcOMG)*balance_btcOMG
              PL_usdOMG <- (m[i,12] - avg_price_usdOMG)*balance_usdOMG
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcOMG <- 0
                PL_usdOMG <- 0
              }
              PL <- PL_usdOMG + PL_btcOMG
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,15] * 0.002) +
                                                abs(balance_usdOMG * m[i,12] * 0.002))
              balance_btcOMG <- 0
              balance_usdOMG <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcOMG * m[i,15] * 0.002) +
                          abs(balance_usdOMG * m[i,12] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              #     "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_positive_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcOMG <- 0
              temp_usdOMG <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcOMG <- 0
              PL_usdOMG <- 0
              
            }
            else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
              #Close positions:
              ##SELL OMG/USD && BUY OMG/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcOMG * m[i,16] * 0.002) +
                abs(balance_usdOMG * m[i,11] * 0.002)
              temp_commission <- abs(balance_btcOMG * m[i,16] * 0.002) +
                abs(balance_usdOMG * m[i,11] * 0.002)
              PL_btcOMG <- (m[i,16] - avg_price_btcOMG)*balance_btcOMG
              PL_usdOMG <- (m[i,11] - avg_price_usdOMG)*balance_usdOMG
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcOMG <- 0
                PL_usdOMG <- 0
              }
              PL <- PL_usdOMG + PL_btcOMG
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcOMG * m[i,16] * 0.002) +
                                                abs(balance_usdOMG * m[i,11] * 0.002))
              balance_btcOMG <- 0
              balance_usdOMG <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcOMG * m[i,16] * 0.002) +
                          abs(balance_usdOMG * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance OMG/BTC:", format(round(balance_btcOMG,2), nsmall = 2),
              #     "Balance OMG/USD:", format(round(balance_usdOMG,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL OMG/USD=",format(round(PL_usdOMG,0), nsmall = 0),
              #     "PL OMG/BTC=",format(round(PL_btcOMG,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcOMG <- 0
              temp_usdOMG <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcOMG <- 0
              PL_usdOMG <- 0
              
            }
          }
        }
      }
      if (i == nrow(m)-4){
        # cat("Start testing:", as.character(as.POSIXct(m[1,1], origin = "1970-01-1")),
        #     "End testing",as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
        #     "Estimating period (Days):",format(round((m[i,1]-m[1,1])/86400,0), nsmall = 0),"\n")
        # cat("Start balance USD:",4000,"\n")
        # cat("End balance USD:",balance_usd,"\n")
        # cat("Total deals:",length(profit_sum),"\n")
        # cat("Total profit:",sum(profit_sum),"\n")
        # cat("Profit per deal:",mean(profit_sum),"\n")
        # cat("Average duration (H):",mean(duration),"\n")
        # cat("Average duration (M):",mean(duration) * 60,"\n")
        # cat("Average duration (S):",mean(duration) * 3600,"\n")
        # cat("Average profit percent:",format(round(mean(profit_perc)*100,2), nsmall = 2),"%\n")
        # cat("Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%\n")
        # cat("Max profit:",max(profit_sum),"\n")
        # cat("Min profit:",min(profit_sum),"\n")
        # cat("Paid commission:",sum(comm),"\n")
      }
    }
    
    t2 <- Sys.time()
    cat("lb=",lb,"ln=",ln,"Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),
        "% Total deals:",length(profit_sum),"time difference of",t2-t1,"secs\n")
    df <- data.frame(lb,ln,format(round((balance_usd/4000-1)*100,2), nsmall = 2),length(profit_sum))
    x <- c("lb", "ln", "profit","deals")
    colnames(df) <- x
    resL <- rbind(resL,df)
  }
}
setwd("C:/Users/AlgoTrader/Downloads")
write.csv(resL, "OMG_ResL.csv", row.names = FALSE)

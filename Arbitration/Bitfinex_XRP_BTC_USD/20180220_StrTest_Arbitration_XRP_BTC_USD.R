library(dplyr)
library(plyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

setwd("C:/btc/Orderbook/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/XRPBTC")
XRPBTC <- read.csv("BITF_XRPBTC_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/XRPUSD")
XRPUSD <- read.csv("BITF_XRPUSD_ob_20170810_20180211.csv", header = TRUE)

BTCUSD$date <- as.POSIXct(BTCUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
BTCUSD$day <- as.Date(BTCUSD$date, 'GMT')
BTCUSD$H <- hour(BTCUSD$date)
BTCUSD$M <- minute(BTCUSD$date)
XRPBTC$date <- as.POSIXct(XRPBTC$date %/% 1000, origin="1970-01-01", tz="GMT")
XRPBTC$day <- as.Date(XRPBTC$date, 'GMT')
XRPBTC$H <- hour(XRPBTC$date)
XRPBTC$M <- minute(XRPBTC$date)
XRPUSD$date <- as.POSIXct(XRPUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
XRPUSD$day <- as.Date(XRPUSD$date, 'GMT')
XRPUSD$H <- hour(XRPUSD$date)
XRPUSD$M <- minute(XRPUSD$date)
BTCUSD <- data.table(BTCUSD)
XRPBTC <- data.table(XRPBTC)
XRPUSD <- data.table(XRPUSD)
require(data.table)
XRP <- BTCUSD[XRPBTC[XRPUSD, mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L],
              mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L]
XRP <- XRP[,-c(7:10,15)]
colnames(XRP) <- c("Date","Index","BTC/USD_bid","BTC/USD_ask","BTC/USD_am_bid",
                   "BTC/USD_am_ask","XRP/BTC_bid","XRP/BTC_ask","XRP/BTC_am_bid",
                   "XRP/BTC_am_ask","XRP/USD_bid","XRP/USD_ask","XRP/USD_am_bid",
                   "XRP/USD_am_ask")
XRP$Date <- as.numeric(XRP$Date)
XRP <- na.locf(XRP)
XRP$"XRP/BTC/USD_bid" <- XRP$"BTC/USD_bid" * XRP$"XRP/BTC_bid"
XRP$"XRP/BTC/USD_ask" <- XRP$"BTC/USD_ask" * XRP$"XRP/BTC_ask"
XRP$S1 <- round((XRP$"XRP/USD_bid" / XRP$"XRP/BTC/USD_ask" - 1) * 100, 2)
XRP$S2 <- round((XRP$"XRP/USD_ask" / XRP$"XRP/BTC/USD_bid" - 1) * 100, 2)
XRP <- XRP[order(XRP[,1],XRP[,2]),]
XRP <- unique(XRP, by=c("Date", "Index"))
XRP <- XRP[!duplicated(XRP),]

head(m,10)
str(XRP)

m <- data.matrix(XRP)
rm(BTCUSD,XRPBTC,XRPUSD,XRP)
gc()
cat("\014")

hb <- -0.4
hn <- -1.4
lb <- 0.4
ln <- 1.4

t1 <- Sys.time()

trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_btcXRP <- 0
balance_usdXRP <- 0
# balance_btc <- 0
# balance_XRP <- 0
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
scaling <- 100.0

for(i in seq(11, nrow(m), by = 5)){
  if(trade == 0){
    if(TRUE){
      if(min(m[i,17],m[i-5,17],m[i-10,17]) > hb){
        trade <- 1
        trade_positive_difference <- 1
        
        #SELL XRP/USD && BUY XRP/BTC/USD
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcXRP <- deal / m[i,16]
        balance_usdXRP <- -deal / m[i,11]
        # balance_btc <- deal / m[i,3]
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdXRP <- m[i,11]
        avg_price_btcXRP <- m[i,16]
        
        start <- m[i,1]
        s <- m[i,17]
        cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Equity USD:",format(round(start_balance,2), nsmall = 2),
            "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
            "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
            "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
            "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
            "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
        cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
            "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
            "Commission:", format(round(commission,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else {
        if(max(m[i,18],m[i-5,18],m[i-10,18]) < lb){
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/XRP && SELL USD/BTC/XRP
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcXRP <- -deal / m[i,15]
          balance_usdXRP <- deal / m[i,12]
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdXRP <- m[i,12]
          avg_price_btcXRP <- m[i,15]
          
          start <- m[i,1]
          s <- m[i,18]
          cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              "Equity USD:",format(round(start_balance,2), nsmall = 2),
              "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
              "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
              "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
              "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
              "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
          cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
              "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
              "Commission:", format(round(commission,2), nsmall = 2),"\n")
          cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if((m[i,17] > s + scaling) && trade_positive_difference == 1 && balance_avail > 100){
      
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
      temp_btcXRP <- balance_btcXRP
      balance_btcXRP <- temp_btcXRP + deal / m[i,16]
      temp_usdXRP <- balance_usdXRP
      balance_usdXRP <- temp_usdXRP - deal / m[i,11]
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
      avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
      
      s <- m[i,17]
      
      cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
          "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
      cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
          "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
          "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
          "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
          "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
      cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
          "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
          "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
      cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
    }
    else{
      if((m[i,18] < s - scaling) && trade_negative_difference == 1 && balance_avail > 100){
        
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
        temp_btcXRP <- balance_btcXRP
        balance_btcXRP <- temp_btcXRP - deal / m[i,15]
        temp_usdXRP <- balance_usdXRP
        balance_usdXRP <- temp_usdXRP + deal / m[i,12]
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
        avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
        
        s <- m[i,18]
        
        cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
            "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
        cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
            "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
            "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
            "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
            "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
        cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
            "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
            "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
          #Close positions:
          ##BUY XRP/USD && SELL XRP/BTC/USD
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcXRP * m[i,15] * 0.002) +
            abs(balance_usdXRP * m[i,12] * 0.002)
          temp_commission <- abs(balance_btcXRP * m[i,15] * 0.002) + 
            abs(balance_usdXRP * m[i,12] * 0.002)
          PL_btcXRP <- (m[i,15] - avg_price_btcXRP)*balance_btcXRP
          PL_usdXRP <- (m[i,12] - avg_price_usdXRP)*balance_usdXRP
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_btcXRP <- 0
            PL_usdXRP <- 0
          }
          PL <- PL_usdXRP + PL_btcXRP
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,15] * 0.002) +
                                            abs(balance_usdXRP * m[i,12] * 0.002))
          balance_btcXRP <- 0
          balance_usdXRP <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcXRP * m[i,15] * 0.002) +
                      abs(balance_usdXRP * m[i,12] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
              "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
              "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
              "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
              "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcXRP <- 0
          temp_usdXRP <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcXRP <- 0
          PL_usdXRP <- 0
          
        }
        else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
          #Close positions:
          ##SELL XRP/USD && BUY XRP/BTC/USD
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcXRP * m[i,16] * 0.002) +
            abs(balance_usdXRP * m[i,11] * 0.002)
          temp_commission <- abs(balance_btcXRP * m[i,15] * 0.002) +
            abs(balance_usdXRP * m[i,4] * 0.002)
          PL_btcXRP <- (m[i,16] - avg_price_btcXRP)*balance_btcXRP
          PL_usdXRP <- (m[i,11] - avg_price_usdXRP)*balance_usdXRP
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_btcXRP <- 0
            PL_usdXRP <- 0
          }
          PL <- PL_usdXRP + PL_btcXRP
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,16] * 0.002) +
                                            abs(balance_usdXRP * m[i,11] * 0.002))
          balance_btcXRP <- 0
          balance_usdXRP <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcXRP * m[i,16] * 0.002) +
                      abs(balance_usdXRP * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("XRP/USD bid:", format(round(m[i,11],4), nsmall = 4),
              "XRP/USD ask:", format(round(m[i,12],4), nsmall = 4),
              "| XRP/BTC/USD bid:", format(round(m[i,15],4), nsmall = 4),
              "XRP/BTC/USD ask:", format(round(m[i,16],4), nsmall = 4),
              "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcXRP <- 0
          temp_usdXRP <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcXRP <- 0
          PL_usdXRP <- 0
          
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
    balance_btcXRP <- 0
    balance_usdXRP <- 0
    # balance_btc <- 0
    # balance_XRP <- 0
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
        if(TRUE){
          if(min(m[i,17],m[i-5,17],m[i-10,17]) > hb){
            trade <- 1
            trade_positive_difference <- 1
            
            #SELL XRP/USD && BUY XRP/BTC/USD
            deal <- balance_avail * step_entry
            commission <- deal * 0.002 * 2
            start_balance <- balance_usd
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_btcXRP <- deal / m[i,16]
            balance_usdXRP <- -deal / m[i,11]
            # balance_btc <- deal / m[i,3]
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdXRP <- m[i,11]
            avg_price_btcXRP <- m[i,16]
            
            start <- m[i,1]
            s <- m[i,17]
            # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
            #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
            # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
            # cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
            #     "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
            #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else {
            if(max(m[i,18],m[i-5,18],m[i-10,18]) < lb){
              trade <- 1
              trade_negative_difference <- 1
              
              #BUY USD/XRP && SELL USD/BTC/XRP
              deal <- balance_avail * step_entry
              commission <- deal * 0.002 * 2
              start_balance <- balance_usd
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_btcXRP <- -deal / m[i,15]
              balance_usdXRP <- deal / m[i,12]
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_usdXRP <- m[i,12]
              avg_price_btcXRP <- m[i,15]
              
              start <- m[i,1]
              s <- m[i,18]
              # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
              #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
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
          temp_btcXRP <- balance_btcXRP
          balance_btcXRP <- temp_btcXRP + deal / m[i,16]
          temp_usdXRP <- balance_usdXRP
          balance_usdXRP <- temp_usdXRP - deal / m[i,11]
          temp_used <- balance_used
          balance_used <- temp_used + deal * 2
          temp_avail <- balance_avail
          balance_avail <- balance_usd * leverage - balance_used
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
          avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
          
          s <- m[i,17]
          
          # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          #     "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
          #     "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
          # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
          #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
          #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
          #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          # cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
          #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
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
            temp_btcXRP <- balance_btcXRP
            balance_btcXRP <- temp_btcXRP - deal / m[i,15]
            temp_usdXRP <- balance_usdXRP
            balance_usdXRP <- temp_usdXRP + deal / m[i,12]
            temp_used <- balance_used
            balance_used <- temp_used + deal * 2
            temp_avail <- balance_avail
            balance_avail <- balance_usd * leverage - balance_used
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
            avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
            
            s <- m[i,18]
            
            # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            #     "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
            #     "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
            # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
            # cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
            #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
            #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else{
            if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
              #Close positions:
              ##BUY XRP/USD && SELL XRP/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcXRP * m[i,15] * 0.002) +
                abs(balance_usdXRP * m[i,12] * 0.002)
              temp_commission <- abs(balance_btcXRP * m[i,15] * 0.002) + 
                abs(balance_usdXRP * m[i,12] * 0.002)
              PL_btcXRP <- (m[i,15] - avg_price_btcXRP)*balance_btcXRP
              PL_usdXRP <- (m[i,12] - avg_price_usdXRP)*balance_usdXRP
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcXRP <- 0
                PL_usdXRP <- 0
              }
              PL <- PL_usdXRP + PL_btcXRP
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,15] * 0.002) +
                                                abs(balance_usdXRP * m[i,12] * 0.002))
              balance_btcXRP <- 0
              balance_usdXRP <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcXRP * m[i,15] * 0.002) +
                          abs(balance_usdXRP * m[i,12] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              #     "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_positive_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcXRP <- 0
              temp_usdXRP <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcXRP <- 0
              PL_usdXRP <- 0
              
            }
            else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
              #Close positions:
              ##SELL XRP/USD && BUY XRP/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcXRP * m[i,16] * 0.002) +
                abs(balance_usdXRP * m[i,11] * 0.002)
              temp_commission <- abs(balance_btcXRP * m[i,16] * 0.002) +
                abs(balance_usdXRP * m[i,11] * 0.002)
              PL_btcXRP <- (m[i,16] - avg_price_btcXRP)*balance_btcXRP
              PL_usdXRP <- (m[i,11] - avg_price_usdXRP)*balance_usdXRP
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcXRP <- 0
                PL_usdXRP <- 0
              }
              PL <- PL_usdXRP + PL_btcXRP
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,16] * 0.002) +
                                                abs(balance_usdXRP * m[i,11] * 0.002))
              balance_btcXRP <- 0
              balance_usdXRP <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcXRP * m[i,16] * 0.002) +
                          abs(balance_usdXRP * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              #     "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcXRP <- 0
              temp_usdXRP <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcXRP <- 0
              PL_usdXRP <- 0
              
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
write.csv(resH, "XRP_ResH.csv", row.names = FALSE)

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
    balance_btcXRP <- 0
    balance_usdXRP <- 0
    # balance_btc <- 0
    # balance_XRP <- 0
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
        if(TRUE){
          if(min(m[i,17],m[i-5,17],m[i-10,17]) > hb){
            trade <- 1
            trade_positive_difference <- 1
            
            #SELL XRP/USD && BUY XRP/BTC/USD
            deal <- balance_avail * step_entry
            commission <- deal * 0.002 * 2
            start_balance <- balance_usd
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_btcXRP <- deal / m[i,16]
            balance_usdXRP <- -deal / m[i,11]
            # balance_btc <- deal / m[i,3]
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdXRP <- m[i,11]
            avg_price_btcXRP <- m[i,16]
            
            start <- m[i,1]
            s <- m[i,17]
            # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
            #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
            # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
            # cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
            #     "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
            #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else {
            if(max(m[i,18],m[i-5,18],m[i-10,18]) < lb){
              trade <- 1
              trade_negative_difference <- 1
              
              #BUY USD/XRP && SELL USD/BTC/XRP
              deal <- balance_avail * step_entry
              commission <- deal * 0.002 * 2
              start_balance <- balance_usd
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_btcXRP <- -deal / m[i,15]
              balance_usdXRP <- deal / m[i,12]
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_usdXRP <- m[i,12]
              avg_price_btcXRP <- m[i,15]
              
              start <- m[i,1]
              s <- m[i,18]
              # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
              #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance BTC/XRP:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance USD/XRP:", format(round(balance_usdXRP,2), nsmall = 2),
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
          temp_btcXRP <- balance_btcXRP
          balance_btcXRP <- temp_btcXRP + deal / m[i,16]
          temp_usdXRP <- balance_usdXRP
          balance_usdXRP <- temp_usdXRP - deal / m[i,11]
          temp_used <- balance_used
          balance_used <- temp_used + deal * 2
          temp_avail <- balance_avail
          balance_avail <- balance_usd * leverage - balance_used
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
          avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
          
          s <- m[i,17]
          
          # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          #     "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
          #     "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
          # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
          #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
          #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
          #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          # cat("SELL XRP/USD && BUY XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
          #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
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
            temp_btcXRP <- balance_btcXRP
            balance_btcXRP <- temp_btcXRP - deal / m[i,15]
            temp_usdXRP <- balance_usdXRP
            balance_usdXRP <- temp_usdXRP + deal / m[i,12]
            temp_used <- balance_used
            balance_used <- temp_used + deal * 2
            temp_avail <- balance_avail
            balance_avail <- balance_usd * leverage - balance_used
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_usdXRP <- (balance_used / 2) / abs(balance_usdXRP)
            avg_price_btcXRP <- (balance_used / 2) / abs(balance_btcXRP)
            
            s <- m[i,18]
            
            # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            #     "Balance XRP/BTC:",format(round(temp_btcXRP,2), nsmall = 2),
            #     "Balance XRP/USD:",format(round(temp_usdXRP,2), nsmall = 2),"\n")
            # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
            #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
            #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
            # cat("BUY XRP/USD && SELL XRP/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
            #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
            #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
            # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
          }
          else{
            if(m[i,18] < hn && trade_positive_difference == 1 || i == nrow(m)-4 && trade_positive_difference == 1){
              #Close positions:
              ##BUY XRP/USD && SELL XRP/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcXRP * m[i,15] * 0.002) +
                abs(balance_usdXRP * m[i,12] * 0.002)
              temp_commission <- abs(balance_btcXRP * m[i,15] * 0.002) + 
                abs(balance_usdXRP * m[i,12] * 0.002)
              PL_btcXRP <- (m[i,15] - avg_price_btcXRP)*balance_btcXRP
              PL_usdXRP <- (m[i,12] - avg_price_usdXRP)*balance_usdXRP
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcXRP <- 0
                PL_usdXRP <- 0
              }
              PL <- PL_usdXRP + PL_btcXRP
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,15] * 0.002) +
                                                abs(balance_usdXRP * m[i,12] * 0.002))
              balance_btcXRP <- 0
              balance_usdXRP <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcXRP * m[i,15] * 0.002) +
                          abs(balance_usdXRP * m[i,12] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              #     "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_positive_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcXRP <- 0
              temp_usdXRP <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcXRP <- 0
              PL_usdXRP <- 0
              
            }
            else if(m[i,17] > ln && trade_negative_difference == 1 || i == nrow(m)-4 && trade_negative_difference == 1){
              #Close positions:
              ##SELL XRP/USD && BUY XRP/BTC/USD
              
              temp_commission <- commission
              commission <- temp_commission + abs(balance_btcXRP * m[i,16] * 0.002) +
                abs(balance_usdXRP * m[i,11] * 0.002)
              temp_commission <- abs(balance_btcXRP * m[i,16] * 0.002) +
                abs(balance_usdXRP * m[i,11] * 0.002)
              PL_btcXRP <- (m[i,16] - avg_price_btcXRP)*balance_btcXRP
              PL_usdXRP <- (m[i,11] - avg_price_usdXRP)*balance_usdXRP
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcXRP <- 0
                PL_usdXRP <- 0
              }
              PL <- PL_usdXRP + PL_btcXRP
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_btcXRP * m[i,16] * 0.002) +
                                                abs(balance_usdXRP * m[i,11] * 0.002))
              balance_btcXRP <- 0
              balance_usdXRP <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_btcXRP * m[i,16] * 0.002) +
                          abs(balance_usdXRP * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("XRP/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "XRP/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "| XRP/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "XRP/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              #     "Balance XRP/BTC:", format(round(balance_btcXRP,2), nsmall = 2),
              #     "Balance XRP/USD:", format(round(balance_usdXRP,2), nsmall = 2),
              #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
              # cat("PL XRP/USD=",format(round(PL_usdXRP,0), nsmall = 0),
              #     "PL XRP/BTC=",format(round(PL_btcXRP,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Total commission:", format(round(commission,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_btcXRP <- 0
              temp_usdXRP <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_btcXRP <- 0
              PL_usdXRP <- 0
              
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
write.csv(resL, "XRP_ResL.csv", row.names = FALSE)

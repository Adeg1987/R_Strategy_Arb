library(dplyr)
library(plyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

setwd("C:/btc/Orderbook/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/BCHBTC")
BCHBTC <- read.csv("BITF_BCHBTC_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/BCHUSD")
BCHUSD <- read.csv("BITF_BCHUSD_ob_20170810_20180211.csv", header = TRUE)

BTCUSD$date <- as.POSIXct(BTCUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
BTCUSD$day <- as.Date(BTCUSD$date, 'GMT')
BTCUSD$H <- hour(BTCUSD$date)
BTCUSD$M <- minute(BTCUSD$date)
BCHBTC$date <- as.POSIXct(BCHBTC$date %/% 1000, origin="1970-01-01", tz="GMT")
BCHBTC$day <- as.Date(BCHBTC$date, 'GMT')
BCHBTC$H <- hour(BCHBTC$date)
BCHBTC$M <- minute(BCHBTC$date)
BCHUSD$date <- as.POSIXct(BCHUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
BCHUSD$day <- as.Date(BCHUSD$date, 'GMT')
BCHUSD$H <- hour(BCHUSD$date)
BCHUSD$M <- minute(BCHUSD$date)
BTCUSD <- data.table(BTCUSD)
BCHBTC <- data.table(BCHBTC)
BCHUSD <- data.table(BCHUSD)
require(data.table)
BCH <- BTCUSD[BCHBTC[BCHUSD, mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L],
              mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L]
BCH <- BCH[,-c(7:10,15)]
colnames(BCH) <- c("Date","Index","BTC/USD_bid","BTC/USD_ask","BTC/USD_am_bid",
                   "BTC/USD_am_ask","BCH/BTC_bid","BCH/BTC_ask","BCH/BTC_am_bid",
                   "BCH/BTC_am_ask","BCH/USD_bid","BCH/USD_ask","BCH/USD_am_bid",
                   "BCH/USD_am_ask")
BCH$Date <- as.numeric(BCH$Date)
BCH <- na.locf(BCH)
BCH$"BCH/BTC/USD_bid" <- BCH$"BTC/USD_bid" * BCH$"BCH/BTC_bid"
BCH$"BCH/BTC/USD_ask" <- BCH$"BTC/USD_ask" * BCH$"BCH/BTC_ask"
BCH$S1 <- round((BCH$"BCH/USD_bid" / BCH$"BCH/BTC/USD_ask" - 1) * 100, 2)
BCH$S2 <- round((BCH$"BCH/USD_ask" / BCH$"BCH/BTC/USD_bid" - 1) * 100, 2)
BCH <- BCH[order(BCH[,1],BCH[,2]),]
BCH <- unique(BCH, by=c("Date", "Index"))
BCH <- BCH[!duplicated(BCH),]

head(BCH,10)
str(BCH)

m <- data.matrix(BCH)
rm(BTCUSD,BCHBTC,BCHUSD,BCH)
gc()
cat("\014")

cond_open <- -0.8
cond_close <- 0.1
stop_loss <- -10.0
stop_loss_dur <- 86400 * 6
trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_BCHBTC <- 0
balance_BCHUSD <- 0
balance_BTCUSD <- 0
leverage <- 2
PL <- 0
commission <- 0
balance_avail <- balance_usd * leverage
balance_used <- 0
deal <- 0
profit_sum <- c()
profit_perc <- c()
duration <- c()
BuyBCHBTC <- 0
BuyBCHUSD <- 0
step_entry <- 0.25
step_continue <- 0.25
comm <- c()
scaling <- 1.2
BTCUSD_cond <- 0

for(i in seq(11, nrow(m), by = 5)){
  if(balance_usd <= 0) break
  if(trade == 0){
    if(((m[i,16]/m[i,11])-1)*100 < cond_open){
      trade <- 1
      trade_positive_difference <- 1
      
      #OPEN 1
      #SELL BCH/USD && BUY BCH/BTC/USD
      deal <- balance_avail * step_entry
      start_balance <- balance_usd
      if(BTCUSD_cond == 0){
        commission <- deal * 0.002 * 2
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_BCHBTC <- deal / m[i,16]
        balance_BCHUSD <- -deal / m[i,11]
        balance_BTCUSD <- 0
      }
      else{
        commission <- deal * 0.002 * 3
        balance_usd <- start_balance - commission
        balance_used <- deal * 3
        balance_avail <- balance_usd * leverage - balance_used
        balance_BCHBTC <- deal / m[i,16]
        balance_BCHUSD <- -deal / m[i,11]
        balance_BTCUSD <- deal / m[i,4]
      }
      
      comm <- c(comm, commission)
      avg_price_BCHUSD <- m[i,11]
      avg_price_BCHBTC <- m[i,8]
      avg_price_BTCUSD <- m[i,4]
      BuyBCHBTC <- 1
      
      start <- m[i,1]
      s <- (m[i,16]/m[i,11]-1)*100
      cat("START. SELL BCH/USD && BUY BCH/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
          "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
      cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
          "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next;
    }
    else {
      if(((m[i,12]/m[i,15])-1)*100 < cond_open){
        trade <- 1
        trade_negative_difference <- 1
        
        #OPEN 2
        
        #BUY USD/BCH && SELL USD/BTC/BCH
        deal <- balance_avail * step_entry
        start_balance <- balance_usd
        if(BTCUSD_cond == 0){
          commission <- deal * 0.002 * 2
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_BCHBTC <- -deal / m[i,15]
          balance_BCHUSD <- deal / m[i,12]
        }
        else{
          commission <- deal * 0.002 * 3
          balance_usd <- start_balance - commission
          balance_used <- deal * 3
          balance_avail <- balance_usd * leverage - balance_used
          balance_BCHBTC <- -deal / m[i,15]
          balance_BCHUSD <- deal / m[i,12]
          balance_BTCUSD <- -deal / m[i,3]
        }
        comm <- c(comm, commission)
        avg_price_BCHUSD <- m[i,12]
        avg_price_BCHBTC <- m[i,7]
        avg_price_BTCUSD <- m[i,3]
        
        BuyBCHUSD <- 1
        
        start <- m[i,1]
        s <- (m[i,12]/m[i,15]-1)*100
        cat("START. BUY BCH/USD && SELL BCH/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
            "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next;
      }
    }
  }
  else{
    if(BuyBCHBTC == 1){
      PL_BCHBTC <- (m[i,7] - avg_price_BCHBTC)*balance_BCHBTC*m[i,3]
      PL_BCHUSD <- (m[i,12] - avg_price_BCHUSD)*balance_BCHUSD
      if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
      else PL_BTCUSD <- {(m[i,3] - avg_price_BTCUSD)*balance_BTCUSD}
      
      if(((PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
          || (PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
          || m[i,1]-start > stop_loss_dur
          || i==nrow(m)-4) && trade_positive_difference == 1){
        #CLOSE 1:
        ##BUY BCH/USD && SELL BCH/BTC/USD
        temp_commission <- commission
        if(BTCUSD_cond == 0){
          commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
            abs(balance_BCHUSD * m[i,12] * 0.002)
          temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
            abs(balance_BCHUSD * m[i,12] * 0.002)
          PL_BTCUSD <- 0
        }
        else{
          commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
            abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
            abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
        }
        PL_BCHBTC <- (m[i,7] - avg_price_BCHBTC)*balance_BCHBTC*m[i,3]
        PL_BCHUSD <- (m[i,12] - avg_price_BCHUSD)*balance_BCHUSD
        #if last row and then we close position and set PL=0
        if(i == nrow(m)-4){
          PL_BCHBTC <- 0
          PL_BCHUSD <- 0
          PL_BTCUSD <- 0
        }
        PL <- PL_BCHUSD + PL_BCHBTC + PL_BTCUSD
        temp_usd <- balance_usd
        balance_usd <- temp_usd + PL - temp_commission
        balance_BCHBTC <- 0
        balance_BCHUSD <- 0
        balance_BTCUSD <- 0
        balance_used <- 0
        balance_avail <- balance_usd * leverage
        
        profit_sum <- c(profit_sum, PL - commission)
        profit_perc <- c(profit_perc, (PL - commission)/start_balance)
        comm <- c(comm, temp_commission)
        
        end <- m[i,1]
        duration <- c(duration, (end - start)/3600)
        
        cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
            "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
            "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
            "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
        cat("Avg price BCH/USD:",format(round(avg_price_BCHUSD,2), nsmall = 2),
            "Avg price BCH/BTC:",format(round(avg_price_BCHBTC,6), nsmall = 6),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("PL BCH/USD=",format(round(PL_BCHUSD,0), nsmall = 0),
            "PL BCH/BTC=",format(round(PL_BCHBTC,0), nsmall = 0),
            "PL BTC/USD=",format(round(PL_BTCUSD,0), nsmall = 0),
            "Commission:", format(round(commission,0), nsmall = 0),
            "PL=", format(round(PL,0), nsmall = 0),
            "Net profit:", format(round(PL - commission,0), nsmall = 0),
            "Balance USD:", format(round(balance_usd,2), nsmall = 2),"\n","\n")
        
        trade <- 0
        trade_positive_difference <- 0
        start <- 0
        end <- 0
        temp_usd <- 0
        temp_BCHBTC <- 0
        temp_BCHUSD <- 0
        temp_avail <- 0
        temp_used <- 0
        commission <- 0
        temp_commission <- 0
        s <- 0
        PL <- 0
        PL_BCHBTC <- 0
        PL_BCHUSD <- 0
        PL_BTCUSD <- 0
        BuyBCHBTC <- 0
        next;
      }
      
      #SCALING 1
      if((m[i,16]/m[i,11]-1)*100 < s - scaling && trade_positive_difference == 1 && balance_avail > 100){
        
        if(balance_avail < 200){
          deal <- balance_avail - 100
        }
        else{
          deal <- balance_avail * step_continue
        }
        
        temp_commission <- commission
        temp_usd <- balance_usd
        temp_used <- balance_used
        temp_BCHBTC <- balance_BCHBTC
        balance_BCHBTC <- temp_BCHBTC + deal / m[i,16]
        temp_BCHUSD <- balance_BCHUSD
        balance_BCHUSD <- temp_BCHUSD - deal / m[i,11]
        temp_BTCUSD <- balance_BTCUSD
        balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
        temp_avail <- balance_avail
        
        if(BTCUSD_cond == 0){
          commission <- temp_commission + deal * 0.002 * 2
          balance_usd <- temp_usd - deal * 0.002 * 2
          balance_used <- temp_used + deal * 2
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_BCHUSD <- (balance_used / 2) / abs(balance_BCHUSD)
        }
        else{
          commission <- temp_commission + deal * 0.002 * 3
          balance_usd <- temp_usd - deal * 0.002 * 3
          balance_used <- temp_used + deal * 3
          comm <- c(comm, deal * 0.002 * 3)
          avg_price_BCHUSD <- (balance_used / 3) / abs(balance_BCHUSD)
          temp_price_BTCUSD <- avg_price_BTCUSD
          avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                    balance_BTCUSD)
        }
        balance_avail <- balance_usd * leverage - balance_used
        temp_price_BCHBTC <- avg_price_BCHBTC
        avg_price_BCHBTC <- abs((temp_price_BCHBTC*temp_BCHBTC + deal/m[i,16]*m[i,8]) /
                                  balance_BCHBTC)
        
        s <- (m[i,16]/m[i,11]-1)*100
        
        cat("SCALING. BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
        cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
            "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      }
    }
    else{
      if(BuyBCHUSD == 1){
        PL_BCHBTC <- (m[i,8] - avg_price_BCHBTC)*balance_BCHBTC*m[i,4]
        PL_BCHUSD <- (m[i,11] - avg_price_BCHUSD)*balance_BCHUSD
        if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
        else PL_BTCUSD <- {(m[i,4] - avg_price_BTCUSD)*balance_BTCUSD}
        
        if(((PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
            || (PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
            || m[i,1]-start > stop_loss_dur
            || i == nrow(m)-4) && trade_negative_difference == 1){
          
          #CLOSE 2:
          ##SELL BCH/USD && BUY BCH/BTC/USD
          
          temp_commission <- commission
          if(BTCUSD_cond == 0){
            commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
              abs(balance_BCHUSD * m[i,12] * 0.002)
            temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
              abs(balance_BCHUSD * m[i,12] * 0.002)
            PL_BTCUSD <- 0
          }
          else{
            commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
              abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
              abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
          }
          PL_BCHBTC <- (m[i,8] - avg_price_BCHBTC)*balance_BCHBTC*m[i,4]
          PL_BCHUSD <- (m[i,11] - avg_price_BCHUSD)*balance_BCHUSD
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_BCHBTC <- 0
            PL_BCHUSD <- 0
            PL_BTCUSD <- 0
          }
          PL <- PL_BCHUSD + PL_BCHBTC + PL_BTCUSD
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_BCHBTC * m[i,16] * 0.002) +
                                            abs(balance_BCHUSD * m[i,11] * 0.002))
          balance_BCHBTC <- 0
          balance_BCHUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_BCHBTC * m[i,16] * 0.002) +
                      abs(balance_BCHUSD * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price BCH/USD:",format(round(avg_price_BCHUSD,2), nsmall = 2),
              "Avg price BCH/BTC:",format(round(avg_price_BCHBTC,6), nsmall = 6),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
          cat("PL BCH/USD=",format(round(PL_BCHUSD,0), nsmall = 0),
              "PL BCH/BTC=",format(round(PL_BCHBTC,0), nsmall = 0),
              "PL BTC/USD=",format(round(PL_BTCUSD,0), nsmall = 0),
              "Commission:", format(round(commission,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Balance USD:", format(round(balance_usd,2), nsmall = 2),"\n","\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_BCHBTC <- 0
          temp_BCHUSD <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_BCHBTC <- 0
          PL_BCHUSD <- 0
          BuyBCHUSD <- 0
          next;
        }
        
        #SCALING 2
        if((m[i,12]/m[i,15]-1)*100 < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
          
          if(balance_avail < 200){
            deal <- balance_avail - 100
          }
          else{
            deal <- balance_avail * step_continue
          }
          
          temp_commission <- commission
          temp_usd <- balance_usd
          temp_used <- balance_used
          temp_BCHBTC <- balance_BCHBTC
          balance_BCHBTC <- temp_BCHBTC - deal / m[i,15]
          temp_BCHUSD <- balance_BCHUSD
          balance_BCHUSD <- temp_BCHUSD + deal / m[i,12]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
          temp_avail <- balance_avail
          
          
          
          if(BTCUSD_cond == 0){
            commission <- temp_commission + deal * 0.002 * 2
            balance_usd <- temp_usd - deal * 0.002 * 2
            balance_used <- temp_used + deal * 2
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_BCHUSD <- (balance_used / 2) / abs(balance_BCHUSD)
          }
          else{
            commission <- temp_commission + deal * 0.002 * 3
            balance_usd <- temp_usd - deal * 0.002 * 3
            balance_used <- temp_used + deal * 3
            comm <- c(comm, deal * 0.002 * 3)
            avg_price_BCHUSD <- (balance_used / 3) / abs(balance_BCHUSD)
            temp_price_BTCUSD <- avg_price_BTCUSD
            avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                      balance_BTCUSD)
          }
          balance_avail <- balance_usd * leverage - balance_used
          temp_price_BCHBTC <- avg_price_BCHBTC
          avg_price_BCHBTC <- abs((temp_price_BCHBTC*temp_BCHBTC - deal/m[i,15]*m[i,7]) /
                                    balance_BCHBTC)

          s <- (m[i,12]/m[i,15]-1)*100
          
          cat("SCALING. BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
          cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
              "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),"\n")
          next;
        }
      }
    }
  }
}
if (TRUE){
  cat("Start testing:", as.character(as.POSIXct(m[1,1], origin = "1970-01-1")),
      "End testing",as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
      "Estimating period (Days):",format(round((m[i,1]-m[1,1])/86400,0), nsmall = 0),"\n")
  cat("Start balance USD:",4000," End balance USD:",balance_usd,"\n")
  cat("Total deals:",length(profit_sum)," Positive deals:",sum(profit_sum>0),
      " Negative deals:",sum(profit_sum<0),"\n")
  cat("Total profit:",sum(profit_sum)," Profit per deal:",mean(profit_sum),"\n")
  cat("Average duration (H):",mean(duration)," (M):",mean(duration) * 60,
      " (S):",mean(duration) * 3600,"\n")
  cat("Average profit percent:",format(round(mean(profit_perc)*100,2), nsmall = 2),
      "% Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%\n")
  cat("Max profit:",max(profit_sum)," Min profit:",min(profit_sum)," Paid commission:",sum(comm),"\n")
}

############################################################################
#############TEST######################
#############CONDITIONS OPEN,CLOSE,STOP#####################################
res <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("cond_open", "cond_close","stop_loss","profit", "deals")
colnames(res) <- x
for(cond_open in seq(-2.0,-0.4,by=0.1)){
  for(cond_close in seq(0.1,2.0,by=0.1)){
    for(stop_loss in seq(-10,-40,by=-10)){
    
    t1 <- Sys.time()
    trade <- 0
    trade_positive_difference <- 0
    trade_negative_difference <- 0
    start <- 0
    end <- 0
    s <- 0
    start_balance <- 4000
    balance_usd <- 4000
    balance_BCHBTC <- 0
    balance_BCHUSD <- 0
    balance_BTCUSD <- 0
    leverage <- 2
    PL <- 0
    commission <- 0
    balance_avail <- balance_usd * leverage
    balance_used <- 0
    deal <- 0
    profit_sum <- c()
    profit_perc <- c()
    duration <- c()
    BuyBCHBTC <- 0
    BuyBCHUSD <- 0
    step_entry <- 0.25
    step_continue <- 0.25
    comm <- c()
    scaling <- 1.2
    BTCUSD_cond <- 0
    stop_loss_dur <- 86400 * 6

    for(i in seq(11, nrow(m), by = 5)){
      if(balance_usd <= 0) break
      if(trade == 0){
        if(((m[i,16]/m[i,11])-1)*100 < cond_open){
          trade <- 1
          trade_positive_difference <- 1
          
          #OPEN 1
          #SELL BCH/USD && BUY BCH/BTC/USD
          deal <- balance_avail * step_entry
          start_balance <- balance_usd
          if(BTCUSD_cond == 0){
            commission <- deal * 0.002 * 2
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_BCHBTC <- deal / m[i,16]
            balance_BCHUSD <- -deal / m[i,11]
            balance_BTCUSD <- 0
          }
          else{
            commission <- deal * 0.002 * 3
            balance_usd <- start_balance - commission
            balance_used <- deal * 3
            balance_avail <- balance_usd * leverage - balance_used
            balance_BCHBTC <- deal / m[i,16]
            balance_BCHUSD <- -deal / m[i,11]
            balance_BTCUSD <- deal / m[i,4]
          }
          
          comm <- c(comm, commission)
          avg_price_BCHUSD <- m[i,11]
          avg_price_BCHBTC <- m[i,8]
          avg_price_BTCUSD <- m[i,4]
          BuyBCHBTC <- 1
          
          start <- m[i,1]
          s <- (m[i,16]/m[i,11]-1)*100
          # cat("START. SELL BCH/USD && BUY BCH/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          #     "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
          # cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
          #     "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
          #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          next;
        }
        else {
          if(((m[i,12]/m[i,15])-1)*100 < cond_open){
            trade <- 1
            trade_negative_difference <- 1
            
            #OPEN 2
            
            #BUY USD/BCH && SELL USD/BTC/BCH
            deal <- balance_avail * step_entry
            start_balance <- balance_usd
            if(BTCUSD_cond == 0){
              commission <- deal * 0.002 * 2
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_BCHBTC <- -deal / m[i,15]
              balance_BCHUSD <- deal / m[i,12]
            }
            else{
              commission <- deal * 0.002 * 3
              balance_usd <- start_balance - commission
              balance_used <- deal * 3
              balance_avail <- balance_usd * leverage - balance_used
              balance_BCHBTC <- -deal / m[i,15]
              balance_BCHUSD <- deal / m[i,12]
              balance_BTCUSD <- -deal / m[i,3]
            }
            comm <- c(comm, commission)
            avg_price_BCHUSD <- m[i,12]
            avg_price_BCHBTC <- m[i,7]
            avg_price_BTCUSD <- m[i,3]
            
            BuyBCHUSD <- 1
            
            start <- m[i,1]
            s <- (m[i,12]/m[i,15]-1)*100
            # cat("START. BUY BCH/USD && SELL BCH/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
            #     "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next;
          }
        }
      }
      else{
        if(BuyBCHBTC == 1){
          PL_BCHBTC <- (m[i,7] - avg_price_BCHBTC)*balance_BCHBTC*m[i,3]
          PL_BCHUSD <- (m[i,12] - avg_price_BCHUSD)*balance_BCHUSD
          if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
          else PL_BTCUSD <- {(m[i,3] - avg_price_BTCUSD)*balance_BTCUSD}
          
          if(((PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
              || (PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
              || m[i,1]-start > stop_loss_dur
              || i==nrow(m)-4) && trade_positive_difference == 1){
            #CLOSE 1:
            ##BUY BCH/USD && SELL BCH/BTC/USD
            temp_commission <- commission
            if(BTCUSD_cond == 0){
              commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
                abs(balance_BCHUSD * m[i,12] * 0.002)
              temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
                abs(balance_BCHUSD * m[i,12] * 0.002)
              PL_BTCUSD <- 0
            }
            else{
              commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
                abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
                abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
            }
            PL_BCHBTC <- (m[i,7] - avg_price_BCHBTC)*balance_BCHBTC*m[i,3]
            PL_BCHUSD <- (m[i,12] - avg_price_BCHUSD)*balance_BCHUSD
            #if last row and then we close position and set PL=0
            if(i == nrow(m)-4){
              PL_BCHBTC <- 0
              PL_BCHUSD <- 0
              PL_BTCUSD <- 0
            }
            PL <- PL_BCHUSD + PL_BCHBTC + PL_BTCUSD
            temp_usd <- balance_usd
            balance_usd <- temp_usd + PL - temp_commission
            balance_BCHBTC <- 0
            balance_BCHUSD <- 0
            balance_BTCUSD <- 0
            balance_used <- 0
            balance_avail <- balance_usd * leverage
            
            profit_sum <- c(profit_sum, PL - commission)
            profit_perc <- c(profit_perc, (PL - commission)/start_balance)
            comm <- c(comm, temp_commission)
            
            end <- m[i,1]
            duration <- c(duration, (end - start)/3600)
            
            # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
            #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
            #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
            #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
            # cat("Avg price BCH/USD:",format(round(avg_price_BCHUSD,2), nsmall = 2),
            #     "Avg price BCH/BTC:",format(round(avg_price_BCHBTC,6), nsmall = 6),
            #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
            # cat("BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("PL BCH/USD=",format(round(PL_BCHUSD,0), nsmall = 0),
            #     "PL BCH/BTC=",format(round(PL_BCHBTC,0), nsmall = 0),
            #     "PL BTC/USD=",format(round(PL_BTCUSD,0), nsmall = 0),
            #     "Commission:", format(round(commission,0), nsmall = 0),
            #     "PL=", format(round(PL,0), nsmall = 0),
            #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
            #     "Balance USD:", format(round(balance_usd,2), nsmall = 2),"\n","\n")
            
            trade <- 0
            trade_positive_difference <- 0
            start <- 0
            end <- 0
            temp_usd <- 0
            temp_BCHBTC <- 0
            temp_BCHUSD <- 0
            temp_avail <- 0
            temp_used <- 0
            commission <- 0
            temp_commission <- 0
            s <- 0
            PL <- 0
            PL_BCHBTC <- 0
            PL_BCHUSD <- 0
            PL_BTCUSD <- 0
            BuyBCHBTC <- 0
            next;
          }
          
          #SCALING 1
          if((m[i,16]/m[i,11]-1)*100 < s - scaling && trade_positive_difference == 1 && balance_avail > 100){
            
            if(balance_avail < 200){
              deal <- balance_avail - 100
            }
            else{
              deal <- balance_avail * step_continue
            }
            
            temp_commission <- commission
            temp_usd <- balance_usd
            temp_used <- balance_used
            temp_BCHBTC <- balance_BCHBTC
            balance_BCHBTC <- temp_BCHBTC + deal / m[i,16]
            temp_BCHUSD <- balance_BCHUSD
            balance_BCHUSD <- temp_BCHUSD - deal / m[i,11]
            temp_BTCUSD <- balance_BTCUSD
            balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
            temp_avail <- balance_avail
            
            if(BTCUSD_cond == 0){
              commission <- temp_commission + deal * 0.002 * 2
              balance_usd <- temp_usd - deal * 0.002 * 2
              balance_used <- temp_used + deal * 2
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_BCHUSD <- (balance_used / 2) / abs(balance_BCHUSD)
            }
            else{
              commission <- temp_commission + deal * 0.002 * 3
              balance_usd <- temp_usd - deal * 0.002 * 3
              balance_used <- temp_used + deal * 3
              comm <- c(comm, deal * 0.002 * 3)
              avg_price_BCHUSD <- (balance_used / 3) / abs(balance_BCHUSD)
              temp_price_BTCUSD <- avg_price_BTCUSD
              avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                        balance_BTCUSD)
            }
            balance_avail <- balance_usd * leverage - balance_used
            temp_price_BCHBTC <- avg_price_BCHBTC
            avg_price_BCHBTC <- abs((temp_price_BCHBTC*temp_BCHBTC + deal/m[i,16]*m[i,8]) /
                                      balance_BCHBTC)
            
            s <- (m[i,16]/m[i,11]-1)*100
            
            # cat("SCALING. BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            #     "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
            # cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
            #     "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          }
        }
        else{
          if(BuyBCHUSD == 1){
            PL_BCHBTC <- (m[i,8] - avg_price_BCHBTC)*balance_BCHBTC*m[i,4]
            PL_BCHUSD <- (m[i,11] - avg_price_BCHUSD)*balance_BCHUSD
            if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
            else PL_BTCUSD <- {(m[i,4] - avg_price_BTCUSD)*balance_BTCUSD}
            
            if(((PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
                || (PL_BCHBTC+PL_BCHUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
                || m[i,1]-start > stop_loss_dur
                || i == nrow(m)-4) && trade_negative_difference == 1){
              
              #CLOSE 2:
              ##SELL BCH/USD && BUY BCH/BTC/USD
              
              temp_commission <- commission
              if(BTCUSD_cond == 0){
                commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
                  abs(balance_BCHUSD * m[i,12] * 0.002)
                temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
                  abs(balance_BCHUSD * m[i,12] * 0.002)
                PL_BTCUSD <- 0
              }
              else{
                commission <- temp_commission + abs(balance_BCHBTC * m[i,15] * 0.002) +
                  abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                temp_commission <- abs(balance_BCHBTC * m[i,15] * 0.002) + 
                  abs(balance_BCHUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
              }
              PL_BCHBTC <- (m[i,8] - avg_price_BCHBTC)*balance_BCHBTC*m[i,4]
              PL_BCHUSD <- (m[i,11] - avg_price_BCHUSD)*balance_BCHUSD
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_BCHBTC <- 0
                PL_BCHUSD <- 0
                PL_BTCUSD <- 0
              }
              PL <- PL_BCHUSD + PL_BCHBTC + PL_BTCUSD
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_BCHBTC * m[i,16] * 0.002) +
                                                abs(balance_BCHUSD * m[i,11] * 0.002))
              balance_BCHBTC <- 0
              balance_BCHUSD <- 0
              balance_BTCUSD <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_BCHBTC * m[i,16] * 0.002) +
                          abs(balance_BCHUSD * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("Avg price BCH/USD:",format(round(avg_price_BCHUSD,2), nsmall = 2),
              #     "Avg price BCH/BTC:",format(round(avg_price_BCHBTC,6), nsmall = 6),
              #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
              # cat("BCH/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "BCH/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              #     "BCH/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
              # cat("PL BCH/USD=",format(round(PL_BCHUSD,0), nsmall = 0),
              #     "PL BCH/BTC=",format(round(PL_BCHBTC,0), nsmall = 0),
              #     "PL BTC/USD=",format(round(PL_BTCUSD,0), nsmall = 0),
              #     "Commission:", format(round(commission,0), nsmall = 0),
              #     "PL=", format(round(PL,0), nsmall = 0),
              #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
              #     "Balance USD:", format(round(balance_usd,2), nsmall = 2),"\n","\n")
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp_usd <- 0
              temp_BCHBTC <- 0
              temp_BCHUSD <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_BCHBTC <- 0
              PL_BCHUSD <- 0
              BuyBCHUSD <- 0
              next;
            }
            
            #SCALING 2
            if((m[i,12]/m[i,15]-1)*100 < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
              
              if(balance_avail < 200){
                deal <- balance_avail - 100
              }
              else{
                deal <- balance_avail * step_continue
              }
              
              temp_commission <- commission
              temp_usd <- balance_usd
              temp_used <- balance_used
              temp_BCHBTC <- balance_BCHBTC
              balance_BCHBTC <- temp_BCHBTC - deal / m[i,15]
              temp_BCHUSD <- balance_BCHUSD
              balance_BCHUSD <- temp_BCHUSD + deal / m[i,12]
              temp_BTCUSD <- balance_BTCUSD
              balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
              temp_avail <- balance_avail
              
              
              
              if(BTCUSD_cond == 0){
                commission <- temp_commission + deal * 0.002 * 2
                balance_usd <- temp_usd - deal * 0.002 * 2
                balance_used <- temp_used + deal * 2
                comm <- c(comm, deal * 0.002 * 2)
                avg_price_BCHUSD <- (balance_used / 2) / abs(balance_BCHUSD)
              }
              else{
                commission <- temp_commission + deal * 0.002 * 3
                balance_usd <- temp_usd - deal * 0.002 * 3
                balance_used <- temp_used + deal * 3
                comm <- c(comm, deal * 0.002 * 3)
                avg_price_BCHUSD <- (balance_used / 3) / abs(balance_BCHUSD)
                temp_price_BTCUSD <- avg_price_BTCUSD
                avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                          balance_BTCUSD)
              }
              balance_avail <- balance_usd * leverage - balance_used
              temp_price_BCHBTC <- avg_price_BCHBTC
              avg_price_BCHBTC <- abs((temp_price_BCHBTC*temp_BCHBTC - deal/m[i,15]*m[i,7]) /
                                        balance_BCHBTC)
              
              s <- (m[i,12]/m[i,15]-1)*100
              
              # cat("SCALING. BCH/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "BCH/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              #     "BCH/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
              # cat("Balance BCH/BTC:", format(round(balance_BCHBTC,2), nsmall = 2),
              #     "Balance BCH/USD:", format(round(balance_BCHUSD,2), nsmall = 2),"\n")
              next;
            }
          }
        }
      }
    }
    
    t2 <- Sys.time()
    t2-t1
    cat("open=",cond_open,"close=",cond_close,"stop=",stop_loss,
        "Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),
        "% Total deals:",length(profit_sum),"time difference of",t2-t1,"secs\n")
    df <- data.frame(cond_open,cond_close,stop_loss,format(round((balance_usd/4000-1)*100,2), nsmall = 2),length(profit_sum))
    x <- c("cond_open", "cond_close","stop_loss","profit", "deals")
    colnames(df) <- x
    res <- rbind(res,df)
    }
  }
}
setwd("C:/btc/Strategy/R_Strategy_Arb/Arbitration/Bitfinex_BCH_BTC_USD")
write.csv(res, "BCH_cond.csv", row.names = FALSE)



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

cond_open <- -0.7
cond_close <- 0.4
stop_loss <- -30.0
stop_loss_dur <- 86400 * 6
trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_OMGBTC <- 0
balance_OMGUSD <- 0
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
BuyOMGBTC <- 0
BuyOMGUSD <- 0
step_entry <- 0.25
step_continue <- 0.55
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
      #SELL OMG/USD && BUY OMG/BTC/USD
      deal <- balance_avail * step_entry
      start_balance <- balance_usd
      if(BTCUSD_cond == 0){
        commission <- deal * 0.002 * 2
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_OMGBTC <- deal / m[i,16]
        balance_OMGUSD <- -deal / m[i,11]
        balance_BTCUSD <- 0
      }
      else{
        commission <- deal * 0.002 * 3
        balance_usd <- start_balance - commission
        balance_used <- deal * 3
        balance_avail <- balance_usd * leverage - balance_used
        balance_OMGBTC <- deal / m[i,16]
        balance_OMGUSD <- -deal / m[i,11]
        balance_BTCUSD <- deal / m[i,4]
      }
      
      comm <- c(comm, commission)
      avg_price_OMGUSD <- m[i,11]
      avg_price_OMGBTC <- m[i,8]
      avg_price_BTCUSD <- m[i,4]
      BuyOMGBTC <- 1
      
      start <- m[i,1]
      s <- (m[i,16]/m[i,11]-1)*100
      cat("START. SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
          "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
      cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
          "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next;
    }
    else {
      if(((m[i,12]/m[i,15])-1)*100 < cond_open){
        trade <- 1
        trade_negative_difference <- 1
        
        #OPEN 2
        
        #BUY USD/OMG && SELL USD/BTC/OMG
        deal <- balance_avail * step_entry
        start_balance <- balance_usd
        if(BTCUSD_cond == 0){
          commission <- deal * 0.002 * 2
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_OMGBTC <- -deal / m[i,15]
          balance_OMGUSD <- deal / m[i,12]
        }
        else{
          commission <- deal * 0.002 * 3
          balance_usd <- start_balance - commission
          balance_used <- deal * 3
          balance_avail <- balance_usd * leverage - balance_used
          balance_OMGBTC <- -deal / m[i,15]
          balance_OMGUSD <- deal / m[i,12]
          balance_BTCUSD <- -deal / m[i,3]
        }
        comm <- c(comm, commission)
        avg_price_OMGUSD <- m[i,12]
        avg_price_OMGBTC <- m[i,7]
        avg_price_BTCUSD <- m[i,3]
        
        BuyOMGUSD <- 1
        
        start <- m[i,1]
        s <- (m[i,12]/m[i,15]-1)*100
        cat("START. BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
            "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next;
      }
    }
  }
  else{
    if(BuyOMGBTC == 1){
      PL_OMGBTC <- (m[i,7] - avg_price_OMGBTC)*balance_OMGBTC*m[i,3]
      PL_OMGUSD <- (m[i,12] - avg_price_OMGUSD)*balance_OMGUSD
      if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
      else PL_BTCUSD <- {(m[i,3] - avg_price_BTCUSD)*balance_BTCUSD}
      
      if(((PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
          || (PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
          || m[i,1]-start > stop_loss_dur
          || i==nrow(m)-4) && trade_positive_difference == 1){
        #CLOSE 1:
        ##BUY OMG/USD && SELL OMG/BTC/USD
        temp_commission <- commission
        if(BTCUSD_cond == 0){
          commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
            abs(balance_OMGUSD * m[i,12] * 0.002)
          temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
            abs(balance_OMGUSD * m[i,12] * 0.002)
          PL_BTCUSD <- 0
        }
        else{
          commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
            abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
            abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
        }
        PL_OMGBTC <- (m[i,7] - avg_price_OMGBTC)*balance_OMGBTC*m[i,3]
        PL_OMGUSD <- (m[i,12] - avg_price_OMGUSD)*balance_OMGUSD
        #if last row and then we close position and set PL=0
        if(i == nrow(m)-4){
          PL_OMGBTC <- 0
          PL_OMGUSD <- 0
          PL_BTCUSD <- 0
        }
        PL <- PL_OMGUSD + PL_OMGBTC + PL_BTCUSD
        temp_usd <- balance_usd
        balance_usd <- temp_usd + PL - temp_commission
        balance_OMGBTC <- 0
        balance_OMGUSD <- 0
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
        cat("Avg price OMG/USD:",format(round(avg_price_OMGUSD,2), nsmall = 2),
            "Avg price OMG/BTC:",format(round(avg_price_OMGBTC,6), nsmall = 6),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("PL OMG/USD=",format(round(PL_OMGUSD,0), nsmall = 0),
            "PL OMG/BTC=",format(round(PL_OMGBTC,0), nsmall = 0),
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
        temp_OMGBTC <- 0
        temp_OMGUSD <- 0
        temp_avail <- 0
        temp_used <- 0
        commission <- 0
        temp_commission <- 0
        s <- 0
        PL <- 0
        PL_OMGBTC <- 0
        PL_OMGUSD <- 0
        PL_BTCUSD <- 0
        BuyOMGBTC <- 0
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
        temp_OMGBTC <- balance_OMGBTC
        balance_OMGBTC <- temp_OMGBTC + deal / m[i,16]
        temp_OMGUSD <- balance_OMGUSD
        balance_OMGUSD <- temp_OMGUSD - deal / m[i,11]
        temp_BTCUSD <- balance_BTCUSD
        balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
        temp_avail <- balance_avail
        
        if(BTCUSD_cond == 0){
          commission <- temp_commission + deal * 0.002 * 2
          balance_usd <- temp_usd - deal * 0.002 * 2
          balance_used <- temp_used + deal * 2
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_OMGUSD <- (balance_used / 2) / abs(balance_OMGUSD)
        }
        else{
          commission <- temp_commission + deal * 0.002 * 3
          balance_usd <- temp_usd - deal * 0.002 * 3
          balance_used <- temp_used + deal * 3
          comm <- c(comm, deal * 0.002 * 3)
          avg_price_OMGUSD <- (balance_used / 3) / abs(balance_OMGUSD)
          temp_price_BTCUSD <- avg_price_BTCUSD
          avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                    balance_BTCUSD)
        }
        balance_avail <- balance_usd * leverage - balance_used
        temp_price_OMGBTC <- avg_price_OMGBTC
        avg_price_OMGBTC <- abs((temp_price_OMGBTC*temp_OMGBTC + deal/m[i,16]*m[i,8]) /
                                  balance_OMGBTC)
        
        s <- (m[i,16]/m[i,11]-1)*100
        
        cat("SCALING. OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
        cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
            "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      }
    }
    else{
      if(BuyOMGUSD == 1){
        PL_OMGBTC <- (m[i,8] - avg_price_OMGBTC)*balance_OMGBTC*m[i,4]
        PL_OMGUSD <- (m[i,11] - avg_price_OMGUSD)*balance_OMGUSD
        if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
        else PL_BTCUSD <- {(m[i,4] - avg_price_BTCUSD)*balance_BTCUSD}
        
        if(((PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
            || (PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
            || m[i,1]-start > stop_loss_dur
            || i == nrow(m)-4) && trade_negative_difference == 1){
          
          #CLOSE 2:
          ##SELL OMG/USD && BUY OMG/BTC/USD
          
          temp_commission <- commission
          if(BTCUSD_cond == 0){
            commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
              abs(balance_OMGUSD * m[i,12] * 0.002)
            temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
              abs(balance_OMGUSD * m[i,12] * 0.002)
            PL_BTCUSD <- 0
          }
          else{
            commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
              abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
              abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
          }
          PL_OMGBTC <- (m[i,8] - avg_price_OMGBTC)*balance_OMGBTC*m[i,4]
          PL_OMGUSD <- (m[i,11] - avg_price_OMGUSD)*balance_OMGUSD
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_OMGBTC <- 0
            PL_OMGUSD <- 0
            PL_BTCUSD <- 0
          }
          PL <- PL_OMGUSD + PL_OMGBTC + PL_BTCUSD
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_OMGBTC * m[i,16] * 0.002) +
                                            abs(balance_OMGUSD * m[i,11] * 0.002))
          balance_OMGBTC <- 0
          balance_OMGUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_OMGBTC * m[i,16] * 0.002) +
                      abs(balance_OMGUSD * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price OMG/USD:",format(round(avg_price_OMGUSD,2), nsmall = 2),
              "Avg price OMG/BTC:",format(round(avg_price_OMGBTC,6), nsmall = 6),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
          cat("PL OMG/USD=",format(round(PL_OMGUSD,0), nsmall = 0),
              "PL OMG/BTC=",format(round(PL_OMGBTC,0), nsmall = 0),
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
          temp_OMGBTC <- 0
          temp_OMGUSD <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_OMGBTC <- 0
          PL_OMGUSD <- 0
          BuyOMGUSD <- 0
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
          temp_OMGBTC <- balance_OMGBTC
          balance_OMGBTC <- temp_OMGBTC - deal / m[i,15]
          temp_OMGUSD <- balance_OMGUSD
          balance_OMGUSD <- temp_OMGUSD + deal / m[i,12]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
          temp_avail <- balance_avail
          
          
          
          if(BTCUSD_cond == 0){
            commission <- temp_commission + deal * 0.002 * 2
            balance_usd <- temp_usd - deal * 0.002 * 2
            balance_used <- temp_used + deal * 2
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_OMGUSD <- (balance_used / 2) / abs(balance_OMGUSD)
          }
          else{
            commission <- temp_commission + deal * 0.002 * 3
            balance_usd <- temp_usd - deal * 0.002 * 3
            balance_used <- temp_used + deal * 3
            comm <- c(comm, deal * 0.002 * 3)
            avg_price_OMGUSD <- (balance_used / 3) / abs(balance_OMGUSD)
            temp_price_BTCUSD <- avg_price_BTCUSD
            avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                      balance_BTCUSD)
          }
          balance_avail <- balance_usd * leverage - balance_used
          temp_price_OMGBTC <- avg_price_OMGBTC
          avg_price_OMGBTC <- abs((temp_price_OMGBTC*temp_OMGBTC - deal/m[i,15]*m[i,7]) /
                                    balance_OMGBTC)

          s <- (m[i,12]/m[i,15]-1)*100
          
          cat("SCALING. OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
          cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
              "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),"\n")
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
    balance_OMGBTC <- 0
    balance_OMGUSD <- 0
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
    BuyOMGBTC <- 0
    BuyOMGUSD <- 0
    step_entry <- 0.25
    step_continue <- 0.55
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
          #SELL OMG/USD && BUY OMG/BTC/USD
          deal <- balance_avail * step_entry
          start_balance <- balance_usd
          if(BTCUSD_cond == 0){
            commission <- deal * 0.002 * 2
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_OMGBTC <- deal / m[i,16]
            balance_OMGUSD <- -deal / m[i,11]
            balance_BTCUSD <- 0
          }
          else{
            commission <- deal * 0.002 * 3
            balance_usd <- start_balance - commission
            balance_used <- deal * 3
            balance_avail <- balance_usd * leverage - balance_used
            balance_OMGBTC <- deal / m[i,16]
            balance_OMGUSD <- -deal / m[i,11]
            balance_BTCUSD <- deal / m[i,4]
          }
          
          comm <- c(comm, commission)
          avg_price_OMGUSD <- m[i,11]
          avg_price_OMGBTC <- m[i,8]
          avg_price_BTCUSD <- m[i,4]
          BuyOMGBTC <- 1
          
          start <- m[i,1]
          s <- (m[i,16]/m[i,11]-1)*100
          # cat("START. SELL OMG/USD && BUY OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
          # cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
          #     "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
          #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          next;
        }
        else {
          if(((m[i,12]/m[i,15])-1)*100 < cond_open){
            trade <- 1
            trade_negative_difference <- 1
            
            #OPEN 2
            
            #BUY USD/OMG && SELL USD/BTC/OMG
            deal <- balance_avail * step_entry
            start_balance <- balance_usd
            if(BTCUSD_cond == 0){
              commission <- deal * 0.002 * 2
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_OMGBTC <- -deal / m[i,15]
              balance_OMGUSD <- deal / m[i,12]
            }
            else{
              commission <- deal * 0.002 * 3
              balance_usd <- start_balance - commission
              balance_used <- deal * 3
              balance_avail <- balance_usd * leverage - balance_used
              balance_OMGBTC <- -deal / m[i,15]
              balance_OMGUSD <- deal / m[i,12]
              balance_BTCUSD <- -deal / m[i,3]
            }
            comm <- c(comm, commission)
            avg_price_OMGUSD <- m[i,12]
            avg_price_OMGBTC <- m[i,7]
            avg_price_BTCUSD <- m[i,3]
            
            BuyOMGUSD <- 1
            
            start <- m[i,1]
            s <- (m[i,12]/m[i,15]-1)*100
            # cat("START. BUY OMG/USD && SELL OMG/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
            #     "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next;
          }
        }
      }
      else{
        if(BuyOMGBTC == 1){
          PL_OMGBTC <- (m[i,7] - avg_price_OMGBTC)*balance_OMGBTC*m[i,3]
          PL_OMGUSD <- (m[i,12] - avg_price_OMGUSD)*balance_OMGUSD
          if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
          else PL_BTCUSD <- {(m[i,3] - avg_price_BTCUSD)*balance_BTCUSD}
          
          if(((PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
              || (PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
              || m[i,1]-start > stop_loss_dur
              || i==nrow(m)-4) && trade_positive_difference == 1){
            #CLOSE 1:
            ##BUY OMG/USD && SELL OMG/BTC/USD
            temp_commission <- commission
            if(BTCUSD_cond == 0){
              commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
                abs(balance_OMGUSD * m[i,12] * 0.002)
              temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
                abs(balance_OMGUSD * m[i,12] * 0.002)
              PL_BTCUSD <- 0
            }
            else{
              commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
                abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
                abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
            }
            PL_OMGBTC <- (m[i,7] - avg_price_OMGBTC)*balance_OMGBTC*m[i,3]
            PL_OMGUSD <- (m[i,12] - avg_price_OMGUSD)*balance_OMGUSD
            #if last row and then we close position and set PL=0
            if(i == nrow(m)-4){
              PL_OMGBTC <- 0
              PL_OMGUSD <- 0
              PL_BTCUSD <- 0
            }
            PL <- PL_OMGUSD + PL_OMGBTC + PL_BTCUSD
            temp_usd <- balance_usd
            balance_usd <- temp_usd + PL - temp_commission
            balance_OMGBTC <- 0
            balance_OMGUSD <- 0
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
            # cat("Avg price OMG/USD:",format(round(avg_price_OMGUSD,2), nsmall = 2),
            #     "Avg price OMG/BTC:",format(round(avg_price_OMGBTC,6), nsmall = 6),
            #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
            # cat("OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("PL OMG/USD=",format(round(PL_OMGUSD,0), nsmall = 0),
            #     "PL OMG/BTC=",format(round(PL_OMGBTC,0), nsmall = 0),
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
            temp_OMGBTC <- 0
            temp_OMGUSD <- 0
            temp_avail <- 0
            temp_used <- 0
            commission <- 0
            temp_commission <- 0
            s <- 0
            PL <- 0
            PL_OMGBTC <- 0
            PL_OMGUSD <- 0
            PL_BTCUSD <- 0
            BuyOMGBTC <- 0
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
            temp_OMGBTC <- balance_OMGBTC
            balance_OMGBTC <- temp_OMGBTC + deal / m[i,16]
            temp_OMGUSD <- balance_OMGUSD
            balance_OMGUSD <- temp_OMGUSD - deal / m[i,11]
            temp_BTCUSD <- balance_BTCUSD
            balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
            temp_avail <- balance_avail
            
            if(BTCUSD_cond == 0){
              commission <- temp_commission + deal * 0.002 * 2
              balance_usd <- temp_usd - deal * 0.002 * 2
              balance_used <- temp_used + deal * 2
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_OMGUSD <- (balance_used / 2) / abs(balance_OMGUSD)
            }
            else{
              commission <- temp_commission + deal * 0.002 * 3
              balance_usd <- temp_usd - deal * 0.002 * 3
              balance_used <- temp_used + deal * 3
              comm <- c(comm, deal * 0.002 * 3)
              avg_price_OMGUSD <- (balance_used / 3) / abs(balance_OMGUSD)
              temp_price_BTCUSD <- avg_price_BTCUSD
              avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                        balance_BTCUSD)
            }
            balance_avail <- balance_usd * leverage - balance_used
            temp_price_OMGBTC <- avg_price_OMGBTC
            avg_price_OMGBTC <- abs((temp_price_OMGBTC*temp_OMGBTC + deal/m[i,16]*m[i,8]) /
                                      balance_OMGBTC)
            
            s <- (m[i,16]/m[i,11]-1)*100
            
            # cat("SCALING. OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
            # cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
            #     "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          }
        }
        else{
          if(BuyOMGUSD == 1){
            PL_OMGBTC <- (m[i,8] - avg_price_OMGBTC)*balance_OMGBTC*m[i,4]
            PL_OMGUSD <- (m[i,11] - avg_price_OMGUSD)*balance_OMGUSD
            if(BTCUSD_cond == 0) {PL_BTCUSD <- 0}
            else PL_BTCUSD <- {(m[i,4] - avg_price_BTCUSD)*balance_BTCUSD}
            
            if(((PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
                || (PL_OMGBTC+PL_OMGUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
                || m[i,1]-start > stop_loss_dur
                || i == nrow(m)-4) && trade_negative_difference == 1){
              
              #CLOSE 2:
              ##SELL OMG/USD && BUY OMG/BTC/USD
              
              temp_commission <- commission
              if(BTCUSD_cond == 0){
                commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
                  abs(balance_OMGUSD * m[i,12] * 0.002)
                temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
                  abs(balance_OMGUSD * m[i,12] * 0.002)
                PL_BTCUSD <- 0
              }
              else{
                commission <- temp_commission + abs(balance_OMGBTC * m[i,15] * 0.002) +
                  abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                temp_commission <- abs(balance_OMGBTC * m[i,15] * 0.002) + 
                  abs(balance_OMGUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
              }
              PL_OMGBTC <- (m[i,8] - avg_price_OMGBTC)*balance_OMGBTC*m[i,4]
              PL_OMGUSD <- (m[i,11] - avg_price_OMGUSD)*balance_OMGUSD
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_OMGBTC <- 0
                PL_OMGUSD <- 0
                PL_BTCUSD <- 0
              }
              PL <- PL_OMGUSD + PL_OMGBTC + PL_BTCUSD
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_OMGBTC * m[i,16] * 0.002) +
                                                abs(balance_OMGUSD * m[i,11] * 0.002))
              balance_OMGBTC <- 0
              balance_OMGUSD <- 0
              balance_BTCUSD <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_OMGBTC * m[i,16] * 0.002) +
                          abs(balance_OMGUSD * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("Avg price OMG/USD:",format(round(avg_price_OMGUSD,2), nsmall = 2),
              #     "Avg price OMG/BTC:",format(round(avg_price_OMGBTC,6), nsmall = 6),
              #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
              # cat("OMG/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "OMG/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              #     "OMG/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
              # cat("PL OMG/USD=",format(round(PL_OMGUSD,0), nsmall = 0),
              #     "PL OMG/BTC=",format(round(PL_OMGBTC,0), nsmall = 0),
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
              temp_OMGBTC <- 0
              temp_OMGUSD <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_OMGBTC <- 0
              PL_OMGUSD <- 0
              BuyOMGUSD <- 0
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
              temp_OMGBTC <- balance_OMGBTC
              balance_OMGBTC <- temp_OMGBTC - deal / m[i,15]
              temp_OMGUSD <- balance_OMGUSD
              balance_OMGUSD <- temp_OMGUSD + deal / m[i,12]
              temp_BTCUSD <- balance_BTCUSD
              balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
              temp_avail <- balance_avail
              
              
              
              if(BTCUSD_cond == 0){
                commission <- temp_commission + deal * 0.002 * 2
                balance_usd <- temp_usd - deal * 0.002 * 2
                balance_used <- temp_used + deal * 2
                comm <- c(comm, deal * 0.002 * 2)
                avg_price_OMGUSD <- (balance_used / 2) / abs(balance_OMGUSD)
              }
              else{
                commission <- temp_commission + deal * 0.002 * 3
                balance_usd <- temp_usd - deal * 0.002 * 3
                balance_used <- temp_used + deal * 3
                comm <- c(comm, deal * 0.002 * 3)
                avg_price_OMGUSD <- (balance_used / 3) / abs(balance_OMGUSD)
                temp_price_BTCUSD <- avg_price_BTCUSD
                avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                          balance_BTCUSD)
              }
              balance_avail <- balance_usd * leverage - balance_used
              temp_price_OMGBTC <- avg_price_OMGBTC
              avg_price_OMGBTC <- abs((temp_price_OMGBTC*temp_OMGBTC - deal/m[i,15]*m[i,7]) /
                                        balance_OMGBTC)
              
              s <- (m[i,12]/m[i,15]-1)*100
              
              # cat("SCALING. OMG/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "OMG/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              #     "OMG/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
              # cat("Balance OMG/BTC:", format(round(balance_OMGBTC,2), nsmall = 2),
              #     "Balance OMG/USD:", format(round(balance_OMGUSD,2), nsmall = 2),"\n")
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
setwd("C:/btc/Strategy/Arbitration/Bitfinex_OMG_BTC_USD")
write.csv(res, "OMG_cond.csv", row.names = FALSE)



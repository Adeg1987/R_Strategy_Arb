library(dplyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)
library(purrr)
library(imputeTS)

setwd("C:/btc/Orderbook/Influx/EOS")
influx <- readRDS("Influx_orderbook_EOSBTCUSD_20180506_20180528.rds")
colnames(influx$EOSUSD) <- c("time", "EOSUSD_bids_amount", "EOSUSD_bids_price",
                             "EOSUSD_asks_amount", "EOSUSD_asks_price")
colnames(influx$EOSBTC) <- c("time", "EOSBTC_bids_amount", "EOSBTC_bids_price",
                             "EOSBTC_asks_amount", "EOSBTC_asks_price")
colnames(influx$BTCUSD) <- c("time", "BTCUSD_bids_amount", "BTCUSD_bids_price",
                             "BTCUSD_asks_amount", "BTCUSD_asks_price")

ob <- data.frame(time = union(union(influx$EOSUSD$time, influx$EOSBTC$time), influx$BTCUSD$time))
ob <- ob %>%
  left_join(influx$BTCUSD, by = "time") %>%
  left_join(influx$EOSUSD, by = "time") %>%
  left_join(influx$EOSBTC, by = "time") %>%
  arrange(time) %>%
  na.locf() %>%
  na.omit()

time_vec <- ob$time / 1000
EOSUSD_bid <- ob$EOSUSD_bids_price
EOSUSD_ask <- ob$EOSUSD_asks_price
EOSUSD_check <- EOSUSD_bid > EOSUSD_ask
EOSBTC_bid <- ob$EOSBTC_bids_price
EOSBTC_ask <- ob$EOSBTC_asks_price
EOSBTC_check <- EOSBTC_bid > EOSBTC_ask
BTCUSD_bid <- ob$BTCUSD_bids_price
BTCUSD_ask <- ob$BTCUSD_asks_price
BTCUSD_check <- BTCUSD_bid > BTCUSD_ask
EOSBTCUSD_bid <- EOSBTC_bid * BTCUSD_bid
EOSBTCUSD_ask <- EOSBTC_ask * BTCUSD_ask
EOSBTCUSD_check <- EOSBTCUSD_bid > EOSBTCUSD_ask
BuyBTC <- (EOSBTCUSD_ask / EOSUSD_bid - 1) * 100
SellBTC <- (EOSUSD_ask / EOSBTCUSD_bid - 1) * 100
n <- nrow(ob)

cond_open <- -0.55
cond_close <- 0.45
stop_loss <- -4.0
stop_loss_dur <- 86400 * 7
step_entry <- 0.25
step_continue <- 0.35
scaling <- 1.4
max_deal <- 1000

BuyBTC_cond <- BuyBTC < cond_open
SellBTC_cond <- SellBTC < cond_open
trail <- 0.0
trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_EOSBTC <- 0
balance_EOSUSD <- 0
balance_BTCUSD <- 0
leverage <- 2
PL <- 0
commission <- 0
balance_avail <- balance_usd * leverage
balance_used <- 0
deal <- 0
profit_sum <- c()
date <- c()
profit_perc <- c()
duration <- c()
BuyEOSBTC <- 0
BuyEOSUSD <- 0
comm <- c()
BTCUSD_deal_cond <- 0
if (BTCUSD_deal_cond == 0) {
  npairs <- 2
} else {
  npairs <- 3
}

t1 <- Sys.time()
t1

for (i in seq(1, n, by = 3)) {
  
  if(balance_usd <= 0) { break }
  
  if(EOSUSD_check[i] + EOSBTC_check[i] + BTCUSD_check[i] + EOSBTCUSD_check[i] > 0) {
    next
  }
  
  if(i == n) {
    PL_EOSBTC <- 0
    PL_EOSUSD <- 0
    PL_BTCUSD <- 0
    break
  }
  
  # START TRADING
  if(trade == 0) {
    if(BuyBTC_cond[i]) {
      trade <- 1
      trade_positive_difference <- 1
      
      # OPEN 1
      # SELL EOS/USD && BUY EOS/BTC/USD
      deal <- min(balance_avail * step_entry, max_deal)
      start_balance <- balance_usd
      commission <- deal * 0.002 * npairs
      balance_usd <- start_balance - commission
      balance_used <- deal * npairs
      balance_avail <- balance_usd * leverage - balance_used
      balance_EOSBTC <- deal / EOSBTCUSD_ask[i]
      balance_EOSUSD <- -deal / EOSUSD_bid[i]
      balance_BTCUSD <- deal / BTCUSD_ask[i] * BTCUSD_deal_cond

      comm <- c(comm, commission)
      avg_price_EOSUSD <- EOSUSD_bid[i]
      avg_price_EOSBTC <- EOSBTC_ask[i]
      avg_price_BTCUSD <- BTCUSD_ask[i]
      BuyEOSBTC <- 1
      
      start <- time_vec[i]
      s <- BuyBTC[i]
      cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
          "SELL EOS/USD && BUY EOS/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
          "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
          "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
          "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2), "\n")
      cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
          "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next
    } else if(SellBTC_cond[i]) {
        trade <- 1
        trade_negative_difference <- 1
        
        # OPEN 2
        # BUY EOS/USD && SELL EOS/BTC/USD
        deal <- min(balance_avail * step_entry, max_deal)
        start_balance <- balance_usd
        commission <- deal * 0.002 * npairs
        balance_usd <- start_balance - commission
        balance_used <- deal * npairs
        balance_avail <- balance_usd * leverage - balance_used
        balance_EOSBTC <- -deal / EOSBTCUSD_bid[i]
        balance_EOSUSD <- deal / EOSUSD_ask[i]
        balance_BTCUSD <- -deal / BTCUSD_bid[i] * BTCUSD_deal_cond

        comm <- c(comm, commission)
        avg_price_EOSUSD <- EOSUSD_ask[i]
        avg_price_EOSBTC <- EOSBTC_bid[i]
        avg_price_BTCUSD <- BTCUSD_bid[i]
        BuyEOSUSD <- 1
        
        start <- time_vec[i]
        s <- SellBTC[i]
        cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            "BUY EOS/USD && SELL EOS/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
            "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
            "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
        cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
            "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next
      }
  } else {
    if(BuyEOSBTC == 1) {
      PL_EOSBTC <- (EOSBTC_bid[i] - avg_price_EOSBTC) * balance_EOSBTC * BTCUSD_bid[i]
      PL_EOSUSD <- (EOSUSD_ask[i] - avg_price_EOSUSD) * balance_EOSUSD
      PL_BTCUSD <- (BTCUSD_bid[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond

      PL <- PL_EOSUSD + PL_EOSBTC + PL_BTCUSD
      
      if(((PL - commission * 2) / balance_used * 100 > cond_close
          | (PL - commission * 2) / balance_used * 100 < stop_loss
          | time_vec[i] - start > stop_loss_dur)
         & trade_positive_difference == 1) {
        
        # CLOSE 1:
        ## BUY EOS/USD && SELL EOS/BTC/USD
        temp_commission <- abs(balance_EOSBTC * EOSBTCUSD_bid[i] * 0.002) +
          abs(balance_EOSUSD * EOSUSD_ask[i] * 0.002) +
          abs(balance_BTCUSD * BTCUSD_bid[i] * 0.002) * BTCUSD_deal_cond

        commission <- commission + temp_commission

        balance_usd <- balance_usd + PL - temp_commission
        balance_EOSBTC <- 0
        balance_EOSUSD <- 0
        balance_BTCUSD <- 0
        balance_used <- 0
        balance_avail <- balance_usd * leverage
        
        profit_sum <- c(profit_sum, PL - commission)
        profit_perc <- c(profit_perc, (PL - commission) / start_balance)
        comm <- c(comm, temp_commission)
        
        end <- time_vec[i]
        duration <- c(duration, (end - start) / 3600)
        date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
        
        cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
            "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
            "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
            "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
        cat("Avg price EOS/USD:",format(round(avg_price_EOSUSD,2), nsmall = 2),
            "Avg price EOS/BTC:",format(round(avg_price_EOSBTC,6), nsmall = 6),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
            "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
            "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
        cat("PL EOS/USD=",format(round(PL_EOSUSD,0), nsmall = 0),
            "PL EOS/BTC=",format(round(PL_EOSBTC,0), nsmall = 0),
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
        temp_EOSBTC <- 0
        temp_EOSUSD <- 0
        temp_avail <- 0
        temp_used <- 0
        commission <- 0
        temp_commission <- 0
        s <- 0
        PL <- 0
        PL_EOSBTC <- 0
        PL_EOSUSD <- 0
        PL_BTCUSD <- 0
        BuyEOSBTC <- 0
        next
      }
      
      #SCALING 1
      if(BuyBTC[i] < s - scaling & trade_positive_difference == 1 & balance_avail > 100) {
        
        if(balance_avail < 200) {
          deal <- balance_avail - 100
        } else {
          deal <- min(balance_avail * step_continue, max_deal)
        }
        
        balance_EOSBTC <- balance_EOSBTC + deal / EOSBTCUSD_ask[i]
        balance_EOSUSD <- balance_EOSUSD - deal / EOSUSD_bid[i]
        balance_BTCUSD <- balance_BTCUSD + deal / BTCUSD_ask[i]
        temp_avail <- balance_avail
        
        commission <- commission + deal * 0.002 * npairs
        balance_usd <- balance_usd - deal * 0.002 * npairs
        balance_used <- balance_used + deal * npairs
        comm <- c(comm, deal * 0.002 * npairs)
        avg_price_EOSUSD <- (balance_used / npairs) / abs(balance_EOSUSD)
        avg_price_BTCUSD <- abs((avg_price_BTCUSD * (balance_BTCUSD - deal / BTCUSD_ask[i])
                                 + deal / BTCUSD_ask[i]) / balance_BTCUSD)
     
        balance_avail <- balance_usd * leverage - balance_used
        avg_price_EOSBTC <- abs((avg_price_EOSBTC * (balance_EOSBTC - deal / EOSBTCUSD_ask[i])
                                 + deal/(EOSBTCUSD_ask[i]) * EOSBTC_ask[i]) / balance_EOSBTC)
        
        s <- BuyBTC[i]
        
        cat("SCALING. EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
            "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
            "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
            "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2),"\n")
        cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
            "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next
      }
    } else {
      if(BuyEOSUSD == 1) {
        PL_EOSBTC <- (EOSBTC_ask[i] - avg_price_EOSBTC) * balance_EOSBTC * BTCUSD_ask[i]
        PL_EOSUSD <- (EOSUSD_bid[i] - avg_price_EOSUSD) * balance_EOSUSD
        PL_BTCUSD <- (BTCUSD_ask[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
        
        PL <- PL_EOSUSD + PL_EOSBTC + PL_BTCUSD
        
        if(((PL - commission * 2) / balance_used * 100 > cond_close
            | (PL - commission * 2) / balance_used * 100 < stop_loss
            | time_vec[i] - start > stop_loss_dur)
           & trade_negative_difference == 1) {
          
          # CLOSE 2:
          ## SELL EOS/USD && BUY EOS/BTC/USD
          temp_commission <- commission
          temp_commission <- abs(balance_EOSBTC * EOSBTCUSD_bid[i] * 0.002) + 
            abs(balance_EOSUSD * EOSUSD_ask[i] * 0.002) +
            abs(balance_BTCUSD * BTCUSD_ask[i] * 0.002) * BTCUSD_deal_cond
 
          commission <- commission + temp_commission

          balance_usd <- balance_usd + PL - (abs(balance_EOSBTC * EOSBTCUSD_ask[i] * 0.002) +
                                            abs(balance_EOSUSD * EOSUSD_bid[i] * 0.002))
          balance_EOSBTC <- 0
          balance_EOSUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission) / start_balance)
          comm <- c(comm, abs(balance_EOSBTC * EOSBTCUSD_ask[i] * 0.002) +
                      abs(balance_EOSUSD * EOSUSD_bid[i] * 0.002))
          
          end <- time_vec[i]
          duration <- c(duration, (end - start) / 3600)
          date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
          
          cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
              "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price EOS/USD:",format(round(avg_price_EOSUSD,2), nsmall = 2),
              "Avg price EOS/BTC:",format(round(avg_price_EOSBTC,6), nsmall = 6),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
              "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
              "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
              "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2),"\n")
          cat("PL EOS/USD=",format(round(PL_EOSUSD,0), nsmall = 0),
              "PL EOS/BTC=",format(round(PL_EOSBTC,0), nsmall = 0),
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
          temp_EOSBTC <- 0
          temp_EOSUSD <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_EOSBTC <- 0
          PL_EOSUSD <- 0
          BuyEOSUSD <- 0
          next
        }
        
        #SCALING 2
        if(SellBTC[i] < s - scaling & trade_negative_difference == 1 & balance_avail > 100) {
          
          if(balance_avail < 200) {
            deal <- balance_avail - 100
          } else {
            deal <- min(balance_avail * step_continue, max_deal)
          }
          
          temp_EOSBTC <- balance_EOSBTC
          balance_EOSBTC <- temp_EOSBTC - deal / EOSBTCUSD_bid[i]
          balance_EOSUSD <- balance_EOSUSD + deal / EOSUSD_ask[i]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / BTCUSD_bid[i]

          commission <- commission + deal * 0.002 * npairs
          balance_usd <- balance_usd - deal * 0.002 * npairs
          balance_used <- balance_used + deal * npairs
          comm <- c(comm, deal * 0.002 * npairs)
          avg_price_EOSUSD <- (balance_used / npairs) / abs(balance_EOSUSD)
          avg_price_BTCUSD <- abs((avg_price_BTCUSD * temp_BTCUSD - deal / BTCUSD_bid[i]) /
                                    balance_BTCUSD) * BTCUSD_deal_cond
          
          balance_avail <- balance_usd * leverage - balance_used
          avg_price_EOSBTC <- abs((avg_price_EOSBTC * temp_EOSBTC
                                   - deal/EOSBTCUSD_bid[i] * EOSBTC_bid[i]) / balance_EOSBTC)
          
          s <- SellBTC[i]
          
          cat("SCALING. EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
              "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
              "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
              "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
          cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
              "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),"\n")
          next
        }
      }
    }
  }
}

t2 <- Sys.time()
t2-t1

if (TRUE){
  cat("Start testing:", as.character(as.POSIXct(time_vec[1], origin = "1970-01-1")),
      "End testing",as.character(as.POSIXct(time_vec[i], origin = "1970-01-1")),
      "Estimating period (Days):",format(round((time_vec[i]-time_vec[1])/86400,0), nsmall = 0),"\n")
  cat("Open condition (sHigh or sLow) <", cond_open, " Close condition (PL) >", cond_close, "\n")
  cat("Start balance USD:",4000," End balance USD:",balance_usd,"\n")
  cat("Total deals:",length(profit_sum)," Positive deals:",sum(profit_sum>0),
      " Negative deals:",sum(profit_sum < 0),"\n")
  cat("Total profit:",sum(profit_sum)," Profit per deal:",mean(profit_sum),"\n")
  cat("Average duration (H):",mean(duration)," (M):",mean(duration) * 60,
      " (S):",mean(duration) * 3600,"\n")
  cat("Average profit percent:",format(round(mean(profit_perc) * 100,2), nsmall = 2),
      "% Total profit percent:", format(round((balance_usd / 4000 - 1) * 100,2), nsmall = 2),"%\n")
  cat("Max profit:",max(profit_sum)," Min profit:",min(profit_sum)," Paid commission:",sum(comm),"\n")
}

res <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("cond_open", "cond_close","stop_loss","profit", "deals")
colnames(res) <- x
for(cond_open in seq(-1.0,-0.1,by=0.05)){
  for(cond_close in seq(0.1,0.6,by=0.05)){
    for(stop_loss in seq(-1,-10,by=-1)){
      
      t1 <- Sys.time()
      
      stop_loss_dur <- 86400 * 7
      step_entry <- 0.2
      step_continue <- 0.35
      scaling <- 1.4
      
      BuyBTC_cond <- BuyBTC < cond_open
      SellBTC_cond <- SellBTC < cond_open
      trail <- 0.0
      trade <- 0
      trade_positive_difference <- 0
      trade_negative_difference <- 0
      start <- 0
      end <- 0
      s <- 0
      start_balance <- 4000
      balance_usd <- 4000
      balance_EOSBTC <- 0
      balance_EOSUSD <- 0
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
      BuyEOSBTC <- 0
      BuyEOSUSD <- 0
      comm <- c()
      BTCUSD_deal_cond <- 0
      if (BTCUSD_deal_cond == 0) {
        npairs <- 2
      } else {
        npairs <- 3
      }
      
      for (i in seq(1, n, by = 3)) {
        
        if(balance_usd <= 0) { break }
        
        if(EOSUSD_check[i] + EOSBTC_check[i] + BTCUSD_check[i] + EOSBTCUSD_check[i] > 0) {
          next
        }
        
        if(i == n) {
          PL_EOSBTC <- 0
          PL_EOSUSD <- 0
          PL_BTCUSD <- 0
          break
        }
        
        # START TRADING
        if(trade == 0) {
          if(BuyBTC_cond[i]) {
            trade <- 1
            trade_positive_difference <- 1
            
            # OPEN 1
            # SELL EOS/USD && BUY EOS/BTC/USD
            deal <- min(balance_avail * step_entry, max_deal)
            start_balance <- balance_usd
            commission <- deal * 0.002 * npairs
            balance_usd <- start_balance - commission
            balance_used <- deal * npairs
            balance_avail <- balance_usd * leverage - balance_used
            balance_EOSBTC <- deal / EOSBTCUSD_ask[i]
            balance_EOSUSD <- -deal / EOSUSD_bid[i]
            balance_BTCUSD <- deal / BTCUSD_ask[i] * BTCUSD_deal_cond
            
            comm <- c(comm, commission)
            avg_price_EOSUSD <- EOSUSD_bid[i]
            avg_price_EOSBTC <- EOSBTC_ask[i]
            avg_price_BTCUSD <- BTCUSD_ask[i]
            BuyEOSBTC <- 1
            
            start <- time_vec[i]
            s <- BuyBTC[i]
            # cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            #     "SELL EOS/USD && BUY EOS/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
            #     "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
            #     "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2), "\n")
            # cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
            #     "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next
          } else if(SellBTC_cond[i]) {
            trade <- 1
            trade_negative_difference <- 1
            
            # OPEN 2
            # BUY EOS/USD && SELL EOS/BTC/USD
            deal <- min(balance_avail * step_entry, max_deal)
            start_balance <- balance_usd
            commission <- deal * 0.002 * npairs
            balance_usd <- start_balance - commission
            balance_used <- deal * npairs
            balance_avail <- balance_usd * leverage - balance_used
            balance_EOSBTC <- -deal / EOSBTCUSD_bid[i]
            balance_EOSUSD <- deal / EOSUSD_ask[i]
            balance_BTCUSD <- -deal / BTCUSD_bid[i] * BTCUSD_deal_cond
            
            comm <- c(comm, commission)
            avg_price_EOSUSD <- EOSUSD_ask[i]
            avg_price_EOSBTC <- EOSBTC_bid[i]
            avg_price_BTCUSD <- BTCUSD_bid[i]
            BuyEOSUSD <- 1
            
            start <- time_vec[i]
            s <- SellBTC[i]
            # cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            #     "BUY EOS/USD && SELL EOS/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
            #     "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            #     "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
            # cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
            #     "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next
          }
        } else {
          if(BuyEOSBTC == 1) {
            PL_EOSBTC <- (EOSBTC_bid[i] - avg_price_EOSBTC) * balance_EOSBTC * BTCUSD_bid[i]
            PL_EOSUSD <- (EOSUSD_ask[i] - avg_price_EOSUSD) * balance_EOSUSD
            PL_BTCUSD <- (BTCUSD_bid[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
            
            PL <- PL_EOSUSD + PL_EOSBTC + PL_BTCUSD
            
            if(((PL - commission * 2) / balance_used * 100 > cond_close
                | (PL - commission * 2) / balance_used * 100 < stop_loss
                | time_vec[i] - start > stop_loss_dur)
               & trade_positive_difference == 1) {
              
              # CLOSE 1:
              ## BUY EOS/USD && SELL EOS/BTC/USD
              temp_commission <- abs(balance_EOSBTC * EOSBTCUSD_bid[i] * 0.002) +
                abs(balance_EOSUSD * EOSUSD_ask[i] * 0.002) +
                abs(balance_BTCUSD * BTCUSD_bid[i] * 0.002) * BTCUSD_deal_cond
              
              commission <- commission + temp_commission
              
              balance_usd <- balance_usd + PL - temp_commission
              balance_EOSBTC <- 0
              balance_EOSUSD <- 0
              balance_BTCUSD <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission) / start_balance)
              comm <- c(comm, temp_commission)
              
              end <- time_vec[i]
              duration <- c(duration, (end - start) / 3600)
              date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
              
              # cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("Avg price EOS/USD:",format(round(avg_price_EOSUSD,2), nsmall = 2),
              #     "Avg price EOS/BTC:",format(round(avg_price_EOSBTC,6), nsmall = 6),
              #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
              # cat("EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
              #     "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
              #     "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
              # cat("PL EOS/USD=",format(round(PL_EOSUSD,0), nsmall = 0),
              #     "PL EOS/BTC=",format(round(PL_EOSBTC,0), nsmall = 0),
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
              temp_EOSBTC <- 0
              temp_EOSUSD <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_EOSBTC <- 0
              PL_EOSUSD <- 0
              PL_BTCUSD <- 0
              BuyEOSBTC <- 0
              next
            }
            
            #SCALING 1
            if(BuyBTC[i] < s - scaling & trade_positive_difference == 1 & balance_avail > 100) {
              
              if(balance_avail < 200) {
                deal <- balance_avail - 100
              } else {
                deal <- min(balance_avail * step_continue, max_deal)
              }
              
              balance_EOSBTC <- balance_EOSBTC + deal / EOSBTCUSD_ask[i]
              balance_EOSUSD <- balance_EOSUSD - deal / EOSUSD_bid[i]
              balance_BTCUSD <- balance_BTCUSD + deal / BTCUSD_ask[i]
              temp_avail <- balance_avail
              
              commission <- commission + deal * 0.002 * npairs
              balance_usd <- balance_usd - deal * 0.002 * npairs
              balance_used <- balance_used + deal * npairs
              comm <- c(comm, deal * 0.002 * npairs)
              avg_price_EOSUSD <- (balance_used / npairs) / abs(balance_EOSUSD)
              avg_price_BTCUSD <- abs((avg_price_BTCUSD * (balance_BTCUSD - deal / BTCUSD_ask[i])
                                       + deal / BTCUSD_ask[i]) / balance_BTCUSD)
              
              balance_avail <- balance_usd * leverage - balance_used
              avg_price_EOSBTC <- abs((avg_price_EOSBTC * (balance_EOSBTC - deal / EOSBTCUSD_ask[i])
                                       + deal/(EOSBTCUSD_ask[i]) * EOSBTC_ask[i]) / balance_EOSBTC)
              
              s <- BuyBTC[i]
              
              # cat("SCALING. EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
              #     "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
              #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
              #     "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2),"\n")
              # cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
              #     "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),
              #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
              next
            }
          } else {
            if(BuyEOSUSD == 1) {
              PL_EOSBTC <- (EOSBTC_ask[i] - avg_price_EOSBTC) * balance_EOSBTC * BTCUSD_ask[i]
              PL_EOSUSD <- (EOSUSD_bid[i] - avg_price_EOSUSD) * balance_EOSUSD
              PL_BTCUSD <- (BTCUSD_ask[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
              
              PL <- PL_EOSUSD + PL_EOSBTC + PL_BTCUSD
              
              if(((PL - commission * 2) / balance_used * 100 > cond_close
                  | (PL - commission * 2) / balance_used * 100 < stop_loss
                  | time_vec[i] - start > stop_loss_dur)
                 & trade_negative_difference == 1) {
                
                # CLOSE 2:
                ## SELL EOS/USD && BUY EOS/BTC/USD
                temp_commission <- commission
                temp_commission <- abs(balance_EOSBTC * EOSBTCUSD_bid[i] * 0.002) + 
                  abs(balance_EOSUSD * EOSUSD_ask[i] * 0.002) +
                  abs(balance_BTCUSD * BTCUSD_ask[i] * 0.002) * BTCUSD_deal_cond
                
                commission <- commission + temp_commission
                
                balance_usd <- balance_usd + PL - (abs(balance_EOSBTC * EOSBTCUSD_ask[i] * 0.002) +
                                                     abs(balance_EOSUSD * EOSUSD_bid[i] * 0.002))
                balance_EOSBTC <- 0
                balance_EOSUSD <- 0
                balance_BTCUSD <- 0
                balance_used <- 0
                balance_avail <- balance_usd * leverage
                
                profit_sum <- c(profit_sum, PL - commission)
                profit_perc <- c(profit_perc, (PL - commission) / start_balance)
                comm <- c(comm, abs(balance_EOSBTC * EOSBTCUSD_ask[i] * 0.002) +
                            abs(balance_EOSUSD * EOSUSD_bid[i] * 0.002))
                
                end <- time_vec[i]
                duration <- c(duration, (end - start) / 3600)
                date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
                
                # cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
                #     "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
                #     "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
                #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
                # cat("Avg price EOS/USD:",format(round(avg_price_EOSUSD,2), nsmall = 2),
                #     "Avg price EOS/BTC:",format(round(avg_price_EOSBTC,6), nsmall = 6),
                #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
                # cat("EOS/USD bid:", format(round(EOSUSD_bid[i],2), nsmall = 2),
                #     "EOS/BTC ask:", format(round(EOSBTC_ask[i],6), nsmall = 6),
                #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
                #     "EOS/BTC/USD ask:", format(round(EOSBTCUSD_ask[i],2), nsmall = 2),"\n")
                # cat("PL EOS/USD=",format(round(PL_EOSUSD,0), nsmall = 0),
                #     "PL EOS/BTC=",format(round(PL_EOSBTC,0), nsmall = 0),
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
                temp_EOSBTC <- 0
                temp_EOSUSD <- 0
                temp_avail <- 0
                temp_used <- 0
                commission <- 0
                temp_commission <- 0
                s <- 0
                PL <- 0
                PL_EOSBTC <- 0
                PL_EOSUSD <- 0
                BuyEOSUSD <- 0
                next
              }
              
              #SCALING 2
              if(SellBTC[i] < s - scaling & trade_negative_difference == 1 & balance_avail > 100) {
                
                if(balance_avail < 200) {
                  deal <- balance_avail - 100
                } else {
                  deal <- min(balance_avail * step_continue, max_deal)
                }
                
                temp_EOSBTC <- balance_EOSBTC
                balance_EOSBTC <- temp_EOSBTC - deal / EOSBTCUSD_bid[i]
                balance_EOSUSD <- balance_EOSUSD + deal / EOSUSD_ask[i]
                temp_BTCUSD <- balance_BTCUSD
                balance_BTCUSD <- temp_BTCUSD - deal / BTCUSD_bid[i]
                
                commission <- commission + deal * 0.002 * npairs
                balance_usd <- balance_usd - deal * 0.002 * npairs
                balance_used <- balance_used + deal * npairs
                comm <- c(comm, deal * 0.002 * npairs)
                avg_price_EOSUSD <- (balance_used / npairs) / abs(balance_EOSUSD)
                avg_price_BTCUSD <- abs((avg_price_BTCUSD * temp_BTCUSD - deal / BTCUSD_bid[i]) /
                                          balance_BTCUSD) * BTCUSD_deal_cond
                
                balance_avail <- balance_usd * leverage - balance_used
                avg_price_EOSBTC <- abs((avg_price_EOSBTC * temp_EOSBTC
                                         - deal/EOSBTCUSD_bid[i] * EOSBTC_bid[i]) / balance_EOSBTC)
                
                s <- SellBTC[i]
                
                # cat("SCALING. EOS/USD ask:", format(round(EOSUSD_ask[i],2), nsmall = 2),
                #     "EOS/BTC bid:", format(round(EOSBTC_bid[i],6), nsmall = 6),
                #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
                #     "EOS/BTC/USD bid:", format(round(EOSBTCUSD_bid[i],2), nsmall = 2),"\n")
                # cat("Balance EOS/BTC:", format(round(balance_EOSBTC,2), nsmall = 2),
                #     "Balance EOS/USD:", format(round(balance_EOSUSD,2), nsmall = 2),"\n")
                next
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
setwd("C:/btc/Strategy/R_Strategy_Arb/Arbitration/Bitfinex_EOS_BTC_USD")
write.csv(res, "EOS_cond.csv", row.names = FALSE)

library(dplyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)
library(purrr)
library(imputeTS)

setwd("C:/btc/Orderbook/Influx/NEO")
influx <- readRDS("Influx_orderbook_NEOBTCUSD_20180506_20180528.rds")
colnames(influx$NEOUSD) <- c("time", "NEOUSD_bids_amount", "NEOUSD_bids_price",
                             "NEOUSD_asks_amount", "NEOUSD_asks_price")
colnames(influx$NEOBTC) <- c("time", "NEOBTC_bids_amount", "NEOBTC_bids_price",
                             "NEOBTC_asks_amount", "NEOBTC_asks_price")
colnames(influx$BTCUSD) <- c("time", "BTCUSD_bids_amount", "BTCUSD_bids_price",
                             "BTCUSD_asks_amount", "BTCUSD_asks_price")

ob <- data.frame(time = union(union(influx$NEOUSD$time, influx$NEOBTC$time), influx$BTCUSD$time))
ob <- ob %>%
  left_join(influx$BTCUSD, by = "time") %>%
  left_join(influx$NEOUSD, by = "time") %>%
  left_join(influx$NEOBTC, by = "time") %>%
  arrange(time) %>%
  na.locf() %>%
  na.omit()

time_vec <- ob$time / 1000
NEOUSD_bid <- ob$NEOUSD_bids_price
NEOUSD_ask <- ob$NEOUSD_asks_price
NEOUSD_check <- NEOUSD_bid > NEOUSD_ask
NEOBTC_bid <- ob$NEOBTC_bids_price
NEOBTC_ask <- ob$NEOBTC_asks_price
NEOBTC_check <- NEOBTC_bid > NEOBTC_ask
BTCUSD_bid <- ob$BTCUSD_bids_price
BTCUSD_ask <- ob$BTCUSD_asks_price
BTCUSD_check <- BTCUSD_bid > BTCUSD_ask
NEOBTCUSD_bid <- NEOBTC_bid * BTCUSD_bid
NEOBTCUSD_ask <- NEOBTC_ask * BTCUSD_ask
NEOBTCUSD_check <- NEOBTCUSD_bid > NEOBTCUSD_ask
BuyBTC <- (NEOBTCUSD_ask / NEOUSD_bid - 1) * 100
SellBTC <- (NEOUSD_ask / NEOBTCUSD_bid - 1) * 100
n <- nrow(ob)

cond_open <- -0.65
cond_close <- 0.45
stop_loss <- -5.0
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
balance_NEOBTC <- 0
balance_NEOUSD <- 0
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
BuyNEOBTC <- 0
BuyNEOUSD <- 0
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
  
  if(NEOUSD_check[i] + NEOBTC_check[i] + BTCUSD_check[i] + NEOBTCUSD_check[i] > 0) {
    next
  }
  
  if(i == n) {
    PL_NEOBTC <- 0
    PL_NEOUSD <- 0
    PL_BTCUSD <- 0
    break
  }
  
  # START TRADING
  if(trade == 0) {
    if(BuyBTC_cond[i]) {
      trade <- 1
      trade_positive_difference <- 1
      
      # OPEN 1
      # SELL NEO/USD && BUY NEO/BTC/USD
      deal <- min(balance_avail * step_entry, max_deal)
      start_balance <- balance_usd
      commission <- deal * 0.002 * npairs
      balance_usd <- start_balance - commission
      balance_used <- deal * npairs
      balance_avail <- balance_usd * leverage - balance_used
      balance_NEOBTC <- deal / NEOBTCUSD_ask[i]
      balance_NEOUSD <- -deal / NEOUSD_bid[i]
      balance_BTCUSD <- deal / BTCUSD_ask[i] * BTCUSD_deal_cond

      comm <- c(comm, commission)
      avg_price_NEOUSD <- NEOUSD_bid[i]
      avg_price_NEOBTC <- NEOBTC_ask[i]
      avg_price_BTCUSD <- BTCUSD_ask[i]
      BuyNEOBTC <- 1
      
      start <- time_vec[i]
      s <- BuyBTC[i]
      cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
          "SELL NEO/USD && BUY NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
          "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
          "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
          "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2), "\n")
      cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
          "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next
    } else if(SellBTC_cond[i]) {
        trade <- 1
        trade_negative_difference <- 1
        
        # OPEN 2
        # BUY NEO/USD && SELL NEO/BTC/USD
        deal <- min(balance_avail * step_entry, max_deal)
        start_balance <- balance_usd
        commission <- deal * 0.002 * npairs
        balance_usd <- start_balance - commission
        balance_used <- deal * npairs
        balance_avail <- balance_usd * leverage - balance_used
        balance_NEOBTC <- -deal / NEOBTCUSD_bid[i]
        balance_NEOUSD <- deal / NEOUSD_ask[i]
        balance_BTCUSD <- -deal / BTCUSD_bid[i] * BTCUSD_deal_cond

        comm <- c(comm, commission)
        avg_price_NEOUSD <- NEOUSD_ask[i]
        avg_price_NEOBTC <- NEOBTC_bid[i]
        avg_price_BTCUSD <- BTCUSD_bid[i]
        BuyNEOUSD <- 1
        
        start <- time_vec[i]
        s <- SellBTC[i]
        cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            "BUY NEO/USD && SELL NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
            "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
            "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
        cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next
      }
  } else {
    if(BuyNEOBTC == 1) {
      PL_NEOBTC <- (NEOBTC_bid[i] - avg_price_NEOBTC) * balance_NEOBTC * BTCUSD_bid[i]
      PL_NEOUSD <- (NEOUSD_ask[i] - avg_price_NEOUSD) * balance_NEOUSD
      PL_BTCUSD <- (BTCUSD_bid[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond

      PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
      
      if(((PL - commission * 2) / balance_used * 100 > cond_close
          | (PL - commission * 2) / balance_used * 100 < stop_loss
          | time_vec[i] - start > stop_loss_dur)
         & trade_positive_difference == 1) {
        
        # CLOSE 1:
        ## BUY NEO/USD && SELL NEO/BTC/USD
        temp_commission <- abs(balance_NEOBTC * NEOBTCUSD_bid[i] * 0.002) +
          abs(balance_NEOUSD * NEOUSD_ask[i] * 0.002) +
          abs(balance_BTCUSD * BTCUSD_bid[i] * 0.002) * BTCUSD_deal_cond

        commission <- commission + temp_commission

        balance_usd <- balance_usd + PL - temp_commission
        balance_NEOBTC <- 0
        balance_NEOUSD <- 0
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
        cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
            "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
            "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
            "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
        cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
            "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
        temp_NEOBTC <- 0
        temp_NEOUSD <- 0
        temp_avail <- 0
        temp_used <- 0
        commission <- 0
        temp_commission <- 0
        s <- 0
        PL <- 0
        PL_NEOBTC <- 0
        PL_NEOUSD <- 0
        PL_BTCUSD <- 0
        BuyNEOBTC <- 0
        next
      }
      
      #SCALING 1
      if(BuyBTC[i] < s - scaling & trade_positive_difference == 1 & balance_avail > 100) {
        
        if(balance_avail < 200) {
          deal <- balance_avail - 100
        } else {
          deal <- min(balance_avail * step_continue, max_deal)
        }
        
        balance_NEOBTC <- balance_NEOBTC + deal / NEOBTCUSD_ask[i]
        balance_NEOUSD <- balance_NEOUSD - deal / NEOUSD_bid[i]
        balance_BTCUSD <- balance_BTCUSD + deal / BTCUSD_ask[i]
        temp_avail <- balance_avail
        
        commission <- commission + deal * 0.002 * npairs
        balance_usd <- balance_usd - deal * 0.002 * npairs
        balance_used <- balance_used + deal * npairs
        comm <- c(comm, deal * 0.002 * npairs)
        avg_price_NEOUSD <- (balance_used / npairs) / abs(balance_NEOUSD)
        avg_price_BTCUSD <- abs((avg_price_BTCUSD * (balance_BTCUSD - deal / BTCUSD_ask[i])
                                 + deal / BTCUSD_ask[i]) / balance_BTCUSD)
     
        balance_avail <- balance_usd * leverage - balance_used
        avg_price_NEOBTC <- abs((avg_price_NEOBTC * (balance_NEOBTC - deal / NEOBTCUSD_ask[i])
                                 + deal/(NEOBTCUSD_ask[i]) * NEOBTC_ask[i]) / balance_NEOBTC)
        
        s <- BuyBTC[i]
        
        cat("SCALING. NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
            "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
            "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
            "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2),"\n")
        cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next
      }
    } else {
      if(BuyNEOUSD == 1) {
        PL_NEOBTC <- (NEOBTC_ask[i] - avg_price_NEOBTC) * balance_NEOBTC * BTCUSD_ask[i]
        PL_NEOUSD <- (NEOUSD_bid[i] - avg_price_NEOUSD) * balance_NEOUSD
        PL_BTCUSD <- (BTCUSD_ask[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
        
        PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
        
        if(((PL - commission * 2) / balance_used * 100 > cond_close
            | (PL - commission * 2) / balance_used * 100 < stop_loss
            | time_vec[i] - start > stop_loss_dur)
           & trade_negative_difference == 1) {
          
          # CLOSE 2:
          ## SELL NEO/USD && BUY NEO/BTC/USD
          temp_commission <- commission
          temp_commission <- abs(balance_NEOBTC * NEOBTCUSD_bid[i] * 0.002) + 
            abs(balance_NEOUSD * NEOUSD_ask[i] * 0.002) +
            abs(balance_BTCUSD * BTCUSD_ask[i] * 0.002) * BTCUSD_deal_cond
 
          commission <- commission + temp_commission

          balance_usd <- balance_usd + PL - (abs(balance_NEOBTC * NEOBTCUSD_ask[i] * 0.002) +
                                            abs(balance_NEOUSD * NEOUSD_bid[i] * 0.002))
          balance_NEOBTC <- 0
          balance_NEOUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission) / start_balance)
          comm <- c(comm, abs(balance_NEOBTC * NEOBTCUSD_ask[i] * 0.002) +
                      abs(balance_NEOUSD * NEOUSD_bid[i] * 0.002))
          
          end <- time_vec[i]
          duration <- c(duration, (end - start) / 3600)
          date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
          
          cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
              "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
              "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
              "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
              "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2),"\n")
          cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
              "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
          temp_NEOBTC <- 0
          temp_NEOUSD <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_NEOBTC <- 0
          PL_NEOUSD <- 0
          BuyNEOUSD <- 0
          next
        }
        
        #SCALING 2
        if(SellBTC[i] < s - scaling & trade_negative_difference == 1 & balance_avail > 100) {
          
          if(balance_avail < 200) {
            deal <- balance_avail - 100
          } else {
            deal <- min(balance_avail * step_continue, max_deal)
          }
          
          temp_NEOBTC <- balance_NEOBTC
          balance_NEOBTC <- temp_NEOBTC - deal / NEOBTCUSD_bid[i]
          balance_NEOUSD <- balance_NEOUSD + deal / NEOUSD_ask[i]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / BTCUSD_bid[i]

          commission <- commission + deal * 0.002 * npairs
          balance_usd <- balance_usd - deal * 0.002 * npairs
          balance_used <- balance_used + deal * npairs
          comm <- c(comm, deal * 0.002 * npairs)
          avg_price_NEOUSD <- (balance_used / npairs) / abs(balance_NEOUSD)
          avg_price_BTCUSD <- abs((avg_price_BTCUSD * temp_BTCUSD - deal / BTCUSD_bid[i]) /
                                    balance_BTCUSD) * BTCUSD_deal_cond
          
          balance_avail <- balance_usd * leverage - balance_used
          avg_price_NEOBTC <- abs((avg_price_NEOBTC * temp_NEOBTC
                                   - deal/NEOBTCUSD_bid[i] * NEOBTC_bid[i]) / balance_NEOBTC)
          
          s <- SellBTC[i]
          
          cat("SCALING. NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
              "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
              "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
              "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
          cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
              "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
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
      balance_NEOBTC <- 0
      balance_NEOUSD <- 0
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
      BuyNEOBTC <- 0
      BuyNEOUSD <- 0
      comm <- c()
      BTCUSD_deal_cond <- 0
      if (BTCUSD_deal_cond == 0) {
        npairs <- 2
      } else {
        npairs <- 3
      }
      
      for (i in seq(1, n, by = 3)) {
        
        if(balance_usd <= 0) { break }
        
        if(NEOUSD_check[i] + NEOBTC_check[i] + BTCUSD_check[i] + NEOBTCUSD_check[i] > 0) {
          next
        }
        
        if(i == n) {
          PL_NEOBTC <- 0
          PL_NEOUSD <- 0
          PL_BTCUSD <- 0
          break
        }
        
        # START TRADING
        if(trade == 0) {
          if(BuyBTC_cond[i]) {
            trade <- 1
            trade_positive_difference <- 1
            
            # OPEN 1
            # SELL NEO/USD && BUY NEO/BTC/USD
            deal <- min(balance_avail * step_entry, max_deal)
            start_balance <- balance_usd
            commission <- deal * 0.002 * npairs
            balance_usd <- start_balance - commission
            balance_used <- deal * npairs
            balance_avail <- balance_usd * leverage - balance_used
            balance_NEOBTC <- deal / NEOBTCUSD_ask[i]
            balance_NEOUSD <- -deal / NEOUSD_bid[i]
            balance_BTCUSD <- deal / BTCUSD_ask[i] * BTCUSD_deal_cond
            
            comm <- c(comm, commission)
            avg_price_NEOUSD <- NEOUSD_bid[i]
            avg_price_NEOBTC <- NEOBTC_ask[i]
            avg_price_BTCUSD <- BTCUSD_ask[i]
            BuyNEOBTC <- 1
            
            start <- time_vec[i]
            s <- BuyBTC[i]
            # cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            #     "SELL NEO/USD && BUY NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
            #     "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
            #     "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2), "\n")
            # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next
          } else if(SellBTC_cond[i]) {
            trade <- 1
            trade_negative_difference <- 1
            
            # OPEN 2
            # BUY NEO/USD && SELL NEO/BTC/USD
            deal <- min(balance_avail * step_entry, max_deal)
            start_balance <- balance_usd
            commission <- deal * 0.002 * npairs
            balance_usd <- start_balance - commission
            balance_used <- deal * npairs
            balance_avail <- balance_usd * leverage - balance_used
            balance_NEOBTC <- -deal / NEOBTCUSD_bid[i]
            balance_NEOUSD <- deal / NEOUSD_ask[i]
            balance_BTCUSD <- -deal / BTCUSD_bid[i] * BTCUSD_deal_cond
            
            comm <- c(comm, commission)
            avg_price_NEOUSD <- NEOUSD_ask[i]
            avg_price_NEOBTC <- NEOBTC_bid[i]
            avg_price_BTCUSD <- BTCUSD_bid[i]
            BuyNEOUSD <- 1
            
            start <- time_vec[i]
            s <- SellBTC[i]
            # cat("START.", as.character(as.POSIXct(start, origin = "1970-01-1")),
            #     "BUY NEO/USD && SELL NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
            #     "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
            #     "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
            # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next
          }
        } else {
          if(BuyNEOBTC == 1) {
            PL_NEOBTC <- (NEOBTC_bid[i] - avg_price_NEOBTC) * balance_NEOBTC * BTCUSD_bid[i]
            PL_NEOUSD <- (NEOUSD_ask[i] - avg_price_NEOUSD) * balance_NEOUSD
            PL_BTCUSD <- (BTCUSD_bid[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
            
            PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
            
            if(((PL - commission * 2) / balance_used * 100 > cond_close
                | (PL - commission * 2) / balance_used * 100 < stop_loss
                | time_vec[i] - start > stop_loss_dur)
               & trade_positive_difference == 1) {
              
              # CLOSE 1:
              ## BUY NEO/USD && SELL NEO/BTC/USD
              temp_commission <- abs(balance_NEOBTC * NEOBTCUSD_bid[i] * 0.002) +
                abs(balance_NEOUSD * NEOUSD_ask[i] * 0.002) +
                abs(balance_BTCUSD * BTCUSD_bid[i] * 0.002) * BTCUSD_deal_cond
              
              commission <- commission + temp_commission
              
              balance_usd <- balance_usd + PL - temp_commission
              balance_NEOBTC <- 0
              balance_NEOUSD <- 0
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
              # cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              #     "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
              #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
              # cat("NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
              #     "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
              #     "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
              # cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
              #     "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
              temp_NEOBTC <- 0
              temp_NEOUSD <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_NEOBTC <- 0
              PL_NEOUSD <- 0
              PL_BTCUSD <- 0
              BuyNEOBTC <- 0
              next
            }
            
            #SCALING 1
            if(BuyBTC[i] < s - scaling & trade_positive_difference == 1 & balance_avail > 100) {
              
              if(balance_avail < 200) {
                deal <- balance_avail - 100
              } else {
                deal <- min(balance_avail * step_continue, max_deal)
              }
              
              balance_NEOBTC <- balance_NEOBTC + deal / NEOBTCUSD_ask[i]
              balance_NEOUSD <- balance_NEOUSD - deal / NEOUSD_bid[i]
              balance_BTCUSD <- balance_BTCUSD + deal / BTCUSD_ask[i]
              temp_avail <- balance_avail
              
              commission <- commission + deal * 0.002 * npairs
              balance_usd <- balance_usd - deal * 0.002 * npairs
              balance_used <- balance_used + deal * npairs
              comm <- c(comm, deal * 0.002 * npairs)
              avg_price_NEOUSD <- (balance_used / npairs) / abs(balance_NEOUSD)
              avg_price_BTCUSD <- abs((avg_price_BTCUSD * (balance_BTCUSD - deal / BTCUSD_ask[i])
                                       + deal / BTCUSD_ask[i]) / balance_BTCUSD)
              
              balance_avail <- balance_usd * leverage - balance_used
              avg_price_NEOBTC <- abs((avg_price_NEOBTC * (balance_NEOBTC - deal / NEOBTCUSD_ask[i])
                                       + deal/(NEOBTCUSD_ask[i]) * NEOBTC_ask[i]) / balance_NEOBTC)
              
              s <- BuyBTC[i]
              
              # cat("SCALING. NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
              #     "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
              #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
              #     "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2),"\n")
              # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
              #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
              #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
              next
            }
          } else {
            if(BuyNEOUSD == 1) {
              PL_NEOBTC <- (NEOBTC_ask[i] - avg_price_NEOBTC) * balance_NEOBTC * BTCUSD_ask[i]
              PL_NEOUSD <- (NEOUSD_bid[i] - avg_price_NEOUSD) * balance_NEOUSD
              PL_BTCUSD <- (BTCUSD_ask[i] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
              
              PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
              
              if(((PL - commission * 2) / balance_used * 100 > cond_close
                  | (PL - commission * 2) / balance_used * 100 < stop_loss
                  | time_vec[i] - start > stop_loss_dur)
                 & trade_negative_difference == 1) {
                
                # CLOSE 2:
                ## SELL NEO/USD && BUY NEO/BTC/USD
                temp_commission <- commission
                temp_commission <- abs(balance_NEOBTC * NEOBTCUSD_bid[i] * 0.002) + 
                  abs(balance_NEOUSD * NEOUSD_ask[i] * 0.002) +
                  abs(balance_BTCUSD * BTCUSD_ask[i] * 0.002) * BTCUSD_deal_cond
                
                commission <- commission + temp_commission
                
                balance_usd <- balance_usd + PL - (abs(balance_NEOBTC * NEOBTCUSD_ask[i] * 0.002) +
                                                     abs(balance_NEOUSD * NEOUSD_bid[i] * 0.002))
                balance_NEOBTC <- 0
                balance_NEOUSD <- 0
                balance_BTCUSD <- 0
                balance_used <- 0
                balance_avail <- balance_usd * leverage
                
                profit_sum <- c(profit_sum, PL - commission)
                profit_perc <- c(profit_perc, (PL - commission) / start_balance)
                comm <- c(comm, abs(balance_NEOBTC * NEOBTCUSD_ask[i] * 0.002) +
                            abs(balance_NEOUSD * NEOUSD_bid[i] * 0.002))
                
                end <- time_vec[i]
                duration <- c(duration, (end - start) / 3600)
                date <- c(date, as.Date(as.POSIXct(end, origin = "1970-01-1")))
                
                # cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
                #     "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
                #     "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
                #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
                # cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
                #     "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
                #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
                # cat("NEO/USD bid:", format(round(NEOUSD_bid[i],2), nsmall = 2),
                #     "NEO/BTC ask:", format(round(NEOBTC_ask[i],6), nsmall = 6),
                #     "BTC/USD ask:", format(round(BTCUSD_ask[i],0), nsmall = 0),
                #     "NEO/BTC/USD ask:", format(round(NEOBTCUSD_ask[i],2), nsmall = 2),"\n")
                # cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
                #     "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
                temp_NEOBTC <- 0
                temp_NEOUSD <- 0
                temp_avail <- 0
                temp_used <- 0
                commission <- 0
                temp_commission <- 0
                s <- 0
                PL <- 0
                PL_NEOBTC <- 0
                PL_NEOUSD <- 0
                BuyNEOUSD <- 0
                next
              }
              
              #SCALING 2
              if(SellBTC[i] < s - scaling & trade_negative_difference == 1 & balance_avail > 100) {
                
                if(balance_avail < 200) {
                  deal <- balance_avail - 100
                } else {
                  deal <- min(balance_avail * step_continue, max_deal)
                }
                
                temp_NEOBTC <- balance_NEOBTC
                balance_NEOBTC <- temp_NEOBTC - deal / NEOBTCUSD_bid[i]
                balance_NEOUSD <- balance_NEOUSD + deal / NEOUSD_ask[i]
                temp_BTCUSD <- balance_BTCUSD
                balance_BTCUSD <- temp_BTCUSD - deal / BTCUSD_bid[i]
                
                commission <- commission + deal * 0.002 * npairs
                balance_usd <- balance_usd - deal * 0.002 * npairs
                balance_used <- balance_used + deal * npairs
                comm <- c(comm, deal * 0.002 * npairs)
                avg_price_NEOUSD <- (balance_used / npairs) / abs(balance_NEOUSD)
                avg_price_BTCUSD <- abs((avg_price_BTCUSD * temp_BTCUSD - deal / BTCUSD_bid[i]) /
                                          balance_BTCUSD) * BTCUSD_deal_cond
                
                balance_avail <- balance_usd * leverage - balance_used
                avg_price_NEOBTC <- abs((avg_price_NEOBTC * temp_NEOBTC
                                         - deal/NEOBTCUSD_bid[i] * NEOBTC_bid[i]) / balance_NEOBTC)
                
                s <- SellBTC[i]
                
                # cat("SCALING. NEO/USD ask:", format(round(NEOUSD_ask[i],2), nsmall = 2),
                #     "NEO/BTC bid:", format(round(NEOBTC_bid[i],6), nsmall = 6),
                #     "BTC/USD bid:", format(round(BTCUSD_bid[i],0), nsmall = 0),
                #     "NEO/BTC/USD bid:", format(round(NEOBTCUSD_bid[i],2), nsmall = 2),"\n")
                # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
                #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
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
setwd("C:/btc/Strategy/R_Strategy_Arb/Arbitration/Bitfinex_NEO_BTC_USD")
write.csv(res, "NEO_cond.csv", row.names = FALSE)

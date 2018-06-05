library(dplyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

setwd("C:/btc/Orderbook/Influx")
influx <- fread("Influx_orderbook_NEOBTCUSD_20180506_20180513.csv", skip = 3, header = FALSE,
                drop = c("V1", "V3"), integer64 = "double",
                col.names = c("amount", "currencyPair", "time", "operationType", "price", "sequenceNumber"))

vec <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(vec) <- c("amount", "price")

operationType_cond <- influx$operationType > 0
amount_is_positive_cond <- influx$amount > 0
prices <- influx$price
amounts <- influx$amount
time_vec <- influx$time / 1000

collect_orderbook <- function(bids_asks) {
  if (operationType_cond[i]) { # IF "Count" > 0
    if (amount_is_positive_cond[i]) { # IF "Amount" > 0 THEN update "Bids"
      if (sum(bids_asks$bids$price == prices[i]) == 0) { # IF WE DON'T HAVE THIS PRICE IN BIDS THEN WE ADD NEW ROW AND SORT
        bids_asks$bids <- bind_rows(bids_asks$bids, c(amount = amounts[i], price = prices[i])) # Add new order to "Bids"
        bids_asks$bids <- bids_asks$bids[order(bids_asks$bids$price, decreasing = TRUE),] # Sort "Bids" by "Price" descending
      } else {
        bids_asks$bids[bids_asks$bids$price == prices[i], "amount"] <- amounts[i] # IF we have this price in Bids then just update amount
      }
      
    } else { # IF "Amount" <= 0 THEN update "Asks"
      if (sum(bids_asks$asks$price == prices[i]) == 0) {
        bids_asks$asks <- bind_rows(bids_asks$asks, c(amount = amounts[i], price = prices[i])) # Add new order to "Asks"
        bids_asks$asks <- bids_asks$asks[order(bids_asks$asks$price),] # Sort "Asks" by "Price"
      } else {
        bids_asks$asks[bids_asks$asks$price == prices[i], "amount"] <- amounts[i]
      }
    }
    
  } else { # IF "Count" == 0
    if (amount_is_positive_cond[i]) { # IF "Amount" > 0 THEN update "Bids"
      bids_asks$bids <- bids_asks$bids[bids_asks$bids$price != prices[i],] # Delete from "Bids" old order

    } else { # IF "Amount" <= 0 THEN update "Asks"
      bids_asks$asks <- bids_asks$asks[bids_asks$asks$price != prices[i],] # Delete from "Asks" old order
    }
  }
  return(bids_asks)
}

clear_orderbook_cond <- c(FALSE,
                          influx[-nrow(influx), sequenceNumber] - influx[-1, sequenceNumber] < -100)

NEOUSD_cond <- influx$currencyPair == "NEO/USD"
NEOBTC_cond <- influx$currencyPair == "NEO/BTC"
BTCUSD_cond <- influx$currencyPair == "BTC/USD"

n <- nrow(influx)

NEOUSD <- list(bids = vec, asks = vec)
NEOBTC <- list(bids = vec, asks = vec)
BTCUSD <- list(bids = vec, asks = vec)
cur_prices <- rep(0, 6)
cur_prices_old <- rep(0, 6)

cond_open <- -0.6
cond_close <- 0.9
stop_loss <- -30.0
stop_loss_dur <- 86400 * 7
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
step_entry <- 0.2
step_continue <- 0.35
comm <- c()
scaling <- 1.4
BTCUSD_deal_cond <- 0
if (BTCUSD_deal_cond == 0) {
  npairs <- 2
} else {
  npairs <- 3
}


t1 <- Sys.time()
t1

for (i in 1:10000) {
  
  # Clear orderbook 
  if(clear_orderbook_cond[i]) {
    NEOUSD <- list(bids = vec, asks = vec)
    NEOBTC <- list(bids = vec, asks = vec)
    BTCUSD <- list(bids = vec, asks = vec)
  }
  
  # Collect orderbook for all pairs
  if(BTCUSD_cond[i]) {
    BTCUSD <- collect_orderbook(BTCUSD)
  } else if(NEOUSD_cond[i]) {
    NEOUSD <- collect_orderbook(NEOUSD)
  } else if(NEOBTC_cond[i]) {
    NEOBTC <- collect_orderbook(NEOBTC)
  }
  
  if(nrow(BTCUSD$bids) < 5 | nrow(BTCUSD$asks) < 5 |
     nrow(NEOUSD$bids) < 5 | nrow(NEOUSD$asks) < 5 |
     nrow(NEOBTC$bids) < 5 | nrow(NEOBTC$asks) < 5) {
    next
  }
  
  cur_prices <- c(BTCUSD$bids$price[1], BTCUSD$asks$price[1],
              NEOUSD$bids$price[1], NEOUSD$asks$price[1],
              NEOBTC$bids$price[1], NEOBTC$asks$price[1])
  names(cur_prices) <- c("BTCUSD_bid", "BTCUSD_ask",
                        "NEOUSD_bid", "NEOUSD_ask",
                        "NEOBTC_bid", "NEOBTC_ask")

  if (sum(cur_prices != cur_prices_old) == 0) {
    # prices the same
    next
  }
  
  if(balance_usd <= 0) { break }
  
  # m[i,3] = BTCUSD$bids$price[1]
  # m[i,4] = BTCUSD$asks$price[1]
  # m[i,7] = NEOBTC$bids$price[1]
  # m[i,8] = NEOBTC$asks$price[1]
  # m[i,11] = NEOUSD$bids$price[1]
  # m[i,12] = NEOUSD$asks$price[1]
  # m[i,15] = NEOBTC$bids$price[1] * BTCUSD$bids$price[1]
  # m[i,16] = NEOBTC$asks$price[1] * BTCUSD$asks$price[1]
  

  if(i == n) {
    PL_NEOBTC <- 0
    PL_NEOUSD <- 0
    PL_BTCUSD <- 0
    break
  }
  
  # START TRADING
  if(trade == 0) {
    if(((cur_prices["NEOBTC_ask"] * cur_prices["BTCUSD_ask"] / cur_prices["NEOUSD_bid"]) - 1) * 100 < cond_open) {
      trade <- 1
      trade_positive_difference <- 1
      
      # OPEN 1
      # SELL NEO/USD && BUY NEO/BTC/USD
      deal <- balance_avail * step_entry
      start_balance <- balance_usd
      commission <- deal * 0.002 * npairs
      balance_usd <- start_balance - commission
      balance_used <- deal * npairs
      balance_avail <- balance_usd * leverage - balance_used
      balance_NEOBTC <- deal / (cur_prices["NEOBTC_ask"] * cur_prices["BTCUSD_ask"])
      balance_NEOUSD <- -deal / cur_prices["NEOUSD_bid"]
      balance_BTCUSD <- deal / cur_prices["BTCUSD_ask"] * BTCUSD_deal_cond

      comm <- c(comm, commission)
      avg_price_NEOUSD <- cur_prices["NEOUSD_bid"]
      avg_price_NEOBTC <- cur_prices["NEOBTC_ask"]
      avg_price_BTCUSD <- cur_prices["BTCUSD_ask"]
      BuyNEOBTC <- 1
      
      start <- time_vec[i]
      s <- ((cur_prices["NEOBTC_ask"] * cur_prices["BTCUSD_ask"] / cur_prices["NEOUSD_bid"]) - 1) * 100
      cat("START. SELL NEO/USD && BUY NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("NEO/USD bid:", format(round(cur_prices["NEOUSD_bid"],2), nsmall = 2),
          "NEO/BTC ask:", format(round(cur_prices["NEOBTC_ask"],6), nsmall = 6),
          "BTC/USD ask:", format(round(cur_prices["BTCUSD_ask"],0), nsmall = 0),
          "NEO/BTC/USD ask:", format(round(cur_prices["NEOBTC_ask"] * cur_prices["BTCUSD_ask"],2), nsmall = 2), "\n")
      cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
          "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next
    } else if(((cur_prices["NEOUSD_ask"] / (cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"])) - 1) * 100 < cond_open) {
        trade <- 1
        trade_negative_difference <- 1
        
        # OPEN 2
        # BUY NEO/USD && SELL NEO/BTC/USD
        deal <- balance_avail * step_entry
        start_balance <- balance_usd
        commission <- deal * 0.002 * npairs
        balance_usd <- start_balance - commission
        balance_used <- deal * npairs
        balance_avail <- balance_usd * leverage - balance_used
        balance_NEOBTC <- -deal / (cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"])
        balance_NEOUSD <- deal / cur_prices["NEOUSD_ask"]
        balance_BTCUSD <- -deal / cur_prices["BTCUSD_bid"] * BTCUSD_deal_cond

        comm <- c(comm, commission)
        avg_price_NEOUSD <- cur_prices["NEOUSD_ask"]
        avg_price_NEOBTC <- cur_prices["NEOBTC_bid"]
        avg_price_BTCUSD <- cur_prices["BTCUSD_bid"]
        BuyNEOUSD <- 1
        
        start <- time_vec[i]
        s <- ((cur_prices["NEOUSD_ask"] / (cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"])) - 1) * 100
        cat("START. BUY NEO/USD && SELL NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("NEO/USD ask:", format(round(cur_prices["NEOUSD_ask"],2), nsmall = 2),
            "NEO/BTC bid:", format(round(cur_prices["NEOBTC_bid"],6), nsmall = 6),
            "BTC/USD bid:", format(round(cur_prices["BTCUSD_bid"],0), nsmall = 0),
            "NEO/BTC/USD bid:", format(round(cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"],2), nsmall = 2),"\n")
        cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next
      }
  } else {
    if(BuyNEOBTC == 1) {
      PL_NEOBTC <- (NEOBTC$bids$price[1] - avg_price_NEOBTC) * balance_NEOBTC * BTCUSD$bids$price[1]
      PL_NEOUSD <- (NEOUSD$asks$price[1] - avg_price_NEOUSD) * balance_NEOUSD
      PL_BTCUSD <- (BTCUSD$bids$price[1] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond

      PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
      
      if(((PL - commission * 2) / balance_used * 100 > cond_close
          || (PL - commission * 2) / balance_used * 100 < stop_loss
          || time_vec[i] - start > stop_loss_dur)
         && trade_positive_difference == 1) {
        
        # CLOSE 1:
        ## BUY NEO/USD && SELL NEO/BTC/USD
        if(BTCUSD_deal_cond == 0) {
          temp_commission <- abs(balance_NEOBTC * cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"] * 0.002) +
            abs(balance_NEOUSD * cur_prices["NEOUSD_ask"] * 0.002)
        } else {
          temp_commission <- abs(balance_NEOBTC * cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"] * 0.002) +
            abs(balance_NEOUSD * cur_prices["NEOUSD_ask"] * 0.002) +
            abs(balance_BTCUSD * cur_prices["BTCUSD_bid"] * 0.002)
        }
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
        
        cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
            "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
            "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
            "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
        cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
            "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("NEO/USD ask:", format(round(cur_prices["NEOUSD_ask"],2), nsmall = 2),
            "NEO/BTC bid:", format(round(cur_prices["NEOBTC_bid"],6), nsmall = 6),
            "BTC/USD bid:", format(round(cur_prices["BTCUSD_bid"],0), nsmall = 0),
            "NEO/BTC/USD bid:", format(round(cur_prices["NEOBTC_bid"] * cur_prices["BTCUSD_bid"],2), nsmall = 2),"\n")
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
      if((NEOBTC$asks$price[1] * BTCUSD$asks$price[1] / NEOUSD$bids$price[1] - 1) * 100 < s - scaling
         && trade_positive_difference == 1 && balance_avail > 100) {
        
        if(balance_avail < 200) {
          deal <- balance_avail - 100
        } else {
          deal <- balance_avail * step_continue
        }
        
        balance_NEOBTC <- balance_NEOBTC + deal / (NEOBTC$asks$price[1] * BTCUSD$asks$price[1])
        balance_NEOUSD <- balance_NEOUSD - deal / (NEOUSD$bids$price[1])
        balance_BTCUSD <- balance_BTCUSD + deal / (BTCUSD$asks$price[1])
        temp_avail <- balance_avail
        
        if(BTCUSD_deal_cond == 0){
          commission <- commission + deal * 0.002 * 2
          balance_usd <- balance_usd - deal * 0.002 * 2
          balance_used <- balance_used + deal * 2
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
        } else {
          commission <- temp_commission + deal * 0.002 * 3
          balance_usd <- temp_usd - deal * 0.002 * 3
          balance_used <- temp_used + deal * 3
          comm <- c(comm, deal * 0.002 * 3)
          avg_price_NEOUSD <- (balance_used / 3) / abs(balance_NEOUSD)
          temp_price_BTCUSD <- avg_price_BTCUSD
          avg_price_BTCUSD <- abs((temp_price_BTCUSD * temp_BTCUSD + deal / BTCUSD$asks$price[1]) /
                                    balance_BTCUSD)
        }
        balance_avail <- balance_usd * leverage - balance_used
        temp_price_NEOBTC <- avg_price_NEOBTC
        avg_price_NEOBTC <- abs((temp_price_NEOBTC*temp_NEOBTC + deal/(NEOBTC$asks$price[1] * BTCUSD$asks$price[1]) * NEOBTC$asks$price[1]) /
                                  balance_NEOBTC)
        
        s <- (NEOBTC$asks$price[1] * BTCUSD$asks$price[1] / NEOUSD$bids$price[1] - 1) * 100
        
        cat("SCALING. NEO/USD bid:", format(round(NEOUSD$bids$price[1],2), nsmall = 2),
            "NEO/BTC ask:", format(round(NEOBTC$asks$price[1],6), nsmall = 6),
            "BTC/USD ask:", format(round(BTCUSD$asks$price[1],0), nsmall = 0),
            "NEO/BTC/USD ask:", format(round(NEOBTC$asks$price[1] * BTCUSD$asks$price[1],2), nsmall = 2),"\n")
        cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      }
    } else {
      if(BuyNEOUSD == 1) {
        PL_NEOBTC <- (cur_prices["NEOBTC_ask"] - avg_price_NEOBTC) * balance_NEOBTC * cur_prices["BTCUSD_ask"]
        PL_NEOUSD <- (cur_prices["NEOUSD_bid"] - avg_price_NEOUSD) * balance_NEOUSD
        PL_BTCUSD <- (cur_prices["BTCUSD_ask"] - avg_price_BTCUSD) * balance_BTCUSD * BTCUSD_deal_cond
        
        PL <- PL_NEOUSD + PL_NEOBTC + PL_BTCUSD
        
        if(((PL - commission * 2) / balance_used * 100 > cond_close
            || (PL - commission * 2) / balance_used * 100 < stop_loss
            || time_vec[i] - start > stop_loss_dur)
           && trade_negative_difference == 1) {
          
          # CLOSE 2:
          ## SELL NEO/USD && BUY NEO/BTC/USD
          temp_commission <- commission
          if(BTCUSD_deal_cond == 0) {
            temp_commission <- abs(balance_NEOBTC * NEOBTC$bids$price[1] * BTCUSD$bids$price[1] * 0.002) + 
              abs(balance_NEOUSD * NEOUSD$asks$price[1] * 0.002)
          } else {
            temp_commission <- abs(balance_NEOBTC * NEOBTC$bids$price[1] * BTCUSD$bids$price[1] * 0.002) + 
              abs(balance_NEOUSD * NEOUSD$asks$price[1] * 0.002) +
              abs(balance_BTCUSD * BTCUSD$asks$price[1] * 0.002)
          }
          commission <- commission + temp_commission

          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_NEOBTC * NEOBTC$asks$price[1] * BTCUSD$asks$price[1] * 0.002) +
                                            abs(balance_NEOUSD * NEOUSD$bids$price[1] * 0.002))
          balance_NEOBTC <- 0
          balance_NEOUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission) / start_balance)
          comm <- c(comm, abs(balance_NEOBTC * NEOBTC$asks$price[1] * BTCUSD$asks$price[1] * 0.002) +
                      abs(balance_NEOUSD * NEOUSD$bids$price[1] * 0.002))
          
          end <- time_vec[i]
          duration <- c(duration, (end - start) / 3600)
          
          cat("End deal:", as.character(as.POSIXct(end, origin = "1970-01-1")),
              "Duration (H):", format(round((end - start) / 3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start) / 60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("NEO/USD bid:", format(round(NEOUSD$bids$price[1],2), nsmall = 2),
              "NEO/BTC ask:", format(round(NEOBTC$asks$price[1],6), nsmall = 6),
              "BTC/USD ask:", format(round(BTCUSD$asks$price[1],0), nsmall = 0),
              "NEO/BTC/USD ask:", format(round(NEOBTC$asks$price[1] * BTCUSD$asks$price[1],2), nsmall = 2),"\n")
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
          next;
        }
        
        #SCALING 2
        if((NEOUSD$asks$price[1] / (NEOBTC$bids$price[1] * BTCUSD$bids$price[1]) - 1) * 100 < s - scaling
           && trade_negative_difference == 1 && balance_avail > 100){
          
          if(balance_avail < 200) {
            deal <- balance_avail - 100
          } else {
            deal <- balance_avail * step_continue
          }
          
          temp_commission <- commission
          temp_usd <- balance_usd
          temp_used <- balance_used
          temp_NEOBTC <- balance_NEOBTC
          balance_NEOBTC <- temp_NEOBTC - deal / (NEOBTC$bids$price[1] * BTCUSD$bids$price[1])
          temp_NEOUSD <- balance_NEOUSD
          balance_NEOUSD <- temp_NEOUSD + deal / NEOUSD$asks$price[1]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / BTCUSD$bids$price[1]
          temp_avail <- balance_avail
          
          
          
          if(BTCUSD_deal_cond == 0) {
            commission <- temp_commission + deal * 0.002 * 2
            balance_usd <- temp_usd - deal * 0.002 * 2
            balance_used <- temp_used + deal * 2
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
          } else {
            commission <- temp_commission + deal * 0.002 * 3
            balance_usd <- temp_usd - deal * 0.002 * 3
            balance_used <- temp_used + deal * 3
            comm <- c(comm, deal * 0.002 * 3)
            avg_price_NEOUSD <- (balance_used / 3) / abs(balance_NEOUSD)
            temp_price_BTCUSD <- avg_price_BTCUSD
            avg_price_BTCUSD <- abs((temp_price_BTCUSD * temp_BTCUSD - deal / BTCUSD$bids$price[1]) /
                                      balance_BTCUSD)
          }
          balance_avail <- balance_usd * leverage - balance_used
          temp_price_NEOBTC <- avg_price_NEOBTC
          avg_price_NEOBTC <- abs((temp_price_NEOBTC * temp_NEOBTC - deal/(NEOBTC$bids$price[1] * BTCUSD$bids$price[1]) * NEOBTC$bids$price[1]) /
                                    balance_NEOBTC)
          
          s <- (NEOUSD$asks$price[1] / (NEOBTC$bids$price[1] * BTCUSD$bids$price[1]) - 1) * 100
          
          cat("SCALING. NEO/USD ask:", format(round(NEOUSD$asks$price[1],2), nsmall = 2),
              "NEO/BTC bid:", format(round(NEOBTC$bids$price[1],6), nsmall = 6),
              "BTC/USD bid:", format(round(BTCUSD$bids$price[1],0), nsmall = 0),
              "NEO/BTC/USD bid:", format(round(NEOBTC$bids$price[1] * BTCUSD$bids$price[1],2), nsmall = 2),"\n")
          cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
              "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
          next;
        }
      }
    }
  }
  cur_prices_old <- cur_prices
}

t2 <- Sys.time()
t2-t1

if (TRUE){
  cat("Start testing:", as.character(as.POSIXct(time_vec[1], origin = "1970-01-1")),
      "End testing",as.character(as.POSIXct(time_vec[i], origin = "1970-01-1")),
      "Estimating period (Days):",format(round((time_vec[i]-time_vec[1])/86400,0), nsmall = 0),"\n")
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
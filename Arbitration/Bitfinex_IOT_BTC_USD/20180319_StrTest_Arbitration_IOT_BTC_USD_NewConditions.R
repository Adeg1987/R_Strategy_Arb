library(dplyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

#NO ORDERBOOK, JUST TRADES

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BTCUSD <- fread("BITF_BTCUSD_20180101_20180506.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/IOTBTC")
IOTBTC <- fread("BITF_IOTBTC_20180101_20180506.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/IOTUSD")
IOTUSD <- fread("BITF_IOTUSD_20180101_20180506.csv", header = TRUE)

time_df <- data.frame(Timestamp = c(1514678401:1525651177), index = 1)

IOT <- time_df %>%
  left_join(BTCUSD, by = "Timestamp") %>%
  mutate(BTCUSD_bid = price / 1.00001, BTCUSD_ask = price * 1.00001, BTCUSD_am_bid = 0, BTCUSD_am_ask = 0) %>%
  select(-price) %>%
  left_join(IOTBTC, by = "Timestamp") %>%
  mutate(IOTBTC_bid = price / 1.001, IOTBTC_ask = price * 1.001, IOTBTC_am_bid = 0, IOTBTC_am_ask = 0) %>%
  select(-price) %>%
  left_join(IOTUSD, by = "Timestamp") %>%
  mutate(IOTUSD_bid = price / 1.00001, IOTUSD_ask = price * 1.001, IOTUSD_am_bid = 0, IOTUSD_am_ask = 0) %>%
  select(-price) %>%
  imputeTS::na.interpolation() %>%
  mutate(IOTBTCUSD_bid = BTCUSD_bid * IOTBTC_bid, IOTBTCUSD_ask = BTCUSD_ask * IOTBTC_ask)

head(IOT,10)

m <- data.matrix(IOT)
rm(BTCUSD,IOTBTC,IOTUSD,IOT, time_df)
gc()
cat("\014")

cond_open <- -1.0
cond_close <- 0.5
stop_loss <- -5.0
stop_loss_dur <- 86400 * 100
trail <- 0.0
trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_IOTBTC <- 0
balance_IOTUSD <- 0
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
BuyIOTBTC <- 0
BuyIOTUSD <- 0
step_entry <- 0.25
step_continue <- 0.25
comm <- c()
scaling <- 1.4
BTCUSD_cond <- 0

for(i in 1:nrow(m)){
  if(balance_usd <= 0) break
  if(trade == 0){
    if(((m[i,16]/m[i,11])-1)*100 < cond_open){
      trade <- 1
      trade_positive_difference <- 1
      
      #OPEN 1
      #SELL IOT/USD && BUY IOT/BTC/USD
      deal <- balance_avail * step_entry
      start_balance <- balance_usd
      if(BTCUSD_cond == 0){
        commission <- deal * 0.002 * 2
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_IOTBTC <- deal / m[i,16]
        balance_IOTUSD <- -deal / m[i,11]
        balance_BTCUSD <- 0
      }
      else{
        commission <- deal * 0.002 * 3
        balance_usd <- start_balance - commission
        balance_used <- deal * 3
        balance_avail <- balance_usd * leverage - balance_used
        balance_IOTBTC <- deal / m[i,16]
        balance_IOTUSD <- -deal / m[i,11]
        balance_BTCUSD <- deal / m[i,4]
      }
      
      comm <- c(comm, commission)
      avg_price_IOTUSD <- m[i,11]
      avg_price_IOTBTC <- m[i,8]
      avg_price_BTCUSD <- m[i,4]
      BuyIOTBTC <- 1
      
      start <- m[i,1]
      s <- (m[i,16]/m[i,11]-1)*100
      cat("START. SELL IOT/USD && BUY IOT/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
          "IOT/BTC ask:", format(round(m[i,8],8), nsmall = 8),
          "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
      cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
          "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
          "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      next;
    }
    else {
      if(((m[i,12]/m[i,15])-1)*100 < cond_open){
        trade <- 1
        trade_negative_difference <- 1
        
        #OPEN 2
        
        #BUY USD/IOT && SELL USD/BTC/IOT
        deal <- balance_avail * step_entry
        start_balance <- balance_usd
        if(BTCUSD_cond == 0){
          commission <- deal * 0.002 * 2
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_IOTBTC <- -deal / m[i,15]
          balance_IOTUSD <- deal / m[i,12]
        }
        else{
          commission <- deal * 0.002 * 3
          balance_usd <- start_balance - commission
          balance_used <- deal * 3
          balance_avail <- balance_usd * leverage - balance_used
          balance_IOTBTC <- -deal / m[i,15]
          balance_IOTUSD <- deal / m[i,12]
          balance_BTCUSD <- -deal / m[i,3]
        }
        comm <- c(comm, commission)
        avg_price_IOTUSD <- m[i,12]
        avg_price_IOTBTC <- m[i,7]
        avg_price_BTCUSD <- m[i,3]
        
        BuyIOTUSD <- 1
        
        start <- m[i,1]
        s <- (m[i,12]/m[i,15]-1)*100
        cat("START. BUY IOT/USD && SELL IOT/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "IOT/BTC bid:", format(round(m[i,7],8), nsmall = 8),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
            "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
        next;
      }
    }
  }
  else{
    if(BuyIOTBTC == 1){
      PL_IOTBTC <- (m[i,7] - avg_price_IOTBTC)*balance_IOTBTC*m[i,3]
      PL_IOTUSD <- (m[i,12] - avg_price_IOTUSD)*balance_IOTUSD
      if(BTCUSD_cond == 0) PL_BTCUSD <- 0
      else PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
      
      if(((PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
          || (PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
          || m[i,1]-start > stop_loss_dur
          || i==nrow(m)-4) && trade_positive_difference == 1){
        #CLOSE 1:
        ##BUY IOT/USD && SELL IOT/BTC/USD
        temp_commission <- commission
        if(BTCUSD_cond == 0){
          commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
            abs(balance_IOTUSD * m[i,12] * 0.002)
          temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
            abs(balance_IOTUSD * m[i,12] * 0.002)
          PL_BTCUSD <- 0
        }
        else{
          commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
            abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
            abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
          PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
        }
        PL_IOTBTC <- (m[i,7] - avg_price_IOTBTC)*balance_IOTBTC*m[i,3]
        PL_IOTUSD <- (m[i,12] - avg_price_IOTUSD)*balance_IOTUSD
        #if last row and then we close position and set PL=0
        if(i == nrow(m)-4){
          PL_IOTBTC <- 0
          PL_IOTUSD <- 0
          PL_BTCUSD <- 0
        }
        PL <- PL_IOTUSD + PL_IOTBTC + PL_BTCUSD
        temp_usd <- balance_usd
        balance_usd <- temp_usd + PL - temp_commission
        balance_IOTBTC <- 0
        balance_IOTUSD <- 0
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
        cat("Avg price IOT/USD:",format(round(avg_price_IOTUSD,2), nsmall = 2),
            "Avg price IOT/BTC:",format(round(avg_price_IOTBTC,8), nsmall = 8),
            "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
        cat("IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "IOT/BTC bid:", format(round(m[i,7],8), nsmall = 8),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("PL IOT/USD=",format(round(PL_IOTUSD,0), nsmall = 0),
            "PL IOT/BTC=",format(round(PL_IOTBTC,0), nsmall = 0),
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
        temp_IOTBTC <- 0
        temp_IOTUSD <- 0
        temp_avail <- 0
        temp_used <- 0
        commission <- 0
        temp_commission <- 0
        s <- 0
        PL <- 0
        PL_IOTBTC <- 0
        PL_IOTUSD <- 0
        PL_BTCUSD <- 0
        BuyIOTBTC <- 0
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
        temp_IOTBTC <- balance_IOTBTC
        balance_IOTBTC <- temp_IOTBTC + deal / m[i,16]
        temp_IOTUSD <- balance_IOTUSD
        balance_IOTUSD <- temp_IOTUSD - deal / m[i,11]
        temp_BTCUSD <- balance_BTCUSD
        balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
        temp_avail <- balance_avail
        
        if(BTCUSD_cond == 0){
          commission <- temp_commission + deal * 0.002 * 2
          balance_usd <- temp_usd - deal * 0.002 * 2
          balance_used <- temp_used + deal * 2
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_IOTUSD <- (balance_used / 2) / abs(balance_IOTUSD)
        }
        else{
          commission <- temp_commission + deal * 0.002 * 3
          balance_usd <- temp_usd - deal * 0.002 * 3
          balance_used <- temp_used + deal * 3
          comm <- c(comm, deal * 0.002 * 3)
          avg_price_IOTUSD <- (balance_used / 3) / abs(balance_IOTUSD)
          temp_price_BTCUSD <- avg_price_BTCUSD
          avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                    balance_BTCUSD)
        }
        balance_avail <- balance_usd * leverage - balance_used
        temp_price_IOTBTC <- avg_price_IOTBTC
        avg_price_IOTBTC <- abs((temp_price_IOTBTC*temp_IOTBTC + deal/m[i,16]*m[i,8]) /
                                  balance_IOTBTC)
        
        s <- (m[i,16]/m[i,11]-1)*100
        
        cat("SCALING. IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "IOT/BTC ask:", format(round(m[i,8],8), nsmall = 8),
            "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
        cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
            "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
            "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
      }
    }
    else{
      if(BuyIOTUSD == 1){
        PL_IOTBTC <- (m[i,8] - avg_price_IOTBTC)*balance_IOTBTC*m[i,4]
        PL_IOTUSD <- (m[i,11] - avg_price_IOTUSD)*balance_IOTUSD
        if(BTCUSD_cond == 0) PL_BTCUSD <- 0
        else PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
        
        if(((PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
            || (PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
            || m[i,1]-start > stop_loss_dur
            || i == nrow(m)-4) && trade_negative_difference == 1){
          
          #CLOSE 2:
          ##SELL IOT/USD && BUY IOT/BTC/USD
          temp_commission <- commission
          if(BTCUSD_cond == 0){
            commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
              abs(balance_IOTUSD * m[i,12] * 0.002)
            temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
              abs(balance_IOTUSD * m[i,12] * 0.002)
            PL_BTCUSD <- 0
          }
          else{
            commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
              abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
              abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
            PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
          }
          PL_IOTBTC <- (m[i,8] - avg_price_IOTBTC)*balance_IOTBTC*m[i,4]
          PL_IOTUSD <- (m[i,11] - avg_price_IOTUSD)*balance_IOTUSD
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_IOTBTC <- 0
            PL_IOTUSD <- 0
            PL_BTCUSD <- 0
          }
          PL <- PL_IOTUSD + PL_IOTBTC + PL_BTCUSD
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_IOTBTC * m[i,16] * 0.002) +
                                            abs(balance_IOTUSD * m[i,11] * 0.002))
          balance_IOTBTC <- 0
          balance_IOTUSD <- 0
          balance_BTCUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_IOTBTC * m[i,16] * 0.002) +
                      abs(balance_IOTUSD * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price IOT/USD:",format(round(avg_price_IOTUSD,2), nsmall = 2),
              "Avg price IOT/BTC:",format(round(avg_price_IOTBTC,8), nsmall = 8),
              "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
          cat("IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "IOT/BTC ask:", format(round(m[i,8],8), nsmall = 8),
              "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
          cat("PL IOT/USD=",format(round(PL_IOTUSD,0), nsmall = 0),
              "PL IOT/BTC=",format(round(PL_IOTBTC,0), nsmall = 0),
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
          temp_IOTBTC <- 0
          temp_IOTUSD <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_IOTBTC <- 0
          PL_IOTUSD <- 0
          BuyIOTUSD <- 0
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
          temp_IOTBTC <- balance_IOTBTC
          balance_IOTBTC <- temp_IOTBTC - deal / m[i,15]
          temp_IOTUSD <- balance_IOTUSD
          balance_IOTUSD <- temp_IOTUSD + deal / m[i,12]
          temp_BTCUSD <- balance_BTCUSD
          balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
          temp_avail <- balance_avail
          
          
          
          if(BTCUSD_cond == 0){
            commission <- temp_commission + deal * 0.002 * 2
            balance_usd <- temp_usd - deal * 0.002 * 2
            balance_used <- temp_used + deal * 2
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_IOTUSD <- (balance_used / 2) / abs(balance_IOTUSD)
          }
          else{
            commission <- temp_commission + deal * 0.002 * 3
            balance_usd <- temp_usd - deal * 0.002 * 3
            balance_used <- temp_used + deal * 3
            comm <- c(comm, deal * 0.002 * 3)
            avg_price_IOTUSD <- (balance_used / 3) / abs(balance_IOTUSD)
            temp_price_BTCUSD <- avg_price_BTCUSD
            avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                      balance_BTCUSD)
          }
          balance_avail <- balance_usd * leverage - balance_used
          temp_price_IOTBTC <- avg_price_IOTBTC
          avg_price_IOTBTC <- abs((temp_price_IOTBTC*temp_IOTBTC - deal/m[i,15]*m[i,7]) /
                                    balance_IOTBTC)

          s <- (m[i,12]/m[i,15]-1)*100
          
          cat("SCALING. IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "IOT/BTC bid:", format(round(m[i,7],8), nsmall = 8),
              "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
          cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
              "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),"\n")
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
x <- c("cond_open", "cond_close","step_entry","profit", "deals")
colnames(res) <- x
for(cond_open in seq(-1.5,0.0,by=0.1)){
  for(cond_close in seq(0.1,1.0,by=0.1)){
    for(step_entry in seq(0.1,0.3,by=0.05)){
    
    t1 <- Sys.time()
    trade <- 0
    trade_positive_difference <- 0
    trade_negative_difference <- 0
    start <- 0
    end <- 0
    s <- 0
    start_balance <- 4000
    balance_usd <- 4000
    balance_IOTBTC <- 0
    balance_IOTUSD <- 0
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
    BuyIOTBTC <- 0
    BuyIOTUSD <- 0
    #step_entry <- 0.2
    step_continue <- 0.25
    comm <- c()
    scaling <- 1.4
    BTCUSD_cond <- 0
    stop_loss_dur <- 86400 * 100

    for(i in 1:nrow(m)){
      if(balance_usd <= 0) break
      if(trade == 0){
        if(((m[i,16]/m[i,11])-1)*100 < cond_open){
          trade <- 1
          trade_positive_difference <- 1
          
          #OPEN 1
          #SELL IOT/USD && BUY IOT/BTC/USD
          deal <- balance_avail * step_entry
          start_balance <- balance_usd
          if(BTCUSD_cond == 0){
            commission <- deal * 0.002 * 2
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_IOTBTC <- deal / m[i,16]
            balance_IOTUSD <- -deal / m[i,11]
            balance_BTCUSD <- 0
          }
          else{
            commission <- deal * 0.002 * 3
            balance_usd <- start_balance - commission
            balance_used <- deal * 3
            balance_avail <- balance_usd * leverage - balance_used
            balance_IOTBTC <- deal / m[i,16]
            balance_IOTUSD <- -deal / m[i,11]
            balance_BTCUSD <- deal / m[i,4]
          }
          
          comm <- c(comm, commission)
          avg_price_IOTUSD <- m[i,11]
          avg_price_IOTBTC <- m[i,8]
          avg_price_BTCUSD <- m[i,4]
          BuyIOTBTC <- 1
          
          start <- m[i,1]
          s <- (m[i,16]/m[i,11]-1)*100
          # cat("START. SELL IOT/USD && BUY IOT/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "IOT/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          #     "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
          # cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
          #     "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
          #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          next;
        }
        else {
          if(((m[i,12]/m[i,15])-1)*100 < cond_open){
            trade <- 1
            trade_negative_difference <- 1
            
            #OPEN 2
            
            #BUY USD/IOT && SELL USD/BTC/IOT
            deal <- balance_avail * step_entry
            start_balance <- balance_usd
            if(BTCUSD_cond == 0){
              commission <- deal * 0.002 * 2
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_IOTBTC <- -deal / m[i,15]
              balance_IOTUSD <- deal / m[i,12]
            }
            else{
              commission <- deal * 0.002 * 3
              balance_usd <- start_balance - commission
              balance_used <- deal * 3
              balance_avail <- balance_usd * leverage - balance_used
              balance_IOTBTC <- -deal / m[i,15]
              balance_IOTUSD <- deal / m[i,12]
              balance_BTCUSD <- -deal / m[i,3]
            }
            comm <- c(comm, commission)
            avg_price_IOTUSD <- m[i,12]
            avg_price_IOTBTC <- m[i,7]
            avg_price_BTCUSD <- m[i,3]
            
            BuyIOTUSD <- 1
            
            start <- m[i,1]
            s <- (m[i,12]/m[i,15]-1)*100
            # cat("START. BUY IOT/USD && SELL IOT/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "IOT/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
            #     "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
            next;
          }
        }
      }
      else{
        if(BuyIOTBTC == 1){
          PL_IOTBTC <- (m[i,7] - avg_price_IOTBTC)*balance_IOTBTC*m[i,3]
          PL_IOTUSD <- (m[i,12] - avg_price_IOTUSD)*balance_IOTUSD
          if(BTCUSD_cond == 0) PL_BTCUSD <- 0
          else PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
          
          if(((PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
              || (PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
              || m[i,1]-start > stop_loss_dur
              || i==nrow(m)-4) && trade_positive_difference == 1){
            #CLOSE 1:
            ##BUY IOT/USD && SELL IOT/BTC/USD
            temp_commission <- commission
            if(BTCUSD_cond == 0){
              commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
                abs(balance_IOTUSD * m[i,12] * 0.002)
              temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
                abs(balance_IOTUSD * m[i,12] * 0.002)
              PL_BTCUSD <- 0
            }
            else{
              commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
                abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
                abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,3] * 0.002)
              PL_BTCUSD <- (m[i,3] - avg_price_BTCUSD)*balance_BTCUSD
            }
            PL_IOTBTC <- (m[i,7] - avg_price_IOTBTC)*balance_IOTBTC*m[i,3]
            PL_IOTUSD <- (m[i,12] - avg_price_IOTUSD)*balance_IOTUSD
            #if last row and then we close position and set PL=0
            if(i == nrow(m)-4){
              PL_IOTBTC <- 0
              PL_IOTUSD <- 0
              PL_BTCUSD <- 0
            }
            PL <- PL_IOTUSD + PL_IOTBTC + PL_BTCUSD
            temp_usd <- balance_usd
            balance_usd <- temp_usd + PL - temp_commission
            balance_IOTBTC <- 0
            balance_IOTUSD <- 0
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
            # cat("Avg price IOT/USD:",format(round(avg_price_IOTUSD,2), nsmall = 2),
            #     "Avg price IOT/BTC:",format(round(avg_price_IOTBTC,6), nsmall = 6),
            #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
            # cat("IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "IOT/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("PL IOT/USD=",format(round(PL_IOTUSD,0), nsmall = 0),
            #     "PL IOT/BTC=",format(round(PL_IOTBTC,0), nsmall = 0),
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
            temp_IOTBTC <- 0
            temp_IOTUSD <- 0
            temp_avail <- 0
            temp_used <- 0
            commission <- 0
            temp_commission <- 0
            s <- 0
            PL <- 0
            PL_IOTBTC <- 0
            PL_IOTUSD <- 0
            PL_BTCUSD <- 0
            BuyIOTBTC <- 0
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
            temp_IOTBTC <- balance_IOTBTC
            balance_IOTBTC <- temp_IOTBTC + deal / m[i,16]
            temp_IOTUSD <- balance_IOTUSD
            balance_IOTUSD <- temp_IOTUSD - deal / m[i,11]
            temp_BTCUSD <- balance_BTCUSD
            balance_BTCUSD <- temp_BTCUSD + deal / m[i,4]
            temp_avail <- balance_avail
            
            if(BTCUSD_cond == 0){
              commission <- temp_commission + deal * 0.002 * 2
              balance_usd <- temp_usd - deal * 0.002 * 2
              balance_used <- temp_used + deal * 2
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_IOTUSD <- (balance_used / 2) / abs(balance_IOTUSD)
            }
            else{
              commission <- temp_commission + deal * 0.002 * 3
              balance_usd <- temp_usd - deal * 0.002 * 3
              balance_used <- temp_used + deal * 3
              comm <- c(comm, deal * 0.002 * 3)
              avg_price_IOTUSD <- (balance_used / 3) / abs(balance_IOTUSD)
              temp_price_BTCUSD <- avg_price_BTCUSD
              avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD + deal/m[i,4]) /
                                        balance_BTCUSD)
            }
            balance_avail <- balance_usd * leverage - balance_used
            temp_price_IOTBTC <- avg_price_IOTBTC
            avg_price_IOTBTC <- abs((temp_price_IOTBTC*temp_IOTBTC + deal/m[i,16]*m[i,8]) /
                                      balance_IOTBTC)
            
            s <- (m[i,16]/m[i,11]-1)*100
            
            # cat("SCALING. IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "IOT/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            #     "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
            # cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
            #     "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),
            #     "Balance BTC/USD:", format(round(balance_BTCUSD,2), nsmall = 2),"\n")
          }
        }
        else{
          if(BuyIOTUSD == 1){
            PL_IOTBTC <- (m[i,8] - avg_price_IOTBTC)*balance_IOTBTC*m[i,4]
            PL_IOTUSD <- (m[i,11] - avg_price_IOTUSD)*balance_IOTUSD
            if(BTCUSD_cond == 0) PL_BTCUSD <- 0
            else PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
            
            if(((PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100>cond_close
                || (PL_IOTBTC+PL_IOTUSD+PL_BTCUSD-commission*2)/balance_used*100<stop_loss
                || m[i,1]-start > stop_loss_dur
                || i == nrow(m)-4) && trade_negative_difference == 1){
              
              #CLOSE 2:
              ##SELL IOT/USD && BUY IOT/BTC/USD
              
              if(BTCUSD_cond == 0){
                commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
                  abs(balance_IOTUSD * m[i,12] * 0.002)
                temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
                  abs(balance_IOTUSD * m[i,12] * 0.002)
                PL_BTCUSD <- 0
              }
              else{
                commission <- temp_commission + abs(balance_IOTBTC * m[i,15] * 0.002) +
                  abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                temp_commission <- abs(balance_IOTBTC * m[i,15] * 0.002) + 
                  abs(balance_IOTUSD * m[i,12] * 0.002) + abs(balance_BTCUSD * m[i,4] * 0.002)
                PL_BTCUSD <- (m[i,4] - avg_price_BTCUSD)*balance_BTCUSD
              }
              PL_IOTBTC <- (m[i,8] - avg_price_IOTBTC)*balance_IOTBTC*m[i,4]
              PL_IOTUSD <- (m[i,11] - avg_price_IOTUSD)*balance_IOTUSD
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_IOTBTC <- 0
                PL_IOTUSD <- 0
                PL_BTCUSD <- 0
              }
              PL <- PL_IOTUSD + PL_IOTBTC + PL_BTCUSD
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_IOTBTC * m[i,16] * 0.002) +
                                                abs(balance_IOTUSD * m[i,11] * 0.002))
              balance_IOTBTC <- 0
              balance_IOTUSD <- 0
              balance_BTCUSD <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_IOTBTC * m[i,16] * 0.002) +
                          abs(balance_IOTUSD * m[i,11] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("Avg price IOT/USD:",format(round(avg_price_IOTUSD,2), nsmall = 2),
              #     "Avg price IOT/BTC:",format(round(avg_price_IOTBTC,6), nsmall = 6),
              #     "Avg price BTC/USD:",format(round(avg_price_BTCUSD,0), nsmall = 0),"\n")
              # cat("IOT/USD bid:", format(round(m[i,11],2), nsmall = 2),
              #     "IOT/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              #     "IOT/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
              # cat("PL IOT/USD=",format(round(PL_IOTUSD,0), nsmall = 0),
              #     "PL IOT/BTC=",format(round(PL_IOTBTC,0), nsmall = 0),
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
              temp_IOTBTC <- 0
              temp_IOTUSD <- 0
              temp_avail <- 0
              temp_used <- 0
              commission <- 0
              temp_commission <- 0
              s <- 0
              PL <- 0
              PL_IOTBTC <- 0
              PL_IOTUSD <- 0
              BuyIOTUSD <- 0
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
              temp_IOTBTC <- balance_IOTBTC
              balance_IOTBTC <- temp_IOTBTC - deal / m[i,15]
              temp_IOTUSD <- balance_IOTUSD
              balance_IOTUSD <- temp_IOTUSD + deal / m[i,12]
              temp_BTCUSD <- balance_BTCUSD
              balance_BTCUSD <- temp_BTCUSD - deal / m[i,3]
              temp_avail <- balance_avail
              
              
              
              if(BTCUSD_cond == 0){
                commission <- temp_commission + deal * 0.002 * 2
                balance_usd <- temp_usd - deal * 0.002 * 2
                balance_used <- temp_used + deal * 2
                comm <- c(comm, deal * 0.002 * 2)
                avg_price_IOTUSD <- (balance_used / 2) / abs(balance_IOTUSD)
              }
              else{
                commission <- temp_commission + deal * 0.002 * 3
                balance_usd <- temp_usd - deal * 0.002 * 3
                balance_used <- temp_used + deal * 3
                comm <- c(comm, deal * 0.002 * 3)
                avg_price_IOTUSD <- (balance_used / 3) / abs(balance_IOTUSD)
                temp_price_BTCUSD <- avg_price_BTCUSD
                avg_price_BTCUSD <- abs((temp_price_BTCUSD*temp_BTCUSD - deal/m[i,3]) /
                                          balance_BTCUSD)
              }
              balance_avail <- balance_usd * leverage - balance_used
              temp_price_IOTBTC <- avg_price_IOTBTC
              avg_price_IOTBTC <- abs((temp_price_IOTBTC*temp_IOTBTC - deal/m[i,15]*m[i,7]) /
                                        balance_IOTBTC)
              
              s <- (m[i,12]/m[i,15]-1)*100
              
              # cat("SCALING. IOT/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "IOT/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              #     "IOT/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
              # cat("Balance IOT/BTC:", format(round(balance_IOTBTC,2), nsmall = 2),
              #     "Balance IOT/USD:", format(round(balance_IOTUSD,2), nsmall = 2),"\n")
              next;
            }
          }
        }
      }
    }
    
    t2 <- Sys.time()
    t2-t1
    cat("open=",cond_open,"close=",cond_close,"step_entry=",step_entry,
        "Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),
        "% Total deals:",length(profit_sum),"time difference of",t2-t1,"secs\n")
    df <- data.frame(cond_open,cond_close,step_entry,format(round((balance_usd/4000-1)*100,2), nsmall = 2),length(profit_sum))
    x <- c("cond_open", "cond_close","step_entry","profit", "deals")
    colnames(df) <- x
    res <- rbind(res,df)
    }
  }
}
setwd("C:/btc/Strategy/R_Strategy_Arb/Arbitration/Bitfinex_IOT_BTC_USD")
write.csv(res, "IOT_cond.csv", row.names = FALSE)



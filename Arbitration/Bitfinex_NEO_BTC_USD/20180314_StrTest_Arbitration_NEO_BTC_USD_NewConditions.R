library(dplyr)
library(plyr)
library(zoo)
library(forecast)
library(lubridate)
library(data.table)

setwd("C:/btc/Orderbook/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_ob_20170810_20180211.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/NEOBTC")
NEOBTC <- read.csv("BITF_NEOBTC_ob_20171025_20180208.csv", header = TRUE)

setwd("C:/btc/Orderbook/Bitfinex/NEOUSD")
NEOUSD <- read.csv("BITF_NEOUSD_ob_20171025_20180208.csv", header = TRUE)

BTCUSD$date <- as.POSIXct(BTCUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
BTCUSD$day <- as.Date(BTCUSD$date, 'GMT')
BTCUSD$H <- hour(BTCUSD$date)
BTCUSD$M <- minute(BTCUSD$date)
NEOBTC$date <- as.POSIXct(NEOBTC$date %/% 1000, origin="1970-01-01", tz="GMT")
NEOBTC$day <- as.Date(NEOBTC$date, 'GMT')
NEOBTC$H <- hour(NEOBTC$date)
NEOBTC$M <- minute(NEOBTC$date)
NEOUSD$date <- as.POSIXct(NEOUSD$date %/% 1000, origin="1970-01-01", tz="GMT")
NEOUSD$day <- as.Date(NEOUSD$date, 'GMT')
NEOUSD$H <- hour(NEOUSD$date)
NEOUSD$M <- minute(NEOUSD$date)
BTCUSD <- data.table(BTCUSD)
NEOBTC <- data.table(NEOBTC)
NEOUSD <- data.table(NEOUSD)
require(data.table)
NEO <- BTCUSD[NEOBTC[NEOUSD, mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L],
              mult = "first", on = c("day","H", "M", "Index"), nomatch = 0L]
NEO <- NEO[,-c(7:10,15)]
colnames(NEO) <- c("Date","Index","BTC/USD_bid","BTC/USD_ask","BTC/USD_am_bid",
                   "BTC/USD_am_ask","NEO/BTC_bid","NEO/BTC_ask","NEO/BTC_am_bid",
                   "NEO/BTC_am_ask","NEO/USD_bid","NEO/USD_ask","NEO/USD_am_bid",
                   "NEO/USD_am_ask")
NEO$Date <- as.numeric(NEO$Date)
NEO <- na.locf(NEO)
NEO$"NEO/BTC/USD_bid" <- NEO$"BTC/USD_bid" * NEO$"NEO/BTC_bid"
NEO$"NEO/BTC/USD_ask" <- NEO$"BTC/USD_ask" * NEO$"NEO/BTC_ask"
NEO$S1 <- round((NEO$"NEO/USD_bid" / NEO$"NEO/BTC/USD_ask" - 1) * 100, 2)
NEO$S2 <- round((NEO$"NEO/USD_ask" / NEO$"NEO/BTC/USD_bid" - 1) * 100, 2)
NEO <- NEO[order(NEO[,1],NEO[,2]),]
NEO <- unique(NEO, by=c("Date", "Index"))
NEO <- NEO[!duplicated(NEO),]

head(NEO,10)
str(NEO)

m <- data.matrix(NEO)
rm(BTCUSD,NEOBTC,NEOUSD,NEO)
gc()
cat("\014")

cond_open <- -0.8
cond_close <- 0.4
stop_loss <- -100.0
t1 <- Sys.time()
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
leverage <- 2
PL <- 0
commission <- 0
balance_avail <- balance_usd * leverage
balance_used <- 0
deal <- 0
profit_sum <- c()
profit_perc <- c()
duration <- c()
step_entry <- 0.2
step_continue <- 0.35
comm <- c()
scaling <- 1.4

for(i in seq(11, nrow(m), by = 5)){
  if(trade == 0){
    if(TRUE){
      if(((m[i,16]/m[i,11])-1)*100 < cond_open){
        trade <- 1
        trade_positive_difference <- 1
        
        #SELL NEO/USD && BUY NEO/BTC/USD
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_NEOBTC <- deal / m[i,16]
        balance_NEOUSD <- -deal / m[i,11]
        # balance_btc <- deal / m[i,3]
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_NEOUSD <- m[i,11]
        avg_price_NEOBTC <- m[i,8]
        
        start <- m[i,1]
        s <- (m[i,16]/m[i,11]-1)*100
        cat("START. SELL NEO/USD && BUY NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
            "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
        # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
        #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
        }
      else {
        if(((m[i,12]/m[i,15])-1)*100 < cond_open){
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/NEO && SELL USD/BTC/NEO
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_NEOBTC <- -deal / m[i,15]
          balance_NEOUSD <- deal / m[i,12]
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_NEOUSD <- m[i,12]
          avg_price_NEOBTC <- m[i,7]
          
          start <- m[i,1]
          s <- (m[i,12]/m[i,15]-1)*100
          cat("START. BUY NEO/USD && SELL NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          cat("NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
          # cat("Balance BTC/NEO:", format(round(balance_NEOBTC,2), nsmall = 2),
          #     "Balance USD/NEO:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if((m[i,16]/m[i,11]-1)*100 < s - scaling && trade_positive_difference == 1 && balance_avail > 100){
      
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
      temp_NEOBTC <- balance_NEOBTC
      balance_NEOBTC <- temp_NEOBTC + deal / m[i,16]
      temp_NEOUSD <- balance_NEOUSD
      balance_NEOUSD <- temp_NEOUSD - deal / m[i,11]
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
      temp_price_NEOBTC <- avg_price_NEOBTC
      avg_price_NEOBTC <- abs((temp_price_NEOBTC*temp_NEOBTC + deal/m[i,16]*m[i,8]) /
                                (temp_NEOBTC + deal/m[i,16]))
      
      s <- (m[i,16]/m[i,11]-1)*100
      
      cat("SCALING. NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
          "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
      cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
          "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
    }
    else{
      if((m[i,12]/m[i,15]-1)*100 < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
        
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
        temp_NEOBTC <- balance_NEOBTC
        balance_NEOBTC <- temp_NEOBTC - deal / m[i,15]
        temp_NEOUSD <- balance_NEOUSD
        balance_NEOUSD <- temp_NEOUSD + deal / m[i,12]
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
        temp_price_NEOBTC <- avg_price_NEOBTC
        avg_price_NEOBTC <- abs((temp_price_NEOBTC*temp_NEOBTC + deal/m[i,16]*m[i,8]) /
                                  (temp_NEOBTC + deal/m[i,16]))
        
        s <- (m[i,12]/m[i,15]-1)*100
        
        cat("SCALING. NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
            "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
        cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
      }
      else{
        PL_NEOBTC <- (m[i,7] - avg_price_NEOBTC)*balance_NEOBTC*m[i,3]
        PL_NEOUSD <- (m[i,12] - avg_price_NEOUSD)*balance_NEOUSD
        
        if(((PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100>cond_close
            || (PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100<stop_loss
            || i==nrow(m)-4) && trade_positive_difference == 1){
          #Close positions:
          ##BUY NEO/USD && SELL NEO/BTC/USD
          k <- i
          temp_commission <- commission
          commission <- temp_commission + abs(balance_NEOBTC * m[i,15] * 0.002) +
            abs(balance_NEOUSD * m[i,12] * 0.002)
          temp_commission <- abs(balance_NEOBTC * m[i,15] * 0.002) + 
            abs(balance_NEOUSD * m[i,12] * 0.002)
          PL_NEOBTC <- (m[i,7] - avg_price_NEOBTC)*balance_NEOBTC*m[i,3]
          PL_NEOUSD <- (m[i,12] - avg_price_NEOUSD)*balance_NEOUSD
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_btcNEO <- 0
            PL_usdNEO <- 0
          }
          PL <- PL_NEOUSD + PL_NEOBTC
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_NEOBTC * m[i,15] * 0.002) +
                                            abs(balance_NEOUSD * m[i,12] * 0.002))
          balance_NEOBTC <- 0
          balance_NEOUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage

          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_NEOBTC * m[i,15] * 0.002) +
                      abs(balance_NEOUSD * m[i,12] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),"\n")
          cat("NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
              "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
          cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
              "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
        
        }
        else {
          PL_NEOBTC <- (m[i,8] - avg_price_NEOBTC)*balance_NEOBTC*m[i,4]
          PL_NEOUSD <- (m[i,11] - avg_price_NEOUSD)*balance_NEOUSD
          
          if(((PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100>cond_close
              || (PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100<stop_loss
              || i == nrow(m)-4) && trade_negative_difference == 1){
          #Close positions:
          ##SELL NEO/USD && BUY NEO/BTC/USD
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_NEOBTC * m[i,16] * 0.002) +
            abs(balance_NEOUSD * m[i,11] * 0.002)
          temp_commission <- abs(balance_NEOBTC * m[i,15] * 0.002) +
            abs(balance_NEOUSD * m[i,4] * 0.002)
          PL_NEOBTC <- (m[i,8] - avg_price_NEOBTC)*balance_NEOBTC*m[i,4]
          PL_NEOUSD <- (m[i,11] - avg_price_NEOUSD)*balance_NEOUSD
          #if last row and then we close position and st PL=0
          if(i == nrow(m)-4){
            PL_NEOBTC <- 0
            PL_NEOUSD <- 0
          }
          PL <- PL_NEOUSD + PL_NEOBTC
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_NEOBTC * m[i,16] * 0.002) +
                                            abs(balance_NEOUSD * m[i,11] * 0.002))
          balance_NEOBTC <- 0
          balance_NEOUSD <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_NEOBTC * m[i,16] * 0.002) +
                      abs(balance_NEOUSD * m[i,11] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),"\n")
          cat("NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
              "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
              "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
              "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
              "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
          cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
              "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
          
        }}
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
    cat("Positive deals:",sum(profit_sum>0),"\n")
    cat("Negative deals:",sum(profit_sum<0),"\n")
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

res <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("cond_open", "cond_close", "profit", "deals")
colnames(resH) <- x
for(cond_open in seq(-2.0,2.0,by=0.2)){
  for(cond_close in seq(0.1,3.0,by=0.1)){
    
    t1 <- Sys.time()
    trade <- 0
    trade_positive_difference <- 0
    trade_negative_difference <- 0
    start <- 0
    end <- 0
    s <- 0
    start_balance <- 4000
    balance_usd <- 4000
    balance_btcNEO <- 0
    balance_usdNEO <- 0
    # balance_btc <- 0
    # balance_NEO <- 0
    leverage <- 2
    PL <- 0
    commission <- 0
    balance_avail <- balance_usd * leverage
    balance_used <- 0
    deal <- 0
    profit_sum <- c()
    profit_perc <- c()
    duration <- c()
    step_entry <- 0.2
    step_continue <- 0.35
    comm <- c()
    scaling <- 1.4
    
    for(i in seq(11, nrow(m), by = 5)){
      if(trade == 0){
        if(TRUE){
          if(((m[i,16]/m[i,11])-1)*100 < cond_open){
            trade <- 1
            trade_positive_difference <- 1
            
            #SELL NEO/USD && BUY NEO/BTC/USD
            deal <- balance_avail * step_entry
            commission <- deal * 0.002 * 2
            start_balance <- balance_usd
            balance_usd <- start_balance - commission
            balance_used <- deal * 2
            balance_avail <- balance_usd * leverage - balance_used
            balance_NEOBTC <- deal / m[i,16]
            balance_NEOUSD <- -deal / m[i,11]
            # balance_btc <- deal / m[i,3]
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_NEOUSD <- m[i,11]
            avg_price_NEOBTC <- m[i,8]
            
            start <- m[i,1]
            s <- (m[i,16]/m[i,11]-1)*100
            # cat("START. SELL NEO/USD && BUY NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
            # cat("NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
            #     "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
            #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
            #     "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2), "\n")
            # # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            # #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
          }
          else {
            if(((m[i,12]/m[i,15])-1)*100 < cond_open){
              trade <- 1
              trade_negative_difference <- 1
              
              #BUY USD/NEO && SELL USD/BTC/NEO
              deal <- balance_avail * step_entry
              commission <- deal * 0.002 * 2
              start_balance <- balance_usd
              balance_usd <- start_balance - commission
              balance_used <- deal * 2
              balance_avail <- balance_usd * leverage - balance_used
              balance_NEOBTC <- -deal / m[i,15]
              balance_NEOUSD <- deal / m[i,12]
              comm <- c(comm, deal * 0.002 * 2)
              avg_price_NEOUSD <- m[i,12]
              avg_price_NEOBTC <- m[i,7]
              
              start <- m[i,1]
              s <- (m[i,12]/m[i,15]-1)*100
              # cat("START. BUY NEO/USD && SELL NEO/BTC | DEAL=",format(round(deal,2), nsmall = 2),"\n")
              # cat("NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              #     "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
              # # cat("Balance BTC/NEO:", format(round(balance_NEOBTC,2), nsmall = 2),
              # #     "Balance USD/NEO:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
            }
          }
        }
        else next
      }
      else{
        if((m[i,16]/m[i,11]-1)*100 < s - scaling && trade_positive_difference == 1 && balance_avail > 100){
          
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
          temp_NEOBTC <- balance_NEOBTC
          balance_NEOBTC <- temp_NEOBTC + deal / m[i,16]
          temp_NEOUSD <- balance_NEOUSD
          balance_NEOUSD <- temp_NEOUSD - deal / m[i,11]
          temp_used <- balance_used
          balance_used <- temp_used + deal * 2
          temp_avail <- balance_avail
          balance_avail <- balance_usd * leverage - balance_used
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
          temp_price_NEOBTC <- avg_price_NEOBTC
          avg_price_NEOBTC <- abs((temp_price_NEOBTC*temp_NEOBTC + deal/m[i,16]*m[i,8]) /
                                    (temp_NEOBTC + deal/m[i,16]))
          
          s <- (m[i,16]/m[i,11]-1)*100
          
          # cat("SCALING. NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
          #     "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
          #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
          #     "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),"\n")
          # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
          #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
        }
        else{
          if((m[i,12]/m[i,15]-1)*100 < s - scaling && trade_negative_difference == 1 && balance_avail > 100){
            
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
            temp_NEOBTC <- balance_NEOBTC
            balance_NEOBTC <- temp_NEOBTC - deal / m[i,15]
            temp_NEOUSD <- balance_NEOUSD
            balance_NEOUSD <- temp_NEOUSD + deal / m[i,12]
            temp_used <- balance_used
            balance_used <- temp_used + deal * 2
            temp_avail <- balance_avail
            balance_avail <- balance_usd * leverage - balance_used
            comm <- c(comm, deal * 0.002 * 2)
            avg_price_NEOUSD <- (balance_used / 2) / abs(balance_NEOUSD)
            temp_price_NEOBTC <- avg_price_NEOBTC
            avg_price_NEOBTC <- abs((temp_price_NEOBTC*temp_NEOBTC + deal/m[i,16]*m[i,8]) /
                                      (temp_NEOBTC + deal/m[i,16]))
            
            s <- (m[i,12]/m[i,15]-1)*100
            
            # cat("SCALING. NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
            #     "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
            #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
            #     "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),"\n")
            # cat("Balance NEO/BTC:", format(round(balance_NEOBTC,2), nsmall = 2),
            #     "Balance NEO/USD:", format(round(balance_NEOUSD,2), nsmall = 2),"\n")
          }
          else{
            PL_NEOBTC <- (m[i,7] - avg_price_NEOBTC)*balance_NEOBTC*m[i,3]
            PL_NEOUSD <- (m[i,12] - avg_price_NEOUSD)*balance_NEOUSD
            
            if(((PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100>cond_close
                || (PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100<stop_loss
                || i==nrow(m)-4) && trade_positive_difference == 1){
              #Close positions:
              ##BUY NEO/USD && SELL NEO/BTC/USD
              k <- i
              temp_commission <- commission
              commission <- temp_commission + abs(balance_NEOBTC * m[i,15] * 0.002) +
                abs(balance_NEOUSD * m[i,12] * 0.002)
              temp_commission <- abs(balance_NEOBTC * m[i,15] * 0.002) + 
                abs(balance_NEOUSD * m[i,12] * 0.002)
              PL_NEOBTC <- (m[i,7] - avg_price_NEOBTC)*balance_NEOBTC*m[i,3]
              PL_NEOUSD <- (m[i,12] - avg_price_NEOUSD)*balance_NEOUSD
              #if last row and then we close position and st PL=0
              if(i == nrow(m)-4){
                PL_btcNEO <- 0
                PL_usdNEO <- 0
              }
              PL <- PL_NEOUSD + PL_NEOBTC
              temp_usd <- balance_usd
              balance_usd <- temp_usd + PL - (abs(balance_NEOBTC * m[i,15] * 0.002) +
                                                abs(balance_NEOUSD * m[i,12] * 0.002))
              balance_NEOBTC <- 0
              balance_NEOUSD <- 0
              balance_used <- 0
              balance_avail <- balance_usd * leverage
              
              profit_sum <- c(profit_sum, PL - commission)
              profit_perc <- c(profit_perc, (PL - commission)/start_balance)
              comm <- c(comm, abs(balance_NEOBTC * m[i,15] * 0.002) +
                          abs(balance_NEOUSD * m[i,12] * 0.002))
              
              end <- m[i,1]
              duration <- c(duration, (end - start)/3600)
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
              # cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
              #     "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),"\n")
              # cat("NEO/USD ask:", format(round(m[i,12],2), nsmall = 2),
              #     "NEO/BTC bid:", format(round(m[i,7],6), nsmall = 6),
              #     "BTC/USD bid:", format(round(m[i,3],0), nsmall = 0),
              #     "NEO/BTC/USD bid:", format(round(m[i,15],2), nsmall = 2),
              #     "Difference:", format(round(m[i,18],2), nsmall = 2), "%\n")
              # cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
              #     "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
              
            }
            else {
              PL_NEOBTC <- (m[i,8] - avg_price_NEOBTC)*balance_NEOBTC*m[i,4]
              PL_NEOUSD <- (m[i,11] - avg_price_NEOUSD)*balance_NEOUSD
              
              if(((PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100>cond_close
                  || (PL_NEOBTC+PL_NEOUSD - commission)/start_balance*100<stop_loss
                  || i == nrow(m)-4) && trade_negative_difference == 1){
                #Close positions:
                ##SELL NEO/USD && BUY NEO/BTC/USD
                
                temp_commission <- commission
                commission <- temp_commission + abs(balance_NEOBTC * m[i,16] * 0.002) +
                  abs(balance_NEOUSD * m[i,11] * 0.002)
                temp_commission <- abs(balance_NEOBTC * m[i,15] * 0.002) +
                  abs(balance_NEOUSD * m[i,4] * 0.002)
                PL_NEOBTC <- (m[i,8] - avg_price_NEOBTC)*balance_NEOBTC*m[i,4]
                PL_NEOUSD <- (m[i,11] - avg_price_NEOUSD)*balance_NEOUSD
                #if last row and then we close position and st PL=0
                if(i == nrow(m)-4){
                  PL_NEOBTC <- 0
                  PL_NEOUSD <- 0
                }
                PL <- PL_NEOUSD + PL_NEOBTC
                temp_usd <- balance_usd
                balance_usd <- temp_usd + PL - (abs(balance_NEOBTC * m[i,16] * 0.002) +
                                                  abs(balance_NEOUSD * m[i,11] * 0.002))
                balance_NEOBTC <- 0
                balance_NEOUSD <- 0
                balance_used <- 0
                balance_avail <- balance_usd * leverage
                
                profit_sum <- c(profit_sum, PL - commission)
                profit_perc <- c(profit_perc, (PL - commission)/start_balance)
                comm <- c(comm, abs(balance_NEOBTC * m[i,16] * 0.002) +
                            abs(balance_NEOUSD * m[i,11] * 0.002))
                
                end <- m[i,1]
                duration <- c(duration, (end - start)/3600)
                
                # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
                #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
                #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
                #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
                # cat("Avg price NEO/USD:",format(round(avg_price_NEOUSD,2), nsmall = 2),
                #     "Avg price NEO/BTC:",format(round(avg_price_NEOBTC,6), nsmall = 6),"\n")
                # cat("NEO/USD bid:", format(round(m[i,11],2), nsmall = 2),
                #     "NEO/BTC ask:", format(round(m[i,8],6), nsmall = 6),
                #     "BTC/USD ask:", format(round(m[i,4],0), nsmall = 0),
                #     "NEO/BTC/USD ask:", format(round(m[i,16],2), nsmall = 2),
                #     "Difference:", format(round(m[i,17],2), nsmall = 2), "%\n")
                # cat("PL NEO/USD=",format(round(PL_NEOUSD,0), nsmall = 0),
                #     "PL NEO/BTC=",format(round(PL_NEOBTC,0), nsmall = 0),
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
                
              }}
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
        # cat("Positive deals:",sum(profit_sum>0),"\n")
        # cat("Negative deals:",sum(profit_sum<0),"\n")
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
    t2-t1
    cat("open=",cond_open,"close=",cond_close,"Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),
        "% Total deals:",length(profit_sum),"time difference of",t2-t1,"secs\n")
    df <- data.frame(cond_open,cond_close,format(round((balance_usd/4000-1)*100,2), nsmall = 2),length(profit_sum))
    x <- c("cond_open", "cond_close", "profit", "deals")
    colnames(df) <- x
    res <- rbind(res,df)
  }
}
setwd("C:/btc/Strategy/Arbitration/Bitfinex_NEO_BTC_USD")
write.csv(res, "NEO_cond.csv", row.names = FALSE)



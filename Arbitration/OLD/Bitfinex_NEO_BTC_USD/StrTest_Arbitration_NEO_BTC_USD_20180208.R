library(dplyr)
library(plyr)
library(zoo)
library(forecast)
library(foreach)
# library(doParallel)
# registerDoParallel(6)
# getDoParWorkers()
# 
# system.time(foreach(i=1:10000) %do% sum(tanh(1:i)))
# 
# system.time(foreach(i=1:10000) %dopar% sum(tanh(1:i)))

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_20170822_20180125.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/NEOBTC")
NEOBTC <- read.csv("BITF_NEOBTC_20171008_20180128.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/NEOUSD")
NEOUSD <- read.csv("BITF_NEOUSD_20171008_20180125.csv", header = TRUE)

mn <- max(min(BTCUSD$Timestamp), min(NEOBTC$Timestamp), min(NEOUSD$Timestamp))
mx <- min(max(BTCUSD$Timestamp), max(NEOBTC$Timestamp), max(NEOUSD$Timestamp))
ts <- data.frame("Date" = mn:mx)

BTCUSD$Timestamp <- as.POSIXct(BTCUSD$Timestamp, origin="1970-01-01", tz="GMT")
NEOBTC$Timestamp <- as.POSIXct(NEOBTC$Timestamp, origin="1970-01-01", tz="GMT")
NEOUSD$Timestamp <- as.POSIXct(NEOUSD$Timestamp, origin="1970-01-01", tz="GMT")
ts$Date <- as.POSIXct(ts$Date, origin="1970-01-01", tz="GMT")

NEO <- merge(x = ts, y = BTCUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
NEO <- merge(x = NEO, y = NEOBTC, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
NEO <- merge(x = NEO, y = NEOUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)

sl <- 1.01

colnames(NEO) <- c("Date", "USD/BTC", "BTC/NEO", "USD/NEO")
NEO$Date <- as.numeric(NEO$Date)
NEO <- na.locf(NEO)
NEO <- NEO[-c(1:100),]
NEO$"USD/BTC/NEO" <- NEO$"USD/BTC" * NEO$"BTC/NEO"
NEO[is.na(NEO)] <- 0
NEO$"USD/NEO_ask" <- NEO$`USD/NEO` / sl
NEO$"USD/NEO_bid" <- NEO$`USD/NEO` * sl
NEO$"USD/BTC/NEO_ask" <- NEO$`USD/BTC/NEO` / sl
NEO$"USD/BTC/NEO_bid" <- NEO$`USD/BTC/NEO` * sl
NEO$S1 <- round((NEO$"USD/NEO_ask" / NEO$"USD/BTC/NEO_bid" - 1) * 100, 2)
NEO$S2 <- round((NEO$"USD/NEO_bid" / NEO$"USD/BTC/NEO_ask" - 1) * 100, 2)

head(NEO,10)
str(NEO)

m <- data.matrix(NEO)
rm(BTCUSD,NEOBTC,NEOUSD,mn,mx,ts,NEO,sl)
gc()
.rs.restartR()
cat("\014")

hb <- 2.4#2.4
hn <- -6.0#-6.0
lb <- -4.0#-6.0
ln <- -2.0#0.9

# resl <- data.frame(matrix(ncol = 3, nrow = 0))
# x <- c("lb", "ln", "profit")
# colnames(res) <- x
# for(lb in seq(-6.0,-3.0,by=0.2)){
#   for(ln in seq(-1.0,2.0,by=0.2)){
#     #ln <- lb + d

# t1 <- Sys.time()

trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_btcneo <- 0
balance_usdneo <- 0
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
orderbook <- 1.005

# condition1 <- min(m[,10],m[-1,10],m[-2,10]) > hb
# condition2 <- max(m[i,11],m[i-1,11],m[i-2,11]) < lb
# condition3 <- m[i,10] > s * scaling && trade_positive_difference == 1 && balance_avail > 100
# condition4 <- m[i,11] < s * scaling && trade_negative_difference == 1 && balance_avail > 100
# condition5 <- m[i,10] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1
# condition6 <- m[i,11] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1

for(i in 3:nrow(m)) {
  if(trade == 0){
    if(TRUE){
      if(min(m[i,10],m[i-1,10],m[i-2,10]) > hb){
        trade <- 1
        trade_positive_difference <- 1
        
        #SELL USD/NEO && BUY USD/BTC/NEO
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcneo <- deal / (m[i,9] * orderbook)
        balance_usdneo <- -deal / (m[i,6] / orderbook)
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdneo <- (balance_used / 2) / abs(balance_usdneo)
        avg_price_btcneo <- (balance_used / 2) / abs(balance_btcneo)
        
        start <- m[i,1]
        s <- m[i,10]
        # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
        #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
        #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
        #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
        #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
        #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
        #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
        # cat("SELL USD/NEO && BUY BTC/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
        #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
        #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
        #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
        # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
        #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      else {
        if(max(m[i,11],m[i-1,11],m[i-2,11]) < lb){
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/NEO && SELL USD/BTC/NEO
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcneo <- -deal / (m[i,8] / orderbook)
          balance_usdneo <- deal / (m[i,7] * orderbook)
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdneo <- (balance_used / 2) / abs(balance_usdneo)
          avg_price_btcneo <- (balance_used / 2) / abs(balance_btcneo)
          
          start <- m[i,1]
          s <- m[i,11]
          # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
          #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          # cat("BUY USD/NEO && SELL BTC/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
          #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
          #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
          # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if(m[i,10] > s * scaling && trade_positive_difference == 1 && balance_avail > 100){
      
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
      temp_btcneo <- balance_btcneo
      balance_btcneo <- temp_btcneo + deal / (m[i,9] * orderbook)
      temp_usdneo <- balance_usdneo
      balance_usdneo <- temp_usdneo - deal / (m[i,6] / orderbook)
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_usdneo <- (balance_used / 2) / abs(balance_usdneo)
      avg_price_btcneo <- (balance_used / 2) / abs(balance_btcneo)
      
      s <- m[i,10]
      
      # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
      #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
      #     "Balance BTC/NEO:",format(round(temp_btcneo,2), nsmall = 2),
      #     "Balance USD/NEO:",format(round(temp_usdneo,2), nsmall = 2),"\n")
      # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
      #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
      #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
      #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
      #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
      # cat("SELL USD/NEO && BUY BTC/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
      #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
      #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
      #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
      # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
      #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
    }
    else{
      if(m[i,11] < s * scaling && trade_negative_difference == 1 && balance_avail > 100){
        
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
        temp_btcneo <- balance_btcneo
        balance_btcneo <- temp_btcneo - deal / (m[i,8] / orderbook)
        temp_usdneo <- balance_usdneo
        balance_usdneo <- temp_usdneo + deal / (m[i,7] * orderbook)
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdneo <- (balance_used / 2) / abs(balance_usdneo)
        avg_price_btcneo <- (balance_used / 2) / abs(balance_btcneo)
        
        s <- m[i,11]
        
        # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
        #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
        #     "Balance BTC/NEO:",format(round(temp_btcneo,2), nsmall = 2),
        #     "Balance USD/NEO:",format(round(temp_usdneo,2), nsmall = 2),"\n")
        # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
        #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
        #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
        #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
        #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
        # cat("BUY USD/NEO && SELL BTC/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
        #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
        #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
        #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
        # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
        #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,10] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){
          #Close positions:
          ##BUY USD/NEO && SELL USD/BTC/NEO
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcneo * m[i,8] * 0.002) +
            abs(balance_usdneo * m[i,7] * 0.002)
          temp_commission <- abs(balance_btcneo * m[i,8] * 0.002) + 
            abs(balance_usdneo * m[i,7] * 0.002)
          PL_btcneo <- (m[i,8] / orderbook - avg_price_btcneo)*balance_btcneo
          PL_usdneo <- (m[i,7] * orderbook - avg_price_usdneo)*balance_usdneo
          PL <- PL_usdneo + PL_btcneo
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcneo * m[i,8] * 0.002) +
                                            abs(balance_usdneo * m[i,7] * 0.002))
          balance_btcneo <- 0
          balance_usdneo <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcneo * m[i,8] * 0.002) +
                      abs(balance_usdneo * m[i,7] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
          #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
          #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
          #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
          #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          # cat("PL USD/NEO=",format(round(PL_usdneo,0), nsmall = 0),
          #     "PL BTC/NEO=",format(round(PL_btcneo,0), nsmall = 0),
          #     "PL=", format(round(PL,0), nsmall = 0),
          #     "Total commission:", format(round(commission,0), nsmall = 0),
          #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
          #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcneo <- 0
          temp_usdneo <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcneo <- 0
          PL_usdneo <- 0
        
        }
        else if(m[i,11] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
          #Close positions:
          ##SELL USD/NEO && BUY USD/BTC/NEO
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcneo * m[i,9] * 0.002) +
            abs(balance_usdneo * m[i,6] * 0.002)
          temp_commission <- abs(balance_btcneo * m[i,8] * 0.002) + 
            abs(balance_usdneo * m[i,7] * 0.002)
          PL_btcneo <- (m[i,9] / orderbook - avg_price_btcneo)*balance_btcneo
          PL_usdneo <- (m[i,6] * orderbook - avg_price_usdneo)*balance_usdneo
          PL <- PL_usdneo + PL_btcneo
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcneo * m[i,9] * 0.002) +
                                            abs(balance_usdneo * m[i,6] * 0.002))
          balance_btcneo <- 0
          balance_usdneo <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcneo * m[i,9] * 0.002) +
                      abs(balance_usdneo * m[i,6] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
          #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
          #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          # cat("USD/NEO ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/NEO bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/NEO ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/NEO bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
          #     "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
          #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          # cat("PL USD/NEO=",format(round(PL_usdneo,0), nsmall = 0),
          #     "PL BTC/NEO=",format(round(PL_btcneo,0), nsmall = 0),
          #     "PL=", format(round(PL,0), nsmall = 0),
          #     "Total commission:", format(round(commission,0), nsmall = 0),
          #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
          #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcneo <- 0
          temp_usdneo <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcneo <- 0
          PL_usdneo <- 0
          
        }
      }
    }
  }
  if (i == nrow(m)){
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

# t2 <- Sys.time()
# cat("lb=",lb,"ln=",ln,"Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%","time difference of",t2-t1,"mins\n")
# df <- data.frame(hb,hn,format(round((balance_usd/4000-1)*100,2), nsmall = 2))
# x <- c("lb", "ln", "profit")
# colnames(df) <- x
# resl <- rbind(resl,df)
#   }
# }

# cat("\014")
# .rs.restartR()
# ls()
# rm(list = ls())
# gc()
# memory.size(max = F)

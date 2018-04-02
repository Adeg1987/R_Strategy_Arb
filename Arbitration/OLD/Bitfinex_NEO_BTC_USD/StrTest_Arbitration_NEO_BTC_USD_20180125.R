library(dplyr)
library(plyr)
library(zoo)
library(forecast)

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

BITF <- NULL
OKFT <- NULL
ts <- NULL

colnames(NEO) <- c("Date", "USD/BTC", "BTC/NEO", "USD/NEO")
NEO$Date <- as.numeric(NEO$Date)
NEO <- na.locf(NEO)
NEO <- NEO[-c(1:100),]
NEO$"USD/BTC/NEO" <- NEO$"USD/BTC" * NEO$"BTC/NEO"
NEO$DIFF <- round((NEO$"USD/NEO" / NEO$"USD/BTC/NEO" - 1) * 100, 2)
NEO[is.na(NEO)] <- 0

head(NEO,10)
str(NEO)

m <- data.matrix(NEO)

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
hb <- 4
hn <- 0.5
lb <- -4
ln <- -0.5
sl <- 1.01
comm <- c()

for(i in 3:nrow(m)){
  if(trade == 0){
    if(TRUE){
      if(min(m[i,6],m[i-1,6],m[i-2,6]) > hb){
        trade <- 1
        trade_positive_difference <- 1
        
        #BUY USD/BTC/NEO && SELL USD/NEO
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcneo <- deal / (m[i,5]*sl)
        balance_usdneo <- -deal / (m[i,4]/sl)
        comm <- c(comm, deal * 0.002 * 2)
        
        start <- m[i,1]
        s <- m[i,6]
        cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Equity USD:",format(round(start_balance,2), nsmall = 2),
            "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
            "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
            "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
            "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
            "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
        cat("BUY BTC/NEO && SELL USD/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
            "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
            "Commission:", format(round(commission,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      else {
        if(max(m[i,6],m[i-1,6],m[i-2,6]) < lb){
          trade <- 1
          trade_negative_difference <- 1
          
          #SELL USD/BTC/NEO && BUY USD/NEO
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcneo <- -deal / (m[i,5]/sl)
          balance_usdneo <- deal / (m[i,4]*sl)
          comm <- c(comm, deal * 0.002 * 2)
          
          start <- m[i,1]
          s <- m[i,6]
          cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              "Equity USD:",format(round(start_balance,2), nsmall = 2),
              "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
              "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
              "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
              "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
              "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
          cat("SELL BTC/NEO && BUY USD/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
              "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
              "Commission:", format(round(commission,2), nsmall = 2),"\n")
          cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if(m[i,6] > s * 1.096 && trade_positive_difference == 1 && balance_avail > 100){
      
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
      balance_btcneo <- temp_btcneo + deal / (m[i,5]*sl)
      temp_usdneo <- balance_usdneo
      balance_usdneo <- temp_usdneo - deal / (m[i,4]/sl)
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      
      s <- m[i,6]
      
      cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          "Balance BTC/NEO:",format(round(temp_btcneo,2), nsmall = 2),
          "Balance USD/NEO:",format(round(temp_usdneo,2), nsmall = 2),"\n")
      cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
          "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
          "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
          "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
          "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
      cat("BUY BTC/NEO && SELL USD/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
          "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
          "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
      cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
    }
    else{
      if(m[i,6] < s * 1.096 && trade_negative_difference == 1 && balance_avail > 100){
        
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
        balance_btcneo <- temp_btcneo - deal / (m[i,5]/sl)
        temp_usdneo <- balance_usdneo
        balance_usdneo <- temp_usdneo + deal / (m[i,4]*sl)
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        
        s <- m[i,6]
        
        cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            "Balance BTC/NEO:",format(round(temp_btcneo,2), nsmall = 2),
            "Balance USD/NEO:",format(round(temp_usdneo,2), nsmall = 2),"\n")
        cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
            "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
            "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
            "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
            "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
        cat("SELL BTC/NEO && BUY USD/NEO | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
            "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
            "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,6] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){
          #Close positions:
          ##SELL USD/BTC/NEO && BUY USD/NEO
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcneo * m[i,5] * 0.002) +
            abs(balance_usdneo * m[i,4] * 0.002)
          PL <- balance_btcneo * (m[i,5]/sl) + balance_usdneo * (m[i,4]*sl)
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcneo * m[i,5] * 0.002) +
                                            abs(balance_usdneo * m[i,4] * 0.002))
          balance_btcneo <- 0
          balance_usdneo <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage

          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcneo * m[i,5] * 0.002) +
                      abs(balance_usdneo * m[i,4] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
              "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
              "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
              "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
              "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
              "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("Profit/Loss:", format(round(PL,2), nsmall = 2),
              "Total commission:", format(round(commission,2), nsmall = 2),
              "Net profit:", format(round(PL - commission,2), nsmall = 2),
              "Net Profit/Loss %:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
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
        
        }
        else if(m[i,6] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
          #Close positions:
          ##BUY USD/BTC/NEO && SELL USD/NEO
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcneo * m[i,5] * 0.002) +
            abs(balance_usdneo * m[i,4] * 0.002)
          PL <- balance_btcneo * (m[i,5]*sl) + balance_usdneo * (m[i,4]/sl)
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcneo * m[i,5] * 0.002) +
                                            abs(balance_usdneo * m[i,4] * 0.002))
          balance_btcneo <- 0
          balance_usdneo <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcneo * m[i,5] * 0.002) +
                      abs(balance_usdneo * m[i,4] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("PRICES - USD/NEO:", format(round(m[i,4],2), nsmall = 2),
              "USD/BTC:", format(round(m[i,2],2), nsmall = 2),
              "BTC/NEO:", format(round(m[i,3],6), nsmall = 6),
              "USD/BTC/NEO:", format(round(m[i,5],2), nsmall = 2),
              "Difference:", format(round(m[i,6],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/NEO:", format(round(balance_btcneo,2), nsmall = 2),
              "Balance USD/NEO:", format(round(balance_usdneo,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("Profit/Loss:", format(round(PL,2), nsmall = 2),
              "Total commission:", format(round(commission,2), nsmall = 2),
              "Net profit:", format(round(PL - commission,2), nsmall = 2),
              "Net Profit/Loss %:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
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


library(dplyr)
library(plyr)
library(zoo)
library(forecast)

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_20170822_20180125.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/XRPBTC")
XRPBTC <- read.csv("BITF_XRPBTC_20170531_20180207.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/XRPUSD")
XRPUSD <- read.csv("BITF_XRPUSD_20170715_20180131.csv", header = TRUE)

mn <- max(min(BTCUSD$Timestamp), min(XRPBTC$Timestamp), min(XRPUSD$Timestamp))
mx <- min(max(BTCUSD$Timestamp), max(XRPBTC$Timestamp), max(XRPUSD$Timestamp))
ts <- data.frame("Date" = mn:mx)

BTCUSD$Timestamp <- as.POSIXct(BTCUSD$Timestamp, origin="1970-01-01", tz="GMT")
XRPBTC$Timestamp <- as.POSIXct(XRPBTC$Timestamp, origin="1970-01-01", tz="GMT")
XRPUSD$Timestamp <- as.POSIXct(XRPUSD$Timestamp, origin="1970-01-01", tz="GMT")
ts$Date <- as.POSIXct(ts$Date, origin="1970-01-01", tz="GMT")

XRP <- merge(x = ts, y = BTCUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
XRP <- merge(x = XRP, y = XRPBTC, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
XRP <- merge(x = XRP, y = XRPUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)

sl <- 1.004

colnames(XRP) <- c("Date", "USD/BTC", "BTC/XRP", "USD/XRP")
XRP$Date <- as.numeric(XRP$Date)
XRP <- na.locf(XRP)
XRP <- XRP[-c(1:100),]
XRP$"USD/BTC/XRP" <- XRP$"USD/BTC" * XRP$"BTC/XRP"
XRP[is.na(XRP)] <- 0
XRP$"USD/XRP_ask" <- XRP$`USD/XRP` / sl
XRP$"USD/XRP_bid" <- XRP$`USD/XRP` * sl
XRP$"USD/BTC/XRP_ask" <- XRP$`USD/BTC/XRP` / sl
XRP$"USD/BTC/XRP_bid" <- XRP$`USD/BTC/XRP` * sl
XRP$S1 <- round((XRP$"USD/XRP_ask" / XRP$"USD/BTC/XRP_bid" - 1) * 100, 2)
XRP$S2 <- round((XRP$"USD/XRP_bid" / XRP$"USD/BTC/XRP_ask" - 1) * 100, 2)

head(XRP,10)
str(XRP)

m <- data.matrix(XRP)
rm(BTCUSD,XRPBTC,XRPUSD,mn,mx,ts,XRP,sl)
gc()
.rs.restartR()
cat("\014")

hb <- 4.5
hn <- 1.0
lb <- -4.5
ln <- -1.0

# for(lb in seq(-6.0,-3.4,by=0.2)){
#   for(d in seq(1.0,-lb,by=0.2)){
#     ln <- lb + d

t1 <- Sys.time()

trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
s <- 0
start_balance <- 4000
balance_usd <- 4000
balance_btcxrp <- 0
balance_usdxrp <- 0
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


for(i in 3:nrow(m)){
  if(trade == 0){
    if(TRUE){
      if(min(m[i,10],m[i-1,10],m[i-2,10]) > hb){
        trade <- 1
        trade_positive_difference <- 1
        
        #SELL USD/XRP && BUY USD/BTC/XRP
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcxrp <- deal / m[i,9]
        balance_usdxrp <- -deal / m[i,6]
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdxrp <- m[i,6]
        avg_price_btcxrp <- m[i,9]
        
        start <- m[i,1]
        s <- m[i,10]
        cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Equity USD:",format(round(start_balance,2), nsmall = 2),
            "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
            "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
            "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
            "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
            "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
        cat("SELL USD/XRP && BUY BTC/XRP | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
            "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
            "Commission:", format(round(commission,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      else {
        if(max(m[i,11],m[i-1,11],m[i-2,11]) < lb){
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/XRP && SELL USD/BTC/XRP
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcxrp <- -deal / m[i,8]
          balance_usdxrp <- deal / m[i,7]
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdxrp <- m[i,7]
          avg_price_btcxrp <- m[i,8]
          
          start <- m[i,1]
          s <- m[i,11]
          cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
              "Equity USD:",format(round(start_balance,2), nsmall = 2),
              "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
              "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
              "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
              "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
              "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          cat("BUY USD/XRP && SELL BTC/XRP | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
              "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
              "Commission:", format(round(commission,2), nsmall = 2),"\n")
          cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
              "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
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
      temp_btcxrp <- balance_btcxrp
      balance_btcxrp <- temp_btcxrp + deal / m[i,9]
      temp_usdxrp <- balance_usdxrp
      balance_usdxrp <- temp_usdxrp - deal / m[i,6]
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_usdxrp <- (balance_used / 2) / abs(balance_usdxrp)
      avg_price_btcxrp <- (balance_used / 2) / abs(balance_btcxrp)
      
      s <- m[i,10]
      
      cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          "Balance USD:",format(round(temp_usd,2), nsmall = 2),
          "Balance BTC/XRP:",format(round(temp_btcxrp,2), nsmall = 2),
          "Balance USD/XRP:",format(round(temp_usdxrp,2), nsmall = 2),"\n")
      cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
          "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
          "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
          "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
          "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
      cat("SELL USD/XRP && BUY BTC/XRP | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
          "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
          "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
      cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
          "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
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
        temp_btcxrp <- balance_btcxrp
        balance_btcxrp <- temp_btcxrp - deal / m[i,8]
        temp_usdxrp <- balance_usdxrp
        balance_usdxrp <- temp_usdxrp + deal / m[i,7]
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdxrp <- (balance_used / 2) / abs(balance_usdxrp)
        avg_price_btcxrp <- (balance_used / 2) / abs(balance_btcxrp)
        
        s <- m[i,11]
        
        cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
            "Balance USD:",format(round(temp_usd,2), nsmall = 2),
            "Balance BTC/XRP:",format(round(temp_btcxrp,2), nsmall = 2),
            "Balance USD/XRP:",format(round(temp_usdxrp,2), nsmall = 2),"\n")
        cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
            "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
            "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
            "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
            "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
        cat("BUY USD/XRP && SELL BTC/XRP | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
            "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
            "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
            "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
        cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
            "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,10] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){
          #Close positions:
          ##BUY USD/XRP && SELL USD/BTC/XRP
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcxrp * m[i,8] * 0.002) +
            abs(balance_usdxrp * m[i,7] * 0.002)
          temp_commission <- abs(balance_btcxrp * m[i,8] * 0.002) + 
            abs(balance_usdxrp * m[i,7] * 0.002)
          PL_btcxrp <- (m[i,8] - avg_price_btcxrp)*balance_btcxrp#balance_btcxrp * m[i,8]
          PL_usdxrp <- (m[i,7] - avg_price_usdxrp)*balance_usdxrp#balance_usdxrp * m[i,7]
          PL <- PL_usdxrp + PL_btcxrp#balance_btcxrp * m[i,8] + balance_usdxrp * m[i,7]
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcxrp * m[i,8] * 0.002) +
                                            abs(balance_usdxrp * m[i,7] * 0.002))
          balance_btcxrp <- 0
          balance_usdxrp <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage

          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcxrp * m[i,8] * 0.002) +
                      abs(balance_usdxrp * m[i,7] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
              "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
              "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
              "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
              "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
              "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL USD/XRP=",format(round(PL_usdxrp,0), nsmall = 0),
              "PL BTC/XRP=",format(round(PL_btcxrp,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcxrp <- 0
          temp_usdxrp <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcxrp <- 0
          PL_usdxrp <- 0
        
        }
        else if(m[i,11] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
          #Close positions:
          ##SELL USD/XRP && BUY USD/BTC/XRP
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcxrp * m[i,9] * 0.002) +
            abs(balance_usdxrp * m[i,6] * 0.002)
          temp_commission <- abs(balance_btcxrp * m[i,8] * 0.002) + 
            abs(balance_usdxrp * m[i,7] * 0.002)
          PL_btcxrp <- (m[i,9] - avg_price_btcxrp)*balance_btcxrp#balance_btcxrp * m[i,9]
          PL_usdxrp <- (m[i,6] - avg_price_usdxrp)*balance_usdxrp#balance_usdxrp * m[i,6]
          PL <- PL_usdxrp + PL_btcxrp#balance_btcxrp * m[i,9] + balance_usdxrp * m[i,6]
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcxrp * m[i,9] * 0.002) +
                                            abs(balance_usdxrp * m[i,6] * 0.002))
          balance_btcxrp <- 0
          balance_usdxrp <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcxrp * m[i,9] * 0.002) +
                      abs(balance_usdxrp * m[i,6] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
              "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
              "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          cat("USD/XRP ask:", format(round(m[i,6],2), nsmall = 2),
              "USD/XRP bid:", format(round(m[i,7],2), nsmall = 2),
              "| USD/BTC/XRP ask:", format(round(m[i,8],2), nsmall = 2),
              "USD/BTC/XRP bid:", format(round(m[i,9],2), nsmall = 2),
              "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
              "Balance BTC/XRP:", format(round(balance_btcxrp,2), nsmall = 2),
              "Balance USD/XRP:", format(round(balance_usdxrp,2), nsmall = 2),
              "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          cat("PL USD/XRP=",format(round(PL_usdxrp,0), nsmall = 0),
              "PL BTC/XRP=",format(round(PL_btcxrp,0), nsmall = 0),
              "PL=", format(round(PL,0), nsmall = 0),
              "Total commission:", format(round(commission,0), nsmall = 0),
              "Net profit:", format(round(PL - commission,0), nsmall = 0),
              "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcxrp <- 0
          temp_usdxrp <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcxrp <- 0
          PL_usdxrp <- 0
          
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

t2 <- Sys.time()
t2-t1
# cat("lb=",lb,"ln=",ln,"Total profit percent:", format(round((balance_usd/4000-1)*100,2), nsmall = 2),"%","time difference of",t2-t1,"secs\n")
#   }
# }

# cat("\014")
# .rs.restartR()
# ls()
# rm()
# gc()
# memory.size(max = F)


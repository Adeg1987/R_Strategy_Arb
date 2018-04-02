library(dplyr)
library(plyr)
library(zoo)
library(forecast)

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BTCUSD <- read.csv("BITF_BTCUSD_20170822_20180125.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/omgBTC")
omgBTC <- read.csv("BITF_omgBTC_20170714_20180130.csv", header = TRUE)

setwd("C:/btc/Tics/Bitfinex/omgUSD")
omgUSD <- read.csv("BITF_omgUSD_20170714_20180129.csv", header = TRUE)

mn <- max(min(BTCUSD$Timestamp), min(omgBTC$Timestamp), min(omgUSD$Timestamp))
mx <- min(max(BTCUSD$Timestamp), max(omgBTC$Timestamp), max(omgUSD$Timestamp))
ts <- data.frame("Date" = mn:mx)

BTCUSD$Timestamp <- as.POSIXct(BTCUSD$Timestamp, origin="1970-01-01", tz="GMT")
omgBTC$Timestamp <- as.POSIXct(omgBTC$Timestamp, origin="1970-01-01", tz="GMT")
omgUSD$Timestamp <- as.POSIXct(omgUSD$Timestamp, origin="1970-01-01", tz="GMT")
ts$Date <- as.POSIXct(ts$Date, origin="1970-01-01", tz="GMT")

omg <- merge(x = ts, y = BTCUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
omg <- merge(x = omg, y = omgBTC, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
omg <- merge(x = omg, y = omgUSD, by.x = "Date", by.y = "Timestamp", all.x = TRUE)

sl <- 1.004

colnames(omg) <- c("Date", "USD/BTC", "BTC/omg", "USD/omg")
omg$Date <- as.numeric(omg$Date)
omg <- na.locf(omg)
omg <- omg[-c(1:100),]
omg$"USD/BTC/omg" <- omg$"USD/BTC" * omg$"BTC/omg"
omg[is.na(omg)] <- 0
omg$"USD/omg_ask" <- omg$`USD/omg` / sl
omg$"USD/omg_bid" <- omg$`USD/omg` * sl
omg$"USD/BTC/omg_ask" <- omg$`USD/BTC/omg` / sl
omg$"USD/BTC/omg_bid" <- omg$`USD/BTC/omg` * sl
omg$S1 <- round((omg$"USD/omg_ask" / omg$"USD/BTC/omg_bid" - 1) * 100, 2)
omg$S2 <- round((omg$"USD/omg_bid" / omg$"USD/BTC/omg_ask" - 1) * 100, 2)

head(omg,10)
str(omg)

m <- data.matrix(omg)
rm(BTCUSD,omgBTC,omgUSD,mn,mx,ts,omg,sl)
gc()
.rs.restartR()
cat("\014")

hb <- 0.2
hn <- -3.0
lb <- -3.4
ln <- -0.2

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
balance_btcomg <- 0
balance_usdomg <- 0
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
        
        #SELL USD/OMG && BUY USD/BTC/OMG
        deal <- balance_avail * step_entry
        commission <- deal * 0.002 * 2
        start_balance <- balance_usd
        balance_usd <- start_balance - commission
        balance_used <- deal * 2
        balance_avail <- balance_usd * leverage - balance_used
        balance_btcomg <- deal / m[i,9]
        balance_usdomg <- -deal / m[i,6]
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdomg <- m[i,6]
        avg_price_btcomg <- m[i,9]
        
        start <- m[i,1]
        s <- m[i,10]
        # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
        #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
        #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
        # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
        #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
        #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
        #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
        #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
        # cat("SELL USD/OMG && BUY BTC/OMG | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
        #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
        #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
        #     "Commission:", format(round(commission,2), nsmall = 2),"\n")
        # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
        #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
        }
      else {
        if(max(m[i,11],m[i-1,11],m[i-2,11]) < lb){
          trade <- 1
          trade_negative_difference <- 1
          
          #BUY USD/OMG && SELL USD/BTC/OMG
          deal <- balance_avail * step_entry
          commission <- deal * 0.002 * 2
          start_balance <- balance_usd
          balance_usd <- start_balance - commission
          balance_used <- deal * 2
          balance_avail <- balance_usd * leverage - balance_used
          balance_btcomg <- -deal / m[i,8]
          balance_usdomg <- deal / m[i,7]
          comm <- c(comm, deal * 0.002 * 2)
          avg_price_usdomg <- m[i,7]
          avg_price_btcomg <- m[i,8]
          
          start <- m[i,1]
          s <- m[i,11]
          # cat("Start deal: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
          #     "Equity USD:",format(round(start_balance,2), nsmall = 2),
          #     "Balance Available:",format(round(start_balance*leverage,2), nsmall = 2),"\n")
          # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          # cat("BUY USD/OMG && SELL BTC/OMG | DEAL=",format(round(deal,2), nsmall = 2),"\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
          #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
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
      temp_btcomg <- balance_btcomg
      balance_btcomg <- temp_btcomg + deal / m[i,9]
      temp_usdomg <- balance_usdomg
      balance_usdomg <- temp_usdomg - deal / m[i,6]
      temp_used <- balance_used
      balance_used <- temp_used + deal * 2
      temp_avail <- balance_avail
      balance_avail <- balance_usd * leverage - balance_used
      comm <- c(comm, deal * 0.002 * 2)
      avg_price_usdomg <- (balance_used / 2) / abs(balance_usdomg)
      avg_price_btcomg <- (balance_used / 2) / abs(balance_btcomg)
      
      s <- m[i,10]
      
      # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
      #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
      #     "Balance BTC/OMG:",format(round(temp_btcomg,2), nsmall = 2),
      #     "Balance USD/OMG:",format(round(temp_usdomg,2), nsmall = 2),"\n")
      # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
      #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
      #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
      #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
      #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
      # cat("SELL USD/OMG && BUY BTC/OMG | DEAL=",format(round(deal,2), nsmall = 2),"\n")
      # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
      #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
      #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
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
        temp_btcomg <- balance_btcomg
        balance_btcomg <- temp_btcomg - deal / m[i,8]
        temp_usdomg <- balance_usdomg
        balance_usdomg <- temp_usdomg + deal / m[i,7]
        temp_used <- balance_used
        balance_used <- temp_used + deal * 2
        temp_avail <- balance_avail
        balance_avail <- balance_usd * leverage - balance_used
        comm <- c(comm, deal * 0.002 * 2)
        avg_price_usdomg <- (balance_used / 2) / abs(balance_usdomg)
        avg_price_btcomg <- (balance_used / 2) / abs(balance_btcomg)
        
        s <- m[i,11]
        
        # cat("DateTime: ", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")),
        #     "Balance USD:",format(round(temp_usd,2), nsmall = 2),
        #     "Balance BTC/OMG:",format(round(temp_btcomg,2), nsmall = 2),
        #     "Balance USD/OMG:",format(round(temp_usdomg,2), nsmall = 2),"\n")
        # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
        #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
        #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
        #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
        #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
        # cat("BUY USD/OMG && SELL BTC/OMG | DEAL=",format(round(deal,2), nsmall = 2),"\n")
        # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
        #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
        #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
        #     "Commission:", format(round(deal * 0.002 * 2,2), nsmall = 2),"\n")
        # cat("Balance Used:", format(round(balance_used,2), nsmall = 2),
        #     "Balance Available:", format(round(balance_avail,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,10] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){
          #Close positions:
          ##BUY USD/OMG && SELL USD/BTC/OMG
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcomg * m[i,8] * 0.002) +
            abs(balance_usdomg * m[i,7] * 0.002)
          temp_commission <- abs(balance_btcomg * m[i,8] * 0.002) + 
            abs(balance_usdomg * m[i,7] * 0.002)
          PL_btcomg <- (m[i,8] - avg_price_btcomg)*balance_btcomg#balance_btcomg * m[i,8]
          PL_usdomg <- (m[i,7] - avg_price_usdomg)*balance_usdomg#balance_usdomg * m[i,7]
          PL <- PL_usdomg + PL_btcomg#balance_btcomg * m[i,8] + balance_usdomg * m[i,7]
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcomg * m[i,8] * 0.002) +
                                            abs(balance_usdomg * m[i,7] * 0.002))
          balance_btcomg <- 0
          balance_usdomg <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage

          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcomg * m[i,8] * 0.002) +
                      abs(balance_usdomg * m[i,7] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
          #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
          #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,10],2), nsmall = 2), "%\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
          #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
          #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          # cat("PL USD/OMG=",format(round(PL_usdomg,0), nsmall = 0),
          #     "PL BTC/OMG=",format(round(PL_btcomg,0), nsmall = 0),
          #     "PL=", format(round(PL,0), nsmall = 0),
          #     "Total commission:", format(round(commission,0), nsmall = 0),
          #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
          #     "Net Profit/Loss,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcomg <- 0
          temp_usdomg <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcomg <- 0
          PL_usdomg <- 0
        
        }
        else if(m[i,11] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
          #Close positions:
          ##SELL USD/OMG && BUY USD/BTC/OMG
          
          temp_commission <- commission
          commission <- temp_commission + abs(balance_btcomg * m[i,9] * 0.002) +
            abs(balance_usdomg * m[i,6] * 0.002)
          temp_commission <- abs(balance_btcomg * m[i,8] * 0.002) + 
            abs(balance_usdomg * m[i,7] * 0.002)
          PL_btcomg <- (m[i,9] - avg_price_btcomg)*balance_btcomg#balance_btcomg * m[i,9]
          PL_usdomg <- (m[i,6] - avg_price_usdomg)*balance_usdomg#balance_usdomg * m[i,6]
          PL <- PL_usdomg + PL_btcomg#balance_btcomg * m[i,9] + balance_usdomg * m[i,6]
          temp_usd <- balance_usd
          balance_usd <- temp_usd + PL - (abs(balance_btcomg * m[i,9] * 0.002) +
                                            abs(balance_usdomg * m[i,6] * 0.002))
          balance_btcomg <- 0
          balance_usdomg <- 0
          balance_used <- 0
          balance_avail <- balance_usd * leverage
          
          profit_sum <- c(profit_sum, PL - commission)
          profit_perc <- c(profit_perc, (PL - commission)/start_balance)
          comm <- c(comm, abs(balance_btcomg * m[i,9] * 0.002) +
                      abs(balance_usdomg * m[i,6] * 0.002))
          
          end <- m[i,1]
          duration <- c(duration, (end - start)/3600)
          
          # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          #     "Duration (H):", format(round((end - start)/3600,2), nsmall = 2),
          #     "Duration (M):", format(round((end - start)/60,2), nsmall = 2),
          #     "Duration (S):", format(round((end - start),2), nsmall = 2),"\n")
          # cat("USD/OMG ask:", format(round(m[i,6],2), nsmall = 2),
          #     "USD/OMG bid:", format(round(m[i,7],2), nsmall = 2),
          #     "| USD/BTC/OMG ask:", format(round(m[i,8],2), nsmall = 2),
          #     "USD/BTC/OMG bid:", format(round(m[i,9],2), nsmall = 2),
          #     "Difference:", format(round(m[i,11],2), nsmall = 2), "%\n")
          # cat("Balance USD:", format(round(balance_usd,2), nsmall = 2),
          #     "Balance BTC/OMG:", format(round(balance_btcomg,2), nsmall = 2),
          #     "Balance USD/OMG:", format(round(balance_usdomg,2), nsmall = 2),
          #     "Commission:", format(round(temp_commission,2), nsmall = 2),"\n")
          # cat("PL USD/OMG=",format(round(PL_usdomg,0), nsmall = 0),
          #     "PL BTC/OMG=",format(round(PL_btcomg,0), nsmall = 0),
          #     "PL=", format(round(PL,0), nsmall = 0),
          #     "Total commission:", format(round(commission,0), nsmall = 0),
          #     "Net profit:", format(round(PL - commission,0), nsmall = 0),
          #     "Net profit,%:", format(round((PL - commission)/start_balance*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp_usd <- 0
          temp_btcomg <- 0
          temp_usdomg <- 0
          temp_avail <- 0
          temp_used <- 0
          commission <- 0
          temp_commission <- 0
          s <- 0
          PL <- 0
          PL_btcomg <- 0
          PL_usdomg <- 0
          
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


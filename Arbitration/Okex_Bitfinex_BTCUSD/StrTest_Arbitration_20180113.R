library(dplyr)
library(plyr)
library(zoo)
library(forecast)

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BITF <- read.csv("BITF_BTCUSD_20170822_20171230.csv", header = TRUE)
BITF$Timestamp <- as.POSIXct(BITF$Timestamp, origin="1970-01-01", tz="GMT")

setwd("C:/btc/Tics/Okex/OKFT3M")
OKFT <- read.csv("1075_OKFT_BTCUSD3M_20170807_20171128.csv", header = FALSE, sep = "\t")
OKFT <- subset(OKFT, select = -c(V1, V2, V3, V4, V6, V7, V9))
OKFT <- aggregate(V5 ~ V8, data = OKFT, mean)
OKFT$V8 <- as.POSIXct(OKFT$V8, origin="1960-01-01", tz="GMT")

ts <- data.frame("Date" = 1503360002:1511901391)
ts$Date <- as.POSIXct(ts$Date, origin="1970-01-01", tz="GMT")

BITF_OK <- merge(x = ts, y = BITF, by.x = "Date", by.y = "Timestamp", all.x = TRUE)
BITF_OK <- merge(x = BITF_OK, y = OKFT, by.x = "Date", by.y = "V8", all.x = TRUE)

BITF <- NULL
OKFT <- NULL
ts <- NULL

colnames(BITF_OK) <- c("Date", "BITF", "OKFT")
BITF_OK$Date <- as.numeric(BITF_OK$Date)
BITF_OK <- na.locf(BITF_OK)
BITF_OK <- BITF_OK[-c(1:5),]
BITF_OK$BITF <- as.numeric(BITF_OK$BITF)
BITF_OK$OKFT <- as.numeric(BITF_OK$OKFT)
BITF_OK$Perc <- round((BITF_OK$OKFT / BITF_OK$BITF - 1) * 100, 2)
BITF_OK$PercR <- (BITF_OK$Perc %/% 5 + 1) * 5
BITF_OK$Diff <- BITF_OK$BITF - BITF_OK$OKFT
BITF_OK$DiffR <- (BITF_OK$Diff %/% 20 + 1) * 20
BITF_OK$MA_Perc <- round(ma(BITF_OK$Perc, 12),2)
BITF_OK$MA_BITF <- round(ma(BITF_OK$BITF, 12),2)
#BITF_OK$Date <- as.POSIXct(BITF_OK$Date, origin="1970-01-01", tz="GMT")
BITF_OK[is.na(BITF_OK)] <- 0

#as.POSIXct(m[1,1], origin = "1970-01-01")

head(BITF_OK,10)
str(BITF_OK)

m <- data.matrix(BITF_OK)


trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
price_b <- 0
price_o <- 0
diff <- 0
start_acc_b_usd <- 4000
start_acc_o_usd <- 4000
acc_b_usd <- 4000
acc_o_usd <- 4000
hedge <- 0
acc_step <- 0.3
acc_b_btc <- c()
acc_o_btc <- c()
contracts_o <- 400
s_acc_b_usd <- 0
s_acc_o_usd <- 0
deal_b <- 0
deal_o <- 0
comm_b <- c()
comm_o <- c()
profit_sum <- c()
profit_perc <- c()
duration <- c()

for(i in 20:nrow(m)){
  if(trade == 0){
    if(TRUE){
      if(m[i,4] > 1.5){
        trade <- 1
        trade_positive_difference <- 1
        #Prodaem futures i pokupaem bitfinex
        min_acc <- min(acc_b_usd, acc_o_usd)
        
        price_b = m[i,2]
        deal_b <- min_acc * acc_step
        comm_b <- c(comm_b, deal_b * 0.002)
        s_acc_b_usd <- acc_b_usd
        temp <- acc_b_usd
        acc_b_usd <- temp - deal_b - deal_b * 0.002
        acc_b_btc <- c(acc_b_btc, deal_b / m[i,2])
        
        price_o = m[i,3]
        deal_o <- min_acc * acc_step
        comm_o <- c(comm_o, deal_o * 0.0015)
        s_acc_o_usd <- acc_o_usd
        temp <- acc_o_usd
        acc_o_usd <- temp + deal_o - deal_o * 0.0015
        acc_o_btc <- c(acc_o_btc, -deal_o / m[i,3])
        temp <- 0
        
        
        start <- m[i,1]
        diff <- m[i,4]
        cat("Start deal: ",
            as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), "\n")
        cat("Account Bitfinex USD: ", format(round(s_acc_b_usd,2), nsmall = 2),
            "Account Okex USD: ", format(round(s_acc_o_usd,2), nsmall = 2), "\n")
        cat("Bitfinex price: ", format(round(m[i,2],2), nsmall = 2),
            "Okex price: ", format(round(m[i,3],2), nsmall = 2),
            "Difference: ", format(round(m[i,4],2), nsmall = 2), "%\n")
        cat("BUY BITFINEX && SELL OKEX\n")
        cat("Deal Bitfinex: ", format(round(deal_b,2), nsmall = 2),
            "Deal Okex: ", format(round(deal_o,2), nsmall = 2), "\n")
        cat("Account Bitfinex USD: ", format(round(acc_b_usd,2), nsmall = 2),
            "Account Okex USD: ", format(round(acc_o_usd,2), nsmall = 2),
            "Account Bitfinex BTC: ", format(round(sum(acc_b_btc),2), nsmall = 2),
            "Account Okex BTC: ", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
        }
      else {
        if(m[i,4] < -5.2){
          trade <- 1
          trade_negative_difference <- 1
          #Prodaem bitfinex i pokupaem futures
          min_acc <- min(acc_b_usd, acc_o_usd)
          
          price_b = m[i,2]
          deal_b <- min_acc * acc_step
          comm_b <- c(comm_b, deal_b * 0.002)
          s_acc_b_usd <- acc_b_usd
          temp <- acc_b_usd
          acc_b_usd <- temp + deal_b - deal_b * 0.002
          acc_b_btc <- c(acc_b_btc, -deal_b / m[i,2])
          
          price_o = m[i,3]
          deal_o <- min_acc * acc_step
          comm_o <- c(comm_o, deal_o * 0.0015)
          s_acc_o_usd <- acc_o_usd
          temp <- acc_o_usd
          acc_o_usd <- temp - deal_o - deal_o * 0.0015
          acc_o_btc <- c(acc_o_btc, deal_o / m[i,3])
          temp <- 0
          
          
          start <- m[i,1]
          diff <- m[i,4]
          cat("Start deal: ",
              as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), "\n")
          cat("Account Bitfinex USD: ", format(round(s_acc_b_usd,2), nsmall = 2),
              "Account Okex USD: ", format(round(s_acc_o_usd,2), nsmall = 2), "\n")
          cat("Bitfinex price: ", format(round(m[i,2],2), nsmall = 2),
              "Okex price: ", format(round(m[i,3],2), nsmall = 2),
              "Difference: ", format(round(m[i,4],2), nsmall = 2), "%\n")
          cat("SELL BITFINEX && BUY OKEX\n")
          cat("Deal Bitfinex: ", format(round(deal_b,2), nsmall = 2),
              "Deal Okex: ", format(round(deal_o,2), nsmall = 2), "\n")
          cat("Account Bitfinex USD: ", format(round(acc_b_usd,2), nsmall = 2),
              "Account Okex USD: ", format(round(acc_o_usd,2), nsmall = 2),
              "Account Bitfinex BTC: ", format(round(sum(acc_b_btc),2), nsmall = 2),
              "Account Okex BTC: ", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    if(m[i,4] > diff * 1.096 && trade_positive_difference == 1){
      min_acc <- min(acc_b_usd, acc_o_usd)
      
      price_b <- m[i,2]
      deal_b <- min_acc * acc_step
      comm_b <- c(comm_b, deal_b * 0.002)
      temp <- acc_b_usd
      acc_b_usd <- temp - deal_b * 0.002 - deal_b
      acc_b_btc <- c(acc_b_btc, deal_b / m[i,2])
      
      price_o <- m[i,3]
      deal_o <- min_acc * acc_step
      comm_o <- c(comm_o, deal_o * 0.0015)
      temp <- acc_o_usd
      acc_o_usd <- temp - deal_o * 0.0015 + deal_o
      acc_o_btc <- c(acc_o_btc, -deal_o / m[i,3])
      temp <- 0
      
      diff <- m[i,4]
      
      cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
          "Okex price:", format(round(m[i,3],2), nsmall = 2),
          "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
      cat("Deal Bitfinex:", format(round(deal_b,2), nsmall = 2),
          "Deal Okex:", format(round(deal_o,2), nsmall = 2), "\n")
      cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
          "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
          "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
          "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
    }
    else{
      if(m[i,4] < diff * 1.096 && trade_negative_difference == 1){
        min_acc <- min(acc_b_usd, acc_o_usd)
        
        price_b <- m[i,2]
        deal_b <- min_acc * acc_step
        comm_b <- c(comm_b, deal_b * 0.002)
        temp <- acc_b_usd
        acc_b_usd <- temp - deal_b * 0.002 + deal_b
        acc_b_btc <- c(acc_b_btc, -deal_b / m[i,2])
        
        price_o <- m[i,3]
        deal_o <- min_acc * acc_step
        comm_o <- c(comm_o, deal_o * 0.0015)
        temp <- acc_o_usd
        acc_o_usd <- temp - deal_o * 0.0015 - deal_o
        acc_o_btc <- c(acc_o_btc, deal_o / m[i,3])
        temp <- 0
        
        diff <- m[i,4]
        
        cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
            "Okex price:", format(round(m[i,3],2), nsmall = 2),
            "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
        cat("Deal Bitfinex:", format(round(deal_b,2), nsmall = 2),
            "Deal Okex:", format(round(deal_o,2), nsmall = 2), "\n")
        cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
            "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
      }
      else{
        if(m[i,4] < 1.0 && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){#Zakrivaem sdelki: pokupaem futures i prodaem bitfinex
          temp <- acc_b_usd
          acc_b_usd <- temp + sum(acc_b_btc) * m[i,2] - sum(acc_b_btc) * m[i,2] * 0.002
          acc_b_btc <- c()
          temp <- acc_o_usd
          acc_o_usd <- temp + sum(acc_o_btc) * m[i,3] - sum(acc_o_btc) * m[i,3] * 0.0015
          acc_o_btc <- c()
          temp <- 0
          
          comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
          comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
          profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
          profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
          
          end <- m[i,1]
          if ((end - start)/3600 < 48){
            duration <- c(duration, (end - start)/3600)
          }
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Time in hours:",(end - start)/3600, "\n")
          cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
              "Okex price:", format(round(m[i,3],2), nsmall = 2),
              "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
          cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
              "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
              "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
              "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
          cat("Profit sum:", format(round(acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd,2), nsmall = 2),
              "Profit percent:", format(round((acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd)*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp <- 0
        
        }
        else if(m[i,4] > -3.2 && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
          temp <- acc_b_usd
          acc_b_usd <- temp + sum(acc_b_btc) * m[i,2] - sum(acc_b_btc) * m[i,2] * 0.002
          acc_b_btc <- c()
          temp <- acc_o_usd
          acc_o_usd <- temp + sum(acc_o_btc) * m[i,3] - sum(acc_o_btc) * m[i,3] * 0.0015
          acc_o_btc <- c()
          temp <- 0
          
          comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
          comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
          profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
          profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
          
          end <- m[i,1]
          if ((end - start)/3600 < 48){
            duration <- c(duration, (end - start)/3600)
          }
          
          cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              "Time in hours:",(end - start)/3600, "\n")
          cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
              "Okex price:", format(round(m[i,3],2), nsmall = 2),
              "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
          cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
              "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
              "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
              "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
          cat("Profit sum:", format(round(acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd,2), nsmall = 2),
              "Profit percent:", format(round((acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd)*100,2), nsmall = 2),"%\n", "\n")
          
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp <- 0
        }
      }
    }
  }
  if (i == nrow(m)){
    cat("Total deals:",length(profit_sum),"\n")
    cat("Total profit",sum(profit_sum),"\n")
    cat("Profit per deal",mean(profit_sum),"\n")
    cat("Average duration",mean(duration),"\n")
    cat("Average profit percent",format(round(mean(profit_perc)*100,2), nsmall = 2),"%\n")
    cat("Total profit percent", format(round(((acc_b_usd+acc_o_usd)/(start_acc_b_usd+start_acc_o_usd)-1)*100,2), nsmall = 2),"%")
    
  }
}





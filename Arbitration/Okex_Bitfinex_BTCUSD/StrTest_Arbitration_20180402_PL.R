library(dplyr)
library(plyr)
library(zoo)
library(forecast)

setwd("C:/btc/Tics/Bitfinex/BTCUSD")
BITF <- read.csv("BITF_BTCUSD_20170822_20180125.csv", header = TRUE)
BITF$Timestamp <- as.POSIXct(BITF$Timestamp, origin="1970-01-01", tz="GMT")

setwd("C:/btc/Tics/Okex/OKFT_BTCUSD3M")
OKFT <- read.csv("OKFT_BTCUSD3M_20170807_20180105.csv", header = TRUE)
OKFT$V8 <- as.POSIXct(OKFT$V8, origin="1960-01-01", tz="GMT")

ts <- data.frame("Date" = 1503360002:1515110400)
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
BITF_OK[is.na(BITF_OK)] <- 0

head(BITF_OK,10)
str(BITF_OK)

m <- data.matrix(BITF_OK)
head(m)
rm(BITF,OKFT,BITF_OK,ts)
gc()
cat("\014")

sl <- 1.0001
cond_open_sell_okft_buy_bitf <- 3.0
cond_open_buy_okft_sell_bitf <- -3.0
cond_close <- 0.9
stop_loss <- -30.0
step_entry <- 0.2
step_continue <- 0.35
scaling <- 1.4

trade <- 0
trade_positive_difference <- 0
trade_negative_difference <- 0
start <- 0
end <- 0
price_b <- 0
price_o <- 0
avg_price_bitf <- 0
avg_price_okft <- 0
balance_used <- 0
diff <- 0
start_acc_b_usd <- 4000
start_acc_o_usd <- 4000
acc_b_usd <- 4000
acc_o_usd <- 4000
bal_btc_o <- acc_o_usd / m[1,3]
order_btc_o <- 0
PL_o <- 0
hedge1 <- (bal_btc_o * m[1,3]) %/% 100
hedge2 <- 0
acc_step_entry <- 0.1
acc_step_continue <- 0.3
acc_b_btc <- c()
acc_o_btc <- c()
s_acc_b_usd <- 0
s_acc_o_usd <- 0
deal <- 0
comm_b <- c()
comm_o <- c()
profit_sum <- c()
profit_perc <- c()
duration <- c()
SELL_OKFT_BUY_BITF <- 0
BUY_OKFT_SELL_BITF <- 0

for(i in 20:nrow(m)){
  if(trade == 0){
    if(m[i,4] > cond_open_sell_okft_buy_bitf){
      trade <- 1
      trade_positive_difference <- 1
      #OPEN 1
      #SELL OKFT & BUY BITF
      min_acc <- min(acc_b_usd, acc_o_usd)
      deal <- ((min_acc * step_entry) %/% 100) * 100
      
      price_b = m[i,2]
      comm_b <- c(comm_b, deal * 0.002)
      s_acc_b_usd <- acc_b_usd
      temp <- acc_b_usd
      acc_b_usd <- temp - deal - deal * 0.002
      acc_b_btc <- c(acc_b_btc, deal / (m[i,2]*sl))
      avg_price_bitf <- price_b
      
      
      price_o = m[i,3]
      comm_o <- c(comm_o, deal * 0.0015)
      s_acc_o_usd <- acc_o_usd
      temp <- acc_o_usd
      acc_o_usd <- temp + deal - deal * 0.0015
      acc_o_btc <- c(acc_o_btc, -deal / (m[i,3]/sl))
      temp <- 0
      avg_price_okft <- price_o
      balance_used <- deal
      
      SELL_OKFT_BUY_BITF <- 1
      start <- m[i,1]
      diff <- m[i,4]
      cat("Start. SELL OKFT & BUY BITF | DEAL=", format(round(deal,2), nsmall = 2), "\n")
      cat("Balance OKFT:", format(round(s_acc_o_usd,2), nsmall = 2),
          "Balance BITF:", format(round(s_acc_b_usd,2), nsmall = 2),"\n")
      cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
          "BITF price:", format(round(m[i,2],2), nsmall = 2),
          "DIFF:", format(round(m[i,4],2), nsmall = 2), "%\n")
      cat("Amount OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
          "Amount BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
          "Amount OKFT BTC:", format(round(sum(acc_o_btc),4), nsmall = 4),
          "Amount BITF BTC:", format(round(sum(acc_b_btc),4), nsmall = 4),"\n")
      next
    }
    else {
      if(m[i,4] < cond_open_buy_okft_sell_bitf){
        trade <- 1
        trade_negative_difference <- 1
        #OPEN 2
        #BUY OKFT & SELL BITF
        min_acc <- min(acc_b_usd, acc_o_usd)
        deal <- ((min_acc * step_entry) %/% 100) * 100
        
        price_b = m[i,2]
        comm_b <- c(comm_b, deal * 0.002)
        s_acc_b_usd <- acc_b_usd
        temp <- acc_b_usd
        acc_b_usd <- temp + deal - deal * 0.002
        acc_b_btc <- c(acc_b_btc, -deal / (m[i,2]/sl))
        avg_price_bitf <- price_b
        
        price_o = m[i,3]
        comm_o <- c(comm_o, deal * 0.0015)
        s_acc_o_usd <- acc_o_usd
        temp <- acc_o_usd
        acc_o_usd <- temp - deal - deal * 0.0015
        acc_o_btc <- c(acc_o_btc, deal / (m[i,3]*sl))
        temp <- 0
        avg_price_okft <- price_o
        balance_used <- deal
        
        BUY_OKFT_SELL_BITF <- 1
        start <- m[i,1]
        diff <- m[i,4]
        cat("Start. BUY OKFT & SELL BITF | DEAL=", format(round(deal,2), nsmall = 2), "\n")
        cat("Balance OKFT:", format(round(s_acc_o_usd,2), nsmall = 2),
            "Balance BITF:", format(round(s_acc_b_usd,2), nsmall = 2),"\n")
        cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
            "BITF price:", format(round(m[i,2],2), nsmall = 2),
            "DIFF:", format(round(m[i,4],2), nsmall = 2), "%\n")
        cat("Amount OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Amount BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Amount OKFT BTC:", format(round(sum(acc_o_btc),4), nsmall = 4),
            "Amount BITF BTC:", format(round(sum(acc_b_btc),4), nsmall = 4),"\n")
        next
      }
    }
  }
  else{
    if(SELL_OKFT_BUY_BITF == 1){
      PL_bitf <- (m[i,2] - avg_price_bitf) * sum(acc_b_btc) - balance_used * 0.002 * 2
      PL_okft <- (m[i,3] - avg_price_okft) * sum(acc_o_btc) - balance_used * 0.0015 * 2
      PL <- PL_bitf + PL_okft
      
      #CLOSE 1:
      if((PL / (balance_used * 2) * 100 > cond_close
          || PL / (balance_used * 2) * 100 < stop_loss
          || i==nrow(m)) && trade_positive_difference == 1){
        
        temp <- acc_b_usd
        acc_b_usd <- temp + sum(acc_b_btc) * (m[i,2]/sl) - abs(sum(acc_b_btc)) * m[i,2] * 0.002
        acc_b_btc <- c()
        temp <- acc_o_usd
        acc_o_usd <- temp + sum(acc_o_btc) * (m[i,3]*sl) - abs(sum(acc_o_btc)) * m[i,3] * 0.0015
        acc_o_btc <- c()
        temp <- 0
        hedge2 <- 0
        order_btc_o <- 0
        bal_btc_o <- acc_o_usd / m[i,3]
        PL_o <- 0
        hedge1 <- acc_o_usd / 100
        
        comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
        comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
        profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
        profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
        
        end <- m[i,1]
        if ((end - start)/3600 < 48){
          duration <- c(duration, (end - start)/3600)
        }
        
        cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
            "Duration (H):",(end - start)/3600,
            "Balance used:",format(round(balance_used,2)),"\n")
        cat("Avg price OKFT:",format(round(avg_price_okft,2)),
            "Avg price BITF:",format(round(avg_price_bitf,2)),"\n")
        cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
            "BITF price:", format(round(m[i,2],2), nsmall = 2),"\n")
        cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
            "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
        cat("PL OKFT:",format(round(PL_okft,2), nsmall = 2),
            "PL BITF:",format(round(PL_bitf,2), nsmall = 2),
            "PL:", format(round(PL,2), nsmall = 2),
            "PL%:", format(round(PL / (balance_used * 2) * 100,2), nsmall = 2),"%\n", "\n")
        
        if(abs(acc_o_usd - acc_b_usd) / (acc_o_usd + acc_b_usd) > 0.1){
          avg_balance <- (acc_o_usd + acc_b_usd) / 2
          acc_o_usd <- avg_balance
          acc_b_usd <- avg_balance
        }
        
        trade <- 0
        trade_positive_difference <- 0
        start <- 0
        end <- 0
        temp <- 0
        PL_okft <- 0
        PL_bitf <- 0
        PL <- 0
        SELL_OKFT_BUY_BITF <- 0
        next
      }
      
      #SCALING 1
      if(m[i,4] > diff + scaling & trade_positive_difference == 1
         & min(acc_b_usd, acc_o_usd) / step_continue > 100){
        #SCALING 1
        #SELL OKFT & BUY BITF
        
        min_acc <- min(acc_b_usd, acc_o_usd)
        deal <- ((min_acc * step_continue) %/% 100) * 100
        
        price_b <- m[i,2]
        comm_b <- c(comm_b, deal * 0.002)
        temp <- acc_b_usd
        acc_b_usd <- temp - deal * 0.002 - deal
        acc_b_btc <- c(acc_b_btc, deal / (m[i,2]*sl))
        
        price_o <- m[i,3]
        comm_o <- c(comm_o, deal * 0.0015)
        temp <- acc_o_usd
        acc_o_usd <- temp - deal * 0.0015 + deal
        acc_o_btc <- c(acc_o_btc, -deal / (m[i,3]/sl))
        
        
        balance_used <- balance_used + deal
        avg_price_okft <- balance_used / sum(acc_o_btc)
        avg_price_bitf <- -balance_used / sum(acc_b_btc)
        
        diff <- m[i,4]
        
        cat("SCALING. OKFT price:", format(round(m[i,3],2), nsmall = 2),
            "BITF price:", format(round(m[i,2],2), nsmall = 2),
            "Diff:", format(round(m[i,4],2), nsmall = 2),
            "% | DEAL=", format(round(deal,2), nsmall = 2),"\n")
        cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
            "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
        next
      }
    }
    else{
      PL_bitf <- (m[i,2] - avg_price_bitf) * sum(acc_b_btc) - balance_used * 0.002 * 2
      PL_okft <- (m[i,3] - avg_price_okft) * sum(acc_o_btc) - balance_used * 0.0015 * 2
      PL <- PL_bitf + PL_okft
      
      #CLOSE 2
      if((PL / (balance_used * 2) * 100 > cond_close
          | PL / (balance_used * 2) * 100 < stop_loss
          | i==nrow(m)) & trade_negative_difference == 1){
        
        temp <- acc_b_usd
        acc_b_usd <- temp + sum(acc_b_btc) * (m[i,2]*sl) - abs(sum(acc_b_btc)) * m[i,2] * 0.002
        acc_b_btc <- c()
        temp <- acc_o_usd
        acc_o_usd <- temp + sum(acc_o_btc) * (m[i,3]/sl) - abs(sum(acc_o_btc)) * m[i,3] * 0.0015
        acc_o_btc <- c()
        temp <- 0
        hedge2 <- 0
        order_btc_o <- 0
        bal_btc_o <- acc_o_usd / m[i,3]
        PL_o <- 0
        hedge1 <- acc_o_usd / 100
        
        comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
        comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
        profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
        profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
        
        end <- m[i,1]
        if ((end - start)/3600 < 48){
          duration <- c(duration, (end - start)/3600)
        }
        
        cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
            "Duration (H):",(end - start)/3600,
            "Balance used:",format(round(balance_used,2)),"\n")
        cat("Avg price OKFT:",format(round(avg_price_okft,2)),
            "Avg price BITF:",format(round(avg_price_bitf,2)),"\n")
        cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
            "BITF price:", format(round(m[i,2],2), nsmall = 2),"\n")
        cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
            "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
        cat("PL OKFT:",format(round(PL_okft,2), nsmall = 2),
            "PL BITF:",format(round(PL_bitf,2), nsmall = 2),
            "PL:", format(round(PL,2), nsmall = 2),
            "PL%:", format(round(PL / (balance_used * 2) * 100,2), nsmall = 2),"%\n", "\n")
        
        if(abs(acc_o_usd - acc_b_usd) / (acc_o_usd + acc_b_usd) > 0.1){
          avg_balance <- (acc_o_usd + acc_b_usd) / 2
          acc_o_usd <- avg_balance
          acc_b_usd <- avg_balance
        }
        
        trade <- 0
        trade_negative_difference <- 0
        start <- 0
        end <- 0
        temp <- 0
        PL_okft <- 0
        PL_bitf <- 0
        PL <- 0
        BUY_OKFT_SELL_BITF <- 0
        next
      }
      
      if(m[i,4] < diff - scaling & trade_negative_difference == 1
         & min(acc_b_usd, acc_o_usd) / step_continue > 100){
        #SCALING 2
        #BUY OKFT & SELL BITF
        
        min_acc <- min(acc_b_usd, acc_o_usd)
        deal <- ((min_acc * step_continue) %/% 100) * 100
        
        price_b <- m[i,2]
        comm_b <- c(comm_b, deal * 0.002)
        temp <- acc_b_usd
        acc_b_usd <- temp - deal * 0.002 + deal
        acc_b_btc <- c(acc_b_btc, -deal / (m[i,2]/sl))
        
        price_o <- m[i,3]
        comm_o <- c(comm_o, deal * 0.0015)
        temp <- acc_o_usd
        acc_o_usd <- temp - deal * 0.0015 - deal
        acc_o_btc <- c(acc_o_btc, deal / (m[i,3]*sl))
        
        balance_used <- balance_used + deal
        avg_price_okft <- -balance_used / sum(acc_o_btc)
        avg_price_bitf <- balance_used / sum(acc_b_btc)
        
        diff <- m[i,4]
        
        cat("SCALING. OKFT price:", format(round(m[i,3],2), nsmall = 2),
            "BITF price:", format(round(m[i,2],2), nsmall = 2),
            "Diff:", format(round(m[i,4],2), nsmall = 2),
            "% | DEAL=", format(round(deal,2), nsmall = 2),"\n")
        cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
            "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
        next
      }
    }
  }
}

if (TRUE){
  cat("Start testing:", as.character(as.POSIXct(m[1,1], origin = "1970-01-1")),
      "End testing",as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
      "Estimating period (Days):",format(round((m[i,1]-m[1,1])/86400,0), nsmall = 0),"\n")
  cat("Total deals:",length(profit_sum),"\n")
  cat("Total profit",sum(profit_sum),"\n")
  cat("Profit per deal",mean(profit_sum),"\n")
  cat("Average duration (H):",mean(duration),"\n")
  cat("Average duration (M):",mean(duration) * 60,"\n")
  cat("Average duration (S):",mean(duration) * 3600,"\n")
  cat("Average profit percent",format(round(mean(profit_perc)*100,2), nsmall = 2),"%\n")
  cat("Total profit percent", format(round(((acc_b_usd+acc_o_usd)/(start_acc_b_usd+start_acc_o_usd)-1)*100,2), nsmall = 2),"%\n")
  cat("Max profit:",max(profit_sum),"\n")
  cat("Min profit:",min(profit_sum),"\n")
  cat("Paid commission:",sum(comm_b)+sum(comm_o),"\n")
}

##########################################################
########TEST

res <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("cond_open_sell_okft_buy_bitf", "cond_open_buy_okft_sell_bitf","scaling",
       "cond_close","profit", "deals")
colnames(res) <- x
for(cond_open_sell_okft_buy_bitf in seq(0.3,6.0,by=0.3)){
  cond_open_buy_okft_sell_bitf <- -cond_open_sell_okft_buy_bitf
  for(scaling in seq(1.0,3.0,by=0.5)){
    for(cond_close in seq(0.6,3.0,by=0.2)){
      
      t1 <- Sys.time()
      trade <- 0
      trade_positive_difference <- 0
      trade_negative_difference <- 0
      start <- 0
      end <- 0
      price_b <- 0
      price_o <- 0
      avg_price_bitf <- 0
      avg_price_okft <- 0
      balance_used <- 0
      diff <- 0
      start_acc_b_usd <- 4000
      start_acc_o_usd <- 4000
      acc_b_usd <- 4000
      acc_o_usd <- 4000
      bal_btc_o <- acc_o_usd / m[1,3]
      order_btc_o <- 0
      PL_o <- 0
      hedge1 <- (bal_btc_o * m[1,3]) %/% 100
      hedge2 <- 0
      acc_step_entry <- 0.1
      acc_step_continue <- 0.3
      acc_b_btc <- c()
      acc_o_btc <- c()
      s_acc_b_usd <- 0
      s_acc_o_usd <- 0
      deal <- 0
      comm_b <- c()
      comm_o <- c()
      profit_sum <- c()
      profit_perc <- c()
      duration <- c()
      SELL_OKFT_BUY_BITF <- 0
      BUY_OKFT_SELL_BITF <- 0
      
      for(i in 20:nrow(m)){
        if(trade == 0){
          if(m[i,4] > cond_open_sell_okft_buy_bitf){
            trade <- 1
            trade_positive_difference <- 1
            #OPEN 1
            #SELL OKFT & BUY BITF
            min_acc <- min(acc_b_usd, acc_o_usd)
            deal <- ((min_acc * step_entry) %/% 100) * 100
            
            price_b = m[i,2]
            comm_b <- c(comm_b, deal * 0.002)
            s_acc_b_usd <- acc_b_usd
            temp <- acc_b_usd
            acc_b_usd <- temp - deal - deal * 0.002
            acc_b_btc <- c(acc_b_btc, deal / (m[i,2]*sl))
            avg_price_bitf <- price_b
            
            
            price_o = m[i,3]
            comm_o <- c(comm_o, deal * 0.0015)
            s_acc_o_usd <- acc_o_usd
            temp <- acc_o_usd
            acc_o_usd <- temp + deal - deal * 0.0015
            acc_o_btc <- c(acc_o_btc, -deal / (m[i,3]/sl))
            temp <- 0
            avg_price_okft <- price_o
            balance_used <- deal
            
            SELL_OKFT_BUY_BITF <- 1
            start <- m[i,1]
            diff <- m[i,4]
            # cat("Start. SELL OKFT & BUY BITF | DEAL=", format(round(deal,2), nsmall = 2), "\n")
            # cat("Balance OKFT:", format(round(s_acc_o_usd,2), nsmall = 2),
            #     "Balance BITF:", format(round(s_acc_b_usd,2), nsmall = 2),"\n")
            # cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
            #     "BITF price:", format(round(m[i,2],2), nsmall = 2),
            #     "DIFF:", format(round(m[i,4],2), nsmall = 2), "%\n")
            # cat("Amount OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
            #     "Amount BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
            #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),4), nsmall = 4),
            #     "Amount BITF BTC:", format(round(sum(acc_b_btc),4), nsmall = 4),"\n")
            next
          }
          else {
            if(m[i,4] < cond_open_buy_okft_sell_bitf){
              trade <- 1
              trade_negative_difference <- 1
              #OPEN 2
              #BUY OKFT & SELL BITF
              min_acc <- min(acc_b_usd, acc_o_usd)
              deal <- ((min_acc * step_entry) %/% 100) * 100
              
              price_b = m[i,2]
              comm_b <- c(comm_b, deal * 0.002)
              s_acc_b_usd <- acc_b_usd
              temp <- acc_b_usd
              acc_b_usd <- temp + deal - deal * 0.002
              acc_b_btc <- c(acc_b_btc, -deal / (m[i,2]/sl))
              avg_price_bitf <- price_b
              
              price_o = m[i,3]
              comm_o <- c(comm_o, deal * 0.0015)
              s_acc_o_usd <- acc_o_usd
              temp <- acc_o_usd
              acc_o_usd <- temp - deal - deal * 0.0015
              acc_o_btc <- c(acc_o_btc, deal / (m[i,3]*sl))
              temp <- 0
              avg_price_okft <- price_o
              balance_used <- deal
              
              BUY_OKFT_SELL_BITF <- 1
              start <- m[i,1]
              diff <- m[i,4]
              # cat("Start. BUY OKFT & SELL BITF | DEAL=", format(round(deal,2), nsmall = 2), "\n")
              # cat("Balance OKFT:", format(round(s_acc_o_usd,2), nsmall = 2),
              #     "Balance BITF:", format(round(s_acc_b_usd,2), nsmall = 2),"\n")
              # cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
              #     "BITF price:", format(round(m[i,2],2), nsmall = 2),
              #     "DIFF:", format(round(m[i,4],2), nsmall = 2), "%\n")
              # cat("Amount OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
              #     "Amount BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
              #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),4), nsmall = 4),
              #     "Amount BITF BTC:", format(round(sum(acc_b_btc),4), nsmall = 4),"\n")
              next
            }
          }
        }
        else{
          if(SELL_OKFT_BUY_BITF == 1){
            PL_bitf <- (m[i,2] - avg_price_bitf) * sum(acc_b_btc) - balance_used * 0.002 * 2
            PL_okft <- (m[i,3] - avg_price_okft) * sum(acc_o_btc) - balance_used * 0.0015 * 2
            PL <- PL_bitf + PL_okft
            
            #CLOSE 1:
            if((PL / (balance_used * 2) * 100 > cond_close
                || PL / (balance_used * 2) * 100 < stop_loss
                || i==nrow(m)) && trade_positive_difference == 1){
              
              temp <- acc_b_usd
              acc_b_usd <- temp + sum(acc_b_btc) * (m[i,2]/sl) - abs(sum(acc_b_btc)) * m[i,2] * 0.002
              acc_b_btc <- c()
              temp <- acc_o_usd
              acc_o_usd <- temp + sum(acc_o_btc) * (m[i,3]*sl) - abs(sum(acc_o_btc)) * m[i,3] * 0.0015
              acc_o_btc <- c()
              temp <- 0
              hedge2 <- 0
              order_btc_o <- 0
              bal_btc_o <- acc_o_usd / m[i,3]
              PL_o <- 0
              hedge1 <- acc_o_usd / 100
              
              comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
              comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
              profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
              profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
              
              end <- m[i,1]
              if ((end - start)/3600 < 48){
                duration <- c(duration, (end - start)/3600)
              }
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):",(end - start)/3600,
              #     "Balance used:",format(round(balance_used,2)),"\n")
              # cat("Avg price OKFT:",format(round(avg_price_okft,2)),
              #     "Avg price BITF:",format(round(avg_price_bitf,2)),"\n")
              # cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
              #     "BITF price:", format(round(m[i,2],2), nsmall = 2),"\n")
              # cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
              #     "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
              #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
              #     "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
              # cat("PL OKFT:",format(round(PL_okft,2), nsmall = 2),
              #     "PL BITF:",format(round(PL_bitf,2), nsmall = 2),
              #     "PL:", format(round(PL,2), nsmall = 2),
              #     "PL%:", format(round(PL / (balance_used * 2) * 100,2), nsmall = 2),"%\n", "\n")
              
              if(abs(acc_o_usd - acc_b_usd) / (acc_o_usd + acc_b_usd) > 0.1){
                avg_balance <- (acc_o_usd + acc_b_usd) / 2
                acc_o_usd <- avg_balance
                acc_b_usd <- avg_balance
              }
              
              trade <- 0
              trade_positive_difference <- 0
              start <- 0
              end <- 0
              temp <- 0
              PL_okft <- 0
              PL_bitf <- 0
              PL <- 0
              SELL_OKFT_BUY_BITF <- 0
              next
            }
            
            #SCALING 1
            if(m[i,4] > diff + scaling & trade_positive_difference == 1
               & min(acc_b_usd, acc_o_usd) / step_continue > 100){
              #SCALING 1
              #SELL OKFT & BUY BITF
              
              min_acc <- min(acc_b_usd, acc_o_usd)
              deal <- ((min_acc * step_continue) %/% 100) * 100
              
              price_b <- m[i,2]
              comm_b <- c(comm_b, deal * 0.002)
              temp <- acc_b_usd
              acc_b_usd <- temp - deal * 0.002 - deal
              acc_b_btc <- c(acc_b_btc, deal / (m[i,2]*sl))
              
              price_o <- m[i,3]
              comm_o <- c(comm_o, deal * 0.0015)
              temp <- acc_o_usd
              acc_o_usd <- temp - deal * 0.0015 + deal
              acc_o_btc <- c(acc_o_btc, -deal / (m[i,3]/sl))
              
              
              balance_used <- balance_used + deal
              avg_price_okft <- balance_used / sum(acc_o_btc)
              avg_price_bitf <- -balance_used / sum(acc_b_btc)
              
              diff <- m[i,4]
              
              # cat("SCALING. OKFT price:", format(round(m[i,3],2), nsmall = 2),
              #     "BITF price:", format(round(m[i,2],2), nsmall = 2),
              #     "Diff:", format(round(m[i,4],2), nsmall = 2),
              #     "% | DEAL=", format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
              #     "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
              #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
              #     "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
              next
            }
          }
          else{
            PL_bitf <- (m[i,2] - avg_price_bitf) * sum(acc_b_btc) - balance_used * 0.002 * 2
            PL_okft <- (m[i,3] - avg_price_okft) * sum(acc_o_btc) - balance_used * 0.0015 * 2
            PL <- PL_bitf + PL_okft
            
            #CLOSE 2
            if((PL / (balance_used * 2) * 100 > cond_close
                | PL / (balance_used * 2) * 100 < stop_loss
                | i==nrow(m)) & trade_negative_difference == 1){
              
              temp <- acc_b_usd
              acc_b_usd <- temp + sum(acc_b_btc) * (m[i,2]*sl) - abs(sum(acc_b_btc)) * m[i,2] * 0.002
              acc_b_btc <- c()
              temp <- acc_o_usd
              acc_o_usd <- temp + sum(acc_o_btc) * (m[i,3]/sl) - abs(sum(acc_o_btc)) * m[i,3] * 0.0015
              acc_o_btc <- c()
              temp <- 0
              hedge2 <- 0
              order_btc_o <- 0
              bal_btc_o <- acc_o_usd / m[i,3]
              PL_o <- 0
              hedge1 <- acc_o_usd / 100
              
              comm_b <- c(comm_b, acc_b_btc * m[i,2] * 0.002)
              comm_o <- c(comm_o, acc_o_btc * m[i,3] * 0.0015)
              profit_sum <- c(profit_sum, acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)
              profit_perc <- c(profit_perc, (acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd))
              
              end <- m[i,1]
              if ((end - start)/3600 < 48){
                duration <- c(duration, (end - start)/3600)
              }
              
              # cat("End deal:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
              #     "Duration (H):",(end - start)/3600,
              #     "Balance used:",format(round(balance_used,2)),"\n")
              # cat("Avg price OKFT:",format(round(avg_price_okft,2)),
              #     "Avg price BITF:",format(round(avg_price_bitf,2)),"\n")
              # cat("OKFT price:", format(round(m[i,3],2), nsmall = 2),
              #     "BITF price:", format(round(m[i,2],2), nsmall = 2),"\n")
              # cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
              #     "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
              #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
              #     "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
              # cat("PL OKFT:",format(round(PL_okft,2), nsmall = 2),
              #     "PL BITF:",format(round(PL_bitf,2), nsmall = 2),
              #     "PL:", format(round(PL,2), nsmall = 2),
              #     "PL%:", format(round(PL / (balance_used * 2) * 100,2), nsmall = 2),"%\n", "\n")
              
              if(abs(acc_o_usd - acc_b_usd) / (acc_o_usd + acc_b_usd) > 0.1){
                avg_balance <- (acc_o_usd + acc_b_usd) / 2
                acc_o_usd <- avg_balance
                acc_b_usd <- avg_balance
              }
              
              trade <- 0
              trade_negative_difference <- 0
              start <- 0
              end <- 0
              temp <- 0
              PL_okft <- 0
              PL_bitf <- 0
              PL <- 0
              BUY_OKFT_SELL_BITF <- 0
              next
            }
            
            if(m[i,4] < diff - scaling & trade_negative_difference == 1
               & min(acc_b_usd, acc_o_usd) / step_continue > 100){
              #SCALING 2
              #BUY OKFT & SELL BITF
              
              min_acc <- min(acc_b_usd, acc_o_usd)
              deal <- ((min_acc * step_continue) %/% 100) * 100
              
              price_b <- m[i,2]
              comm_b <- c(comm_b, deal * 0.002)
              temp <- acc_b_usd
              acc_b_usd <- temp - deal * 0.002 + deal
              acc_b_btc <- c(acc_b_btc, -deal / (m[i,2]/sl))
              
              price_o <- m[i,3]
              comm_o <- c(comm_o, deal * 0.0015)
              temp <- acc_o_usd
              acc_o_usd <- temp - deal * 0.0015 - deal
              acc_o_btc <- c(acc_o_btc, deal / (m[i,3]*sl))
              
              balance_used <- balance_used + deal
              avg_price_okft <- -balance_used / sum(acc_o_btc)
              avg_price_bitf <- balance_used / sum(acc_b_btc)
              
              diff <- m[i,4]
              
              # cat("SCALING. OKFT price:", format(round(m[i,3],2), nsmall = 2),
              #     "BITF price:", format(round(m[i,2],2), nsmall = 2),
              #     "Diff:", format(round(m[i,4],2), nsmall = 2),
              #     "% | DEAL=", format(round(deal,2), nsmall = 2),"\n")
              # cat("Balance OKFT USD:", format(round(acc_o_usd,2), nsmall = 2),
              #     "Balance BITF USD:", format(round(acc_b_usd,2), nsmall = 2),
              #     "Amount OKFT BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),
              #     "Amount BITF BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),"\n")
              next
            }
          }
        }
      }
      
      t2 <- Sys.time()
      t2-t1
      cat("open1=",cond_open_sell_okft_buy_bitf,"open2=",cond_open_buy_okft_sell_bitf,
          "scaling",scaling,"close=",cond_close,
          "Total profit percent:", format(round(((sum(profit_sum)+8000)/8000-1)*100,2), nsmall = 2),
          "% Total deals:",length(profit_sum),"time:",format(round(t2-t1,2), nsmall = 2),"\n")
      df <- data.frame(cond_open_sell_okft_buy_bitf,cond_open_buy_okft_sell_bitf,scaling,
                       cond_close,format(round(((sum(profit_sum)+8000)/8000-1)*100,2), nsmall = 2),length(profit_sum))
      x <- c("cond_open_sell_okft_buy_bitf", "cond_open_buy_okft_sell_bitf","scaling",
             "cond_close","profit", "deals")
      colnames(df) <- x
      res <- rbind(res,df)
    }
  }
}
setwd("C:/btc/Strategy/R_Strategy_Arb/Arbitration/Okex_Bitfinex_BTCUSD")
write.csv(res, "OKFT_BITF_cond.csv", row.names = FALSE)
      
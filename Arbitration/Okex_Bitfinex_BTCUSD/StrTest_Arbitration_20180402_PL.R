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

hb <- 2.0
hn <- 0.8
lb <- -6.2
ln <- -3.2
sl <- 1.0001

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
      if(m[i,4] > hb){
        trade <- 1
        trade_positive_difference <- 1
        #Prodaem futures i pokupaem bitfinex
        min_acc <- min(acc_b_usd, acc_o_usd)
        
        price_b = m[i,2]
        deal_b <- min_acc * acc_step_entry
        comm_b <- c(comm_b, deal_b * 0.002)
        s_acc_b_usd <- acc_b_usd
        temp <- acc_b_usd
        acc_b_usd <- temp - deal_b - deal_b * 0.002
        acc_b_btc <- c(acc_b_btc, deal_b / (m[i,2]*sl))
        
        price_o = m[i,3]
        deal_o <- min_acc * acc_step_entry
        comm_o <- c(comm_o, deal_o * 0.0015)
        s_acc_o_usd <- acc_o_usd
        temp <- acc_o_usd
        acc_o_usd <- temp + deal_o - deal_o * 0.0015
        acc_o_btc <- c(acc_o_btc, -deal_o / (m[i,3]/sl))
        temp <- 0
        
        
        
        start <- m[i,1]
        diff <- m[i,4]
        cat("Start deal: ",
            as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), "\n")
        cat("Account Bitfinex USD:", format(round(s_acc_b_usd,2), nsmall = 2),
            "Account Okex USD:", format(round(s_acc_o_usd,2), nsmall = 2),
            "hedge1:",format(round(hedge1,2), nsmall = 2),
            "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
        cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
            "Okex price:", format(round(m[i,3],2), nsmall = 2),
            "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
        cat("BUY BITFINEX && SELL OKEX\n")
        cat("Deal Bitfinex:", format(round(deal_b,2), nsmall = 2),
            "Deal Okex:", format(round(deal_o,2), nsmall = 2), "\n")
        cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
            "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
            "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
            "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
        
        hedge2 <- deal_o / 100
        order_btc_o <- hedge2 * 100 / m[i,3]
        cat("hedge1:",format(round(hedge1,2), nsmall = 2),
            "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
        }
      else {
        if(m[i,4] < lb){
          trade <- 1
          trade_negative_difference <- 1
          #Prodaem bitfinex i pokupaem futures
          min_acc <- min(acc_b_usd, acc_o_usd)
          
          price_b = m[i,2]
          deal_b <- min_acc * acc_step_entry
          comm_b <- c(comm_b, deal_b * 0.002)
          s_acc_b_usd <- acc_b_usd
          temp <- acc_b_usd
          acc_b_usd <- temp + deal_b - deal_b * 0.002
          acc_b_btc <- c(acc_b_btc, -deal_b / (m[i,2]/sl))
          
          price_o = m[i,3]
          deal_o <- min_acc * acc_step_entry
          comm_o <- c(comm_o, deal_o * 0.0015)
          s_acc_o_usd <- acc_o_usd
          temp <- acc_o_usd
          acc_o_usd <- temp - deal_o - deal_o * 0.0015
          acc_o_btc <- c(acc_o_btc, deal_o / (m[i,3]*sl))
          temp <- 0
          
          
          
          start <- m[i,1]
          diff <- m[i,4]
          cat("Start deal: ",
              as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), "\n")
          cat("Account Bitfinex USD: ", format(round(s_acc_b_usd,2), nsmall = 2),
              "Account Okex USD: ", format(round(s_acc_o_usd,2), nsmall = 2),
              "hedge1:",format(round(hedge1,2), nsmall = 2),
              "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
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
          
          hedge2 <- deal_o / 100
          order_btc_o <- hedge2 * 100 / m[i,3]
          cat("hedge1:",format(round(hedge1,2), nsmall = 2),
              "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
        }
      }
    }
    else next
  }
  else{
    
    bal_btc_o <- hedge1 * 100 / m[i,3]
    temp <- order_btc_o
    order_btc_o <- hedge2 * 100 / m[i,3]
    if(trade_positive_difference){
      PL_o <- order_btc_o - temp
    }
    else{
      PL_o <- temp - order_btc_o
    }
    

    if(abs(price_o / m[i,3] - 1) > 0.05 && abs(PL_o * m[i,3]) > 100){ #abs(PL_o) / bal_btc_o > 0.03
      cat("BTC balance=",format(round(bal_btc_o,4), nsmall = 4),
          "BTC order=",format(round(order_btc_o,4), nsmall = 4),"\n")
      temp <- bal_btc_o
      bal_btc_o <- temp + PL_o
      hedge1 <- (bal_btc_o * m[i,3]) / 100
      cat("HEDGE!!!","Bal_BTC=",format(round(bal_btc_o,4), nsmall = 4),
          "Price=",format(round(m[i,3],2), nsmall = 2),
          "PL=", format(round(PL_o,4), nsmall = 4),
          "hedge1=",format(round(hedge1,2), nsmall = 2),
          "hedge2=",format(round(hedge2,2), nsmall = 2),"\n")
      price_o <- m[i,3]
      }

    if(m[i,4] > diff * 1.096 && trade_positive_difference == 1){
      min_acc <- min(acc_b_usd, acc_o_usd)
      
      price_b <- m[i,2]
      deal_b <- min_acc * acc_step_continue
      comm_b <- c(comm_b, deal_b * 0.002)
      temp <- acc_b_usd
      acc_b_usd <- temp - deal_b * 0.002 - deal_b
      acc_b_btc <- c(acc_b_btc, deal_b / (m[i,2]*sl))
      
      price_o <- m[i,3]
      deal_o <- min_acc * acc_step_continue
      comm_o <- c(comm_o, deal_o * 0.0015)
      temp <- acc_o_usd
      acc_o_usd <- temp - deal_o * 0.0015 + deal_o
      acc_o_btc <- c(acc_o_btc, -deal_o / (m[i,3]/sl))
      temp <- 0
      temp <- hedge2
      hedge2 <- temp + deal_o / 100
      order_btc_o <- hedge2 * 100 / m[i,3]
      
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
      cat("hedge1:",format(round(hedge1,2), nsmall = 2),
          "hedge2:",format(round(hedge2,2), nsmall = 2),
          "Current PL Okex:",format(round(hedge2,4), nsmall = 4),"\n")
    }
    else{
      if(m[i,4] < diff * 1.096 && trade_negative_difference == 1){
        min_acc <- min(acc_b_usd, acc_o_usd)
        
        price_b <- m[i,2]
        deal_b <- min_acc * acc_step_continue
        comm_b <- c(comm_b, deal_b * 0.002)
        temp <- acc_b_usd
        acc_b_usd <- temp - deal_b * 0.002 + deal_b
        acc_b_btc <- c(acc_b_btc, -deal_b / (m[i,2]/sl))
        
        price_o <- m[i,3]
        deal_o <- min_acc * acc_step_continue
        comm_o <- c(comm_o, deal_o * 0.0015)
        temp <- acc_o_usd
        acc_o_usd <- temp - deal_o * 0.0015 - deal_o
        acc_o_btc <- c(acc_o_btc, deal_o / (m[i,3]*sl))
        temp <- 0
        temp <- hedge2
        hedge2 <- temp + deal_o / 100
        order_btc_o <- hedge2 * 100 / m[i,3]
        
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
        cat("hedge1:",format(round(hedge1,2), nsmall = 2),
            "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
      }
      else{
        if(m[i,4] < hn && trade_positive_difference == 1 || i == nrow(m) && trade_positive_difference == 1){#Zakrivaem sdelki: pokupaem futures i prodaem bitfinex
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
              "Time in hours:",(end - start)/3600, "\n")
          cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
              "Okex price:", format(round(m[i,3],2), nsmall = 2),
              "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
          cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
              "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
              "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
              "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
          cat("hedge1:",format(round(hedge1,2), nsmall = 2),
              "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
          cat("Profit sum:", format(round(acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd,2), nsmall = 2),
              "Profit percent:", format(round((acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd)*100,2), nsmall = 2),"%\n", "\n")
          
          if(abs(acc_b_usd / acc_o_usd - 1) * 100 > 10){
            cat("Balance adjustment\n")
            temp <- acc_b_usd + acc_o_usd
            acc_b_usd <- temp / 2
            acc_o_usd <- temp / 2
            hedge1 <- acc_o_usd / 100
            cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
                "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
                "hedge1:",format(round(hedge1,2), nsmall = 2),"\n\n")
          }
          
          hedge2 <- 0
          trade <- 0
          trade_positive_difference <- 0
          start <- 0
          end <- 0
          temp <- 0
        
        }
        else if(m[i,4] > ln && trade_negative_difference == 1 || i == nrow(m) && trade_negative_difference == 1){
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
              "Time in hours:",(end - start)/3600, "\n")
          cat("Bitfinex price:", format(round(m[i,2],2), nsmall = 2),
              "Okex price:", format(round(m[i,3],2), nsmall = 2),
              "Difference:", format(round(m[i,4],2), nsmall = 2), "%\n")
          cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
              "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
              "Account Bitfinex BTC:", format(round(sum(acc_b_btc),2), nsmall = 2),
              "Account Okex BTC:", format(round(sum(acc_o_btc),2), nsmall = 2),"\n")
          cat("hedge1:",format(round(hedge1,2), nsmall = 2),
              "hedge2:",format(round(hedge2,2), nsmall = 2),"\n")
          cat("Profit sum:", format(round(acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd,2), nsmall = 2),
              "Profit percent:", format(round((acc_b_usd - s_acc_b_usd + acc_o_usd - s_acc_o_usd)/(s_acc_b_usd + s_acc_o_usd)*100,2), nsmall = 2),"%\n", "\n")
          
          if(abs(acc_b_usd / acc_o_usd - 1) * 100 > 10){
            cat("Balance adjustment\n")
            temp <- acc_b_usd + acc_o_usd
            acc_b_usd <- temp / 2
            acc_o_usd <- temp / 2
            hedge1 <- acc_o_usd / 100
            cat("Account Bitfinex USD:", format(round(acc_b_usd,2), nsmall = 2),
                "Account Okex USD:", format(round(acc_o_usd,2), nsmall = 2),
                "hedge1:",format(round(hedge1,2), nsmall = 2),"\n\n")
          }
          
          hedge2 <- 0
          trade <- 0
          trade_negative_difference <- 0
          start <- 0
          end <- 0
          temp <- 0
        }
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

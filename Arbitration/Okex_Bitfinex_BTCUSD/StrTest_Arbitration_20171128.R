library(dplyr)
library(plyr)
library(zoo)
library(forecast)

setwd("C:/btc/Strategy/Arbitration/BITF")
BITF <- read.csv("30-11-2017-all.csv", header = TRUE)
BITF <- subset(BITF, select = -c(X, TradeId, Amount, Type))
BITF <- aggregate(Price ~ Timestamp, data = BITF, mean)
BITF$Timestamp <- as.POSIXct(BITF$Timestamp, origin="1970-01-01", tz="GMT")
head(BITF,10)
# b <- read.csv("842_BITF_BTCUSD_20170829_20171017.csv", header = FALSE, sep = "\t")
# 
# s1 <- read.csv("2017-10-21-trades-BTCUSD (1).csv", header = TRUE, sep = ",")
# s2 <- read.csv("2017-10-21-trades-BTCUSD (2).csv", header = TRUE, sep = ",")
# s3 <- read.csv("2017-10-21-trades-BTCUSD (3).csv", header = TRUE, sep = ",")
# s4 <- read.csv("2017-10-21-trades-BTCUSD (4).csv", header = TRUE, sep = ",")
# s5 <- read.csv("2017-10-21-trades-BTCUSD (5).csv", header = TRUE, sep = ",")
# s6 <- read.csv("2017-10-21-trades-BTCUSD (6).csv", header = TRUE, sep = ",")
# s7 <- read.csv("2017-10-21-trades-BTCUSD (7).csv", header = TRUE, sep = ",")
# s8 <- read.csv("2017-10-21-trades-BTCUSD (8).csv", header = TRUE, sep = ",")
# s9 <- read.csv("2017-10-21-trades-BTCUSD (9).csv", header = TRUE, sep = ",")
# s10 <- read.csv("2017-10-21-trades-BTCUSD (10).csv", header = TRUE, sep = ",")
# s11 <- read.csv("2017-10-21-trades-BTCUSD (11).csv", header = TRUE, sep = ",")
# s12 <- read.csv("2017-10-21-trades-BTCUSD (12).csv", header = TRUE, sep = ",")
# 
# s <- s[order(s$Timestamp),]
# b <- b[order(b$V8),]
# head(s,10)
# s$Timestamp <- as.POSIXct(s$Timestamp, origin = "1970-01-01", tz = "GMT")
# max(s$Timestamp)
# s <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12)
# s$D <- as.Date(s$Timestamp)
# str(ss)
# b$D <- as.Date(b$V8)
# ss <- subset(s, D >= "2017-08-29")
# head(b,20)
# bb <- b[!duplicated(b), ]
# 
# aggregate(bb$V6, by=list(Date=bb$D), FUN=sum)
# aggregate(s$Amount, by=list(Date=s$D), FUN=sum)

setwd("C:/btc/Strategy/Arbitration/OKFT3M")
OKFT <- read.csv("1075_OKFT_BTCUSD3M_20170807_20171128.csv", header = FALSE, sep = "\t")
OKFT <- subset(OKFT, select = -c(V1, V2, V3, V4, V6, V7, V9))
OKFT <- aggregate(V5 ~ V8, data = OKFT, mean)
OKFT$V8 <- as.POSIXct(OKFT$V8, origin="1960-01-01", tz="GMT")
head(OKFT,10)
max(OKFT$V8)
max(BITF$Timestamp)

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

t1 <- Sys.time()
trade <- 0
start_deal <- 0
end_deal <- 0
start_bitf <- 0
start_okft <- 0
start_diff <- 0
start_acoount <- 4000
comission <- 0
profit <- c()
duration <- c()
prc <- c()
matr <- data.frame(matrix(ncol = 10, nrow = 0))
x <- c("StartDeal", "EndDeal", "StartBITF","StartOKFT","CloseBITF","CloseOKFT",
       "ProfitBITF","ProfitOKFT","Comission","Profit")
colnames(matr) <- x

for(i in 20:nrow(m)){
  if(trade == 0){
    if(m[i,3]-m[i,2]>m[i,3]*0.003+m[i,2]*0.004){
      if(m[i,4] > 4.0){
        trade <- 1
        #Prodaem futures i pokupaem bitfinex po 10 BTC
        start_bitf = m[i,2]
        start_okft = m[i,3]
        start_deal <- m[i,1]
        start_diff <- m[i,4]
        cat("Start deal at:", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), " ")
        }
      else next
    }
    else next
  }
  else{
    if(m[i,4] < 1.0){
      #Zakrivaem sdelki: pokupaem futures i prodaem bitfinex
      profit_bitf <- m[i,2] - start_bitf
      profit_okft <- start_okft - m[i,3]
      comission <- (m[i,2] + start_bitf)*0.002 + start_okft*0.003
      profit <- c(profit, profit_bitf + profit_okft - comission)
      prc <- c(prc, (profit_bitf + profit_okft - comission)/start_bitf)
      end_deal <- m[i,1]
      if ((end_deal - start_deal)/3600 < 48){
        duration <- c(duration, (end_deal - start_deal)/3600)
      }
      matr[nrow(matr)+1,] <- c(start_deal,end_deal,start_bitf,start_okft,m[i,2],m[i,3],profit_bitf,profit_okft,comission,profit_bitf + profit_okft - comission)
      cat("End deal at:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          "Time diff in hours:",(end_deal - start_deal)/3600,"\n",
          "StartBITF:", format(round(start_bitf,0), nsmall = 0),
          "StartOKFT:", format(round(start_okft,0), nsmall = 0),
          "CloseBITF:", format(round(m[i,2],0), nsmall = 0),
          "CloseOKFT:", format(round(m[i,3],0), nsmall = 0),
          "ProfitBITF:", format(round(profit_bitf,0), nsmall = 0),
          "ProfitOKFT:", format(round(profit_okft,0), nsmall = 0),
          "Comission:", format(round(comission,0), nsmall = 0),
          "Profit:", format(round(profit_bitf + profit_okft - comission,0), nsmall = 0), "\n")
      trade <- 0
      start_bitf <- 0
      start_okft <- 0
      start_deal <- 0
      end_deal <- 0
      comission <- 0
    }
  }
}
cat("Total deals:",length(profit))
cat("Total profit",sum(profit))
cat("Profit per deal",mean(profit))
cat("Average duration",mean(duration))
cat("Average prc",mean(prc))
t2 <- Sys.time()
t2-t1
#matr
#setwd("C:/btc/Strategy/Arbitration")
#write.csv(matr, "log.csv", row.names = FALSE, quote = FALSE)
#profit
#format(round(duration,4), nsmall = 4)

#--------------------------------------------------------------

t1 <- Sys.time()
trade <- 0
start_deal <- 0
end_deal <- 0
start_bitf <- 0
start_okft <- 0
comission <- 0
#profit <- c()
#duration <- c()
#prc <- c()
matr <- data.frame(matrix(ncol = 10, nrow = 0))
x <- c("StartDeal", "EndDeal", "StartBITF","StartOKFT","CloseBITF","CloseOKFT",
       "ProfitBITF","ProfitOKFT","Comission","Profit")
colnames(matr) <- x

for(i in 20:nrow(m)){
  if(trade == 0){
    if(TRUE){
      if(m[i,4] < -5.2){
        trade <- 1
        #Prodaem bitfinex i pokupaem futures po 10 BTC
        start_bitf = m[i,2]
        start_okft = m[i,3]
        start_deal <- m[i,1]
        cat("Start deal at:", as.character(as.POSIXct(m[i,1], origin = "1970-01-01")), " ")
      }
      else next
    }
    else next
  }
  else{
    if(m[i,4] > -3.2){
      #Zakrivaem sdelki: pokupaem futures i prodaem bitfinex
      profit_bitf <- -m[i,2] + start_bitf
      profit_okft <- -start_okft + m[i,3]
      comission <- (m[i,2] + start_bitf)*0.002 + start_okft*0.003
      profit <- c(profit, profit_bitf + profit_okft - comission)
      prc <- c(prc, (profit_bitf + profit_okft - comission)/start_bitf)
      end_deal <- m[i,1]
      if ((end_deal - start_deal)/3600 < 48){
        duration <- c(duration, (end_deal - start_deal)/3600)
      }
      matr[nrow(matr)+1,] <- c(start_deal,end_deal,start_bitf,start_okft,m[i,2],m[i,3],profit_bitf,profit_okft,comission,profit_bitf + profit_okft - comission)
      cat("End deal at:", as.character(as.POSIXct(m[i,1], origin = "1970-01-1")),
          "Time diff in hours:",(end_deal - start_deal)/3600,"\n",
          "StartBITF:", format(round(start_bitf,0), nsmall = 0),
          "StartOKFT:", format(round(start_okft,0), nsmall = 0),
          "CloseBITF:", format(round(m[i,2],0), nsmall = 0),
          "CloseOKFT:", format(round(m[i,3],0), nsmall = 0),
          "ProfitBITF:", format(round(profit_bitf,0), nsmall = 0),
          "ProfitOKFT:", format(round(profit_okft,0), nsmall = 0),
          "Comission:", format(round(comission,0), nsmall = 0),
          "Profit:", format(round(profit_bitf + profit_okft - comission,0), nsmall = 0), "\n")
      trade <- 0
      start_bitf <- 0
      start_okft <- 0
      start_deal <- 0
      end_deal <- 0
      comission <- 0
    }
  }
}
cat("Total deals:",length(profit))
cat("Total profit",sum(profit))
cat("Profit per deal",mean(profit))
cat("Average duration",mean(duration))
cat("Average prc",mean(prc))
t2 <- Sys.time()
t2-t1

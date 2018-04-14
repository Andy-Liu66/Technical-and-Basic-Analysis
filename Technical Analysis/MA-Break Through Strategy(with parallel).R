rm(list=ls())
time1 <- Sys.time()

Sys.setlocale(locale = "cht")
library(readr)
library(dplyr)
library(lubridate)
library(quantmod)
library(ggplot2)
library(TTR)
library(parallel)
load("~/stockprice_data_practice.Rdata")   #2014~2017

stockprice_data <- arrange(stockprice_data, code, date)

#------------initial parameter-------------------
position_increase <-  F   #決定是否加碼(可同時持有多張同公司的股票)
transaction_cost <- 0.00585
#------------initial parameter-------------------
stockprice_data$open <- as.numeric(as.character(stockprice_data$open))
stockprice_data$volume <- as.numeric(stockprice_data$volume)
stockprice_data$market_value <- as.numeric(stockprice_data$market_value)
company_code <- unique(stockprice_data$code)

# stockprice_data <- stockprice_data %>% group_by(code) %>% filter(n()>=60) %>%
#   mutate(kbar = close - open) %>%
#   mutate(MA5 = EMA(close, 5)) %>%
#   mutate(MA10 = EMA(close, 10)) %>%
#   mutate(MA20 = EMA(close, 20)) %>%
#   mutate(MA60 = EMA(close, 60)) %>%
#   mutate(RSI_5 = RSI(close, 5, maType="EMA")) %>%
#   mutate(entangle = as.numeric(((abs(MA5 - MA10) / MA10) <= 0.05) &
#                                  (((abs(lag(MA5, 5) - lag(MA10, 5))) / lag(MA10, 5)) <= 0.1) &
#                                  (((abs(lag(MA10, 5) - lag(MA20, 5))) / lag(MA20, 5)) <= 0.15))) %>%
#   mutate(kbar_size = kbar / close) %>%
#   mutate(upper_line = abs(close - high)) %>%
#   mutate(MA5_signal = as.numeric((close - MA5) > 0)) %>%
#   mutate(MA5_volume = EMA(volume, 5))

stockprice_data <- stockprice_data %>% group_by(code) %>% filter(n()>=60) %>%
  mutate(kbar = close - open) %>%
  mutate(MA5 = EMA(close, 5)) %>%
  mutate(MA10 = EMA(close, 10)) %>%
  mutate(MA20 = EMA(close, 20)) %>%
  mutate(MA60 = EMA(close, 60)) %>%
  mutate(RSI_5 = RSI(close, 5, maType="EMA")) %>%
  mutate(entangle = as.numeric((abs(MA5 - MA10) <= 0.5) &
                                 (((abs(lag(MA5, 10) - lag(MA10, 10)))) <= 0.5) &
                                 (((abs(lag(MA10, 10) - lag(MA20, 10)))) <= 0.5))) %>%
  mutate(kbar_size = kbar / close) %>%
  mutate(upper_line = abs(close - high)) %>%
  mutate(MA5_signal = as.numeric((close - MA5) > 0)) %>%
  mutate(MA5_turn_up = as.numeric(MA5 > lag(MA5, 1))) %>%
  mutate(MA5_volume = EMA(volume, 5))

#------------決定回測年份-------------------
stockprice_data$year <- year(stockprice_data$date)
# stockprice_data <- stockprice_data %>% filter(year %in% c(2015, 2016, 2017))
#------------決定回測年份-------------------

#---------------------------------------------------------
myCoreNums <- detectCores() - 1                    # 判斷電腦有幾個核心數
cl <- makeCluster(myCoreNums)                  # 啟用平行運算(建議啟用核心數為電腦總核心數-1，讓電腦至少有1個核心能做其他事情)
clusterExport(cl, c("stockprice_data", "company_code", "position_increase"))  # 匯入變數至cl內
clusterEvalQ(cl, c(library(dplyr), library(lubridate), library(TTR)))  

keyprogram <- function(i){
  temp_data <- stockprice_data %>% filter(code == company_code[i])
  temp_data$date <- temp_data$date %>% as.Date(, format = "%Y/%m/%d")
  condition_in <- data.frame(date = 0)
  condition_in$date <- 0
  condition_in <- temp_data %>% filter(kbar_size >= 0.07,
                                       upper_line <= 0.01,
                                       MA5_signal == 1,
                                       MA5_turn_up == 1,
                                       entangle == 1,
                                       volume >= 2 * MA5_volume,
                                       volume >= 300,
                                       RSI_5 > 95)

  condition_out <- temp_data %>% filter((close < MA20) | (kbar_size <= -0.05))

  if(length(condition_in$date) == 0){return(NULL)}
  
  final_index <- apply(sapply(condition_in$date,function(x) x - condition_out$date), 2, function(y) min(which(y < 0))) #可能會出現warings，代表到資料結束前都無出場日，則以資料最後
  
  condition_out <- condition_out[final_index, ]
  
  condition_out[which(is.na(condition_out$code) == T),] <- temp_data[nrow(temp_data), ]
  
  if(position_increase == F){
    condition_in <- condition_in[which(duplicated(condition_out) == F), ]
    condition_out <- condition_out[which(duplicated(condition_out) == F), ]
  }else{
    ondition_in <- condition_in
    condition_out <- condition_out
  }

  result <- cbind(condition_in %>% select(date, close), condition_out %>% select(date, close)) %>% mutate(return = (close1 - close) / close,
                                                                                                          holding_days = date1 - date)

}


Result <- parLapply(cl, 1:length(company_code), keyprogram)
trade_detail <- setNames(do.call(rbind.data.frame, Result), c("code", "date_in", "close_in", "code", "date_out", "close_out", "return", "holding_days"))
trade_detail <- trade_detail[,-4]

stopCluster(cl)

time2 <- Sys.time()
time2 - time1

#----------------績效----------------------
winning_rate <- length(which(trade_detail$return > 0)) / nrow(trade_detail)
print(winning_rate)

mean(trade_detail$return[which(trade_detail$return > 0)])
mean(trade_detail$return[which(trade_detail$return < 0)])
#----------------績效----------------------

#----------------報酬分布分顏色----------------------
trade_detail <- trade_detail[,-4] %>% mutate(return_type = return > 0)

draw <- trade_detail %>% filter(return > 0)
ggplot(data = draw, aes(draw$return)) + 
  geom_histogram(breaks = seq(min(draw$return), max(draw$return), 0.01),
                 col = "white", 
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "#E88492", high = "#F70727")



draw <- trade_detail %>% filter(return < 0)
ggplot(data = draw, aes(draw$return)) + 
  geom_histogram(breaks = seq(min(draw$return), max(draw$return), 0.002),
                 col = "white", 
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "#5BEB8E", high = "#49B36E")
#----------------報酬分布分顏色----------------------

##-------------繪圖追蹤----------------
company <- 6187
load("C:/Users/user/Desktop/Technical Analysis/stockprice_data.Rdata")
#-------------參數設定-------------
# start_day <- as.Date("2016-03-04", format="%Y/%m/%d")
# end_day <- as.Date("2016-04-26", format="%Y/%m/%d")
colnames(stockprice_data) <- c("Code", "Date", "Open", "High", "Low", "Close", "Volume", "Market_Value")
trading_company_figure <- filter(stockprice_data, Code == company) %>% arrange(Date)
trading_company_figure <- na.omit(trading_company_figure)
trading_company_figure <- xts(trading_company_figure, order.by=as.Date(trading_company_figure$Date, format="%Y/%m/%d"))[,-2]
storage.mode(trading_company_figure) <- "numeric"
#-------------參數設定-------------
chartSeries(trading_company_figure,
            name = "6187萬潤",
            TA=c(addVo(),
                 addEMA(n=5, with.col = "Close", col="red"),
                 addEMA(n=10, with.col = "Close", col="orange"),
                 addEMA(n=20, with.col = "Close", col="black")),
            subset='2014-01-15::2014-03-18',
            theme=chartTheme('white' , fg.col = "black", grid.col = "grey", 
                             dn.up.border = "black",dn.dn.col = "red",up.up.col = "red"), 
            dn.col="green",up.col="red")
##-------------繪圖追蹤----------------
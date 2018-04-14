rm(list=ls())
time1 <- Sys.time()
Sys.setlocale(locale = "cht")
library(dplyr)
library(lubridate)
library(TTR)
library(zoo)
library(ggplot2)
library(parallel)
load("~/stockprice_data.Rdata")

company_code <- unique(stockprice_data$code)

#------------initial parameter-------------------
position_increase <- F    #決定是否加碼(可同時持有多張通公司的股票)
transaction_cost <- 0.00585
highest_high_days <- 40   #突破近兩個月新高才進場
lowest_low_days <- 10
#------------initial parameter-------------------

#
stockprice_data$open <- as.numeric(as.character(stockprice_data$open))
stockprice_data$volume <- as.numeric(stockprice_data$volume)
stockprice_data$market_value <- as.numeric(stockprice_data$market_value)
company_code <- unique(stockprice_data$code)

stockprice_data <- stockprice_data %>% group_by(code) %>% filter(n() > 100) %>%
  mutate(highest_high = c(rep(NA, (highest_high_days - 1)) ,rollapply(close, highest_high_days, max))) %>%
  mutate(lowest_low = c(rep(NA, (lowest_low_days - 1)), rollapply(close, lowest_low_days, min))) %>% 
  mutate(mean_high_low = (highest_high + lowest_low) / 2) %>% 
  mutate(MA5_volume = EMA(volume, 5))


#------------Main Program-------------------------
keyprogram <- function(i){
  temp_data <- stockprice_data %>% filter(code == company_code[i])
  temp_data$date <- temp_data$date %>% as.Date(, format = "%Y/%m/%d")
  condition_in <- data.frame(date = 0)
  condition_in$date <- 0
  
  condition_in <- temp_data %>% filter(close == highest_high, 
                                       volume >= 1.5 * MA5_volume)
  
  #condition_in$date <- as.Date(condition_in$date, format= "%Y/%m/%d")
  
  condition_out <- temp_data %>% filter(close < mean_high_low)
  #condition_out$date <- as.Date(condition_out$date, format = "%Y/%m/%d")
  
  if(length(condition_in$date) == 0){return(NULL)}
  
  
  final_index <- apply(sapply(condition_in$date,function(x) x - condition_out$date), 2, function(y) min(which(y < 0))) #可能會出現warings，代表到資料結束前都無出場日，則以資料最後
 
  #final_index[which(final_index == Inf)] <- nrow(temp_data)
  
  condition_out <- condition_out[final_index, ]
  
  condition_out[which(is.na(condition_out$code) == T),] <- temp_data[nrow(temp_data), ]
  
  # if(final_index[length(final_index)] == Inf){
  #   final_index[length(final_index)] <- nrow(temp_data)
  #   condition_out <- rbind(condition_out[final_index[-length(final_index)], ], temp_data[final_index[length(final_index)], ])
  # }else{
  #   condition_out <- condition_out[final_index, ]
  # }
  
  if(position_increase == F){
    condition_in <- condition_in[which(duplicated(condition_out) == F), ]
    condition_out <- condition_out[which(duplicated(condition_out) == F), ]
  }else{
    ondition_in <- condition_in
    condition_out <- condition_out
  }
  
  result <- cbind(condition_in %>% select(date, close), condition_out %>% select(date, close)) %>% mutate(return = ((close1 - close) / close) - transaction_cost,
                                                                                                          holding_days = date1 - date)
}

myCoreNums <- detectCores()                      # 判斷電腦有幾個核心數
cl <- makeCluster(myCoreNums)                  # 啟用平行運算(建議啟用核心數為電腦總核心數-1，讓電腦至少有1個核心能做其他事情)
clusterExport(cl, c("stockprice_data", "company_code", "position_increase", "transaction_cost"))  # 匯入變數至cl內
clusterEvalQ(cl, c(library(dplyr), library(lubridate), library(TTR))) 

Result <- parLapply(cl, 1:length(company_code), keyprogram)
trade_detail <- setNames(do.call(rbind.data.frame, Result), c("code", "date_in", "close_in", "code", "date_out", "close_out", "return", "holding_days"))

stopCluster(cl)

# peformance <- data.frame(winning_rate = length(which(trade_detail$return > 0 ))/ nrow(trade_detail),
#                          positive_mean = mean(trade_detail[which(trade_detail$return > 0),]$return), 
#                          negative_mean = mean(trade_detail[which(trade_detail$return <= 0),]$return), 
#                          annual_return = mean(c(positive_mean, negative_mean) / mean(trade_detail$holding_days) * 252))

time2 <- Sys.time()
time2 - time1

##-------------繪圖追蹤----------------
company <-5011 
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
            TA=c(addVo(), 
                 addBBands(), 
                 addMACD( histogram = TRUE),
                 addEMA(n=5, with.col = "Close", col="red"),
                 addEMA(n=10, with.col = "Close", col="orange"),
                 addEMA(n=20, with.col = "Close", col="black")),
            subset='2008-07-01::2008-07-21',
            theme=chartTheme('white'), 
            dn.col="green",up.col="red")
##-------------繪圖追蹤----------------
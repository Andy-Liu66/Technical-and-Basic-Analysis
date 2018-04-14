rm(list=ls())
time1 <- Sys.time()
# require(compiler)
# enableJIT(0)

Sys.setlocale(locale = "cht")
library(readr)
library(dplyr)
library(lubridate)
library(quantmod)
library(ggplot2)
load("~/stockprice_data.Rdata")
stockprice_data <- arrange(stockprice_data, code, date)

company_code <- unique(stockprice_data$code)

#------------initial parameter-------------------
position_increase <- F    #決定是否加碼(可同時持有多張通公司的股票)
transaction_cost <- 0.00585
#------------initial parameter-------------------


trading_table <- data.frame()
for(i in 1:length(company_code)){
  ##----------資料整理-----------------
  code_i <- company_code[i]
  cat(sprintf("目前正在回測股票代碼 %d 公司，進度：%d / %d \n", code_i, i, length(company_code)))
  
  trading_company <- filter(stockprice_data, code == code_i) %>% arrange(date)
  trading_company <- na.omit(trading_company)
  trading_date <- as.Date(trading_company$date, format="%Y/%m/%d")
  trading_company <- xts(trading_company, order.by=as.Date(trading_company$date, format="%Y/%m/%d"))[,-1]
  storage.mode(trading_company) <- "numeric"
  trading_company$day_index <- seq(1,nrow(trading_company),1)
  if(nrow(trading_company) <= 252){next}
  
  # trading_company <- trading_company[-which(trading_company$market_value == 0),]
  ##----------資料整理-----------------
  
  
  ##----------基本型態設定---------------
  #股價報酬  (trading_company$close-lag(trading_company$close, 1))/trading_company$close
  
  #K棒
  trading_company$kbar <- (trading_company$close - trading_company$open)
  
  #EMA5
  trading_company$MA5 <- EMA(trading_company$close, 5)
  
  #EMA5上彎信號
  #trading_company$MA5_signal <- as.numeric(trading_company$MA5 - lag(trading_company$MA5, 1) > 0)
  trading_company$MA5_signal <- as.numeric(trading_company$close - trading_company$MA5 > 0)
  
  #EMA10
  trading_company$MA10 <- EMA(trading_company$close, 10)
  
  #EMA10上彎信號
  trading_company$MA10_signal <- as.numeric(trading_company$MA10 - lag(trading_company$MA10, 1) > 0)
  
  #EMA20
  trading_company$MA20 <- EMA(trading_company$close, 20)
  
  #EMA60
  trading_company$MA60 <- EMA(trading_company$close, 60)
  
  #RSI,5
  trading_company$RSI_5 <- RSI(trading_company$close, 5, maType="EMA")
  
  #5天內急下降趨勢
  #trading_company$downtrend_5 <- as.numeric(((trading_company$close - lag(trading_company$close, 5)) / lag(trading_company$close, 5)) <= -0.04)
  trading_company$downtrend_10 <- as.numeric(((trading_company$close - lag(trading_company$close, 10))/ 10 <= -0.6) & 
                                              ((trading_company$close - lag(trading_company$close, 10))/ 10 >= -5))
  
  #10均線糾結(不好)
  # trading_company$MA10_high <- EMA(trading_company$high, 10)
  # trading_company$MA10_low <- EMA(trading_company$low, 10)
  # trading_company$entangle <- as.numeric((lag(trading_company$close, 1) < lag(trading_company$MA10, 1)) &
  #                                        (trading_company$close > trading_company$MA10) & 
  #                                          (trading_company$MA10_high - trading_company$MA10_low) < 2)
  

  # #10均線糾結(調整後寫法)
  trading_company$entangle <- as.numeric(((abs(trading_company$MA5 - trading_company$MA10) / trading_company$MA10) <= 0.05) &
                                           (((abs(lag(trading_company$MA5, 5) - lag(trading_company$MA10, 5))) / lag(trading_company$MA10, 5)) <= 0.1) &
                                           (((abs(lag(trading_company$MA10, 5) - lag(trading_company$MA20, 5))) / lag(trading_company$MA20, 5)) <= 0.15))
  
  #股價高於10
  #trading_company$bigger_10 <- as.numeric(trading_company$close > 10)

  
                                         
  #前五日交易量平均
  trading_company$MA5_volume <- as.numeric(EMA(trading_company$volume, 5))
  ##----------基本型態設定---------------
  
  
  ##----------技術分析格局---------------
  # #--------Yo Sen(Adjusted)--------
  condition_in <- t(as.matrix(which(((trading_company$kbar / trading_company$close) >= 0.05) &
          (trading_company$high - trading_company$close <= 0.01) &
          (trading_company$MA5_signal == 1) &
            (trading_company$entangle == 1) &
            (trading_company$volume >= 2*trading_company$MA5_volume) &
            (trading_company$volume >= 300) &
            (trading_company$RSI_5 > 95))))
  
  # & 
  #   (trading_company$bigger_10 == 1)

  condition_out <- which((trading_company$close < trading_company$MA10)|
                           ((trading_company$kbar / trading_company$close) <= -0.04))
  
  # mean_return annual_return avg_holding_days trading_num winning_rate
  # 0.007057776      0.223046         7.973958        1344    0.3854167
  # #--------Yo Sen(Adjusted)--------
  
  
  #--------Hammer--------
  # trading_company$hammer <- as.numeric((trading_company$kbar > 0 & abs(trading_company$high - trading_company$close) <= 0.1)|
  #                                        (trading_company$kbar < 0 & abs(trading_company$high - trading_company$open) <= 0.1))
  # 
  # condition_in <- t(as.matrix(which((trading_company$hammer == 1) &
  #                                     ((abs(trading_company$low - trading_company$close) >= 3*abs(trading_company$kbar))|
  #                                        (abs(trading_company$low - trading_company$open) >= 3*abs(trading_company$kbar))) &
  #                                     (trading_company$downtrend_10 == 1) &
  #                                     trading_company$close < lag(trading_company$low, 1))))
  # 
  # condition_out <- which(trading_company$close < trading_company$MA10)
  # 
  # mean_return annual_return avg_holding_days trading_num winning_rate
  # 8.348965e-05   0.009576892         2.196891         193    0.4611399
  #--------Hammer--------
  
  
  #--------Teacher's Strategy(Adjusted)--------
  # condition_in <- t(as.matrix(which(((trading_company$close > trading_company$MA5) & (lag(trading_company$close, 1) < lag(trading_company$MA5, 1))) &
  #                                     (trading_company$close > trading_company$MA10) &
  #                                     (trading_company$close > trading_company$MA20) &
  #                                     (trading_company$close > trading_company$MA60) &
  #                                     ((trading_company$kbar / trading_company$close) > 0.05) &
  #                                     (trading_company$volume >= 2*trading_company$MA5_volume) &
  #                                     (trading_company$volume >= 500))))
  # 
  # condition_out <- which((trading_company$close < trading_company$MA10)|
  #                          (trading_company$kbar / trading_company$close) <= -0.04)
  #--------Teacher's Strategy(Adjusted)--------
  
  
  ##----------技術分析格局---------------
  
  
  ##----------進出場資料處理---------------
  #若無任何符合進場的條件則跳過
  if(length(condition_in) < 1){next}
  
  #以下目的為找出最接近進場日的出場日
  #(1)自訂function，使每個進場日減去出場日
  cloest <- function(x){
    x - condition_out
  }
  test <- apply(condition_in, 2, cloest)
  
  #(2)自訂function，找出由正數轉負數的臨界點(意即找出離進場日最近的出場日，若到最後一天無出場日，則以資料最後一天出場)
  find_position <- function(y){
    min(which(y < 0))
  }
  final_index <- apply(test, 2, find_position) #可能會出現warings，則代表到資料結束前都無出場日，則以資料最後一天出場
  condition_out <- condition_out[final_index]
  condition_out[which(is.na(condition_out)==T)] <- condition_in[length(condition_in)]
  
  #將原先matrix轉成numeric
  condition_in <- as.numeric(condition_in)
  
  #以下為處理是否加碼的問題
  if(position_increase == F){
    condition_in <- condition_in[which(duplicated(condition_out) == F)]
    condition_out <- condition_out[which(duplicated(condition_out) == F)]
  }else{
    ondition_in <- condition_in
    condition_out <- condition_out
  }
  ##----------進出場資料處理---------------

  ##----------交易紀錄表---------------
  aaa <- data.frame(company_code = code_i,
                    buy_date = trading_date[condition_in],
                    sell_date = trading_date[condition_out],
                    buy_price = as.numeric(trading_company$close[condition_in]),
                    sell_price= as.numeric(trading_company$close[condition_out]),
                    return = ((as.numeric(trading_company$close[condition_out]) - as.numeric(trading_company$close[condition_in])) / as.numeric(trading_company$close[condition_in])) - transaction_cost,
                    buy_date_index = as.numeric(trading_company$day_index[condition_in]),
                    sell_date_index = as.numeric(trading_company$day_index[condition_out]))
  trading_table <- rbind(trading_table, aaa)
  ##----------交易紀錄表---------------
}#外層for loop

##-------------紀錄報酬----------------
trading_table$holding_days <- trading_table$sell_date_index - trading_table$buy_date_index +1
print(data.frame(mean_return = mean(trading_table$return),
                 annual_return = mean(trading_table$return)/mean(trading_table$holding_days)*252,
                 avg_holding_days = mean(trading_table$holding_days),
                 trading_num = nrow(trading_table),
                 winning_rate = length(trading_table$return[which(trading_table$return > 0)])/nrow(trading_table))) #可以決定報酬多少以上才算勝

hist(trading_table$return, breaks = 100)
# hist(trading_table$return[which(trading_table$return > 0)])
# hist(trading_table$return[which(trading_table$return <=  0)])
##-------------紀錄報酬----------------
time2 <- Sys.time()
time2-time1

positive <- trading_table[which(trading_table$return > 0),]
negative <- trading_table[which(trading_table$return <= 0),]
mean(positive$return)
mean(positive$holding_days)
mean(negative$return)
mean(negative$holding_days)

##-------------各年績效追蹤----------------
#製作年份標籤
year <- year(trading_table$buy_date)
trading_table$year <- year(trading_table$buy_date)

#trading_table_year用來檢視各年績效
trading_table_year <- trading_table %>% group_by(year) %>% summarise(mean_return = mean(return),
                                                                     mean_holding_days = mean(holding_days))
                                                                                                                                       
#計算各年年化報酬
trading_table_year$year_return <- (trading_table_year$mean_return / trading_table_year$mean_holding_days) *252 

#計算各年勝率
total_trading_num <- trading_table %>% group_by(year) %>% summarise(total_trading_num = n())
positive_return_num <- trading_table %>% filter(return > 0) %>% group_by(year) %>% summarise(positive_return_num = n())
trading_table_year$trading_number <- total_trading_num$total_trading_num
trading_table_year$winning_rate <- (positive_return_num$positive_return_num / total_trading_num$total_trading_num)

#計算各年平均賣出價格
mean_of_sell_price <- trading_table %>% group_by(year) %>% summarise(mean(sell_price))
trading_table_year$average_sell_price <- mean_of_sell_price$`mean(sell_price)`

#繪圖
#技術分析部分
ggplot(trading_table_year, aes(year, year_return)) + 
  geom_line() + 
  scale_x_continuous(breaks = trading_table_year$year) + 
  geom_point(aes(size = winning_rate), alpha = 1/2, col="red")

#技術分析部分vs大盤
Market_Performance <- read_csv("C:/Users/user/Desktop/Technical Analysis/Market_Performance.csv")
Market_Performance <- Market_Performance[c(1:10), c(1:3)]
colnames(Market_Performance) <- c("code", "year", "return")
Market_Performance$return <- (Market_Performance$return / 100)

test <- inner_join(Market_Performance, trading_table_year, by="year")

ggplot(test, aes(year)) + 
  geom_line(aes(y = return, colour = "大盤表現"), size=1.5, alpha = 0.5) + 
  geom_line(aes(y=year_return, colour = "技術分析策略"), size=1.5, alpha = 0.5) + 
  scale_x_continuous(breaks = test$year) + 
  geom_point(aes(size = winning_rate, y=year_return), alpha = 1, col="grey") + 
  ggtitle("技術分析策略 vs 大盤表現") + 
  theme(legend.position = c(0.9, 0.8), plot.title = element_text(size = 20, face = "bold")) + 
  labs(x = "年份", y = "年化報酬率", col = "比較對象", size = "技術分析勝率")
##-------------各年績效追蹤----------------


##-------------繪圖追蹤----------------
company <-3685 
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
            subset='2015-03-10::2015-05-05',
            theme=chartTheme('white'), 
            dn.col="green",up.col="red")
##-------------繪圖追蹤----------------

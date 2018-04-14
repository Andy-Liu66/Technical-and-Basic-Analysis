rm(list=ls())
Sys.setlocale(locale = "cht")
load("~/stockprice_data.Rdata")
load("~/Report_Price_Category.Rdata")
rm(StockPrice)

library(dplyr)
library(lubridate)
#-----------------------資料整理------------------------
stockprice_data$date <- stockprice_data$date %>% as.Date(, format = "%Y/%m/%d")

test <- FinancialReport %>% filter(year == 2015)

data_2015 <- test %>% group_by(code) %>% select(PBR, PE, EPS, ROE) %>% mutate(PBR = mean(PBR, na.omit = T),
                                                                PE = mean(PE, na.omit = T), 
                                                                EPS = mean(EPS, na.omit = T), 
                                                                ROE = mean(ROE, na.omit = T)) %>% distinct() %>% mutate(is_TDR = code - 10000)
data_2015 <- data_2015[-which(data_2015$is_TDR > 0),]

#-----------------------開始投資------------------------
temp <- data_2015 %>% arrange(desc(ROE))  #PBR, PE, desc(ROE), desc(EPS)

target <- stockprice_data %>% filter(code %in% temp[1:30, ]$code)

target <- target %>% mutate(year = year(date)) %>% filter(year == c(2016, 2017))

company_code <- unique(target$code)

program <- function(i){
  temp_data <- target %>% filter(code == company_code[i])
  in_position <- head(temp_data, 1)
  out_position <- tail(temp_data, 1)
  result <- cbind(in_position %>% select(code, date, close), out_position %>% select(date, close))
}

result <- setNames(do.call(rbind.data.frame, lapply(1:length(company_code), program)), 
                   c("Code", "Date_in", "Close_in", "Date_out", "Close_out"))

result <- result %>% mutate(return = (Close_out - Close_in) / Close_in, 
                            money_earned = (Close_out - Close_in) * 1000)
hist(result$return, breaks = 10)

winning_rate <- length(which(result$return > 0)) / nrow(result)
print(winning_rate)

mean_return <- mean(result$return)
print(mean_return)

total_money_earn <- sum(result$money_earned)
print(total_money_earn)
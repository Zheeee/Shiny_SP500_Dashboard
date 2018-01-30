library(googleVis)
library(quantmod)
library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(TTR)
library(data.table)
library(zoo)
library(shinydashboard)
library(DT)
library(textshape)
library(plotly)
library(tidyr)
library(rsconnect)
library(dygraphs)

# Load stock, sector information and indicators
cf = read.csv('./data/con_f.csv', stringsAsFactors = F)
con_cf = read.csv('./data/con.csv', stringsAsFactors = F)
stocks = read.csv('./data/all_stocks_1yr.csv', stringsAsFactors = F)

# Load sector data
a = "XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLRE,XLK,XLU,VOX"
b = strsplit(a, split = ',')[[1]]

av_api = function(symble, itv = 'TIME_SERIES_DAILY', opz = 'compact') {
  http = 'https://www.alphavantage.co/query?function='
  b = '&symbol='
  c = '&outputsize='
  d = '&apikey=RJZ9H4BTXR3YW06Q&datatype=csv'
  api = paste0(c(http, itv, b, symble, c, opz, d), collapse = '')
  av = read.csv(api)
  return(av)
}

sector_list = lapply(b, function(x) av_api(x, opz = 'full'))
sec_row_num = sapply(sector_list, nrow)
sector_name = rep(b, sec_row_num)
sector_data = do.call(rbind, sector_list)
sector_data$name = sector_name
spy_temp = read.csv('./data/SPY.csv', stringsAsFactors = F)
spy = spy_temp %>% 
  select(., -(Adj.Close))
spy$Name = rep('SPY', nrow(spy))
write.csv(sector_data, file = './data/sector_data.csv',row.names = F)

# Make stocks with Sector
temp = con_cf %>% 
  mutate(., name = gsub('Industrials', 'XLI', x = Sector, fixed = T)) %>% 
  mutate(., name = gsub('Health Care', 'XLV', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Information Technology', 'XLK', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Consumer Staples', 'XLP', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Energy', 'XLE', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Financials', 'XLF', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Materials', 'XLB', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Real Estate', 'XLRE', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Utilities', 'XLU', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Consumer Discretionary', 'XLY', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Telecommunications Services', 'VOX', x = name, fixed = T)) 
temp = temp %>% 
  select(., Name = Symbol, Sector = name)
stocks_w_sec = stocks %>% 
  left_join(., temp, by = "Name")
stocks_w_spy = stocks_w_sec %>% 
  select(., -c(Sector)) %>% 
  rbind(., spy)
write.csv(stocks_w_sec, file = './data/stocks_w_sec.csv',row.names = F)

# Make indicator with Sector
temp = cf %>% 
  mutate(., name = gsub('Industrials', 'XLI', x = Sector, fixed = T)) %>% 
  mutate(., name = gsub('Health Care', 'XLV', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Information Technology', 'XLK', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Consumer Staples', 'XLP', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Energy', 'XLE', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Financials', 'XLF', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Materials', 'XLB', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Real Estate', 'XLRE', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Utilities', 'XLU', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Consumer Discretionary', 'XLY', x = name, fixed = T)) %>% 
  mutate(., name = gsub('Telecommunications Services', 'VOX', x = name, fixed = T)) 
indict_w_sec = temp %>% 
  subset(., select = -c(SEC.Filings, Name, Sector)) %>% 
  rename(., Sector = name) %>% 
  na.aggregate.default(., FUN = median, na.rm = T)
indict_w_sec$Price = as.numeric(indict_w_sec$Price)
indict_w_sec$Dividend.Yield = as.numeric(indict_w_sec$Dividend.Yield)
indict_w_sec$Price.Earnings = as.numeric(indict_w_sec$Price.Earnings)
indict_w_sec$Earnings.Share = as.numeric(indict_w_sec$Earnings.Share)
indict_w_sec$Book.Value = as.numeric(indict_w_sec$Book.Value)
indict_w_sec$X52.week.low = as.numeric(indict_w_sec$X52.week.low)
indict_w_sec$X52.week.high = as.numeric(indict_w_sec$X52.week.high)
indict_w_sec$Market.Cap = as.numeric(indict_w_sec$Market.Cap)
indict_w_sec$EBITDA = as.numeric(indict_w_sec$EBITDA)
indict_w_sec$Price.Sales = as.numeric(indict_w_sec$Price.Sales)
indict_w_sec$Price.Book = as.numeric(indict_w_sec$Price.Book)
write.csv(indict_w_sec, file = './data/indict_w_sec.csv',row.names = F)

df_add = data.frame(Name = c('S&P500'), Sector = c(NA),
                    Volume = c(71955600), day_ret = c(0.66))
# get the return of stock for the treemap
# ret_stock = stocks_w_sec %>%
#   arrange(., desc(Date)) %>%
#   mutate(., day_ret = (Close - Open) / Open * 100) %>%
#   select(., Name, Sector, Volume, day_ret, Date) %>%
#   filter(., is.null(day_ret) == F) %>%
#   filter(., Date == '2017-08-11') %>%
#   na.omit(.) %>%
#   select(., Name, Sector, Volume, day_ret)
# 
# ret_sec = sector_data %>%
#   mutate(., day_ret = (close - open) / open * 100) %>%
#   select(., Name = name, Date = timestamp, Volume = volume, day_ret) %>%
#   filter(., is.null(day_ret) == F) %>%
#   group_by(., Name) %>%
#   top_n(., 1, wt = Date) %>%
#   mutate(., Sector = 'S&P500') %>%
#   select(., Name, Sector, Volume, day_ret)
# 
# ret_sec_stock = rbind.data.frame(ret_stock, ret_sec)
# rownames(ret_sec_stock) = 1:nrow(ret_sec_stock)
# 
# ret_day = rbind(ret_sec_stock, df_add) %>%
#   rename(., Parent = Sector) %>%
#   arrange(., Parent)
# ret_day$Name = as.factor(ret_day$Name)
# ret_day$Parent = as.factor(ret_day$Parent)

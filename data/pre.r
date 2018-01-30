library(dplyr)
library(zoo)

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

sector_list = lapply(b, function(x) av_api(x))
sector_name = rep(b, rep(100, 11))
sector_data = do.call(rbind, sector_list)
sector_data$name = sector_name
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
write.csv(indict_w_sec, file = './data/indict_w_sec.csv',row.names = F) 

# Setup ########################################################################
source("R/dependencies.R")
data_path <-"C:/Finance/WS23/RMM/Data"

# Get Data #####################################################################
data_eq <- read_excel(paste0(data_path, "/MarketData.xlsx"), sheet="Equity")
data_cr <- read_excel(paste0(data_path, '/MarketData.xlsx'), sheet = "Crypto")
data_fx <- read_excel(paste0(data_path, '/MarketData.xlsx'), sheet = "FX")

## Process FX
data_fx <- data_fx %>% 
  mutate("USDEUR"= 1/EURUSD,
         "GBPEUR"=GBPUSD/EURUSD,
         "JPYEUR" = JPYUSD/EURUSD,
         "EUREUR" = 1) %>% 
  select(Date, USDEUR, GBPEUR, JPYEUR, EUREUR)

## Join all together
price_data <- data_eq %>%
  left_join(., data_cr, by = "Date") %>% 
  left_join(.,data_fx, by = "Date")

## Get Portfolio Data
pf_eq <- read_excel(paste0(data_path, "/Portfolio.xlsx"),sheet="Equity") %>% select(PositionSize, Ticker)
pf_cr <- read_excel(paste0(data_path, "/Portfolio.xlsx"), sheet="Crypto") %>% select(PositionSize, Ticker)
fx_info <- read_excel(paste0(data_path, "/MarketData.xlsx"),sheet="Overview") %>% select(Ticker,currency=`Product Currency`, asset_class =`Asset Class` )
portfolio_data <- pf_eq %>% rbind(.,pf_cr) %>% 
  left_join(fx_info, by = "Ticker") %>% 
  mutate(currency = paste0(currency, "EUR"))

## create price data complete in EUR
# price_data_eur <- price_data %>% 
#   select(-c(USDEUR, GBPEUR, JPYEUR, EUREUR)) %>% 
#   pivot_longer(., cols = colnames(.)[colnames(.)!="Date"], names_to = "Ticker") %>% 
#   left_join(., portfolio_data %>% select(Ticker, currency), by = "Ticker") %>% 
#   
#   left_join(., price_data %>% 
#               select(Date, USDEUR, GBPEUR, JPYEUR, EUREUR) %>% 
#               pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
#               select(Date, name, fx=value),
#             by = c("Date"="Date", "currency"="name")) %>% 
#   filter(!is.na(fx)) %>% 
#   mutate(value = value / fx) %>% 
#   select(Date, Ticker, value) %>% 
#   pivot_wider(names_from = Ticker, values_from = value) %>% 
#   left_join(., price_data %>% 
#          select(Date, USDEUR, GBPEUR, JPYEUR, EUREUR),
#        by = "Date")

################################################################################
# Simulation Start #
################################################################################
## dates
end_date = "2023-06-30" %>% as.Date() 
start_date = "2020-01-01" %>% as.Date()


## Calculate PnL

PRICES <- price_data %>% filter(Date>=start_date) %>% filter(Date <= end_date) %>% select(-Date)
Port_PRICES <- PRICES[,1:11]
FX_PRICES <- PRICES[,-c(1:11)]
FX_PRICES <- FX_PRICES[, match(portfolio_data$currency, colnames(FX_PRICES))]
FX_PRICES <- 1/FX_PRICES 
Port_PRICES <- Port_PRICES * FX_PRICES
PnL <- CHANGE(as.matrix(Port_PRICES) %*% as.numeric(portfolio_data$PositionSize))









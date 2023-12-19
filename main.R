# Setup ########################################################################
source("R/dependencies.R")
source("R/CONFIG.R")
source("R/MonteCarlo.R")

# Get Data #####################################################################
data_eq <- read_excel(paste0(DATA_PATH, "/MarketData.xlsx"), sheet="Equity")
data_cr <- read_excel(paste0(DATA_PATH, '/MarketData.xlsx'), sheet = "Crypto")
data_fx <- read_excel(paste0(DATA_PATH, '/MarketData.xlsx'), sheet = "FX")

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
pf_eq <- read_excel(paste0(DATA_PATH, "/Portfolio.xlsx"),sheet="Equity") %>% select(PositionSize, Ticker)
pf_cr <- read_excel(paste0(DATA_PATH, "/Portfolio.xlsx"), sheet="Crypto") %>% select(PositionSize, Ticker)
fx_info <- read_excel(paste0(DATA_PATH, "/MarketData.xlsx"),sheet="Overview") %>% select(Ticker,currency=`Product Currency`, asset_class =`Asset Class` )
portfolio_data <- pf_eq %>% rbind(.,pf_cr) %>% 
  left_join(fx_info, by = "Ticker") %>% 
  mutate(currency = paste0(currency, "EUR"))

################################################################################
# Simulation Start #
################################################################################
## dates
end_date = "2023-06-30" %>% as.Date() 
start_date = "2020-01-01" %>% as.Date()

## Calculate PnL
PRICES <- price_data %>% filter(Date>=start_date-1) %>% filter(Date <= end_date) %>% select(-Date)
Port_PRICES <- PRICES[,1:11]
FX_PRICES <- PRICES[,-c(1:11)]
FX_PRICES <- FX_PRICES[, match(portfolio_data$currency, colnames(FX_PRICES))]
FX_PRICES <- 1/FX_PRICES 
Port_PRICES <- Port_PRICES * FX_PRICES
PnL <- CHANGE(as.matrix(Port_PRICES) %*% as.numeric(portfolio_data$PositionSize)) %>% na.omit()

## Simple Gausian VaR
simpleGaus_df <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "gaus",
                                 level = 0.01)

simpleGaus_weighted_df <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "gaus", 
                                 cov_method = "weighted", level = 0.01)


## Simple Students t VaR
simpleStudents_df <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "t_simple",
                                 level = 0.01)

# Visualize results ############################################################
Eval_df <- simpleGaus_df %>% 
  left_join(., simpleGaus_weighted_df %>% select(Date, Gaus_weighted = VaR_gaus), by = "Date") %>% 
  left_join(., simpleStudents_df, by ="Date")

Eval_df$PnL <- PnL

p <- Eval_df %>% 
  ggplot(.) + 
  geom_line(aes(x=Date, y = VaR_gaus, color = "VaR_gaus")) +
  geom_line(aes(x=Date, y = VaR_t_simple, color = "VaR_t_simple")) +
  geom_line(aes(x=Date, y = Gaus_weighted, color = "VaR_gaus_w")) +
  geom_point(aes(x=Date, y = PnL, color = "PnL")) + 
  theme_tq()

p %>% ggplotly()

## Check for breaches
mean(Eval_df$VaR_gaus>Eval_df$PnL, na.rm=T)
mean(Eval_df$Gaus_weighted>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_t_simple>Eval_df$PnL, na.rm=T)

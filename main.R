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
start_date2 = "2022-07-15" %>% as.Date()

## Calculate PnL
PRICES <- price_data %>% filter(Date>=start_date-1) %>% filter(Date <= end_date) %>% select(-Date)
Port_PRICES <- PRICES[,1:11]
FX_PRICES <- PRICES[,-c(1:11)]
FX_PRICES <- FX_PRICES[, match(portfolio_data$currency, colnames(FX_PRICES))]
FX_PRICES <- 1/FX_PRICES 
Port_PRICES <- Port_PRICES * FX_PRICES
PnL <- CHANGE(as.matrix(Port_PRICES) %*% as.numeric(portfolio_data$PositionSize)) %>% na.omit()

## Simple Gausian VaR
simpleGaus_df_100 <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "gaus",
                                 level = 0.01) %>% select(Date, VaR_gaus_100 = VaR_gaus)

simpleGaus_df_250 <- MonteCarlo_main(prices=price_data, window=250,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data, sim_method = "gaus",
                                     level = 0.01) %>% select(Date, VaR_gaus_250 = VaR_gaus)

simpleGaus_weighted_df <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "gaus", 
                                 cov_method = "weighted", level = 0.01, extending = TRUE) %>%
  select(Date, VaR_gaus_weighted = VaR_gaus)


## Simple Students t VaR
simpleStudents_df_100 <- MonteCarlo_main(prices=price_data, window=100,
                                 start_date=start_date, end_date=end_date,
                                 portfolio_data=portfolio_data, sim_method = "t_simple",
                                 level = 0.01) %>%
  select(Date, VaR_t_100 = VaR_t_simple)

simpleStudents_df_250 <- MonteCarlo_main(prices=price_data, window=250,
                                         start_date=start_date, end_date=end_date,
                                         portfolio_data=portfolio_data, sim_method = "t_simple",
                                         level = 0.01) %>%
  select(Date, VaR_t_250 = VaR_t_simple)

simpleStudents_weighted_df <- MonteCarlo_main(prices=price_data, window=100,
                                         start_date=start_date, end_date=end_date,
                                         portfolio_data=portfolio_data, sim_method = "t_simple",
                                         cov_method = "weighted", level = 0.01, extending = TRUE) %>%
  select(Date, VaR_t_weighted = VaR_t_simple)


# Gaus Residuals
gaußResiduals_df <- MonteCarlo_main(prices=price_data, window=100,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data, sim_method = "gausResiduals",
                                     level = 0.01, extending = TRUE)
save(gaußResiduals_df, file="gaußResiduals_df.RData")
## Simple historical simulation
historical_df <- MonteCarlo_main(prices=price_data, window=100,
                                    start_date=start_date, end_date=end_date,
                                    portfolio_data=portfolio_data, sim_method = "historical",
                                    level = 0.01, extending = TRUE, N = 250)

# Visualize results ############################################################
Eval_df <- simpleGaus_df_100 %>% 
  left_join(., simpleGaus_df_250, by = "Date") %>% 
  left_join(., simpleGaus_weighted_df, by = "Date") %>% 
  left_join(., simpleStudents_df_100, by ="Date") %>% 
  left_join(., simpleStudents_df_250, by ="Date") %>% 
  left_join(., simpleStudents_weighted_df, by ="Date") %>% 
  left_join(., gaußResiduals_df, by ="Date") %>% 
  left_join(., historical_df, by ="Date") 
  

Eval_df$PnL <- PnL

p <- Eval_df %>% 
  ggplot(.) + 
  geom_line(aes(x=Date, y = VaR_gaus_100, color = "VaR_gaus_100")) +
  geom_line(aes(x=Date, y = VaR_gaus_250, color = "VaR_gaus_250")) +
  geom_line(aes(x=Date, y = VaR_gaus_weighted, color = "VaR_gaus_weighted")) +
  
  geom_line(aes(x=Date, y = VaR_t_100, color = "VaR_t_100")) +
  geom_line(aes(x=Date, y = VaR_t_250, color = "VaR_t_250")) +
  geom_line(aes(x=Date, y = VaR_t_weighted, color = "VaR_t_weighted")) +
  
  geom_line(aes(x=Date, y = VaR_gausResiduals, color = "VaR_gausResiduals")) +
  
  geom_line(aes(x=Date, y = VaR_historical, color = "VaR_historical")) +
  geom_point(aes(x=Date, y = PnL, color = "PnL")) + 
  theme_tq() + ylab("PnL / VaR")

p %>% ggplotly()

## Check for breaches
mean(Eval_df$VaR_gaus_100>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_gaus_250>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_gaus_weighted>Eval_df$PnL, na.rm=T)

mean(Eval_df$VaR_t_100>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_t_250>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_t_weighted>Eval_df$PnL, na.rm=T)

mean(Eval_df$VaR_historical>Eval_df$PnL, na.rm=T)



portfolio_data_crypto <- portfolio_data %>% filter(asset_class == "Crypto")
price_data

## Simple Gausian VaR
simpleGaus_df_100_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data_crypto, sim_method = "gaus",
                                     level = 0.01) %>% select(Date, VaR_gaus_100 = VaR_gaus)

simpleGaus_df_250_crypto <- MonteCarlo_main(prices=price_data, window=250,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data_crypto, sim_method = "gaus",
                                     level = 0.01) %>% select(Date, VaR_gaus_250 = VaR_gaus)

simpleGaus_weighted_df_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                          start_date=start_date, end_date=end_date,
                                          portfolio_data=portfolio_data_crypto, sim_method = "gaus", 
                                          cov_method = "weighted", level = 0.01, extending = TRUE) %>%
  select(Date, VaR_gaus_weighted = VaR_gaus)


## Simple Students t VaR
simpleStudents_df_100_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                         start_date=start_date, end_date=end_date,
                                         portfolio_data=portfolio_data_crypto, sim_method = "t_simple",
                                         level = 0.01, max_df = 5) %>%
  select(Date, VaR_t_100 = VaR_t_simple)

simpleStudents_df_250_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                         start_date=start_date, end_date=end_date,
                                         portfolio_data=portfolio_data_crypto, sim_method = "t_simple",
                                         level = 0.01, max_df = 5) %>%
  select(Date, VaR_t_250 = VaR_t_simple)

simpleStudents_weighted_df_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                              start_date=start_date, end_date=end_date,
                                              portfolio_data=portfolio_data_crypto, sim_method = "t_simple",
                                              cov_method = "weighted", level = 0.01, extending = TRUE,
                                              max_df = 5) %>%
  select(Date, VaR_t_weighted = VaR_t_simple)
# 
# 
# # Gaus Residuals
# gaußResiduals_df <- MonteCarlo_main(prices=price_data, window=100,
#                                     start_date=start_date, end_date=end_date, cov_method = "weighted",
#                                     portfolio_data_crypto=portfolio_data_crypto, sim_method = "gausResiduals",
#                                     level = 0.01, extending = TRUE)
# save(gaußResiduals_df, file="gaußResiduals_df.RData")
# 
# gaußResiduals_df_weighted <- MonteCarlo_main(prices=price_data, window=100,
#                                              start_date=start_date, end_date=end_date,
#                                              portfolio_data_crypto=portfolio_data_crypto, sim_method = "gausResiduals",
#                                              level = 0.01, extending = TRUE)
# 
# save(gaußResiduals_df_weighted, file="gaußResiduals_df_weighted.RData")
# 
# # t Residuals
# tResiduals_df <- MonteCarlo_main(prices=price_data, window=100,
#                                  start_date=start_date, end_date=end_date,
#                                  portfolio_data_crypto=portfolio_data_crypto, sim_method = "tResiduals",
#                                  level = 0.01, extending = TRUE, max_df = 5)
# save(tResiduals_df, file="tResiduals_df.RData")

## Simple historical simulation
historical_df_252_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data_crypto, sim_method = "historical",
                                     level = 0.01, extending = TRUE, N = 252) %>% select(Date, VaR_historical_252 = VaR_historical)

historical_df_504_crypto <- MonteCarlo_main(prices=price_data, window=100,
                                     start_date=start_date, end_date=end_date,
                                     portfolio_data=portfolio_data_crypto, sim_method = "historical",
                                     level = 0.01, extending = TRUE, N = 504) %>% select(Date, VaR_historical_504 = VaR_historical)

# Visualize results ############################################################
Eval_df <- simpleGaus_df_100_crypto %>% 
  left_join(., simpleGaus_df_250_crypto, by = "Date") %>% 
  left_join(., simpleGaus_weighted_df_crypto, by = "Date") %>% 
  left_join(., simpleStudents_df_100_crypto, by ="Date") %>% 
  left_join(., simpleStudents_df_250_crypto, by ="Date") %>% 
  left_join(., simpleStudents_weighted_df_crypto, by ="Date") %>% 
  #left_join(., gaußResiduals_df, by ="Date") %>% 
  #left_join(., tResiduals_df, by ="Date") %>% 
  left_join(., historical_df_252_crypto, by ="Date") %>% 
  left_join(., historical_df_504_crypto, by ="Date")

PnL <- CHANGE(as.matrix(Port_PRICES[,7:11]) %*% as.numeric(portfolio_data_crypto$PositionSize)) %>% na.omit()

Eval_df$PnL <- PnL

p <- Eval_df %>% 
  ggplot(.) + 
  geom_line(aes(x=Date, y = VaR_gaus_100, color = "VaR_gaus_100")) +
  geom_line(aes(x=Date, y = VaR_gaus_250, color = "VaR_gaus_250")) +
  geom_line(aes(x=Date, y = VaR_gaus_weighted, color = "VaR_gaus_weighted")) +

  geom_line(aes(x=Date, y = VaR_t_100, color = "VaR_t_100")) +
  geom_line(aes(x=Date, y = VaR_t_250, color = "VaR_t_250")) +
  geom_line(aes(x=Date, y = VaR_t_weighted, color = "VaR_t_weighted")) +

  #geom_line(aes(x=Date, y = VaR_gausResiduals, color = "VaR_gausResiduals")) +
  #geom_line(aes(x=Date, y = VaR_tResiduals, color = "VaR_tResiduals")) +

  geom_line(aes(x=Date, y = VaR_historical_252, color = "VaR_historical_250")) +
  geom_line(aes(x=Date, y = VaR_historical_504, color = "VaR_historical_500")) +
  
  geom_point(aes(x=Date, y = PnL, color = "PnL")) + 
  theme_tq() + ylab("PnL / VaR") +
  ggtitle("Value at Risk - Backtest")

p %>% ggplotly()

## Check for breaches
mean(Eval_df$VaR_gaus_100>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_gaus_250>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_gaus_weighted>Eval_df$PnL, na.rm=T)

mean(Eval_df$VaR_t_100>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_t_250>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_t_weighted>Eval_df$PnL, na.rm=T)

mean(Eval_df$VaR_gausResiduals>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_tResiduals>Eval_df$PnL, na.rm=T)

mean(Eval_df$VaR_historical_252>Eval_df$PnL, na.rm=T)
mean(Eval_df$VaR_historical_504>Eval_df$PnL, na.rm=T)


## Check for breaches
sum(Eval_df$VaR_gaus_100>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_gaus_250>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_gaus_weighted>Eval_df$PnL, na.rm=T)

sum(Eval_df$VaR_t_100>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_t_250>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_t_weighted>Eval_df$PnL, na.rm=T)

sum(Eval_df$VaR_gausResiduals>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_tResiduals>Eval_df$PnL, na.rm=T)

sum(Eval_df$VaR_historical_252>Eval_df$PnL, na.rm=T)
sum(Eval_df$VaR_historical_504>Eval_df$PnL, na.rm=T)

## Average VaR
mean(Eval_df$VaR_gaus_100, na.rm=T)
mean(Eval_df$VaR_gaus_250, na.rm=T)
mean(Eval_df$VaR_gaus_weighted, na.rm=T)

mean(Eval_df$VaR_t_100, na.rm=T)
mean(Eval_df$VaR_t_250, na.rm=T)
mean(Eval_df$VaR_t_weighted, na.rm=T)

mean(Eval_df$VaR_gausResiduals, na.rm=T)
mean(Eval_df$VaR_tResiduals, na.rm=T)

mean(Eval_df$VaR_historical_252, na.rm=T)
mean(Eval_df$VaR_historical_504, na.rm=T)


p <- data.frame(
  "method"=Eval_df %>% select(-Date, -PnL) %>% colnames(),
  "meanVaR" = Eval_df %>% select(-Date, -PnL) %>% apply(.,2,function(x){mean(x, na.rm=T)}) %>% as.numeric(),
  "Breaches"=Eval_df %>% select(-Date, -PnL) %>% apply(.,2,function(x){mean(x>Eval_df$PnL, na.rm=T)}) %>% as.numeric()
) %>% 
  ggplot(.,aes(x=(meanVaR), y = Breaches, color =method, label=method)) +
  geom_point(aes(x=(meanVaR), y = Breaches, color =method)) +
  ggtitle("Value at Risk - Backtest (Contribution)") +
  theme_tq() + xlab("mean VaR") +
  ggtitle("average VaR vs Breaches")

ggplotly(p)



################################################################################
# Visualize contribution #
################################################################################
## Simple Gausian VaR
simpleGaus_df_100_contrib <- MonteCarlo_main(prices=price_data, window=100,
                                             start_date=start_date, end_date=end_date,
                                             portfolio_data=portfolio_data, sim_method = "gaus",
                                             contrib = T,
                                             level = 0.01)


p <- simpleGaus_df_100_contrib %>%
  as.data.frame() %>% 
  pivot_longer(., cols = colnames(.)[colnames(.)!= "Date"]) %>% 
  ggplot(.) +
  geom_col(aes(x=Date, y = value, fill = name))+ 
  theme_tq() + ylab("PnL / VaR") +
  ggtitle("Value at Risk - Backtest (Contribution)")

ggplotly(p)  


dollar_size <- sweep(Port_PRICES, 2, portfolio_data$PositionSize,  `*`)
weighted_contrib <- (1/dollar_size[-1,]) * simpleGaus_df_100_contrib[,-1]
weighted_contrib <- sweep(1/Port_PRICES[-1,], 2, simpleGaus_df_100_contrib[,-1],  `*`)



p <- weighted_contrib %>% 
  as.data.frame() %>% 
  mutate("Date"=simpleGaus_df_100_contrib$Date) %>% 
  pivot_longer(., cols = colnames(.)[colnames(.)!= "Date"]) %>% 
  group_by(name) %>% 
  arrange(Date) %>% 
  mutate(value = SMA(value, 252)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  ggplot(.) +
  geom_line(aes(x = Date, y=value, color = name))+ 
  theme_tq() + ylab("VaR per EUR") +
  ggtitle("Value at Risk - Backtest (Contribution)")

ggplotly(p)  



weighted_contrib %>% apply(., 2, mean) %>% round(.,3)
weighted_contrib %>% apply(., 2, sd) %>% round(.,3)

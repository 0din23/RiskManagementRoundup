MonteCarlo_main <- function(prices, window, start_date, end_date, portfolio_data, sim_method = "gaus"){
  
  # Extract range
  DateRange <- prices %>% filter(Date>=start_date) %>% filter(Date <= end_date) %>% pull(Date)
  
  # Cycle through
  VaR <- DateRange %>% 
    lapply(., function(chunck_date){
    
      data_chunk <- prices %>% filter(Date <= chunck_date) %>% tail(.,window)
      VaR <- applyMonteCarlo(price_df=data_chunk, portfolio = portfolio_data, sim_method = sim_method)
      print(paste0("Date: ", chunck_date, " VaR: ", VaR))
      return(VaR)
    }) %>% unlist()
  
  ## combine 
  res <- data.frame(
    "Date"=DateRange,
    "VaR"=VaR
  )
  colnames(res) <- c("Date", paste0("VaR_", sim_method))
  return(res)
}


################################################################################
# MONTE CARLO VAR FUNCTION #
################################################################################

applyMonteCarlo <- function(price_df, portfolio, N = 10000, level = 0.05, sim_method){
  
  ## Calculate base price
  base_price <- price_df %>% select(-Date) %>% tail(.,1) %>% t() %>% as.data.frame() %>% mutate(Ticker = rownames(.)) %>% select(Ticker, Price = V1)
  portfolio <- portfolio %>% left_join(., base_price, by = "Ticker")
  portfolio <- portfolio %>% left_join(., base_price %>% select(Ticker, FX_RATE = Price), by=c("currency" = "Ticker")) 
  pVal <- sum(portfolio$PositionSize * portfolio$Price * 1/(portfolio$FX_RATE))
  
  ## Simulate Returns
  if(sim_method == "gaus"){
    SIM_RETURNS <- gausReturn(prices %>% select(-EUREUR), mean = NULL, log = TRUE, N = N) 
  } else if(sim_method == "t_simple"){
    SIM_RETURNS <- tStudendReturn_simple(prices %>% select(-EUREUR), mean = NULL, log = TRUE, N = N) 
  }
  
  ## Calculate PnL
  PRETURNS <- sweep(1+SIM_RETURNS[,1:11] , 2, base_price$Price[1:11], `*`)
  CRETURNS <- sweep(1+SIM_RETURNS[,-c(1:11)] , 2, base_price$Price[12:14], `*`) 
  CRETURNS <- cbind(CRETURNS,rep(1,nrow(CRETURNS)))
  colnames(CRETURNS) <- c(colnames(CRETURNS)[1:3],"EUREUR")
  CRETURNS <- CRETURNS[, match(portfolio$currency, colnames(CRETURNS))]
  CRETURNS <- 1 / CRETURNS
  RETURNS <- CRETURNS * PRETURNS
  PnL <- (RETURNS %*% portfolio$PositionSize) - pVal
  
  return(quantile(PnL, probs = level) %>% as.numeric())
}

################################################################################
# Return Generation # 
################################################################################

# Gauß Returns
gausReturn <- function(prices, mean = NULL, log = TRUE, N){
  
  ## Calculate Returns
  if(log){
    returns <-  prices %>% 
    pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
    group_by(name) %>% 
    mutate(value = log(1 + RETURN(value))) %>% 
    ungroup() %>% 
    pivot_wider(names_from = name, values_from=value) %>% 
    select(-Date)
  } else {
    returns <-  prices %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
      group_by(name) %>% 
      mutate(value = RETURN(value)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = name, values_from=value) %>% 
      select(-Date)
  }
  
  ## Estimate Covariance Matrix and decomposition
  CoMa <- returns %>% standardEstimator(.)
  
  ## Calculate Cholesky decomposition
  Cholesky <- chol(CoMa)
  
  # Generate standard normal
  temp_sim <- data.frame(matrix(rnorm(n = N*ncol(CoMa)), ncol = ncol(CoMa)))
  colnames(temp_sim) <- colnames(CoMa)
  
  ## apply cholesky
  SIM_RETURNS <- as.matrix(temp_sim) %*% Cholesky
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  return(SIM_RETURNS)
}

# Students t Returns simple version
tStudendReturn_simple <- function(prices, mean = NULL, log = TRUE, N){
  
  ## Calculate Returns
  if(log){
    returns <-  prices %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
      group_by(name) %>% 
      mutate(value = log(1 + RETURN(value))) %>% 
      ungroup() %>% 
      pivot_wider(names_from = name, values_from=value) %>% 
      select(-Date)
  } else {
    returns <-  prices %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
      group_by(name) %>% 
      mutate(value = RETURN(value)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = name, values_from=value) %>% 
      select(-Date)
  }
  
  ## Excess kurtosis
  excess_kurtosis <- apply(returns, 2, function(x) {
    x <- na.omit(x)
    n <- length(x)
    mean_x <- mean(x)
    s <- sd(x)
    sum(((x - mean_x)/s)^4)/n - 3
  })
  
  ## Estimate Covariance Matrix
  CoMa <- returns %>% standardEstimator(.)
  
  ## Calibrate CoMa
  degrees_of_freedom <- (6 / excess_kurtosis) + 4
  degrees_of_freedom <- round(mean(degrees_of_freedom))
  
  CoMa <- (degrees_of_freedom-2)/(degrees_of_freedom)*  CoMa
  
  # Generate standard normal
  temp_sim <- data.frame(matrix(rnorm(n = N*ncol(CoMa)), ncol = ncol(CoMa)))
  colnames(temp_sim) <- colnames(CoMa)
  
  ## Calculate Cholesky decomposition
  Cholesky <- chol(CoMa)
  
  ## apply cholesky
  SIM_RETURNS <- as.matrix(temp_sim) %*% Cholesky
  
  ## Simulate Chi squared
  chi_sim <- matrix(rnorm(n = N*degrees_of_freedom)^2, ncol = degrees_of_freedom) %>% rowSums()
  chi_sim <- 1/sqrt(chi_sim/degrees_of_freedom)
  
  ## Scale Returns 
  SIM_RETURNS <- sweep(SIM_RETURNS , 1, as.matrix(chi_sim), `*`)
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  return(SIM_RETURNS)
}

################################################################################
# COV Standard Estimator #
################################################################################
 standardEstimator <- function(returns){
   
   returns %>%
     apply(.,2,function(x){
       res <- sweep(returns , 1, as.matrix(x), `*`)
       colSums(res, na.rm=T) / apply(res, 2,function(c){length(na.omit(x))})
       })
   
 }


MonteCarlo_main <- function(prices, window, start_date, end_date, portfolio_data,
                            sim_method = "gaus", cov_method = "standard", level = 0.01){
  
  # Extract range
  DateRange <- prices %>% filter(Date>=start_date) %>% filter(Date <= end_date) %>% pull(Date)
  
  # Cycle through
  VaR <- DateRange %>% 
    lapply(., function(chunck_date){
    
      data_chunk <- prices %>% filter(Date <= chunck_date) %>% tail(.,window)
      VaR <- applyMonteCarlo(price_df=data_chunk, portfolio = portfolio_data, 
                             sim_method = sim_method, cov_method=cov_method, 
                             level=level)
      print(paste0("Date: ", chunck_date, " VaR: ", VaR))
      return(VaR)
    }) %>% unlist()
  
  ## combine 
  res <- data.frame(
    "Date"=DateRange,
    "VaR"=VaR %>% lag()
  )
  colnames(res) <- c("Date", paste0("VaR_", sim_method))
  return(res)
}


################################################################################
# MONTE CARLO VAR FUNCTION #
################################################################################

applyMonteCarlo <- function(price_df, portfolio, N = 10000, level = 0.01, sim_method, cov_method){
  
  ## Calculate base price
  base_price <- price_df %>% select(-Date) %>% tail(.,1) %>% t() %>% as.data.frame() %>% mutate(Ticker = rownames(.)) %>% select(Ticker, Price = V1)
  portfolio <- portfolio %>% left_join(., base_price, by = "Ticker")
  portfolio <- portfolio %>% left_join(., base_price %>% select(Ticker, FX_RATE = Price), by=c("currency" = "Ticker")) 
  pVal <- sum(portfolio$PositionSize * portfolio$Price * 1/(portfolio$FX_RATE))
  
  ## Simulate Returns
  if(sim_method == "gaus"){
    SIM_RETURNS <- gausReturn(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method) 
  } else if(sim_method == "t_simple"){
    SIM_RETURNS <- tStudendReturn_simple(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method) 
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
gausReturn <- function(prices, mean = NULL, log = TRUE, N, cov_method="standard"){
  
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
  CoMa <- returns %>% covEstimator(., cov_method)
  
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
tStudendReturn_simple <- function(prices, mean = NULL, log = TRUE, N, cov_method="standard"){
  
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
  excess_kurtosis <- apply(returns, 2, function(x) {kurtosis(na.omit(x))-3})
  degrees_of_freedom <- (6/excess_kurtosis) + 4
  degrees_of_freedom <- ifelse(degrees_of_freedom <4, 4,degrees_of_freedom)
  degrees_of_freedom <- round(mean(degrees_of_freedom, na.rm=TRUE))
  
  ## Estimate Covariance Matrix
  CoMa <- returns %>% covEstimator(., cov_method)
  CoMa <- (degrees_of_freedom-2)/(degrees_of_freedom) *  CoMa
  
  # Generate standard normal
  temp_sim <- data.frame(matrix(rnorm(n = N*ncol(CoMa)), ncol = ncol(CoMa)))
  colnames(temp_sim) <- colnames(CoMa)
  
  ## Calculate Cholesky decomposition
  Cholesky <- chol(CoMa)
  
  ## apply cholesky
  SIM_RETURNS <- as.matrix(temp_sim) %*% Cholesky
  
  ## Simulate Chi squared
  chi_sim <- rchisq(n = N*degrees_of_freedom, df = degrees_of_freedom)
  chi_sim <- sqrt(chi_sim /degrees_of_freedom)
  chi_sim <- 1/chi_sim
  
  ## Transform to Students T distribution
  SIM_RETURNS <- sweep(SIM_RETURNS , 1, as.matrix(chi_sim), `*`)
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  return(SIM_RETURNS)
}

################################################################################
# COV Estimator #
################################################################################

## Wrapper for the different covariance estimators
covEstimator <- function(returns, cov_method){
  if(cov_method == "standard"){
    standardEstimator(returns)
  } else if(cov_method == "weighted"){
    weightedEstimator(returns)    
  }
}


standardEstimator <- function(returns){
   
   returns %>%
     apply(.,2,function(x){
       res <- sweep(returns , 1, as.matrix(x), `*`)
       colSums(res, na.rm=T) / (apply(res, 2,function(c){length(na.omit(c))})-1)
       })
   
}

weightedEstimator <- function(returns, lambda = 0.25){
  
  returns %>%
    apply(.,2,function(x){
      
      res <- sweep(returns , 1, as.matrix(x), `*`)
      n_res <- apply(res, 2,function(c){length(na.omit(c))})
      lambda_vec <-  apply(res, 2,function(c){(1-lambda)^(n_res[1]:1-1)})
      res <- sweep(na.omit(res) , 1, lambda_vec, `*`)
      colSums(res, na.rm=T) * (lambda)
    })
  
}


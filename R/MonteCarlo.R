MonteCarlo_main <- function(prices, window, start_date, end_date, portfolio_data,
                            sim_method = "gaus", cov_method = "standard", level = 0.01,
                            extending = FALSE, residual_cov_lag = 100, N=100000,
                            contrib = FALSE, max_df = 10){
  
  # Extract range
  DateRange <- prices %>% filter(Date>=start_date) %>% filter(Date <= end_date) %>% pull(Date)
  
  # Cycle through
  if(!extending){
      VaR <- DateRange %>% 
        lapply(., function(chunck_date){
        
          data_chunk <- prices %>% filter(Date <= chunck_date) %>% tail(.,window)
          VaR <- applyMonteCarlo(price_df=data_chunk, portfolio = portfolio_data, 
                                 sim_method = sim_method, cov_method=cov_method, 
                                 level=level, N=N, max_df=max_df, contrib = contrib)
          print(paste0("Date: ", chunck_date, " VaR: ", VaR))
          return(VaR)
        })
      if(!contrib){VaR <- VaR %>% unlist()}else{VaR <- VaR %>% rbindlist()}
  } else{
    VaR <- DateRange %>% 
      lapply(., function(chunck_date){
        
        data_chunk <- prices %>% filter(Date <= chunck_date) %>% na.omit()
        VaR <- applyMonteCarlo(price_df=data_chunk, portfolio = portfolio_data, 
                               sim_method = sim_method, cov_method=cov_method, 
                               level=level, residual_cov_lag = residual_cov_lag, N=N,
                               max_df = max_df, contrib = contrib)
        print(paste0("Date: ", chunck_date, " VaR: ", VaR))
        
        return(VaR)
      })
    if(!contrib){VaR <- VaR %>% unlist()}else{VaR <- VaR %>% rbindlist()}
  }

  
  ## combine 
  if(!contrib){
    res <- data.frame(
      "Date"=DateRange,
      "VaR"=VaR %>% lag()
    )
    colnames(res) <- c("Date", paste0("VaR_", sim_method)) 
  } else {
    res <- data.frame(
      "Date"=DateRange,
      VaR
    )
  }
  return(res)
}


################################################################################
# MONTE CARLO VAR FUNCTION #
################################################################################
applyMonteCarlo <- function(price_df, portfolio, N = 10000, level = 0.01, sim_method, cov_method,
                            residual_cov_lag = 100, contrib = FALSE, max_df = 10){
  
  ## Calculate base price
  base_price <- price_df %>% select(-Date) %>% tail(.,1) %>% t() %>% as.data.frame() %>% mutate(Ticker = rownames(.)) %>% select(Ticker, Price = V1)
  portfolio <- portfolio %>% left_join(., base_price, by = "Ticker")
  portfolio <- portfolio %>% left_join(., base_price %>% select(Ticker, FX_RATE = Price), by=c("currency" = "Ticker")) 
  positions <- portfolio$PositionSize * portfolio$Price * 1/(portfolio$FX_RATE)
  pVal <- sum(positions)
  
  ## Simulate Returns
  if(sim_method == "gaus"){
    SIM_RETURNS <- gausReturn(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method) 
  } else if(sim_method == "t_simple"){
    SIM_RETURNS <- tStudendReturn_simple(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method, max_df=max_df) 
  } else if(sim_method == "gausResiduals"){
    SIM_RETURNS <- gausResiduals(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method, residual_cov_lag = residual_cov_lag) 
  } else if(sim_method == "historical"){
    SIM_RETURNS <-histSimulation(price_df %>% select(-EUREUR), N = N) 
  } else if(sim_method == "tResiduals"){
    SIM_RETURNS <- tResiduals(price_df %>% select(-EUREUR), mean = NULL, log = TRUE, N = N, cov_method, residual_cov_lag = residual_cov_lag,
                              max_df = max_df) 
  }
  
  ## Calculate PnL
  # browser()
  PRETURNS <- sweep(1+SIM_RETURNS[,1:11] , 2, base_price$Price[1:11], `*`)
  CRETURNS <- sweep(1+SIM_RETURNS[,-c(1:11)] , 2, base_price$Price[12:14], `*`) 
  CRETURNS <- cbind(CRETURNS,rep(1,nrow(CRETURNS)))
  colnames(CRETURNS) <- c(colnames(CRETURNS)[1:3],"EUREUR")
  CRETURNS <- CRETURNS[, match(portfolio$currency, colnames(CRETURNS))]
  CRETURNS <- 1 / CRETURNS
  PRETURNS <- PRETURNS %>% as.data.frame() %>% select(all_of(portfolio$Ticker)) %>% as.matrix()
  RETURNS <- CRETURNS * PRETURNS
  colnames(RETURNS) <- colnames(PRETURNS)
  
  if(!contrib){
    PnL <- (as.matrix(RETURNS) %*% portfolio$PositionSize) - pVal
    return(quantile(PnL, probs = level, na.rm = T) %>% as.numeric()) 
  } else {
    PnL <- sweep(as.matrix(RETURNS) , 2, portfolio$PositionSize, `*`) 
    PnL <- sweep(PnL , 2, positions, `-`) 
    PnL_total <- PnL %>% rowSums()
    VaR <- sort(PnL_total)
    VaR <- VaR[ceiling(level * length(VaR))]
    check_col <- PnL_total == VaR
    VaR_contrib <- PnL[check_col, ]
    VaR_contrib <- VaR_contrib %>% t() %>%  as.data.frame()
    return(VaR_contrib)
  }
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
tStudendReturn_simple <- function(prices, mean = NULL, log = TRUE, N, cov_method="standard", max_df =10){
  
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
  print(paste0("T-Dist df: ", degrees_of_freedom, " - max. df: ", max_df))
  if(!is.null(max_df)){degrees_of_freedom <- ifelse(degrees_of_freedom > max_df, max_df,degrees_of_freedom)}
  
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
  chi_sim <- rchisq(n = N, df = degrees_of_freedom)
  chi_sim <- sqrt(chi_sim /degrees_of_freedom)
  chi_sim <- 1/chi_sim
  
  ## Transform to Students T distribution
  SIM_RETURNS <- sweep(SIM_RETURNS , 1, as.matrix(chi_sim), `*`)
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  return(SIM_RETURNS)
}

# Residual method
gausResiduals <- function(prices, mean = NULL, log = TRUE, N, cov_method="standard", residual_cov_lag = 100){
  
  #print(prices$Date %>% tail(1))
  ## Calculate Returns
  if(log){
    returns <- prices %>% 
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
  
  ## residualize return
  # browser()
  #print("1")
  res_returns <- returns %>% na.omit() %>% apply(., 2, simpleGarchResiduals) %>% as.data.frame()
  
  #print("2")
  ## Estimate Covariance Matrix and decomposition
  if(cov_method == "weighted"){
    CoMa <- res_returns %>% covEstimator(., cov_method)
  } else{
    CoMa <- res_returns %>% tail(residual_cov_lag) %>%  cor(.)
  }
  
  #print("3")
  ## Calculate Cholesky decomposition
  Cholesky <- chol(CoMa)
  
  #print("4")
  ## Generate standard normal
  temp_sim <- data.frame(matrix(rnorm(n = N*ncol(CoMa)), ncol = ncol(CoMa)))
  colnames(temp_sim) <- colnames(CoMa)
  
  #print("5")
  ## Forecast volatility and rescale
  #browser()
  forecast_vol <- lapply(c(1:ncol(returns)), function(col){
    simpleGARCHVol(returns[[col]]) %>% as.numeric()
  }) %>% unlist()
  #forecast_vol <- returns %>% na.omit() %>% apply(., 2, simpleGARCHVol)
  SIM_RETURNS <- as.matrix(temp_sim) %*% Cholesky
  
  #print("6")
  SIM_RETURNS <- sweep(SIM_RETURNS , 2, forecast_vol, `*`)
  
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  
  return(SIM_RETURNS)
}

# Residual method with t distribution
tResiduals <- function(prices, mean = NULL, log = TRUE, N, cov_method="standard", residual_cov_lag = 100, max_df = 10){
  
  ## Calculate Returns
  if(log){
    returns <- prices %>% 
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
  
  ## residualize return
  res_returns <- returns %>% na.omit() %>% apply(., 2, simpleGarchResiduals) %>% as.data.frame()
  
  ## Excess kurtosis
  excess_kurtosis <- apply(res_returns, 2, function(x) {kurtosis(na.omit(x))-3})
  degrees_of_freedom <- (6/excess_kurtosis) + 4
  degrees_of_freedom <- ifelse(degrees_of_freedom <4, 4,degrees_of_freedom)
  degrees_of_freedom <- round(mean(degrees_of_freedom, na.rm=TRUE))
  print(paste0("T-Dist df: ", degrees_of_freedom, " - max. df: ", max_df))
  if(!is.null(max_df)){degrees_of_freedom <- ifelse(degrees_of_freedom > max_df, max_df,degrees_of_freedom)}
  
  ## Estimate Covariance Matrix
  if(cov_method == "weighted"){
    CoMa <- res_returns %>% covEstimator(., cov_method)
  } else{
    CoMa <- res_returns %>% tail(residual_cov_lag) %>%  cor(.)
  }
  
  CoMa <- (degrees_of_freedom-2)/(degrees_of_freedom) *  CoMa
  
  # Generate standard normal
  temp_sim <- data.frame(matrix(rnorm(n = N*ncol(CoMa)), ncol = ncol(CoMa)))
  colnames(temp_sim) <- colnames(CoMa)
  
  ## Calculate Cholesky decomposition
  Cholesky <- chol(CoMa)
  
  ## apply cholesky
  SIM_RETURNS <- as.matrix(temp_sim) %*% Cholesky
  
  ## Simulate Chi squared
  chi_sim <- rchisq(n = N, df = degrees_of_freedom)
  chi_sim <- sqrt(chi_sim /degrees_of_freedom)
  chi_sim <- 1/chi_sim
  
  ## Transform to Students T distribution
  SIM_RETURNS <- sweep(SIM_RETURNS , 1, as.matrix(chi_sim), `*`)
  
  ## Forecast volatility and rescale
  forecast_vol <- lapply(c(1:ncol(returns)), function(col){
    simpleGARCHVol(returns[[col]]) %>% as.numeric()
  }) %>% unlist()
  SIM_RETURNS <- sweep(SIM_RETURNS , 2, forecast_vol, `*`)
  
  if(log){SIM_RETURNS <- exp(SIM_RETURNS) -1}
  
  
  return(SIM_RETURNS)
}

# Historical Simulation
histSimulation <- function(prices,N){
  
  ## Calculate Returns
  #browser()
  SIM_RETURNS <-prices %>% 
      pivot_longer(., cols = colnames(.)[colnames(.)!="Date"]) %>% 
      group_by(name) %>% 
      mutate(value = RETURN(value)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = name, values_from=value) %>% 
      select(-Date) %>% 
    tail(N)
  
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

weightedEstimator <- function(returns, lambda = 0.15){
  
  returns %>%
    apply(.,2,function(x){
      
      res <- sweep(returns , 1, as.matrix(x), `*`)
      n_res <- apply(res, 2,function(c){length(na.omit(c))})
      # browser()
      lambda_vec <-  apply(res, 2,function(c){(1-lambda)^(n_res[1]:1-1)})
      res <- sweep(na.omit(res) , 1, lambda_vec, `*`)
      colSums(res, na.rm=T) * (lambda)
    })
  
}

################################################################################
# Vol Models #
################################################################################
simpleGARCHVol <- function(series){
  # print("hi")
  garch_returns <- series %>% na.omit() %>% as.numeric()
  if(sd(garch_returns)==0){
    return(1)
  } else {
    # Specify a GARCH(1,1) model
    spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                             garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 0), 
                                         include.mean = TRUE), 
                       distribution.model = "std")
    
    # Fit the model
    fit <- ugarchfit(spec = spec, data = garch_returns)
    
    if(fit@fit$convergence==1){
      return(sd(garch_returns))
    } else{
      forecast <- ugarchforecast(fit, n.ahead = 1)
      forecasted_volatility <- sigma(forecast)
      return(as.numeric(forecasted_volatility))  
    }

  }
  
}

simpleGarchResiduals <- function(series){
  
  garch_returns <- series %>% na.omit() %>% as.numeric()
  
  if(sd(garch_returns)==0){
    garch_returns <- ifelse(garch_returns== 0,1,garch_returns)
    res <- garch_returns/garch_returns
    return(as.numeric(res))
  } else {
    res <- tryCatch(expr = {
      # Specify a GARCH(1,1) model
      spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0), 
                                           include.mean = TRUE), 
                         distribution.model = "std")
      
      # Fit the model
      fit <- ugarchfit(spec = spec, data = garch_returns)
      res <- sigma(fit)
      
      if(length(res)==0){
        print("GARCH Problem")
        res <- garch_returns
      } else {
        res <- as.data.frame(res)[[1]]
        res <- garch_returns / res
      }
      
      return(as.numeric(res))
    }, error = function(cond){
      res <- garch_returns/mean(sqrt(3.14/2)*abs(garch_returns))
      return(as.numeric(res))
    })
    
    # if(!(sd(res)==0)){
    #   
    #   res <- garch_returns / res 
    # }
    # if(length(res)==0){
    #   res <- garch_returns
    # }
    return(as.numeric(res))
  }

}

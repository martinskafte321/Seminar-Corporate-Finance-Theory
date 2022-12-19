########################################################
#########3 Packages that we should use #################
########################################################

library(RSQLite)
library(tidyverse)
library(tidymodels) 
library(dplyr)
library(furrr) 
library(lubridate)
library(kableExtra)
library(tidyquant)
library(Quandl)
library(readxl)
library(quadprog)
library(timetk)
library(keras)
library(ggpubr)
library(ggplot2)
library(rsample)
library(caret)
library(Boruta)
library(vip)
library(upstartr)
########################################################
########## Reading the data from Excel files############
########################################################

features <- read.csv("data/features.csv") %>% rename(month = "ï..month" )
characteristics <- read.csv("data/characteristics.csv") %>% rename(month = "ï..month" )


########################################################
########## Momentum portfolio strategy #################
########################################################

# The function: "assign_portfolio" allows a specific variable (in this case momentum) and the number of portfolios to divide a dataset into portfolio sorts: 

assign_portfolio <- function(data, var, n_portfolios) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile({{ var }}, 
                                    #quantile() produces - in this case - 10 quantiles to split the data into, by a sequences from 0 to 1 by the number of portfolios. Thus creating a breakpoint for which we can split the portfolios into. 
                                    probs = seq(0, 1, length.out = n_portfolios + 1),
                                    na.rm = TRUE #Removes all NA's
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  data %>%
    mutate(portfolio = findInterval({{ var }}, #Given a vector of breakpoints, we find the interval containing each element of breakpoints
                                    breakpoints,
                                    all.inside = TRUE 
    )) %>%
    pull(portfolio) #Returns the portfolio number for each security
}


# Calling assign_portfolios to sort stocks into 10 portfolios using the momentum as dependent:

momentum_portfolios = characteristics %>% 
  select(month,permno,characteristic_mom12m,ret_excess,mktcap_lag, mkt_excess) %>%
  mutate(characteristic_mom12m = 100*characteristic_mom12m) %>%
  group_by(month) %>%
  mutate(
    month = as.Date(month),
    portfolio = assign_portfolio( #calling the function we created earlier to sort stocks into 10 pf's using momentum column in assign_portfolios dataset and add portfolio column to data_for_sorting
      data = cur_data(), #use current set, i.e. assign_portfolios
      var = characteristic_mom12m,
      n_portfolios = 10
    ),
    portfolio = as.factor(portfolio))

momentum_portfolios %>% 
  group_by(portfolio) %>%
  summarise(
    mom_mean = mean(characteristic_mom12m), #arithmetic average of momentum
    mc_mean = mean(mktcap_lag) #arithmetic average of mktcap
  ) %>%kbl(caption = "Table 4: Mean momentum and market capitalization for sorted portfolios", digits = 3) %>% 
  kable_classic(full_width = F, html_font = "Cambria")

### **Finally, analyse the momentum strategy: a portfolio that goes long past winners and short past losers.**

strategy_portfolio_original = momentum_portfolios %>% 
  mutate(portfolio = as.numeric(portfolio)) %>%
  group_by(month) %>%
  mutate(breakpoint_low = 1,
         breakpoint_high = 10, # We take the portfolios of highest and lowest momentum as a subset to evaluate by creating breakpoints. 
         portfolio = case_when(portfolio <= breakpoint_low ~ "low",
                               portfolio >= breakpoint_high ~ "high")) %>%
  # The two portfolios are renamed as low and high to distinquish between one another.
  group_by(month,portfolio) %>%
  summarise(excess_ret = weighted.mean(ret_excess, mktcap_lag)) # Value weighted return by high or low grouped momentum values. 


performance_momentum <- strategy_portfolio_original %>%
  pivot_wider(names_from = portfolio, values_from = excess_ret) %>%
  mutate(high_low = high - low) %>% select(month,high,high_low)
 # Dont gonna use the last patr in this analysis
  #%>% # subtracting the low-portfolio alpha, hoping that this will be negative and increase the alpha of the strategy. 
  #left_join(factors_ff_monthly, by = "month") %>%
  #lm(high_low ~ 1 + mkt_excess, data = .) %>% broom::tidy() %>% kbl(caption = "Table 5: Performance of the portfolio that goes long past winners and short past losers ", digits = 3) %>% 
  #kable_classic(full_width = F, html_font = "Cambria")
  

  remove(momentum_portfolios, strategy_portfolio_original,assign_portfolio)
# Again a linear regression of the high-low strategy on market excess return to get the alpha. 

########################################################
########## Tangency portfolio strategy #################
########################################################

# Select all stocks from the CRSP universe

data <- characteristics %>%
  select(permno,month,ret_excess) %>%
  group_by(permno) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(-n)

  # Filter the data to only get a random subset of 20% of the available CRSP dataset:
#  set.seed(333)
 # subsample <- sample_n(data, n_distinct(data$permno)*0.3) %>% select(permno)
  
  
  # This function is implemented to select a random subset of which we do a regression on the lagged value to see if it's significant. We only keep significant tests and see how big a fraction this is of the sample. 
  #data <- data %>% 
   # filter(permno %in% subsample$permno)
  
######### FUNCTIONS ####################################
# Function to compute optimal portfolio weights:
efficient_weights <- function(mu, sigma, gamma = 4, lambda = 0, 
                              w_prev) {
  
  iota <- rep(1, ncol(sigma))
  sigma_adj <- sigma + (2 * lambda * sigma)/gamma
  mu_adj <- mu + 2*lambda * sigma %*%  naive_weights
  
  
  sigma_inv <- solve(sigma_adj)
  
  w_mvp <- sigma_inv %*% iota
  w_mvp <- as.vector(w_mvp / sum(w_mvp))
  
  w_opt <- w_mvp + 1/gamma * (sigma_inv - 1 / sum(sigma_inv) * sigma_inv %*% iota %*% t(iota) %*% sigma_inv) %*% mu_adj
  
  return(as.vector(w_opt))
}

# Ledoit-Wolf estimation
compute_ledoit_wolf <- function(x) {
  # Computes Ledoit-Wolf shrinkage covariance estimator
  # This function generates the Ledoit-Wolf covariance estimator  as proposed in Ledoit, Wolf 2004 (Honey, I shrunk the sample covariance matrix.)
  # X is a (t x n) matrix of returns
  t <- nrow(x)
  n <- ncol(x)
  x <- apply(x, 2, function(x) if (is.numeric(x)) # demean x
    x - mean(x) else x)
  sample <- (1/t) * (t(x) %*% x)
  var <- diag(sample)
  sqrtvar <- sqrt(var)
  rBar <- (sum(sum(sample/(sqrtvar %*% t(sqrtvar)))) - n)/(n * (n - 1))
  prior <- rBar * sqrtvar %*% t(sqrtvar)
  diag(prior) <- var
  y <- x^2
  phiMat <- t(y) %*% y/t - 2 * (t(x) %*% x) * sample/t + sample^2
  phi <- sum(phiMat)
  
  repmat = function(X, m, n) {
    X <- as.matrix(X)
    mx = dim(X)[1]
    nx = dim(X)[2]
    matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T)
  }
  
  term1 <- (t(x^3) %*% x)/t
  help <- t(x) %*% x/t
  helpDiag <- diag(help)
  term2 <- repmat(helpDiag, 1, n) * sample
  term3 <- help * repmat(var, 1, n)
  term4 <- repmat(var, 1, n) * sample
  thetaMat <- term1 - term2 - term3 + term4
  diag(thetaMat) <- 0
  rho <- sum(diag(phiMat)) + rBar * sum(sum(((1/sqrtvar) %*% t(sqrtvar)) * thetaMat))
  
  gamma <- sum(diag(t(sample - prior) %*% (sample - prior)))
  kappa <- (phi - rho)/gamma
  shrinkage <- max(0, min(1, kappa/t))
  if (is.nan(shrinkage))
    shrinkage <- 1
  sigma <- shrinkage * prior + (1 - shrinkage) * sample
  return(sigma)
}


# calculate the returns matrix

stock_matrix <- data %>%
  pivot_wider(
    names_from = permno,
    values_from = ret_excess
  ) 

ret_matrix <- stock_matrix %>% 
  select(-month)

remove(data)

# Recomputing optimal weights by a specific window of data for every point forecast:

window_length <- 150 # 150 datapoints in each window to calculate estimates:
periods <-  nrow(ret_matrix) - window_length # 581 periods forecast
lambda = 50/10000 # 0.5% transaction cost


# using the returns matrix, we can compute the covariance matrix and sample mean
sigma <- cov(ret_matrix)
mu <- colMeans(ret_matrix)

# Calculating the minimum variance weights
w_mvp <- solve(sigma) %*% rep(1, ncol(sigma))
w_mvp <- as.vector(w_mvp / sum(w_mvp)) 

# Creating the initial naive weight
naive_weights = 1/ncol(sigma) * rep(1,ncol(sigma))

remove(sigma,mu,w_mvp)


performance_values <- matrix(NA,
                             nrow = periods,
                             ncol = 3) 

colnames(performance_values) = c("raw_return","turnover","net_return")

# Creates the list, which we populate with the values calculated later:
performance_values <- list("MV" = performance_values, 
                           "Naive" = performance_values)

w_prev_2 <- w_prev_3 <- naive_weights # Initial weights of all portfolios


# Helper function to adjust weights due to returns changing
adjust_weights <- function(w, next_return){
  w_prev <- 1 + w * next_return
  as.numeric(w_prev / sum(as.vector(w_prev)))
}

# Helper function to calculate performance evaluation: compute realized returns net of transaction costs.
evaluate_performance <- function(w, w_previous, next_return, lambda){
  raw_return <- as.matrix(next_return) %*% w
  turnover <- sum(abs(w - w_previous))
  net_return <- raw_return - lambda * turnover
  c(raw_return, turnover, net_return)
}


#We include two helper functions. The first one adjusts the weights given changes in the underlying returns. 
# The second calculates the performance evaluations by computing the realized returns af transaction costs have been deducted. 
#With the helper functions, we are ready to perform the rolling-window estimation. This is done using a for loop through all the periods. 
# Lastly, the performance is evaluated through the summarized key figures.

# The following code chunk performs rolling-window estimation of all three portfolio strategies:
for (p in 1:periods) {
  
  
  # Below variables are general for all three estimation methods:
  returns_window <- ret_matrix[p : (p + window_length - 1), ] # window of return rows for different companies:
  one_ahead_return <- ret_matrix[p + window_length,] # Next periods return
  
  
  mu  <- colMeans(returns_window)
  sigma_lw <- compute_ledoit_wolf(returns_window)
  
  # Mean-variance without short-selling portfolio (still with transaction costs):
  w_MV_NSS <- solve.QP(Dmat = 4 * sigma_lw,
                       dvec = mu,
                       Amat = cbind(1,diag(ncol(returns_window))),
                       bvec = c(1,rep(0, ncol(returns_window))),
                       meq = 1
  )$solution
  
  performance_values[[1]][p,] <- evaluate_performance(w = w_MV_NSS,
                                                      w_previous = w_prev_2,
                                                      next_return = one_ahead_return,
                                                      lambda = lambda
  ) 
  
  w_prev_2 = adjust_weights(w_MV_NSS, one_ahead_return)
  
  
  # Naive portfolio allocation:
  
  performance_values[[2]][p,] <- evaluate_performance(w = naive_weights,
                                                      w_previous = w_prev_3,
                                                      next_return = one_ahead_return,
                                                      lambda = lambda)
  
  w_prev_3 = adjust_weights(naive_weights, one_ahead_return)
  
}


performance_MV <- lapply(performance_values, as_tibble) %>% 
  bind_rows(.id = "strategy") %>%
  filter(strategy == "MV") %>%
  mutate(month = as.Date(stock_matrix[window_length:(length(stock_matrix$month)-1),]$month)) %>%
  select(-c(turnover,net_return))

performance_naive <- lapply(performance_values, as_tibble) %>% 
  bind_rows(.id = "strategy") %>%
  filter(strategy == "Naive") %>%
  mutate(month = as.Date(stock_matrix[window_length:(length(stock_matrix$month)-1),]$month)) %>%
  select(-c(turnover,net_return))


remove(one_ahead_return,ret_matrix,sigma_lw, performance_values,stock_matrix,returns_window,lambda,mu,naive_weights,p,periods,w_prev_2,w_prev_3,w_MV_NSS,adjust_weights,assign_portfolio,compute_ledoit_wolf,efficient_weights,evaluate_performance,window_length)


########################################################
######### Fama French 3-factor portfolio ###############
########################################################



#Loading in the CSV file
features <- read.csv("data/features.csv") %>% rename(month = "ï..month" )
characteristics <- read.csv("data/characteristics.csv") %>% rename(month = "ï..month" ) 

#Dataset
data <- characteristics %>% select(month,permno,ret_excess,mktcap_lag,rf,mkt_excess,smb,hml) %>%
  filter(month >= "1990-01-01") 

data <- data %>%
  group_by(permno) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n == max(n)) %>%
  select(-n) 
  
permnos = data %>% count(permno) %>% select(-n) %>% slice_sample(n=100)

data <- data %>% inner_join(permnos, by = "permno")

#Estimation:
#Creating matrix to store alpha's and coefficient values
coefficients <- matrix(NA,
                       nrow = nrow(unique(data['permno'])),
                       ncol = 5) 

colnames(coefficients) = c("Permno","Alpha","beta","smb","hml")


for (i in 1:nrow(unique(data['permno']))) { #Looping through permnos
  
  
  t =  unique(data['permno'])[i,]
  coefficients[i,2:5] <- lm(ret_excess ~ mkt_excess + smb + hml, 
                            data = data %>% filter(permno == t[[1]] )) %>% coefficients()
  
  coefficients[i,1] <- t[[1]]
  
}

coefficients %>% kbl(caption = "Table 1: Summary of Fama-French 3 Factor model regression coefficients",digits = 4) %>%
  kable_classic(full_width = F, html_font = "Cambria")




ret_matrix <- matrix(NA,
                     nrow = nrow(unique(data['month'])),
                     ncol = 100)


# Loop which creates a prediction from the FF-3 factor model. The model is created and predictions are made for each permno for each month. 
l=1
m=1
for (m in 1:nrow(unique(data['permno']))) {
  
  l = unique(data['permno'])[m,]
  
  ret_matrix[,m] <-  predict(lm(ret_excess ~ mkt_excess + smb + hml, 
                                data = data %>% filter(permno == l[[1]])), 
                             newdata = data %>% filter(permno==l[[1]]) %>% select(mkt_excess,smb,hml))
}



ret_matrix <- ret_matrix %>% cbind(unique(data['month']))

performance_3_factor = ret_matrix %>% pivot_longer(!month, names_to ="stock", values_to = "return") %>% group_by(month) %>% summarise(return = mean(return))


write_excel_csv(performance_3_factor,"data/portfolio_3_factor.csv")


########################################################
######### Fama French 3-factor portfolio ###############
########################################################

#Dataset
data <- characteristics %>% select(month,permno,ret_excess,mktcap_lag,mktcap,gvkey,characteristic_beta,characteristic_bm) %>%
  filter(month >= "1990-01-01") 


me_ff <- data %>%
  filter(month(month) == 6) %>%
  mutate(sorting_date = month %m+% months(1)) %>%
  select(permno, sorting_date, me_ff = mktcap)

me_ff_dec <- data %>%
  filter(month(month) == 12) %>%
  mutate(sorting_date = ymd(str_c(year(month) + 1, "0701)"))) %>%
  select(permno, gvkey, sorting_date, bm_me = mktcap)

bm_ff <- be %>%
  mutate(sorting_date = ymd(str_c(year(as.Date(datadate)) + 1, "0701"))) %>%
  select(gvkey, sorting_date, bm_be = be) %>%
  drop_na() %>%
  inner_join(me_ff_dec, by = c("gvkey", "sorting_date")) %>%
  mutate(bm_ff = bm_be / bm_me) %>%
  select(permno, sorting_date, bm_ff)

variables_ff <- me_ff %>%
  inner_join(bm_ff, by = c("permno", "sorting_date")) %>%
  drop_na() %>%
  distinct(permno, sorting_date, .keep_all = TRUE)


assign_portfolio <- function(data, var, percentiles) {
  breakpoints <- data %>%
    summarize(breakpoint = quantile(
      {{ var }},
      probs = {{ percentiles }},
      na.rm = TRUE
    )) %>%
    pull(breakpoint) %>%
    as.numeric()
  
  assigned_portfolios <- data %>%
    mutate(portfolio = findInterval({{ var }},
                                    breakpoints,
                                    all.inside = TRUE
    )) %>%
    pull(portfolio)
  
  return(assigned_portfolios)
}

portfolios_ff <- variables_ff %>%
  inner_join(data, by = c("permno" = "permno", "sorting_date" = "month")) %>%
  group_by(sorting_date) %>%
  mutate(
    portfolio_me = assign_portfolio(
      data = cur_data(),
      var = me_ff,
      percentiles = c(0, 0.5, 1)
    ),
    portfolio_bm = assign_portfolio(
      data = cur_data(),
      var = bm_ff,
      percentiles = c(0, 0.3, 0.7, 1)
    )
  ) %>%
  select(permno, sorting_date, portfolio_me, portfolio_bm)





portfolios_ff <- data %>%
  mutate(sorting_date = case_when(
    month(month) <= 6 ~ ymd(str_c(year(month) - 1, "0701")),
    month(month) >= 7 ~ ymd(str_c(year(month), "0701"))
  )) %>%
  inner_join(portfolios_ff, by = c("permno", "sorting_date"))




factors_ff_monthly_replicated <- portfolios_ff %>%
  mutate(portfolio = str_c(portfolio_me, portfolio_bm)) %>%
  group_by(month,portfolio) %>%
  summarise(excess_ret = weighted.mean(ret_excess, mktcap_lag)) %>%
  pivot_wider(names_from = portfolio, values_from = excess_ret) %>%
  mutate(portfolio_return = )
  

########################################################
######### Fama French 3-factor portfolio ###############
########################################################

data = data %>%
  group_by(month) %>%
  mutate(
    n = n(),
  )

n_parameters <- 3
theta <- rep(1.5, n_parameters)
names(theta) <- c("size","bm","beta")

# Function to calculate weights: step 1) calculate scaled characteristics and matrix multiply by theta to get a new column "characteristic_tilt", step 2) create a benchmark (1/n) and weigh the tilt. 
compute_portfolio_weights <- function(theta,
                                      data) {
  data %>%
    group_by(month) %>%
    bind_cols(
      characteristic_tilt = data %>% ungroup %>%
        transmute(size = size / n,
                  bm = bm / n,
                  beta = beta / n) %>%
        as.matrix() %*% theta %>% as.numeric()
    ) %>%
    mutate(
      # Definition of benchmark weight
      weight_benchmark = 1 / n,
      # Parametric portfolio weights
      weight_tilt = weight_benchmark + characteristic_tilt,
      # Weights sum up to 1
      weight_tilt = weight_tilt / sum(weight_tilt) 
    ) %>%
    ungroup()
}



#########################################################

 # Value weighted return by high or low grouped momentum values. 


performance_momentum <- strategy_portfolio_original %>%
  pivot_wider(names_from = portfolio, values_from = excess_ret) %>%
  mutate(high_low = high - low) %>% select(month,high,high_low)

#################################################3
  
  
  
  summarize(
    ret = weighted.mean(ret_excess, mktcap_lag), .groups = "drop",
    portfolio_me = unique(portfolio_me),
    portfolio_bm = unique(portfolio_bm)
  ) %>%
  
  group_by(month) %>%
  summarize(
    smb_replicated = mean(ret[portfolio_me == 1]) -
      mean(ret[portfolio_me == 2]),
    hml_replicated = mean(ret[portfolio_bm == 3]) -
      mean(ret[portfolio_bm == 1])
  ) %>% left_join(factors_ff_monthly %>% select(month,mkt_excess), by = "month")



ret_matrix <- matrix(NA,
                     nrow = nrow(unique(data['month'])),
                     ncol = 100)


# Loop which creates a prediction from the FF-3 factor model. The model is created and predictions are made for each permno for each month. 

for (m in 1:nrow(unique(data['permno']))) {
  
  l = unique(data['permno'])[m,]
  
  ret_matrix[,m] <-  predict(lm(ret_excess ~ mkt_excess + smb + hml, 
                                data = data %>% filter(permno == l[[1]] )), 
                             newdata = data %>% filter(permno==l[[1]]) %>% select(mkt_excess,smb,hml))
}

########################################################
######### MACHINE LEARNING SETUP FOR 3-FACTOR ##########
########################################################


ML_data_3 <- features %>%
  select(-c("Value.x","Value.y",ret_excess,mktcap_lag)) %>%
  filter(month >= "1990-02-01") %>%
  mutate(month = as.Date(month)) %>%
  left_join(performance_3_factor %>% select(ret_excess = return,month), by = "month") %>%
  mutate(return = as.factor(ifelse(ret_excess > 0, 1, 0))) %>%
  select(-ret_excess,-GVZCLS) %>% na.omit()


# Splitting the data: 80% for training and 20% for testing:
split <- initial_time_split(
  ML_data_3, prop = 4 / 5 
)

recipe = recipe(return  ~  ., data = training(split)) %>% # True training data to estimate a model
  step_rm(month) %>%
  step_normalize(all_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_pca(all_numeric())


# K fold cross validation for both models. 
data_folds <- vfold_cv(
  training(split),
  v = 10,
  strata = return
)


# We tune both models before moving on with the estimation.
set.seed(9844)

### Neural Network:

# Specifying a model (only partly tuning, due to computation time

nnet <- mlp(
  epochs = tune(),
  hidden_units = tune(), dropout = 0.1, activation = "relu"
) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = FALSE)

# Fit the model with the training data.

nn_fit <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nnet) 



# Neural Network model:

nn_tune_1 <- nn_fit %>% 
  tune_grid(
    resample = data_folds,
    grid = grid_random(hidden_units(c(1,40)), epochs(c(1,1500)), size = 175),
    metrics = metric_set(roc_auc)
  )


autoplot(nn_tune_1,xlab = list("e","t"), metric = "roc_auc") + 
  geom_smooth(metse=FALSE, size=2) + 
  labs(x = "test")

models = nn_tune_1 %>% 
  show_best(n=10) %>% 
  select(-c(.metric,.estimator,n,.config)) %>% 
  add_column('Rank' = 1:10, .before = 'hidden_units')

xtable(models, type = "latex",digits = 3)




test_model = tibble(
  hidden_units = 16,
  epochs = 685,
  .config = "Preprocessor1_Model031"
)

best_model = tibble(
  hidden_units = 17,
  epochs = 967,
  .config = "Preprocessor1_Model128"
)


# Choosing the model with least complexity and least standard errors. 
test %>% select_by_one_std_err(metric = NULL, mean)

nn_fit_model <- nn_fit %>%
  finalize_workflow(parameters = test %>% select_best()) %>%
  fit(data = training(split))

nn_fit_model_test <- nn_fit %>%
  finalize_workflow(parameters = test %>% select_by_one_std_err(metric = NULL, mean)) %>%
  fit(data = training(split))



# Use this fitted model to generate return predictions in the validation test set and evaluate the errors:

# First, calculate out-of-sample predictions for both models:
pred_collected_testing <- tibble(
  month = testing(split) %>% pull(month),
  actual = testing(split) %>% pull(return),
  prediction = nn_fit_model %>% predict(new_data = testing(split)) %>% pull(.pred_class),
  probability = nn_fit_model %>% predict(new_data = testing(split), type = "prob") %>% pull(.pred_1))

########################################################
######### MACHINE LEARNING SETUP FOR MOMENTUM ##########
########################################################


ML_data_mom <- features %>%
  select(-c("Value.x","Value.y",ret_excess,mktcap_lag)) %>%
  filter(month >= "1990-02-01") %>%
  mutate(month = month) %>%
  left_join(performance_momentum %>% select(ret_excess = high_low,month), by = "month") %>%
  mutate(return = as.factor(ifelse(ret_excess > 0, 1, 0))) %>%
  select(-ret_excess,-GVZCLS) %>% na.omit()


# Splitting the data: 80% for training and 20% for testing:
split <- initial_time_split(
  ML_data_mom, prop = 4 / 5 
)



recipe = recipe(return  ~  ., data = training(split)) %>% # True training data to estimate a model
  step_rm(month) %>%
  step_normalize(all_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_pca(all_numeric())
  
 
# K fold cross validation for both models. 
data_folds <- vfold_cv(
  training(split),
  v = 10,
  strata = return
)


# We tune both models before moving on with the estimation.
set.seed(9844)

### Neural Network:

# Specifying a model (only partly tuning, due to computation time

nnet <- mlp(
  epochs = tune(),
  hidden_units = tune(), dropout = 0.1, activation = "relu"
  ) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = FALSE)

# Fit the model with the training data.

nn_fit <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nnet) 



# Neural Network model:

nn_tune <- nn_fit %>%
  tune_grid(
    resample = data_folds,
    grid = grid_regular(hidden_units(), epochs(), levels = c(15,10)),   # Tuning
    metrics = metric_set(roc_auc)
  )

nn_tune_1 <- nn_fit %>% 
  tune_grid(
    resample = data_folds,
    grid = grid_random(hidden_units(c(1,20)), epochs(c(1,1500)), size = 150),
    metrics = metric_set(roc_auc)
)

nn_tune_2 <- nn_fit %>% 
  tune_grid(
    resample = data_folds,
    grid = grid_random(hidden_units(c(20,40)), epochs(c(500,1000)), size = 25),
    metrics = metric_set(roc_auc)
  )

test=rbind(nn_tune_1,nn_tune_2)


autoplot(test,xlab = list("e","t"), metric = "roc_auc") + 
  geom_smooth(metse=FALSE, size=2) + 
  labs(x = "test")

models = test %>% 
  show_best(n=10) %>% 
  select(-c(.metric,.estimator,n,.config)) %>% 
  add_column('Rank' = 1:10, .before = 'hidden_units')

xtable(models, type = "latex",digits = 3)




test_model = tibble(
  hidden_units = 16,
  epochs = 685,
  .config = "Preprocessor1_Model031"
)

best_model = tibble(
  hidden_units = 17,
  epochs = 967,
  .config = "Preprocessor1_Model128"
)


# Choosing the model with least complexity and least standard errors. 
test %>% select_by_one_std_err(metric = NULL, mean)

nn_fit_model <- nn_fit %>%
  finalize_workflow(parameters = test %>% select_best()) %>%
  fit(data = training(split))

nn_fit_model_test <- nn_fit %>%
  finalize_workflow(parameters = test %>% select_by_one_std_err(metric = NULL, mean)) %>%
  fit(data = training(split))
 


# Use this fitted model to generate return predictions in the validation test set and evaluate the errors:

# First, calculate out-of-sample predictions for both models:
pred_collected_testing <- tibble(
  month = testing(split) %>% pull(month),
  actual = testing(split) %>% pull(return),
  prediction = nn_fit_model %>% predict(new_data = testing(split)) %>% pull(.pred_class),
  probability = nn_fit_model %>% predict(new_data = testing(split), type = "prob") %>% pull(.pred_1))


pred_collected_testing_new <- tibble(
  month = testing(split) %>% pull(month),
  actual = testing(split) %>% pull(return),
  prediction = nn_fit_model_test %>% predict(new_data = testing(split)) %>% pull(.pred_class),
  probability = nn_fit_model_test %>% predict(new_data = testing(split), type = "prob") %>% pull(.pred_1))

########################################################
######### MACHINE LEARNING SETUP FOR MEAN VARIANCE #####
########################################################

performance_mv <- read_csv("data/mv_portfolio.csv", 
                            col_types = cols(month = col_date(format = "%Y-%m-%d")))



ML_data_MV <- features %>%
  select(-c("Value.x","Value.y",ret_excess,mktcap_lag)) %>%
  filter(month >= "1990-01-01") %>%
  mutate(month = as.Date(month)) %>%
  left_join(performance_MV, by = "month") %>%
  mutate(return = as.factor(ifelse(raw_return > 0, 1, 0))) %>%
  select(-raw_return,-GVZCLS,-strategy) %>% na.omit()


# Splitting the data: 80% for training and 20% for testing:
split_MV <- initial_time_split(
  ML_data_MV, prop = 4 / 5 
)



recipe_MV = recipe(return  ~  ., data = training(split_MV)) %>% # True training data to estimate a model
  step_rm(month) %>%
  # step_interact(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_pca(all_numeric())


# K fold cross validation for both models. 
data_folds_MV <- vfold_cv(
  training(split_MV),
  v = 5,
  strata = return
)


# We tune both models before moving on with the estimation.
set.seed(9855)

### Neural Network:

# Specifying a model (only partly tuning, due to computation time

nnet_MV <- mlp(
  epochs = tune(),
  hidden_units = tune()
) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = FALSE)

# Fit the model with the training data.

nn_fit_MV <- workflow() %>%
  add_recipe(recipe_MV) %>%
  add_model(nnet_MV) 



# Neural Network model:

nn_tune_MV <- nn_fit_MV %>%
  tune_grid(
    resample = data_folds_MV,
    grid = grid_regular(hidden_units(), epochs(), levels = c(10, 10)),   # Tuning
    metrics = metric_set(roc_auc)
  )


nn_tune_MV %>% 
  show_best(n=10) %>% 
  select(-c(.metric,.estimator,n,.config)) %>% 
  add_column('Rank' = 1:10, .before = 'hidden_units') %>%
  kbl(digits = 3) %>%
  kable_classic(full_width = F, html_font = "Cambria")


test_model_MV = tibble(
  hidden_units = 6,
  epochs = 340,
  .config = "Preprocessor1_Model036"
)


# Identify the best model and fit with the training data
nn_fit_model_MV <- nn_fit_MV %>%
  finalize_workflow(parameters =test_model_MV) %>%
  # Fitting the model on the training data.
  fit(data = training(split_MV))


autoplot(nn_tune_MV, metric = "roc_auc") 

# Use this fitted model to generate return predictions in the validation test set and evaluate the errors:

# First, calculate out-of-sample predictions for both models:

pred_collected_testing_MV <- tibble(
  month = testing(split_MV) %>% pull(month),
  actual = testing(split_MV) %>% pull(return),
  prediction = nn_fit_model_MV %>% predict(new_data = testing(split_MV)) %>% pull(.pred_class),
  probability = nn_fit_model_MV %>% predict(new_data = testing(split_MV), type = "prob") %>% pull(.pred_1))



########################################################
############ MACHINE LEARNING PERFORMANCE ##############
########################################################


write_excel_csv(pred_collected_testing_new,"Github/AEF/data/predictions_mom_new.csv")
write_excel_csv(pred_collected_testing_MV,"data/predictions_MV.csv")

pred_collected_testing <- read.csv("data/predictions_mom_new.csv") %>% rename(month = "ï..month" ) %>%
  mutate(actual = as.factor(actual),
         prediction = as.factor(prediction),
         month = as.Date(month))

performance_momentum <- read.csv("data/mom_portfolio.csv") %>% rename(month = "ï..month" ) %>%
  mutate(month = as.character(month))
    
performance_momentum <- performance_momentum %>% mutate(month = as.Date.character(month)) 

pred_collected_testing_MV <- read_csv("data/predictions_MV.csv", 
                                      col_types = cols(month = col_date(format = "%Y-%m-%d"))) %>%
  mutate(actual = as.factor(actual),
         prediction = as.factor(prediction))

read_csv("data/predictions_MV.csv", 
                  col_types = cols(month = col_date(format = "%Y-%m-%d")))



portfolio_strategy_MV <- read_csv("data/predictions_MV.csv", 
                                  col_types = cols(month = col_date(format = "%Y-%m-%d"))) %>% 
  left_join(read_csv("data/mv_portfolio.csv", 
                     col_types = cols(month = col_date(format = "%Y-%m-%d"))), by = "month") %>%
  mutate(risk_managed = case_when(
    probability >= 0.51 ~ 1,
    probability < 0.51 ~ 0),
    trade = raw_return * risk_managed # Step out of the trade if prob < 0.6. 
           )

portfolio_strategy_MV %>%
  summarise(
    Mean = 12 * mean(100 * trade),
    SD = sqrt(12) * sd(100 * trade),
    'Sharpe Ratio' = if_else(Mean > 0, Mean / SD, NA_real_))


yearly_return = portfolio_strategy_MV %>%
  summarise_by_time(
    .date_var = month,
    .by = "year",
    value = 12* mean(100*trade)) %>%
  transmute(month,ML_Risk = value)

portfolio_strategy_MV %>%
  summarise_by_time(
    .date_var = month,
    .by = "year",
    value = 12* mean(100*raw_return)) %>%
  transmute(month,Actual = value) %>%
  left_join(yearly_return, by = "month") %>%
  transmute(Year = year(month),ML_Risk,Actual) %>%
  kbl(digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria")

######### Momentum ##########
#############################

y = pred_collected_testing %>% mutate(month = as.Date(month)) %>% select(month)


portfolio_strategy_mom_new <- pred_collected_testing %>% 
  left_join(performance_momentum, by = "month") %>%
  mutate(risk_managed = case_when(
    probability >= 0.65 ~ 1,
    probability < 0.65 ~ 0),
    trade = high_low * risk_managed # Step out of the trade if prob < 0.6. 
  )



yearly_return_new = portfolio_strategy_mom_new %>%
  summarise_by_time(
    .date_var = month,
    .by = "year",
    value = 12* mean(100*trade)) %>%
  transmute(month,ML_Risk = value)

portfolio_strategy_mom_new %>%
  summarise_by_time(
    .date_var = month,
    .by = "year",
    value = 12* mean(100*high_low)) %>%
  transmute(month,Actual = value) %>%
  left_join(yearly_return_new, by = "month") %>%
  transmute(Year = year(month),ML_Risk,Actual) %>%
  kbl(digits = 2) %>%
  kable_classic(full_width = F, html_font = "Cambria") 


portfolio_strategy_mom_new %>%
  summarise(
    Mean = 12 * mean(100 * trade),
    SD = sqrt(12) * sd(100 * trade),
    'Sharpe Ratio' = if_else(Mean > 0, Mean / SD, NA_real_)) 



portfolio_strategy_mom %>%
  summarise(
    Mean = 12 * mean(100 * high_low),
    SD = sqrt(12) * sd(100 * high_low),
    'Sharpe Ratio' = if_else(Mean > 0, Mean / SD, NA_real_))

portfolio_strategy_mom_new %>% select(month,"Original" = high_low, "Risk" = trade) %>% gather("Portfolio","trade", -month) %>%
ggplot(aes(x = month, y = trade)) + 
  geom_line(aes(color = Portfolio,linetype = Portfolio)) + 
  scale_linetype_manual(values=c("twodash", "solid")) +
  scale_color_manual(values = c("steelblue","darkred")) +
  labs(x = "Date", y = "Return in %")+
  scale_y_percent(breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"), date_labels = "%Y") +
  theme_light() +
  theme(legend.position = "top")


portfolio_strategy_MV %>% select(month,"Original" = raw_return, "Risk" = trade) %>% gather("Portfolio","trade", -month) %>%
  ggplot(aes(x = month, y = trade)) + 
  geom_line(aes(color = Portfolio,linetype = Portfolio)) + 
  scale_linetype_manual(values=c("twodash", "solid")) +
  scale_color_manual(values = c("steelblue","darkred")) +
  labs(x = "Date", y = "Return in %")+
  scale_y_percent(breaks = c(-0.1,-0.05,0,0.05,0.1)) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 year"), date_labels = "%Y") +
  theme(legend.position = "NONE")



########## Confusion matrix


pred_collected_testing_MV %>% 
  conf_mat(truth = actual, estimate = prediction)

pred_collected_testing_MV %>% 
  accuracy(truth = actual, prediction)

cm = confusionMatrix(pred_collected_testing_MV$prediction,pred_collected_testing_MV$actual, dnn = c("Prediction","Reference"))
plt = as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels = rev(levels(plt$Prediction)))

plot<- ggplot(plt, aes(Reference,Prediction, fill=Freq)) + 
  geom_tile(show.legend = FALSE) + geom_text(aes(label=Freq), size = 6) +
  scale_fill_gradient(low="white", high = "#009194") +
  labs(x = "Actual", y = "Prediction") +
  scale_x_discrete(labels = c("Loss","Profit"),position = "top") + 
  scale_y_discrete(labels = c("Profit", "Loss")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))

pred_collected_testing_new %>% 
  conf_mat(truth = actual, prediction)

pred_collected_testing_new %>% 
  accuracy(truth = actual, prediction)



cm = confusionMatrix(pred_collected_testing$prediction,pred_collected_testing$actual, dnn = c("Prediction","Reference"))
plt = as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels = rev(levels(plt$Prediction)))
plot_mom <- ggplot(plt, aes(Reference,Prediction, fill=Freq)) + 
  geom_tile(show.legend = FALSE) + geom_text(aes(label=Freq),size =6) +
  scale_fill_gradient(low="white", high = "#009194") +
  labs(x = "Actual", y = "Prediction") +
  scale_x_discrete(labels = c("Loss","Profit"),position = "top") + 
  scale_y_discrete(labels = c("Profit", "Loss")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold")) + theme_minimal()

ggarrange(plot, plot_mom)

b <- Boruta(return~., data = ML_data_mom, doTrace = 2)
plot(b, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(b$ImpHistory),function(i)
  b$ImpHistory[is.finite(b$ImpHistory[,i]),i])
names(lz) <- colnames(b$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(b$ImpHistory), cex.axis = 0.7)

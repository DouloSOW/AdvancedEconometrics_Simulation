#########The Indirect Inference Method
rm(list = ls(all=TRUE))
library(dplyr)
load("New_DataSimulatedLoader.RData")
##1. Use the MLE as the Baseline Estimator
##2. The auxiliarry model as the true one
###The MLE method computes the ratio of two quadratic forms,
###where the numerator measures the covariance between the changes in y and the lagged changes in y,
###and the denominator measures the variance of the lagged changes in y. 
mle_est <- function(data_df){
  data_df = data_df %>% group_by(id) %>% mutate(y_lag = lag(y, 1))
  data_df = data_df %>% group_by(id) %>% mutate(ylag_bar = mean(y_lag, na.rm = TRUE))
  data_df = data_df %>% group_by(id) %>% mutate(y_bar = mean(y, na.rm = TRUE))
  
  ###Compute the MLE by hand
  cn = sum((data_df$y-data_df$y_bar)*(data_df$y_lag-data_df$ylag_bar), na.rm = TRUE)
  dn = sum((data_df$y_lag-data_df$ylag_bar)^2, na.rm = TRUE)
  mle_hat = cn/dn
  return(mle_hat)
}




#####################
##Define the II function that estimate the II phi

##Change the value of phi_index to 1, 2, 3, or 4 meaning resp. phi_true=0; 0.3; 0.6; or 0.9

ii_function <- function(phi_h, data = dataDGP1_N100_T5, nRep_H = 10, phi_index, sim_index, T=5){#default
  
  ##########################
  df = as.data.frame(data[,,phi_index, sim_index]) 
  df$id = seq(1, nrow(df))
  df_long = reshape(df, direction = "long", idvar = 'id', varying = list(1:T), v.names = "y")
  #Declare the data in a plm format which enables gmm to understand the data (id and time variable)
  df_long = pdata.frame(df_long, index = c("id", "time"))
  ##########################
  ##Estimate the mle estimator
  
  phi_mle <- mle_est(df_long)
  
  #Phi_identifier = c(paste0('Phi=', phi_h))
  
  ##Simulate H paths as function of phi (the argument under which to minimize)
  sim_h <-  sim_functionDGP1(N=100, T=5, Phi_values = phi_h)
  phi_mle_h = c()

  
  for(h in 1:nRep_H){
    
    ##########################
    df_h = as.data.frame(sim_h[,,1,h]) 
    df_h$id = seq(1, nrow(df_h))
    df_long_h = reshape(df_h, direction = "long", idvar = 'id', varying = list(1:5), v.names = "y")
    #Declare the data in a plm format which enables gmm to understand the data (id and time variable)
    df_long_h = pdata.frame(df_long_h, index = c("id", "time"))
    ##########################

    phi_mle_h <- mle_est(df_long_h)
  }
  ##The value to minimize according to phi_h
  return((phi_mle - mean(phi_mle_h))^2)
}
#ii_function(0, sim_index = 120, phi_index = 3)
###Herein, you need to change v=by hand the name of the data for wgich you would like to applied the method
nRep = 5000
##Get the value of the Indirect Inference Estimate

bias_II =c()
RMSE_II = c()

for(phi_index in seq(1, 5)){
II_estimate  = c()
for (i in seq(1, nRep)) {
  II = optimize(ii_function,
                interval = c(-1, 1), maximum = FALSE, sim_index = i, nRep_H=10,  phi_index = phi_index, data = dataDGP1_N100_T20, tol = 0.01)
  II_estimate = c(II_estimate, II$minimum)
}

bias_II = c(bias_II, mean(II_estimate) - Phi_values[1])
RMSE_II = c(RMSE_II, sd(II_estimate - Phi_values[1]))
}

II_results =data.frame(bias = bias_II, RMSE = RMSE_II)
write.csv(II_results, file = paste0("table_out_II", '_', "DGP1_N100_T20", '.csv' ))

#

























##########################
phi_index = sim_index  = 1
df = as.data.frame(dataDGP1_N100_T5[,,'Phi=0.3', 100]) 
df$id = seq(1, nrow(df))
df_long = reshape(df, direction = "long", idvar = 'id', varying = list(1:5), v.names = "y")
#Declare the data in a plm format which enables gmm to understand the data (id and time variable)
df_long = pdata.frame(df_long, index = c("id", "time"))
##########################
##Estimate the mle estimator
estimate = plm(
  y~-1 +lag(y,1),
  effect = c("individual"), 
  data = df_long,
  model = c("between")
)
phi_mle <- as.numeric(estimate$coefficients[2])

Phi_identifier = c(paste0('Phi=', phi_h))

##Simulate H paths as function of phi (the argument under which to minimize)
sim_h <-  sim_functionDGP1(N=100, T=5, Phi_values = phi_h)
phi_mle_h = c()


for(h in 1:nRep_H){
  
  ##########################
  df_h = as.data.frame(sim_h[,,1,h]) 
  df_h$id = seq(1, nrow(df_h))
  df_long_h = reshape(df_h, direction = "long", idvar = 'id', varying = list(1:5), v.names = "y")
  #Declare the data in a plm format which enables gmm to understand the data (id and time variable)
  df_long_h = pdata.frame(df_long_h, index = c("id", "time"))
  ##########################
  
  estimate_h = plm(
    y~lag(y),
    data = df_long_h,
    model = c("between")
  )
  phi_mle_h <- c(phi_mle_h, as.numeric(estimate_h$coefficients[2]))
}
##The value to minimize according to phi_h
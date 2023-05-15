library(haven)
library(plm)
library(dplyr)
rm(list = ls(all=TRUE))
load("New_DataSimulatedLoader.RData")
nRep = 5000
bias = 0
RMSE = 0
RMSEs = c()
Bias  = c()
Phi_values = c(0, 0.3, 0.6, 0.9, 0.99)
Phi_identifier = c(paste0(rep('Phi=', length(Phi_values)), Phi_values))

###Create the function for simulated Gmm method

MLENT <- function(data = dataDGP1_N100_T5, identifier = 'N=100&T=5', TimeT=5){#default
  
  for (phi_i in seq(1, length(Phi_values))) {
    for (i in 1:nRep) {
      df = as.data.frame(data[,,phi_i,i]) 
      df$id = seq(1, nrow(df))
      df_long = reshape(df, direction = "long", idvar = 'id', varying = list(1:TimeT), v.names = "y")
      #write_dta(df_long, 'df_long.dta')
      
      #Declare the data in a plm format which enables gmm to understand the data (id and time variable)
      df_long_h = plm.data(df_long, index = c("id", "time"))
      
      df_long_h = df_long_h %>% group_by(id) %>% mutate(y_lag = lag(y, 1))
      df_long_h = df_long_h %>% group_by(id) %>% mutate(ylag_bar = mean(y_lag, na.rm = TRUE))
      df_long_h = df_long_h %>% group_by(id) %>% mutate(y_bar = mean(y, na.rm = TRUE))
      
      ###Compute the MLE by hand
      cn = sum((df_long_h$y-df_long_h$y_bar)*(df_long_h$y_lag-df_long_h$ylag_bar), na.rm = TRUE)
      dn = sum((df_long_h$y_lag-df_long_h$ylag_bar)^2, na.rm = TRUE)
      estimate_mle = cn/dn
      
      bias = bias + (estimate_mle-Phi_values[phi_i])/nRep
      RMSE = RMSE+ (estimate_mle - Phi_values[phi_i])^2/nRep
    }
    RMSEs = c(RMSEs, sqrt(RMSE))
    Bias = c(Bias, bias)
  }
  
  table_out_MLE = data.frame(Bias, RMSEs)
  rownames(table_out_MLE) = Phi_identifier
  write.csv(table_out_MLE, file = paste0("table_out_MLE", '_', identifier, '.csv' ))
}

###dataDGP1_N100_T5
MLENT()

###dataDGP1_N100_T0
MLENT(dataDGP1_N100_T10, identifier = 'N=100&T=10', TimeT=10)

###dataDGP1_N100_T20
MLENT(dataDGP1_N100_T20, identifier = 'N=100&T=20', TimeT=20)

###dataDGP1_N200_T5
MLENT(dataDGP1_N200_T5, identifier = 'N=200&T=5', TimeT=5)

###dataDGP1_N200_T10
MLENT(dataDGP1_N200_T10, identifier = 'N=200&T=10', TimeT=10)

###dataDGP1_N200_T20
MLENT(dataDGP1_N200_T20, identifier = 'N=200&T=20', TimeT=20)

library(haven)
library(plm)
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

GmmNT <- function(data = dataDGP1_N100_T5, identifier = 'N=100&T=5', TimeT=5){#default

for (phi_index in seq(1, length(Phi_values))) {
for (i in 1:nRep) {
df = as.data.frame(data[,,phi_index,i]) 
df$id = seq(1, nrow(df))
df_long = reshape(df, direction = "long", idvar = 'id', varying = list(1:TimeT), v.names = "y")
#write_dta(df_long, 'df_long.dta')

#Declare the data in a plm format which enables gmm to understand the data (id and time variable)
df_long = pdata.frame(df_long, index = c("id", "time"))

z1 <- pgmm(y ~ lag(y, 1) | lag(y, 2:TimeT-2),#gmm condition E(\Delatyit*yit-s) s=2, ..., T-1
           data = df_long, effect = "individual", model = "twosteps")

dd = summary(z1, robust = FALSE)
bias = bias + (dd$coefficients[1]-Phi_values[phi_index])/nRep
RMSE = RMSE+ (dd$coefficients[1] - Phi_values[phi_index])^2/nRep
}
RMSEs = c(RMSEs, sqrt(RMSE))
Bias = c(Bias, bias)
}
table_out_GMM = data.frame(bias_ = Bias, RMSEs_ =  RMSEs)
rownames(table_out_GMM) = Phi_identifier
write.csv(table_out_GMM, file = paste0("table_out_GMM", '_', identifier, '.csv' ))
return(table_out_GMM)
}

###dataDGP1_N100_T5
GmmNT(TimeT=5)

###dataDGP1_N100_T0
GmmNT(dataDGP1_N100_T10, identifier = 'N=100&T=10', TimeT=10)

###dataDGP1_N100_T20
GmmNT(dataDGP1_N100_T20, identifier = 'N=100&T=20', TimeT=20)

###dataDGP1_N200_T5
GmmNT(dataDGP1_N200_T5, identifier = 'N=200&T=5', TimeT=5)

###dataDGP1_N200_T10
GmmNT(dataDGP1_N200_T10, identifier = 'N=200&T=10', TimeT=10)

###dataDGP1_N200_T20
GmmNT(dataDGP1_N200_T20, identifier = 'N=200&T=20', TimeT=20)


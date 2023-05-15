rm(list = ls(all=TRUE))
#Simulate the first GDP and the ones of the extension with beta*t added (default beta =0) or with covariate
set.seed(20290105)
Phi_values = c(0, 0.3, 0.6, 0.9, 0.99)
sim_functionDGP1<- function(N, T, Phi_values, beta=0, alpha_index = 1, covariate_coef = 0 ,  nRep = 5000){

  Phi_identifier = c(paste0(rep('Phi=', length(Phi_values)), Phi_values))
  
  #####################################  Arguments of the function  #####################################
  
  #N = number of observations
  #T = number of time points
  #Phi_values = all the values of Phi, in the DGP, that are considered
  #beta = if set to 1 we consider a dynamic panel model with an incident trend, that is the DGP2 in the extension
  #alpha_index = if we consider a fixed effect (DGP1) or not (extension)
  #covariate_index if set to 1it corresponds to adding a covariates (DGP1 of the extension section)
  
  #####################################  #####################################  #####################################  
  ##The dataset array
  Y <- array(NA, c(N, T ,length(Phi_values), nRep), dimnames = list(NULL, NULL, Phi_identifier, NULL))
  
  ##Start Replication loop
  for(rep_index in seq(1, nRep)){
  
  for (phi_index in seq(1, length(Phi_values))) {
    ##Generate \alpha
    alpha <- matrix(rnorm(N,0,1),N,1)*alpha_index
    beta_i <- matrix(rnorm(N,0,1),N,1)*beta
    ##Generate Epsilon
    epsilon <- matrix(rnorm(N*T,0,1),N,T)
    
    ##Generate the covariates: This part of the function need to be customized if one need to consider other distribution for the covariate.
    ###Herein, to simplify we just consider a normal distribution as epsilon
    covariate <- matrix(rnorm(N*T,0,1),N,T)
    
    ##Fill in the array dataset with the DGP
    
    ##Generate y0
    #y0 <-  matrix(rnorm(N,alpha/(1-Phi_values[phi_index]),1/sqrt(1-Phi_values[phi_index]^2)),N,1)
    y0 <-  matrix(rnorm(N,alpha/(1-Phi_values[phi_index]),1/(1-Phi_values[phi_index]^2)),N,1)
    #Y[,1, phi_index,1] <- y0
    ##Generate y1
    Y[,1, phi_index, rep_index] <- alpha + beta_i*1 + covariate[,1]*covariate_coef + Phi_values[phi_index]*y0 + epsilon[,1]
    ##Generate the full dataset

    for (t in 2:T){
      Y[,t, phi_index, rep_index] <- alpha + beta_i*t + covariate[,t]*covariate_coef +  Phi_values[phi_index]*Y[,t-1, phi_index, rep_index] + epsilon[,t]
    }
  }
  }
  return(Y)
}

#Generate the datasets for each combination of values

dataDGP1_N100_T5  = sim_functionDGP1(N=100, T=5, Phi_values)
dataDGP1_N100_T10 = sim_functionDGP1(N=100, T=10, Phi_values)
dataDGP1_N100_T20 = sim_functionDGP1(N=100, T=20, Phi_values)

dataDGP1_N200_T5  = sim_functionDGP1(N=200, T=5, Phi_values)
dataDGP1_N200_T10 = sim_functionDGP1(N=200, T=10, Phi_values)
dataDGP1_N200_T20  = sim_functionDGP1(N=200, T=20, Phi_values)

##Save the results, so we do not need to simulate each time
save.image(file='New_DataSimulatedLoader.RData') ## This correponds to the DGPb in our document
###To get the simulations of a particular Phi use, e.g:
dataDGP1_N100_T5[,,'Phi=0.3',]



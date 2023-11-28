library(depmixS4)
library(tidyverse)

# https://medium.com/analytics-vidhya/hidden-markov-models-for-time-series-in-r-studio-5ae2b9fb0701

# timeseries of number of prescriptions
prescriptions <- c(12,16,45,45,56,67,78,98,120,124,156)

# Initialised as 2 states for now
HMM_model <- depmix(prescriptions ~ 1, nstates=2, ntimes=length(prescriptions))

HMM_fm <- fit(HMM_model)

## Transition probabilities
summary(HMM_fm)

HMM_fm@transition

##posterior states
probs <- posterior(HMM_fm)

probs

plot(probs)
plot(HMM_fm)


# Apply to arrivals data

arrivals_hmm1 <- depmix(arrivals ~1+weekend, nstates=2, data=arrivals_1hr)

arrivals_fit1 <- fit(arrivals_hmm1)

summary(arrivals_fit1)

b<-simulate(arrivals_fit1, nsim = 36)

predict(arrivals_hmm1)

methods(class="depmix")

?methods


###### THis works but it not a forecast.
# Picked up stack overflow:

#n_state <- 2

## My series
draws <- data.frame(obs=rnorm(10000))

# Model
#mod <- depmix(obs ~ 1, data = draws, nstates = n_state, stationary=TRUE)
#fit.mod <- fit(mod)

# extract the state-transition matrix
#transition_mat <- rbind(getpars(getmodel(fit.mod,"transition",1)),getpars(getmodel(fit.mod,"transition",2)))

transition_mat <- rbind(getpars(getmodel(arrivals_fit1 ,"transition",1))
                        ,getpars(getmodel(arrivals_fit1 ,"transition",2)))

# extract the probability of the states at the final time point in the data (t=T)
# this will act as a "prior" to compute the forecasted state distributions
prior_vec <- as.numeric(posterior(arrivals_fit1)[1000,-1])
#prior_vec <- as.numeric(posterior(fit.mod)[1000,-1])

# state-wise predictions for the observed variables
pred_r_by_state <- c(getpars(getmodel(arrivals_fit1,"response",1))[1],
                     getpars(getmodel(arrivals_fit1,"response",2))[1])
# pred_r_by_state <- c(getpars(getmodel(fit.mod,"response",1))[1],
#                      getpars(getmodel(fit.mod,"response",2))[1])


# for T + 1
# the forecasted state distribution is (prior_vec %*% transition_mat)
# so hence the prediction of the observed variable is
sum(pred_r_by_state * (prior_vec %*% transition_mat))

# for T + 2
# the forecasted state distribution is (prior_vec %*% transition_mat %*% transition_mat)
# so hence the prediction of the observed variable is
sum(pred_r_by_state * (prior_vec %*% transition_mat %*% transition_mat))

# for T + 3
sum(pred_r_by_state * (prior_vec %*% transition_mat %*% transition_mat %*% transition_mat))

#prediction_function
hmm_forecast <- function(prior_vec, transition_mat, pred_r_by_state, forecasts){
  require(expm)
  out <- data.frame(forecast_number =  seq(forecasts), forecast_value = as.numeric(NA))
  
  for (t in seq(forecasts)){
  
  out[t,2] <- sum(pred_r_by_state * (prior_vec %*% (transition_mat %^% t)))
  
  }
  return(out)
}


a<-hmm_forecast(prior_vec, transition_mat,pred_r_by_state , 36)

a



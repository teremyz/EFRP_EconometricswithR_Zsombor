library(dplyr)

source('C:/Users/Hp/Desktop/kurzus2020_2021/EFRG/financial econometrics/hw_1_aic_bic/func_analysis_aic_bic_perf.R')
source('C:/Users/Hp/Desktop/kurzus2020_2021/EFRG/financial econometrics/hw_1_aic_bic/func_param_def_aic_bic.R')

#Empty list for inputs
input = list()

#Define inputs for time series simulation
input = def_params_for_sim(input)

#Define input for model fitting
input = params_for_model_fit(input)

#It yields the result of successful fits of auto.arima based on AIC and BIC
output = complex_analysis(input)


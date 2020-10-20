def_params_for_sim = function(input){
  # Function defines parameters for simulation of times series following ARMA  DGP process
  # theta is a vector containing the coefficients of the time series own laggs
  # gamma contains the coefficients MA processes
  # sig is the assumed variances in DGP
  # N is equal the Number of simulations
  # t is the length of time series
  
  input$theta = c(0.5)
  input$gamma = c(0.1)
  input$sig = c(0.25, 0.5, 1, 2)
  #input$sig = c(0.25)
  input$t = c(100, 250, 500, 1000)
  #input$t = c(100)
  input$N = 100
  input
}

params_for_model_fit = function(input){
  # Function defines parameters for the comparison arbitrarily picked infomation criterions
  # crit_1 and crit_2 are the two information criterion that will be compared
  # max_q and max_p give the maximum value of maximum order of AR and MA will be tested
  
  input$crit_1 = 'aic'
  input$crit_2 = 'bic'
  input$max_q = 3
  input$max_p = 3
  input
}
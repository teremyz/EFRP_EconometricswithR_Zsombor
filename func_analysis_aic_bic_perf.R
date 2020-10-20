data_simulation = function(input){
  # Simulate N time series following ARIMA DGP process
  # Theata and gamma are vectors containing the coefficients of ARs and MAs
  # df for storing the solutin
  df = data.frame(matrix(0L, nrow = input$t, ncol = input$N))
  
  for (i in 1:input$N){
    #Iterate through the df's columns and fill them up with simulated ARIMA processes
    df[,i] = arima.sim(list(ar = input$theta, ma = input$gamma), n = input$t, sd = input$sig)
  }
  # It results a txN matrix 
  return(df)
}


model_fit = function(data, input){
  # Funtion fits an arima model for every column of a table
  # It gives back the number of AR and MA lags
  # being estimated by auto.arima function based on different information criterions
  # input variable is a list that contains 
  
  # Matrix for the results
  res_mat = matrix(0L, nrow = input$N, ncol = 2)
  
  for (i in 1:input$N){
    # Iterates through the data columns and fits auto arima model
    auto_arima = forecast::auto.arima(data[,i], max.p = input$max_p, 
                                      max.q = input$max_q, ic = input$crit_1)[['arma']]
    #Store the order of AR
    res_mat[i, 1] = auto_arima[1]
    #Store the order of MA
    res_mat[i, 2] = auto_arima[2]
  }
  # Matrix has two colums: #ARs, #MAs
  res_mat
}


complex_analysis = function(input){
  # Perform the whole analysis
  # 1. Simulate time series in Nxt matrix
  # 2. Simulation can be done for different parameters
  # 3. fit models based on two information criteria
  # 4. Giving back a ratio indicating the right shoot of auto.arima function
  # based on AIC and BIC
  # A solution is right if auto.arima function finds the exact number of MAs and ARs at the same time
  # real_q and real_p are the real values being sought. So the oreder of MA and AR
  
  real_q = length(input$gamma)
  real_p = length(input$theta)
  
  #Separately stored t and sig because those will be overwritten few lines later
  iter_t = input$t
  iter_sig = input$sig
  
  crit_1 = input$crit_1
  crit_2 = input$crit_2
  
  # Vars for the results
  t_sig_aic = c()
  t_sig_bic = c()
  
  #Error messages
  if (input$crit_1 != 'aic')
      if(input$crit_1 != 'bic')
        if (input$crit_1 != 'aicc'){
              stop('crit_1 must be equal to aic, bic or aicc')}
  
  if (input$crit_2 != 'aic')
    if(input$crit_2 != 'bic')
      if (input$crit_2 != 'aicc'){
        stop('crit_2 must be equal to aic, bic or aicc')}
  
  
  if (input$N < 100){
    print('Number of simulations is too low. The analysis will not be reliable')}
  
  for (t in iter_t){
    for (sig in iter_sig){
      #Double for cycles iterate through all possible combination of t (length of time series) and sig (noise)
      
      input$t = t
      input$sig = sig
      
      # Simulate dataframe containing N columns and t rows based on ARMA model
      sim_df = data_simulation(input)
      
      # It fits arima model and picks based on criteria 1
      input$crit_1 = crit_1
      res_aic = model_fit(sim_df, input)
      
      # It fits arima model and picks based on criteria 2
      input$crit_1 = crit_2
      res_bic = model_fit(sim_df, input)
      
      #Filter the results based on real_p and real_q
      succ_aic = res_aic[(res_aic[,1] == real_p & res_aic[,2] == real_q),] %>% nrow()
      succ_bic =  res_bic[(res_bic[,1] == real_p & res_bic[,2] == real_q),] %>% nrow()
      
      # If there is 0 right shoot then the result of the above filtering will be NULL
      # if converts to 0
      if (is.null(succ_aic)){
        succ_aic = 0
      }
      
      if (is.null(succ_bic)){
        succ_bic = 0
      }
      
      #Ration of successful shoots in a list
      t_sig_aic = append(t_sig_aic, succ_aic/input$N)
      t_sig_bic = append(t_sig_bic, succ_bic/input$N)
      
    }
  }
  # Convert the list to matrix where rows indicate the different values of the noise
  # Columns indicate the different lenfgth of time series
  list(aic = matrix(t_sig_aic, nrow = length(iter_t), ncol = length(iter_sig)),
       bic = matrix(t_sig_bic, nrow = length(iter_t), ncol = length(iter_sig)))
}
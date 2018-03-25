folds = 5
lags = 5
neurons = c(1,5,10,15,20,25,30,35,40,45,50)
#df is a dataframe of preprocessed data for a particular user
#y is the true value
#maxs and mins are the vectors c
c_v = function(df, lags, neurons, folds,maxs, mins, y) {
  foldID = rep(1:folds, length.out = dim(df)[1])
  foldID = sample(foldID)
  err = as.data.frame(matrix(ncol = lags, nrow = neurons))
  colnames(err) = paste('lag',1:lags, sep='')
  #loop neurons
  for(m in 1:length(neurons)) {
    print(m)
    neuron = neurons[m]
    #loop lags
    for(k in 1:lags){
      print(k)
      nn = names(df[, c(2:(2+3*k))])
      f = as.formula(paste("mood_t ~", paste(nn[!n %in% "mood_t"], collapse = " + ")))
      # of inputs
      input = 1 + 3*k
      para = (input  + 1)* neuron + neuron + 1 
      error = numeric()
      #cross validation for a particular set of neuron and lag
      for(u in 1:folds) {
        train = df[which(foldID != u), ]
        test = df[which(foldID == u),]
        fit = neuralnet(f, data = cur_nn, hidden = neuron,
                        stepmax = 1e+08,
                        act.fct = 'logistic', 
                        err.fct = 'sse', algorithm = "rprop+",
                        lifesign = 'none', learningrate.factor = list(minus = 0.5, plus = 1.2),
                        linear.output = T, likelihood = F)
        yhat = neuralnet::compute(fit,test[,c(2:(2+3*k))])
        yhat = yhat$net.result*(maxs[1] - mins[1])+mins[1]
        error[u] = mean((yhat - y[foldID == u])^2)
      }
      err[m,k] = mean(error)
    }
  }
  
  return(err)
}
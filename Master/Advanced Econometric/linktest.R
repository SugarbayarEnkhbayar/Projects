
linktest = function(model) {
  # written by dr Rafal Wozniak, Faculty of Economic Sciences, University of Warsaw
  # 2019-04-18
  #
  # arguments:
  # ------------------
  # model - model estimated by glm function
  
  # check if it is of class 'glm'
  
  # Linktest
  y = model$y
  yhat = log(model$fitted.values/(1-model$fitted.values))
  yhat2 = yhat^2
  # auxiliary regression
  aux.reg = glm(y~yhat+yhat2, family=binomial(link=model$family$link))
  show(summary(aux.reg))
  return(aux.reg)
}
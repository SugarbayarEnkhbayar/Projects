
marginaleffects = function(model, x) {
  # written by dr Rafal Wozniak, Faculty of Economic Sciences, University of Warsaw
  # 2019-04-18
  #
  # arguments:
  # ------------------
  # model - model estimated by glm function
  # x - user defined characteristics
  # comments:
  # ------------------
  # Please make sure that the vector contains one for the constant term.
  
  # check if model is of class 'glm'
  # check which variables are dummies
  # Statistical significance of the marginal effects
  
  # independent variables
  indeps = model$model[,-1]
  # dummy variables indices
  dvi = rep(0, times=ncol(indeps))
  for(i in 1:ncol(indeps)) {
    if(is.numeric(indeps[,i])==1) {
      if(length(unique(indeps[,i]))==2L) {
        # the variable is dummy
        dvi[i] = 1
      }
    }
  } 
  dvi = which(dvi==1)+1
  # plus one because of the constant in the model
  if(length(dvi)>0){ 
    x0 = x
    x0[dvi] = 0
    x1 = x0
  }
  
  # Marginal effects
  if(model$family$link=="probit") {
    meff = as.vector(dnorm(x%*%model$coefficients))*model$coefficients
    if(length(dvi)>0) {
      for(i in 1:length(dvi)) {
        x1[dvi[i]] = 1
        meff[dvi[i]] = pnorm(x1%*%model$coefficients)-pnorm(x0%*%model$coefficients)
        x1[dvi[i]] = 0
      }
    }
  } else if(model$family$link=="logit") {
    meff = as.vector(dlogis(x%*%model$coefficients))*model$coefficients
    if(length(dvi)>0) {
      for(i in 1:length(dvi)) {
        x1[dvi[i]] = 1
        meff[dvi[i]] = plogis(x1%*%model$coefficients)-plogis(x0%*%model$coefficients)
        x1[dvi[i]] = 0
      }  
    }
  }
  meff = cbind(meff, x)  
  colnames(meff) = c("Marginal effects", "at X=")
  if(length(dvi)>0) {
    rn = rownames(meff)
    for(i in 1:length(dvi)) {
      rn[dvi[i]] <- paste(rn[dvi[i]], "!", sep="")
    }
    rownames(meff) = rn
  }
  show(meff[-1,])
  if(length(dvi)>0){
    cat("(!) indicates marginal effect was calculated for discrete change of dummy variable from 0 to 1")
  }
  
}
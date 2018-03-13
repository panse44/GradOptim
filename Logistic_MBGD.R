Logistic.MBGD = function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01, n_Iter = 10, batch_size = 10, SE_estimates=FALSE)
{
  if(is.null(train.Y))
  {
    
    #warning("Response Variable cannot be Null")
    stop("Response Variable cannot be Null")
  }
  
  if(is.null(train.X))
  {
    
    stop("Predictor Variable cannot be Null")
    
  }
  
  if(length(unique(train.Y)) > 2)
  {
    stop("The response variable contains more than 2 classes. Use the OneVsAll method instead.")
  }
  
  N=nrow(train.X)
  temp=N%%batch_size
  train.X=train.X[1:(N-temp),]
  train.Y=train.Y[1:(N-temp)]
  
  train.X=input.split(df=train.X)
  SE=0
  if(SE_estimates)
  {
    SE=MBGD.Logistic.Boot(train.Y,train.X,alpha,tolerance,n_Iter,batch_size)
    
  }
  train.X = cbind(1,train.X)
  
  nweights = ncol(train.X)
  weight = matrix(rep(0,nweights),nrow=nweights)
  N_new=nrow(train.X)
  delta = 1
  
  for( i in 1:n_Iter )
  {  
    for( j in seq(1,N_new,batch_size))
    {
      delta = t(train.X)[,j:j-1+batch_size] %*% (sigmoid(train.X[j:j-1+batch_size,] %*% weight) - train.Y[j:j-1+batch_size])
      weight = weight - alpha*delta
    }
  }
  
  rownames(weight)=c(colnames(train.X))
  colnames(weight) = "Estimate"
  
  loglik.full = sum((train.Y*log(sigmoid(train.X %*% weight))) + ((1-train.Y)*log(1-sigmoid(train.X %*% weight))))
  loglik.null = sum((train.Y*log(sigmoid(weight[1]))) + ((1-train.Y)*log(1-sigmoid(weight[1]))))
  object = list(Weights = weight,
                Null.Deviance = -2*loglik.null,
                Residual.Deviance = -2*loglik.full,
                DoF.Null = nrow(train.X)-1,
                Dof.Residual = nrow(train.X)-ncol(train.X),
                AIC = -2*loglik.full + 2* nweights,
                BIC = -2*loglik.full + nweights*log(nrow(train.X)),
                Standard.Error= cbind(weight,SE))
  
  return(object)
  
}


#Sigmoid Transformation
sigmoid = function(z)
{
  return(1/(1 + exp(-z)))
}


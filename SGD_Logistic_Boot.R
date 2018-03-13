SGD.Logistic.Boot = function(train.Y,train.X,alpha=0.001,tolerance=0.01,n_Iter = 10)
{
  col=ncol(train.X)
  data=cbind(train.X,train.Y) 
  Boot=matrix(0,col+1,350)
  for(i in 1:350)
  {
    index=sample(1:nrow(data),nrow(data),replace = TRUE)
    temp=data[index,]
    Y=temp[,col+1]
    X=temp[,1:col]
    X = cbind(1,X)
    nweights = ncol(X)
    X=as.matrix(X)
    delta = 1
    
    
    weight = matrix(rep(0,nweights),nrow=nweights)
    
    
    for( j in 1:n_Iter )
    {  
      for( k in 1:N)
      {
        delta = t(X)[,k] %*% (sigmoid(X[k,] %*% weight) - Y[k])
        weight = weight - alpha*delta
      }
    }
    Boot[,i]=weight
    
  }
  
  SE=apply(Boot,1,sd)
  return(SE)
    
}
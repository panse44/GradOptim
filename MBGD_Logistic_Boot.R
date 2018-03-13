MBGD.Logistic.Boot = function(train.Y,train.X,alpha=0.001,tolerance=0.01,n_Iter = 10, batch_size = 10)
{
  N=nrow(train.X)
  temp=N%%batch_size
  train.X=train.X[1:(N-temp),]
  train.Y=train.Y[1:(N-temp)]
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
    N_new = nrow(X)
    nweights = ncol(X)
    X=as.matrix(X)
    delta = 1
    s
    
    weight = matrix(rep(0,nweights),nrow=nweights)
    
    
    for( j in 1:n_Iter )
    {  
      for( k in seq(1,N_new,batch_size))
      {
        delta = t(X)[,k:k-1+batch_size] %*% (sigmoid(X[k:k-1+batch_size,] %*% weight) - Y[k:k-1+batch_size])
        weight = weight - alpha*delta
      }
    }
    Boot[,i]=weight
    
  }
  
  SE=apply(Boot,1,sd)
  return(SE)
    
}
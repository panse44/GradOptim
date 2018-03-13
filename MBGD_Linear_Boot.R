# A function to calculate the estimates of the standard error for each of the dependent 
# variable used in the regression task which is passed as an argument to the function. 
# It uses the Mini Batch Gradient descent optimization technique for each iteration of 
# the bootstrapping sample to estimate the coefficients.
MBGD_Linear_Boot <- function(train.Y,train.X,alpha=0.001,tolerance=0.01)

{
  col=ncol(train.X)
  data=cbind(train.X,train.Y) 
# Initializing the matrix for storing the estimates for each of the bootstrap sample   
  Boot=matrix(0,col+1,350)
# A for loop to compute the the estimates for a particular bootstrap sample    
  for(i in 1:350)
   {
      index=sample(1:nrow(data),nrow(data),replace = TRUE)
      temp=data[index,]
      Y=temp[,col+1]
      X=temp[,1:col]
      X=scale(X)
      Y=scale(Y)
      X = cbind(1,X)
      nweights = ncol(X)
      X=as.matrix(X)
      delta = 1
      weight = matrix(rep(0,nweights),nrow=nweights)

# While loop which etimates the weights for the given sample of data        
      while(sum(delta^2) > tolerance)
      {
        delta = t(X)%*%(Y - (X%*%weight))
        weight = weight + alpha*delta

      }
     Boot[,i]=weight
     
  }
# Computing the final Standard error estimated by taking the Standard deviation across the row  
 SE=apply(Boot,1,mean)
 return(SE)
   
}  
  




  
  
  
  
  
  
  
  
  
  
  
  

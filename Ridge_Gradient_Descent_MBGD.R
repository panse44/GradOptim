##############################################################################################################
#   Function to estimate the weight of the Multiple Linear Regression problem with L2 penalty on the         #
#   coefficients using Mini Batch Gradient Descent technique.                                                           #
##############################################################################################################

Ridge_Gradient_Descent_MBGD <- function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01,lambda=0,n_Iter=1,batch_size=10)

{

# Checking of the Output variable is NULL or not    
  if(is.null(train.Y))
  {
   stop("Response Variable cannot be Null")
  }
 
# Checking if the predictor variables is NULL or not   
  if(is.null(train.X))
  {
   stop("Predictor Variable cannot be Null")
  }
  
  N=nrow(train.X)
  temp=N%%batch_size
  train.X=train.X[1:(N-temp),]
  train.Y=train.Y[1:(N-temp)]
  
# Passing the input data through a user defined function to convert categorical data into dummy variables    
  train.X=input.split(df=train.X)
  train.X = cbind(1,train.X)
  nweights = ncol(train.X)
  delta = 1
  weight = matrix(rep(0,nweights),nrow=nweights)
  N=nrow(train.X)
  
# A while loop which iterates till the weights becomes close to each other thresholded by a tolerance value  
  for( i in 1:n_Iter)
  {
    for( i in 1:seq(1,N,batch_size))
    {

# Gradient for a Ridge Cost Linear Regression is given by the following formula          
          delta = t(train.X)[,i:i-1+batch_size]%*%(train.Y[i:i-1+batch_size] - train.X[i:i-1+batch_size,]%*%weight)-(lambda*diag(nweights) %*% c(0,weight[-1]))

# Updating the weights for each iteration           
          weight = weight + alpha*delta 
        
    }
  }
  rownames(weight)=c(colnames(train.X))
  
# At last returning the estimated weights to the original calling function  
  return(weight)

}









  
  
  
  
  

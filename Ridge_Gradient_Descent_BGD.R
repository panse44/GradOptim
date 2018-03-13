##############################################################################################################
#   Function to estimate the weight of the Multiple Linear Regression problem with L2 penalty on the         #
#   coefficients using Gradient Descent technique.                                                           #
##############################################################################################################

Ridge_Gradient_Descent_BGD <- function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01,lambda=0)

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

# Passing the input data through a user defined function to convert categorical data into dummy variables    
  train.X=input.split(df=train.X)
  train.X = cbind(1,train.X)
  nweights = ncol(train.X)
  delta = 1
  weight = matrix(rep(0,nweights),nrow=nweights)
  #weight = solve( (t(train.X) %*% train.X) + (lambda * diag(nweights))) %*% t(train.X) %*% train.Y
  
# A while loop which iterates till the weights becomes close to each other thresholded by a tolerance value  
  while(sum(delta^2) > tolerance)
  {

# Gradient for a Ridge Cost Linear Regression is given by the following formula          
          delta = t(train.X)%*%(train.Y - train.X%*%weight)-(lambda*diag(nweights) %*% c(0,weight[-1]))

# Updating the weights for each iteration           
          weight = weight + alpha*delta 
        
  }
  
  
# At last returning the estimated weights to the original calling function  
  return(weight)

}









  
  
  
  
  

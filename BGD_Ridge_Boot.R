############################################################################################################
# Function to Calculate the Standard error for the Ridge Multiple Linear Regression using Gradient Descent #
#             for all the passed variables using the bootstrapped samples                                  #
############################################################################################################

BGD_Ridge_Boot <- function(train.Y,train.X,alpha=0.001,tolerance=0.01,lambda=0)
{
  #Calculating the number of columns in the passed Training data
  col=ncol(train.X)
  
  #Combining the X and Y portions of data to get the whole dataset
  data=cbind(train.X,train.Y) 
  
  #Defining a boot matrix to store the weights for all bootstrapped variables code fixing it to 350 samples
  Boot=matrix(0,col+1,350)
  
  # For each bootstrapped sample it will try to estimate the corresponding weights using Gradient Descent 
  # considering the Ridge Regression cost function and the weights will be store in the Boot matrix 
  for(i in 1:350)
   {
  # Generating a bootstapped Sample    
      index=sample(1:nrow(data),nrow(data),replace = TRUE)
      temp=data[index,]
      
  # Preparing the data for performing the Gradient descent technique    
      Y=temp[,col+1]
      X=temp[,1:col]
      X=scale(X)
      Y=scale(Y)
      X = cbind(1,X)
      nweights = ncol(X)
      X=as.matrix(X)
      delta = 1
      #weight = matrix(rep(0,nweights),nrow=nweights)
      weight = solve( (t(X) %*% X) + (lambda * diag(nweights))) %*% t(X) %*% Y
      
  # A while loop which performs the Gradient descent for the Ridge Cost        
      while(sum(delta^2) > tolerance)
      {
  # Gradient for a Ridge Cost Linear Regression is given by the following formula     
        delta = t(X)%*%(Y - X%*%weight)-(lambda*diag(nweights) %*% c(0,weight[-1]))
  # Updating the weights for each iteration      
        weight = weight + alpha*delta 
        
      }
  # Storing the eatimated weights for a particular bootstrap sample in the Boot matrix    
     Boot[,i]=weight
     
  }
  
  # 
  SE=apply(Boot,1,sd)
 return(SE)
   
}  
  



  
  
  
  
  
  
  
  
  
  
  

###############################################################################################################
#   Function to train the data for a Multiple Linear Regression using the L2 Norm penaly for coefficients     #
#   and return the estimated weights using Gradient descent optimization technique. It takes on a grid        #
#   of lamda parameters and estimate the weights based on the best selected lambda using the CV technique     #
#   At last it trains the whole data on the selected best lambda parameter and return different parameters    #
###############################################################################################################


Ridge_Train_MBGD <- function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01,lambda=0,nfolds=5,plot=FALSE,SE_estimates=FALSE,n_Iter=1)
{
  
  if(is.null(train.Y))
  {
    stop("Response Variable cannot be Null")
  }
  
  if(is.null(train.X))
  {
    stop("Predictor Variable cannot be Null")
  }

#Calculating the number of predictors in the training dataset  
  col=ncol(train.X)

#Calculating the number of samples present in the training dataset  
  row=nrow(train.X)
  data=cbind(train.X,train.Y)
  options(warn=-1)
  
# Splitting the data into different folds for performing cross validation using different lambda parameters  
  temp=split(data,rep(1:ceiling(row/nfolds)))
  library(dplyr)

# Initilizing the Test_Error vector which will store the Test Error for each Lambda parameter using k-fold CV    
  Test_error=numeric()
  
# Outer Loop iterates for each possible values of Lambda  
  for(j in lambda)
  {

# Inner loop iterates over different folds created in the dataset for calculating the Test error for a 
# particular lambda parameter    
    error=numeric()
    for( i in 1:nfolds)
    {
      
# For each fold data will be trained on other remaining folds and weights will be estimated using Gradient Descent Technique      
      test_data=as.data.frame(temp[[i]])
      train_data=setdiff(data,test_data)
      Y=train_data[,col+1]
      X=train_data[,1:col]
      
# Calling the function to estimate weights using Gradient Descent technique for a Ridge Cost
      weight=Ridge_Gradient_Descent_MBGD(train.Y = Y,train.X = X,alpha = alpha,tolerance = tolerance,lambda = j)
      
      test.Y=test_data[,col+1]
      test.X=test_data[,1:col]
      
      test.X=input.split(test.X)
      test.X=data.matrix(test.X)

# Predicting the output values on test data using the estimated weights and thus calculating the RMSE            
      predict_value=weight[1]+test.X%*%weight[-1]
      MSE=sum((predict_value-test.Y)^2)/nrow(test.X)
      RMSE=sqrt(MSE)
      error=append(error,RMSE)
      
    }
  
# At last final test for a particular lambda will be the mean of all Test errors for different folds    
    Test_error=append(Test_error,mean(error))
    
  }  

# The optimum value of Lamba parameter will be the one with the lowest test error calculated using Cross Validation    
  best_lambda=lambda[which.min(Test_error)]
  
# Final weights will be calculated by training the entire dataset on the optimum value of lambda
    final_weight=Ridge_Gradient_Descent_MBGD(train.Y,train.X,alpha = alpha,tolerance = tolerance,lambda = best_lambda,n_Iter = n_Iter)    

# The corresponding Test error will be the minimum of all the test error calculated for all values of Lambda parameter
    Min_error=min(Test_error)

  train.X=input.split(df=train.X)
  
# Calculating the different paramters associated with the model for better interpretation
  predict_values=final_weight[1]+train.X%*%final_weight[-1]
  residual=train.Y-predict_values
  SSE=sum(residual^2)
  SST=sum((train.Y-mean(train.Y))^2)
  N=nrow(train.X)
  P=ncol(train.X)-1
  RSE=sqrt(SSE/(N-P-1))
  R_sq=1-(SSE/SST)
  R_sq_Adj=R_sq-((P-1)/(N-P))*(1-R_sq)
  AIC=N+N*log10(2*pi)+N*log10(SSE/N)+2*(P+2)
  BIC=N+N*log10(2*pi)+N*log10(SSE/N)+log10(N)*(P+2)
  
# IF the plt varibale is made TRUE while calling the function then the corresponding graphs will be printed out  
  if(plot==TRUE)
  {
    readline("Residual Error Graphs:Press Enter to continue plotting with the graph")
    plot(predict_values,scale(residual),main = "Standardized Residual Vs Fitted Value",xlab = "Fitted Value",ylab = "Standardized Residuals")
    readline("Residual Error Graphs:Press Enter to continue plotting with the graph")
    qqnorm(scale(residual),main = "Normal Q-Q plot of Residuals")
    plot(Leverage,scale(residual),main = "Residual Vs Leverage",xlab = "Leverage", ylab = "Standardized Residuals")
    
  }
  
# If the user wants to have Standard error of the estimates then it will be calculated in the below lines  
  SE=0
  if(SE_estimates==TRUE)
  {

# Function which calculates the Standard Error of the weights        
    SE=MBGD_Ridge_Boot(train.Y,train.X,lambda = best_lambda)
    
  }
  
# Returning all the important parameters in a list object  
  object=list(Coefficients=final_weight,Optimized_lambda=best_lambda,Minimum_RMSE=Min_error,
              Residuals=residual,Residuals_Summary=summary(residual),Fitted_Values=predict_values,Residual_Std_Error=RSE,
              R_Square=R_sq,R_square_Adjst=R_sq_Adj,AIC=AIC,BIC=BIC,Standard_Error= cbind(weight,SE))
  
  return(object)
  
}

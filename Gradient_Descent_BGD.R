# A function to build a linear regression model on the given training data by estimating the
# coefficients using the Batch Gradient Descent Optimization technique.

Gradient_Descent_BGD <- function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01,plot=FALSE,SE_estimates=FALSE)
{
 
# Check if all the required arguments has been passed or not  
  if(is.null(train.Y))
  {
  stop("Response Variable cannot be Null")
  }
  
  if(is.null(train.X))
  {
  stop("Predictor Variable cannot be Null")
  }
  
# Use the Input Slpit function to convert the Categorical and Numerical Variables  
  train.X=input.split(df=train.X)
  
# Call the BGD_Linear_Boot function if the corresponding argument passed is set as TRUE  
  SE=0
  if(SE_estimates)
  {
    SE=BGD_Linear_Boot(train.Y,train.X,alpha,tolerance)
    
  }

# Add an extra column of 1's to the existing dataset    
  train.X = cbind(1,train.X)
  
# Number of coefficients to be estimated  
  nweights = ncol(train.X)
  
# Initializing the loop variables  
  count = 0
  delta = 1
  weight = matrix(rep(0,nweights),nrow=nweights)
  
# A while loop for estimating the weights
              while(sum(delta^2) > tolerance)
                {
                delta = t(train.X)%*%(train.Y - train.X%*%weight)
                weight = weight + alpha*delta
                count = count + 1
                }

# Calculating the various metrics of the model using the estimated coefficients
  predict_values=weight[1]+train.X[,-1]%*%weight[-1]
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
  
# If the plot variables is set as TRUE then plot the corresponding residual plots  
  if(plot==TRUE)
  {
    readline("Residual Error Graphs:Press Enter to continue plotting with the graph")
    plot(predict_values,scale(residual),main = "Standardized Residual Vs Fitted Value",xlab = "Fitted Value",ylab = "Standardized Residuals")
    readline("Residual Error Graphs:Press Enter to continue plotting with the graph")
    qqnorm(scale(residual),main = "Normal Q-Q plot of Residuals")
    
  }
  
  
# Combines all the resutls in a list object which will be the output of the function  
  object=list(Coefficients=weight,Residuals=residual,Residuals_Summary=summary(residual),Fitted_Values=predict_values,Residual_Std_Error=RSE,
              R_Square=R_sq,R_square_Adjst=R_sq_Adj,AIC=AIC,BIC=BIC,Standard_Error= cbind(weight,SE))
  

  return(object)
  
}





    












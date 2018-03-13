# A function which predicts the output values for the given test dataset based on the coefficients
# passed as an argument to the function. It also returns the various metrics of accuracy if the
# corresponding true values are also passed to the function.

Predict <- function(weights,test.X=NULL,test.Y=NULL)
{

# Check if the test data has been provided or not    
  if(is.null(test.X))
  {
    warning("Testing dataset can't be NULL")
    stop()
    
  }
  
# Use the input split function to convert the Numerical and Categorical functions  
  test.X=input.split(test.X)

# Compute the predicted values for the test dataset provied to the function    
  predict_values=weights[1]+test.X%*%weights[-1]

# Compute the final list object depending on whether the corresponding true values has been 
# passed or not  
  if(is.null(test.Y))
  {
   object=list(predict_values)
  }
  else
  {
    MSE=sum((predict_values-test.Y)^2)/nrow(test.X)
    RMSE=sqrt(MSE)
    MAE=sum(abs(predict_values-test.Y))/nrow(test.X)
    MAPE=sum(abs(predict_values-test.Y)/test.Y)/nrow(test.X)*100
    object=list(Prediction=predict_values,MSE=MSE,RMSE=RMSE,MAE=MAE,MAPE=MAPE)
  }
  
  return(object)
}



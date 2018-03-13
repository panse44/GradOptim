#A function to build a multi-class logistic regression model on the given training data by 
#estimating the coefficients using the Stochastic Gradient Descent Optimization technique.
OneVsAll.SGD = function(train.Y=NULL, train.X=NULL, alpha = 0.001, tolerance = 0.01, n_Iter = 10,SE_estimates = FALSE)
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
  output = train.Y
  # Use the Input Slpit function to convert the Categorical and Numerical Variables
  train.X=input.split(df=train.X)
  #Creating a copy of the train data to pass as an argument to the SGD.Logistic.Boot function
  train.X.Boot = train.X
  train.X = cbind(1,train.X)
  #Number of coefficients to be estimated
  nweights = ncol(train.X)
  #Calculating the number of classes in the response variable
  num.classes = length(unique(train.Y))
  class.labels = unique(train.Y)
  object = list()
  #Create a logistic regression model for each of the classes VS all others
  for(i in 1:num.classes)
  {
    # For each class, it will retransform the response variable to binary classes, 0 & 1
    train.Y = ifelse(output == i, 1,0)
    SE=0
    # Call the SGD.Logistic.Boot function if the corresponding argument passed is set as TRUE
    if(SE_estimates)
    {
      SE=SGD.Logistic.Boot(train.Y =train.Y ,train.X = train.X.Boot ,alpha = alpha,tolerance = tolerance, n_Iter = n_Iter)
      
    }
    # Initializing the loop variables
    weight = matrix(rep(0,nweights),nrow=nweights)
    N=nrow(train.X)
    delta = 1
    #A FOR loop for the number of outer iteration to be done
    for( j in 1:n_Iter )
    { 
      #An inner FOR loop for estimating the coefficients
      for( k in 1:N)
      {
        delta = t(train.X)[,k] %*% (sigmoid(train.X[k,] %*% weight) - train.Y[k])
        weight = weight - alpha*delta
      }
    }
    
    rownames(weight)=c(colnames(train.X))
    colnames(weight) = "Estimate"
    #Calculating the related performance metrics
    loglik.full = sum((train.Y*log(sigmoid(train.X %*% weight))) + ((1-train.Y)*log(1-sigmoid(train.X %*% weight))))
    loglik.null = sum((train.Y*log(sigmoid(weight[1]))) + ((1-train.Y)*log(1-sigmoid(weight[1]))))
    #For each model, store the following metrics as a list object
    object[[paste("Model.",i,sep = "")]] = list(Weights = weight,
                                                Null.Deviance = -2*loglik.null,
                                                Residual.Deviance = -2*loglik.full,
                                                DoF.Null = nrow(train.X)-1,
                                                Dof.Residual = nrow(train.X)-ncol(train.X),
                                                AIC = -2*loglik.full + 2* nweights,
                                                BIC = -2*loglik.full + nweights*log(nrow(train.X)),
                                                Standard.Error= cbind(weight,SE),
                                                Summary = paste("Positive Class:",i,"Negative Classes:",paste(class.labels[-i],collapse=",")))
    
  }
  
  return(object)
}


#Function to plot ROC Curve
ROC = function(actual.class, predicted.prob)
{
  #Initializing sensitivity and specificity
  sens = c(1)
  spec = c(0)
  
  for(threshold in seq(0.1,0.9,0.1))
  {
    #For each value of threshold, classify the predicted probabilities into classes
    predicted.class = ifelse(predicted.prob>threshold,1,0)
    #Create a confusion matrix of the predicted and actual classes
    ConfMatrix = table(Actual = actual.class,Predicted = predicted.class)
    #Calculate the sensitivity and specificity
    sens = c(sens, ConfMatrix[2,2] / sum(ConfMatrix[2,]))
    spec = c(spec, ConfMatrix[1,1] / sum(ConfMatrix[1,]))
  }
  
  #Appending the end values of sensitivity and specificity
  sens = c(sens,0)
  spec = c(spec,1)
  
  #Plotting sensitivity on the x axis and 1-specificity on the y axis
  plot(1-spec,sens, type = "l", col = "red",main = "ROC Curve", xlab = "FPR (False Alarm)", ylab = "TPR (Correct Hits)")
  abline(a=0,b=1,lty = 3)
}





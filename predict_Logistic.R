#Prediction function for Logistic Regression using Gradient Descent

predict.Logistic = function(weights,test.X = NULL,test.Y= NULL,class = F, cutoff = 0.5)
{
  if(is.null(test.X))
  {
    warning("Testing dataset cannot be NULL")
    stop()
  }
  test.X=input.split(test.X)
  
  predicted.probability = sigmoid(weights[1] + test.X %*% weights[-1])
  
  predicted.class = ifelse(predicted.probability >= cutoff, 1, 0)
  
  if(is.null(test.Y))
  {
    if(class == F)
    {
      object = list(Predicted.Probability = predicted.probability)
    }
    else
    {
      object = list(Predicted.Class = predicted.class)
    }
  }
  else
  {
    #Performance Metrics
    ConfMatrix = table(Actual = test.Y,Predicted = predicted.class)
    Accuracy = (ConfMatrix[1,1] + ConfMatrix[2,2]) / (ConfMatrix[1,1] + ConfMatrix[1,2] + ConfMatrix[2,1] + ConfMatrix[2,2])
    Sensitivity = ConfMatrix[2,2] / (ConfMatrix[2,2] + ConfMatrix[2,1])
    Specificity = ConfMatrix[1,1] / (ConfMatrix[1,1] + ConfMatrix[1,2])
    Precision = ConfMatrix[2,2] / (ConfMatrix[2,2] + ConfMatrix[1,2])
    Recall = Sensitivity 
    F1.Score = 2 * ((Precision * Recall)/(Precision + Recall ))
    AUC = AUC(as.logical(test.Y),predicted.probability)
    ROC(test.Y,predicted.probability)
    object = list(Predicted.Class = predicted.class, 
                  Confusion.Matrix = ConfMatrix, 
                  Accuracy = Accuracy, 
                  Sensitivity = Sensitivity,
                  Specificity = Specificity,
                  Precision = Precision,
                  Recall = Sensitivity,
                  F1.Score = F1.Score,
                  AUC = AUC)
  }
  
  return(object)
}



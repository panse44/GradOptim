predict.oneVsall = function(model.obj,test.X = NULL,test.Y= NULL)
{
  if(is.null(test.X))
  {
    warning("Testing dataset cannot be NULL")
    stop()
  }
  test.X=input.split(test.X)
  predicted.prob = matrix(NA,nrow = nrow(test.X),0)
  for(i in 1:length(model.obj))
  {
    weights = model.obj[[i]][[1]]
    predicted.prob = cbind(predicted.prob,sigmoid(weights[1] + test.X %*% weights[-1]))
  }
  predicted.class = apply(predicted.prob,1,which.max)
  
  if(is.null(test.Y))
  {
    object = list(Predicted.Class = predicted.class)
  }
  else
  {
    cm = table(Actual = test.Y,Predicted = predicted.class)
    diag = diag(cm)
    rowsums = apply(cm, 1, sum) # number of instances per class
    colsums = apply(cm, 2, sum) # number of predictions per class
    precision = round(diag / colsums, 4)
    recall = round(diag / rowsums, 4) 
    f1 = round(2 * precision * recall / (precision + recall), 4)
    object = list(Predicted.Class = predicted.class,
                  ConfMatrix = table(Actual = test.Y,Predicted = predicted.class),
                  Accuracy = round(sum(diag(cm))/sum(cm),4),
                  PerClass.Metrics = data.frame(Precision = precision, Recall = recall, F1Score = f1))
                  
  }
  return(object)
}




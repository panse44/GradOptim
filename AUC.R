# The probabilistic interpretation is that if you randomly choose a positive case and a negative case, 
# the probability that the positive case #outranks the negative case according to the classifier is given by the AUC


AUC = function(actual,predicted,N=1e7)
{
  #All the predicted probability for the actual positive cases
  pos = sample(predicted[actual], N, replace = T)
  #All the predicted probability for the actual negative cases
  neg = sample(predicted[!actual], N, replace = T)
  #Calculating AUC and handling ties
  auc = (sum(pos > neg) + sum(pos == neg)/2) / N

  return(auc)
}


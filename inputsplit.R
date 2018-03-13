input.split = function(df)
{
  df.categorical = as.data.frame(matrix(NA,nrow(df),0))
  df.numerical = as.data.frame(matrix(NA,nrow(df),0))
  df.binary = as.data.frame(matrix(NA,nrow(df),0))
  
  colnames.binary = vector()
  colnames.categorical = vector()
  colnames.numerical = vector()
  
  for(i in 1:ncol(df))
  {
    if(sum(is.element(c(0,1),sort(unique(df[,i])))) == 2 && length(sort(unique(df[,i])))==2)
    {
      df.binary = cbind(df.binary,df[,i])
      colnames.binary = c(colnames.binary,colnames(df)[i])
    }
    else if(is.factor(df[,i]))
    {
      df.categorical = cbind(df.categorical,df[,i])
      colnames.categorical = c(colnames.categorical,colnames(df)[i])
    }
    else
    {
      df.numerical = cbind(df.numerical,df[,i])
      colnames.numerical = c(colnames.numerical,colnames(df)[i])
    }
  }
  
  colnames(df.categorical) = colnames.categorical
  colnames(df.binary) = colnames.binary
  colnames(df.numerical) = colnames.numerical
  
  #Creating Dummy Variables
  if(ncol(df.categorical) != 0)
  {
    df.categorical = model.matrix(~., data=df.categorical)
    df.categorical = df.categorical[,-1]
  }
  
  df.numerical = scale(df.numerical)
  
  
  #View(df.categorical)
  #View(df.binary)
  #View(df.numerical)
  
  final_data = as.matrix(cbind(df.numerical,df.categorical,df.binary))
  #View(final_data)
  return(final_data)
}

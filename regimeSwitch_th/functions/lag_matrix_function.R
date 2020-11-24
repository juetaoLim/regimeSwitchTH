lag.matrix <- function(x,lags){
  
  library(quantmod)
  if(class(x) == "vector"|class(x) == "numeric"){
    y <- Lag(x=x, k=1:lags)
    return(y)
  }
  
  if(class(x) == "matrix" | class(x) == "data.frame"){
    
      y <- c()
      
      for(i in 1:ncol(x)){
      y <- cbind(y,Lag(x=x[,i], k=1:lags))
      }

    return(y)
  }
}

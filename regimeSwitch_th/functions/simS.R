simS <- function(sin,pmat){
  states <- sin
  Rnd <- runif(n=nrow(sin))
  
  for (i in 2:nrow(sin))
  {
    state_past <- which(states[i-1,]==1)
    
  
    if (Rnd[i] < pmat[state_past,state_past]){
      states[i,state_past] <-1
    } 
    
    else {
      idx_other <- which(states[i-1,]==0) 
      prob2 <- pmat[,state_past]
      a <- c(pmat[state_past,state_past],prob2[idx_other])
      cum_sum <- cumsum(a)
      sorted <- sort(x=c(cum_sum, Rnd[i]))
      
      idx <- which(Rnd[i]==sorted)-1
      states[i,idx_other[idx]]=1;
    }
  }
  return(states)
}


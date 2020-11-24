#transition matrix, 2state
transmat_post <- function(u00,u01,u11,u10,S,G){
  tranmat <- switchcount(s=S,g=G)
  N00 <- tranmat[1,1]
  N01 <- tranmat[1,2]
  N10 <- tranmat[2,1]
  N11 <- tranmat[2,2]
  
  #draw for dirc
  alpha1 <- c(N00 + u00, N01 + u01) 
  p0 <- rdirichlet(n=1, alpha= alpha1)
  p = p0[1]
  
  alpha2 <- c(N10 + u10, N11 + u11) 
  q0 <- rdirichlet(n=1, alpha= alpha2)
  q = q0[2]
  
  pmat <- rbind(c(p,1-p),c(1-q,q))
    
  return(pmat)
  
}

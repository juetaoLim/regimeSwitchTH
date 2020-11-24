beta_post <- function(x,y,S,Sigma0,sig1,sig2,B0,v0,d0){
  
  chol_coef <- ncol(x)
  ####state 1 coef
  id <- which(S==0)
  x1 <- x[id,]
  y1 <- y[id]
  if (length(x1)==ncol(Sigma0)){x1 <- t(as.matrix(x1))}
  M <- solve(solve(Sigma0) + (1/sig1) * (t(x1) %*% x1)) %*% (solve(Sigma0) %*% B0 + (1/sig1) * t(x1) %*% y1)
  V <- solve(solve(Sigma0) + (1/sig1) * (t(x1) %*% x1)) 
  phi1 <- M + t(rnorm(n=chol_coef) %*% chol(x=V))
  
  #### state 2 coef
  id <- which(S==1)
  x2 <- x[id,]
  y2 <- y[id]
  if (length(x2)==ncol(Sigma0)){x2 <- t(as.matrix(x2))}
  M <- solve(solve(Sigma0) + (1/sig2) * (t(x2) %*% x2)) %*% (solve(Sigma0) %*% B0 + (1/sig2) * t(x2) %*% y2)
  V <- solve(solve(Sigma0) + (1/sig2) * (t(x2) %*% x2)) 
  phi2 <- M + t(rnorm(n=chol_coef) %*% chol(x=V))
  
  ####state 1 Sigma
  
  e1 <- y1 - x1 %*% phi1
  D1 <- (d0 + t(e1) %*% e1)/2
  T1 <- (v0 + nrow(e1))/2
  sig1 <- rinvgamma(n=1,shape=T1 , scale=D1)
  
  ####state 2 Sigma
  
  e2 <- y2 - x2 %*% phi2
  D2 <- (d0 + t(e2) %*% e2)/2
  T2 <- (v0 + nrow(e2))/2
  sig2 <- rinvgamma(n=1,shape=T2 , scale=D2)
  
  params <- list(phi1,phi2,sig1,sig2)
  names(params) <- c("phi1",'phi2',"sig1","sig2")
  return(params)
}
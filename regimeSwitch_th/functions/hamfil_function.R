#only works for 2 state
#x=x;y=y;S1=sig1;S2=sig2;B1=phi1;B2=phi2;P=pmat
hamFilt <- function(x,y,S1,S2,B1,B2,P){
  #initialize
  t <- length(y)
  A <- rbind(diag(1,nrow=2)-P,matrix(data=1,nrow=1,ncol=2))
  E <- rbind(matrix(data=0,nrow=2,ncol=1),1)
  ett11 <- solve(t(A) %*% A) %*% t(A) %*% E
  iS1 <- 1/S1
  iS2 <- 1/S2
  filter <- matrix(data=0,nrow=t,ncol=2)
  lik <- 0 
  
  
for (i in 1:t){
  em1 <- y[i] - x[i,] %*% B1 
  em2 <- y[i] - x[i,] %*% B2 
  
  neta1 <- (1/sqrt(S1)) * exp (-0.5 * (t(em1) %*%  em1 * iS1))
  neta2 <- (1/sqrt(S2)) * exp (-0.5 * (t(em2) %*%  em2 * iS2))
  
  ##pred step##
  ett10 <- P %*% ett11

  ##update step##
  ett11 <- ett10 * rbind(neta1,neta2)
  fit <- sum(ett11)
  ett11 <- ett11/fit
  filter[i,] <- ett11
  lik <- lik+log(fit)

  neta01 <- neta1
  neta02 <- neta2
}

#Backward Recursion

S <- rep(0,t) 
p1 <- filter[t,1]
p2 <- filter[t,2]
p <- p1/(p1+p2)
u <- runif(n=1)
if(u >= p){
  S[t] <- 1 
} else {S[t] <- 0}

for (i in (t-1):1){
  if (S[i+1] == 0)
  {
    p00 <-P[1,1] * filter[i,1]
    p01 <-P[1,2] * filter[i,2]
  }
  
  if (S[i+1] == 1)
  {
    p00 <-P[2,1] * filter[i,1]
    p01 <-P[2,2] * filter[i,2]
  }
  
  u <- runif(n=1)
  p <- p00/(p01+p00)
  if  (u<p){
    S[i] <- 0
  } else {S[i] <- 1}
}  

#return(filter)
return(S)
}

#to test
#hi <- hamFilt(x=x,y=y,S1=S1,S2=S2,B1=rbind(C1,B1),B2=rbind(C2,B2),P=pmat)
#dev.off()
#plot(y=y,x=1:t,type="l")
#plot(y=hi,x=1:t,type="l")
#points(y=strue[,2],x=1:t,col="red")

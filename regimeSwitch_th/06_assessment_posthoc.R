rm(list=ls())
setwd("~\\nBox\\regime_switch_hamfil_thailand")
require("MCMCpack")
require('quantmod')
source("simS.R")
source("hamfil_function.R")
source("transmat_function.R")
source("switchcount_function.R")
source("beta_posterior_function.R")
source("lag_matrix_function.R")
df <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/merged_ALL.csv")
df$X <- NULL
rotate <- function(x) t(apply(x, 2, rev))


for (j in 1:ncol(df)){
  s1_store <- rep(NA,nrow(df))
  for (l in 20:nrow(df)){
  ind <- 1:l
  time.week <- df[ind,j]
  #data
  
  lags <- 2
  #y = diff(time.week)
  y <- time.week
  y <- (y-min(y))/(max(y)-min(y))
  x = lag.matrix(x=as.numeric(y),lags=lags)
  x <- cbind(1,x)
  x <- x[-c(1:lags),]
  y <- y[-c(1:lags)]
  #hyperparams
  B0<- matrix(data=0,nrow=ncol(x),ncol=1)
  Sigma0 <- diag(x=10,nrow=ncol(x),ncol=ncol(x))
  
  phi1 = matrix(data=0,nrow=ncol(x),ncol=1)
  phi2 = matrix(data=0,nrow=ncol(x),ncol=1)
  
  sig1 <- 1
  sig2 <- 1
  p <- 0.95
  q <- 0.95
  pmat <- rbind(c(p,1-p),c(1-q,q))
  
  #Priors
  
  #Variance prior
  d0 <- 0.5
  v0 <- 1
  
  #Transition probability prior
  u00 <- 25 #p11 ~ D(u00,u01)
  u01 <- 5
  u11 <- 25 #p11 ~ D(u11,u10)
  u10 <- 5
  
  #storage matrices
  out1 <- list() #States/Regimes
  out2 <- list() #Transition Probabilities
  out3 <- list() #Beta/Variance 
  
  #mcmc settings
  reps <- 5000
  burnin <- 1000
  
  #MCMC
  
  for (i in 1:reps){
    
    out1[[i]] <- hamFilt(x=x,y=y,S1=sig1,S2=sig2,B1=phi1,B2=phi2,P=pmat) 
    
    S <- unlist(out1[i]) 
    
    out2[[i]] <- transmat_post(u00=u00,u01=u01,u11=u11,u10 = u10, S=S+1,G = rbind(1,2))
    
    pmat <- out2[[i]]
    
    out3[[i]] <- beta_post(x=x,y=y,S=S,Sigma0=Sigma0,sig1=sig1,sig2=sig2,B0=B0,v0=v0,d0=d0)
    
    phi1 <- out3[[i]]$phi1
    phi2 <- out3[[i]]$phi2
    sig1 <- out3[[i]]$sig1
    sig2 <- out3[[i]]$sig2
    
    #impose identification restriction on constant term
    #if (out3[[i]]$phi1[1,]> out3[[i]]$phi2[1,]){
    #impose identification restriction on sigma
    if (sig1 > sig2){
      out1[[i]] <- 1 - out1[[i]]
      out2[[i]] <- rotate(rotate(out2[[i]]))
      pmat <- out2[[i]]
      
      phi1 <- out3[[i]]$phi2
      phi2 <- out3[[i]]$phi1
      
      out3[[i]]$phi1 <- phi1
      out3[[i]]$phi2 <- phi2
      
      sig2 <- out3[[i]]$sig1
      sig1 <- out3[[i]]$sig2
      
      out3[[i]]$sig2 <- sig2
      out3[[i]]$sig1 <- sig1
      
    }
  }
  
  #discard burnin
  out1 <- out1[-c(1:burnin)]
  out2 <- out2[-c(1:burnin)]
  out3 <- out3[-c(1:burnin)]
  
  ##############compute posterior means
  ##############compute posterior means
  ##############compute posterior means
  bmr <- reps - burnin
  
  s1 <- unlist(out1[1])
  for (i in 2:bmr){
    s1 <- s1 + unlist(out1[i])
  }
  
  
  
 postProbs <- s1/(bmr)
 s1_store[l] <- postProbs[length(postProbs)]  
}



  
  filename <- paste("~/nBox/regime_switch_hamfil_thailand/plots_posthocClass3Sep/",colnames(df)[j],".Rdata",sep="")
  save.image(filename)
}


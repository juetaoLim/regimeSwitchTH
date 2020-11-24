#ANALYSIS OF CLIMATE ON REGIMES
#ANALYSIS OF CLIMATE ON REGIMES
#ANALYSIS OF CLIMATE ON REGIMES
#ANALYSIS OF CLIMATE ON REGIMES
rm(list=ls())
setwd("~/nBox/regime_switch_hamfil_thailand/")
source("lag_matrix_function.R")
library(glmnet)
setwd("~/nBox/regime_switch_hamfil_thailand/results_posthocClass/")
files <- list.files()

pmatStore <- list()
sStore <- list()
yStore <- list()

for (k in 1:length(files)){
  load(files[[k]])
  pmatStore[[k]] <- pmat
  sStore[[k]] <- s1
  yStore[[k]] <- y
}
rm(list=setdiff(ls(), c("pmatStore","sStore","yStore","files","lag.matrix")))
AH <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/AHo.csv")[,-1]
RH <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/RHo.csv")[,-1]
TP <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/TPo.csv")[,-1]
AT <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/ATo.csv")[,-1]

files = substr(files,1,nchar(files)-6)
AH <- AH[,order(colnames(AH))]
RH <- RH[,order(colnames(RH))]
TP <- TP[,order(colnames(TP))]
AT <- AT[,order(colnames(AT))]
#check

colnames(AH) == files

#account for differenced counts in climate
ind <- c(nrow(AH)-1,nrow(AH))
AH <- AH[-ind,]
RH <- RH[-ind,]
TP <- TP[-ind,]
AT <- AT[-ind,]

# 
# #bootstrap LASSO procedure\
# samp <- 1:200
# for (j in 1:length(sStore)){
#   lags <- 4
#   y <- sStore[[j]]/4000
#   y[y>0.5] <- 1
#   y[y<0.5] <- 0
#   x <- cbind(AH[,j],RH[,j],TP[,j],AT[,j])
#   x <- lag.matrix(x,lags)
#   
#   x <- x[-c(1:lags),]
#   y <- y[-c(1:lags)] 
#   coef_store <- list()
# for (i in samp){
#   
#   ind_size <- runif(60,min=1,max=length(y))
#   ind_size <- round(ind_size)  #Randomly select size of sample
#   ind <- runif(ind_size,min=1,max=length(y))  #randomly select samples given size
#   ind <- round(ind)
#   temp_x <- x[ind,]
#   temp_y <- y[ind]
#   
#   try( temp_cv <- cv.glmnet(x=temp_x,
#                             y=temp_y,
#                             alpha = 1,
#                             type.measure ="auc",
#                             family=c("binomial")) )
#   
#   temp_lam <- temp_cv$lambda.min
#   try( temp_las <- glmnet(x=temp_x,
#                           y=temp_y,
#                           alpha=1,
#                           lambda = temp_lam,
#                           family=c("binomial")))
#   temp_coef <- temp_las$beta
#   temp_coef <- as.matrix(temp_coef)
#   coef_store[[i]] <- temp_coef
#   
#   toprint <- paste(i,j)
#   print(toprint)
#   
# }
#   name <- paste("~/nBox/regime_switch_hamfil_thailand/results_lassoBoot/",colnames(AH)[j],".Rdata",sep="")
#   save.image(name)
# }
# 


##################Results
##################Results
##################Results
##################Results
##################Results

setwd("~/nBox/regime_switch_hamfil_thailand/results_lassoBoot/")
FILES <- list.files()
coefs <- list()
for (k in 1 :length(FILES)){
load(FILES[[k]])
coefs[[k]] <- matrix(unlist(coef_store),ncol=length(coef_store))
}

rm(list=setdiff(ls(), c("coefs","sStore","FILES","AH","AT","RH","TP")))

names(coefs) <- FILES
store <- list()
#remove LASSO didnt work portions, get inclusion statistics
for (i in 1:length(coefs)){
  temp <- coefs[[i]]
  temp <- temp[,-which(colSums(temp)==0)]
  mean <- rowMeans(temp)
  quant <- apply(temp,MARGIN=1,quantile,probs = c(0.025,0.975))
  incl <- apply(temp,MARGIN=1,function(x)(sum(x!=0)/length(x)))
  
  temp <- cbind(mean,t(quant),incl)
  store[[i]] <- temp
}

save.image("~/nBox/regime_switch_hamfil_thailand/results_lassoBoot_all/out.RData")

#ANALYSIS OF CLIMATE ON REGIMES, recovers epidemic potential
#ANALYSIS OF CLIMATE ON REGIMES, recovers epidemic potential
#ANALYSIS OF CLIMATE ON REGIMES, recovers epidemic potential
#ANALYSIS OF CLIMATE ON REGIMES, recovers epidemic potential

# rm(list=ls())
setwd("~/nBox/regime_switch_hamfil_thailand/")
source("lag_matrix_function.R")
library(glmnet)
setwd("~/nBox/regime_switch_hamfil_thailand/results_posthocClass/")
files <- list.files()

pmatStore <- list()
sStore <- list()
yStore <- list()

for (k in 1:length(files)){
  load(files[[k]])
  pmatStore[[k]] <- pmat
  sStore[[k]] <- s1
  yStore[[k]] <- y
}
rm(list=setdiff(ls(), c("pmatStore","sStore","yStore","files","lag.matrix")))
AH <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/AHo.csv")[,-1]
RH <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/RHo.csv")[,-1]
TP <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/TPo.csv")[,-1]
AT <- read.csv("~/nBox/regime_switch_hamfil_thailand/data/climate/ATo.csv")[,-1]

files = substr(files,1,nchar(files)-6)
AH <- AH[,order(colnames(AH))]
RH <- RH[,order(colnames(RH))]
TP <- TP[,order(colnames(TP))]
AT <- AT[,order(colnames(AT))]
#check

colnames(AH) == files

#account for differenced counts in climate
ind <- c(nrow(AH)-1,nrow(AH))
AH <- AH[-ind,]
RH <- RH[-ind,]
TP <- TP[-ind,]
AT <- AT[-ind,]

coef_store <- list()
assess_store <- list()
epiPot <- list()
#bootstrap LASSO procedure as in "Statistical Learning with Sparsitiy pg 142-143"

for (j in 1:length(sStore)){
  lags <- 4
  y <- sStore[[j]]/4000
  y[y>0.5] <- 1
  y[y<0.5] <- 0
  x <- cbind(AH[,j],RH[,j],TP[,j],AT[,j])
  x <- lag.matrix(x,lags)
  
  x <- x[-c(1:lags),]
  y <- y[-c(1:lags)]
  
  try( temp_cv <- cv.glmnet(x=x,
                            y=y,
                            alpha = 1,
                            type.measure ="auc",
                            family=c("binomial")) )
  
  temp_lam <- temp_cv$lambda.min
  try( temp_las <- glmnet(x=x,
                          y=y,
                          alpha=1,
                          lambda = temp_lam,
                          family=c("binomial")))
  temp_coef <- temp_las$beta
  temp_coef <- as.matrix(temp_coef)
  coef_store[[j]] <- temp_las
  assess_store[[j]] <-temp_cv$cvm[which(temp_cv$lambda==temp_cv$lambda.min)]
  epiPot[[j]] <- cbind(predict(temp_las,newx=x,type="response"),y)
  toprint <- paste(j)
  print(toprint)
  
}

for (j in 1:length(epiPot)){
  plot.ts(epiPot[[j]])
  
}

save.image("~/nBox/regime_switch_hamfil_thailand/results_lassoBoot_all/out_epiPot.RData")

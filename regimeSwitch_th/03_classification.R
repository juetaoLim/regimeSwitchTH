library(sf)
library(pROC)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(readxl)
Counts <- read.csv("nBox/regime_switch_hamfil_thailand/data/merged_ALL.csv")
Counts <- Counts[,-c(1,2,79)]
indClass <- seq(from=nrow(Counts)/3,to=nrow(Counts))
Counts <- Counts[,order(colnames(Counts))]

#THIS ESTIMATES ALTERNATIVE MODELS FOR EPIDEMIC CLASSIFICATION
#THIS ESTIMATES ALTERNATIVE MODELS FOR EPIDEMIC CLASSIFICATION
#THIS ESTIMATES ALTERNATIVE MODELS FOR EPIDEMIC CLASSIFICATION
#THIS ESTIMATES ALTERNATIVE MODELS FOR EPIDEMIC CLASSIFICATION

#### Simple Regression
#### Simple Regression
#### Simple Regression
#### Simple Regression

epi_pred1 = c()
ym = c()
sm = c()
tstat= c()
p = c()

epi_pred_f <- function(pid,m_ad){
  minVal <- min(indClass) - 1
  for (i in indClass){
    province1 = Counts[,pid]
    ym[i - minVal] = mean(province1[(i - m_ad):(i - 1)])
    sm[i - minVal] = sqrt(var(province1[(i - m_ad):(i - 1)]))
    tstat[i - minVal] = (province1[i] - ym[i - minVal] ) / (sm[i - minVal] * sqrt(1 + 1 / m_ad))
    p[i - minVal] = pt(tstat[i - minVal],df = m_ad - 1)
    }
  return(p)
}

sr = list()
for(pid in 1:ncol(Counts)){
  sr[[pid]] = epi_pred_f(pid,m_ad = 5)
}


#### CUSUM
#### CUSUM
#### CUSUM
#### CUSUM

# Calculate C_t+
a = c()
ymsev = c()
smsev = c()
a[1] = 0

zsta <- function(pid,t,d,k){
  province2 = Counts[,pid]
  for (j in 1:d){
    ymsev[j] = mean(province2[(t - d + j - 9) : (t - d + j - 3)])
    smsev[j] = sqrt(var(province2[(t - d + j - 9) : (t - d + j -3)]))
    a[j + 1] = (province2[t + j - 2] - ymsev[j]) / smsev[j] - k +  a[j]
    if(a[j + 1] > 0){
      a[j + 1] = a[j + 1]
      }
    else(a[j + 1] = 0)
  }
  return (a[d + 1]) 
}  

p2 = c()
epi_pred_g <- function(pid,d,k){
  minVal <- min(indClass) - 1
  for (t in indClass){
    p2[t - minVal] = pnorm(zsta(pid,t,d,k))}
    return(p2)
}

cus = list()

for(pid in 1:ncol(Counts)){
  try(cus[[pid]] <- epi_pred_g(pid,d=2,k=2))
}

cus[[which(unlist(lapply(cus,is.null)))]] <- rep(0.5,length(cus[[1]]))
test<-lapply(sr,function(x) {y <- x
                             y[which(is.nan(y))] <- 0
                            return(y)})

#### get  AUC across all provinces
#### get  AUC across all provinces
#### get  AUC across all provinces
#### get  AUC across all provinces

library(pROC)


roc_sr1 = c()
roc_sr2 = c()
#roc_posthoc1 =c()
roc_cusum1 = c()
roc_cusum2 = c()

for (pid in ind){
  try({
roc_sr1[pid] = auc(roc(response=actual[[pid]][indClass],predictor=sr[[pid]],na.rm=T))
roc_sr2[pid] = auc(roc(posthoc[[pid]][indClass]~sr[[pid]],data = data_sr2,na.rm=T))
#roc_posthoc1[pid] <- auc(roc(actual_new[[pid]]~posthoc[[pid]][49:142],data = data_posthoc))
roc_cusum1[pid] = auc(roc(actual[[pid]][indClass]~cus[[pid]],data = data_cusum1,na.rm=T))
roc_cusum2[pid] = auc(roc(posthoc[[pid]][indClass]~cus[[pid]],data = data_cusum2,na.rm=T))
})
}

mean(na.omit(roc_sr1))
mean(na.omit(roc_sr2))
#mean(na.omit(roc_posthoc1))
mean(na.omit(roc_cusum1))
mean(na.omit(roc_cusum2))


#### Validate posthoc method 
#### Validate posthoc method 
#### Validate posthoc method 
#### Validate posthoc method 

# posthoc~CUSUM
# posthoc~CUSUM

area <- st_read('~/nBox/EVT_thailand/data/shapefile_plot/tha_admbnda_adm1_rtsd_20190221.shp')
colcu <- c(rep('#BCD9EB',13),rep('#ABD0E5',1),rep('#98C6DF', 6),rep('#7BB3D5',8),rep('#5EA0CC',18),rep('#428DC2',31))
od = c(order(roc_cusum2)[66:76],order(roc_cusum2)[1:65])
cusum_mapind1= c()

for (i in 1:76){
  if (od[i] > 3){
    cusum_mapind1[i] = od[i] + 1}
  else{cusum_mapind1[i] = od[i]}
 }

cusum_mapind = append(cusum_mapind1,4,after = 60)

test_d <- c()
for (i in 1:length(cusum_mapind)){
  temp_col <- colcu[i]
  test_d[cusum_mapind[i]] <- temp_col
}


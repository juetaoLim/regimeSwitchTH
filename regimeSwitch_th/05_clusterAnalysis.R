#ANALYSIS OF CLUSTERS
#ANALYSIS OF CLUSTERS
#ANALYSIS OF CLUSTERS
#ANALYSIS OF CLUSTERS
rm(list=ls())
setwd("~/nBox/regime_switch_hamfil_thailand/results/")

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


rm(list=setdiff(ls(), c("pmatStore","sStore","yStore","files")))

library(dtw)
library(dtwclust)
library(factoextra)
dtwStore <- lapply(sStore,function(x)x/4000)
dtwStore <- matrix(unlist(dtwStore),nrow=76,byrow=TRUE)
#dtwStore[dtwStore>0.5] <- 1
#dtwStore[dtwStore<0.5] <- 0
dtw <- dtwDist(mx=dtwStore)
names <- substr(files,1,nchar(files)-6)
names <-  gsub('\\.', ' ', names)
rownames(dtw) <- colnames(dtw) <- names

#standardize dist mat

dtw1 <- apply(dtw,MARGIN=1,function(x){(x-min(x))/(max(x)-min(x))})
#ordered dissimilairty image:
#ordered dissimilairty image:
#pdf(file="~/nBox/regime_switch_hamfil_thailand/output/plots/orderSimMatrix.pdf",width=8.27,height=8.27)
store <- fviz_dist(as.dist(dtw1),
          gradient = list(low = "#0072B2", mid = "white", high = "#FC4E07"),
          lab_size = 50,show_labels=F,order=TRUE) + theme(
    legend.title = element_text(color = "Black", size = 14),
    legend.text = element_text(color = "Black", size = 12),
    legend.key.size = unit(0.7, "cm"),
    legend.position = 'right'
    
  )

store
dev.off()


##########QUASI-CV approach to DTW#########
##########QUASI-CV approach to DTW#########
##########QUASI-CV approach to DTW#########
##########QUASI-CV approach to DTW#########
set.seed(1)
length <- 40 
folds <- 30
store_res <- list()
store_y <- list()
store_clust <- list()
for (i in 1:folds){
  temp_ind <- round(runif(n=1,1,100))
  temp_ind <- temp_ind:(temp_ind+length)
  temp_dtw <- dtwStore[,temp_ind]
  
  test <- tsclust(temp_dtw, k=2L:10L,distance="dtw", centroid="pam")
  temp <- lapply(test,cvi)
  temp <- matrix(unlist(temp),nrow=length(temp),byrow=TRUE)
  store_res[[i]] <- temp
  
  test <- tsclust(temp_dtw, k=2L,distance="dtw", centroid="pam")
  
  store_clust[[i]] <- test@cluster
  store_y[[i]] <- temp_dtw
  
}

sapply(store,plot.ts)

clust_mat <- matrix(unlist(store_clust),ncol=(length(store_clust)))
plot.ts(clust_mat)

#assign "more" group as 1, "less" group as 2
#assign "more" group as 1, "less" group as 2
#assign "more" group as 1, "less" group as 2
#assign "more" group as 1, "less" group as 2

for (i in 1:ncol(clust_mat)){
  temp <- clust_mat[,i]
  if (sum(temp==2) > sum(temp==1)){
    temp <- temp*(-1)+3
  }
  clust_mat[,i] <- temp
}
plot.ts(clust_mat)

save.image("~/nBox/regime_switch_hamfil_thailand/results_clust/clust_CV.RData")

rm(list=ls())
load("~/nBox/regime_switch_hamfil_thailand/results_mapAll/out_cluster.RData")
clust <- ind
load("~/nBox/regime_switch_hamfil_thailand/results_lassoBoot_all/out.RData")

#preprocess
store <- list()

for (i in 1:length(coefs)){
temp <- coefs[[i]]
inclProb <- apply(temp,MARGIN=1,function(x)length(which(x==0))/length(x))
store[[i]]<-inclProb}

store <- Reduce(rbind,store)
store <- 1- store
store1 <- store[which(ind==1),]
store2 <- store[which(ind==2),]
ah <- 1:4
rh <- 5:8
tp <- 9:12
at <- 13:16
pdf(file="~/nBox/regime_switch_hamfil_thailand/plots/climInclu.pdf",height=10,width=7)
par(mfrow=c(4,2),pty='s',oma=c(4,4,0,4),mar=c(2,1,2,1),las=1)
boxplot(store1[,ah],ylab="",main="Absolute Humidity C1",col="lightblue", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)
boxplot(store2[,ah],ylab="",main="Absolute Humidity C2",col="orangered", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)

boxplot(store1[,rh],ylab="",main="Relative Humidity C1",col="lightblue", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)
boxplot(store2[,rh],ylab="",main="Relative Humidity C2",col="orangered", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)

boxplot(store1[,tp],ylab="",main="Total Precipitation C1",col="lightblue", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)
boxplot(store2[,tp],ylab="",main="Total Precipitation C2",col="orangered", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)

boxplot(store1[,at],ylab="",main="Average Temperature C1",col="lightblue", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)
boxplot(store2[,at],ylab="",main="Average Temperature C2",col="orangered", outline = F,ylim=c(0,1))
abline(h=0.5,col="grey",lty=2)
par(las=0)
mtext(outer=T,text="Lag (Month)",side=1,cex=2,padj=0.7)
mtext(outer=T,text="Inclusion Probabilities",side=2,cex=2)
dev.off()

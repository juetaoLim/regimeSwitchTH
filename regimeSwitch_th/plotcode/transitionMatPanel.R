rm(list=ls())
load("~/nBox/regime_switch_hamfil_thailand/output/summarize.Rdata")
endProb <- Reduce(rbind,endProb)
epiProb <- Reduce(rbind,epiProb)

indOrder <- order(epiProb[,2])
endProb <- endProb[indOrder,]
epiProb <- epiProb[indOrder,]

lab <- substr(files,1,nchar(files)-6)
lab <- gsub("[.]"," ",lab)
lab <- lab[indOrder]

indLeft <- seq(from=1,to=length(lab),by=2)
indRight <- indLeft+1

load("~/nBox/regime_switch_hamfil_thailand/output/out_cluster.Rdata")
cluster <- test[[1]]@cluster
cluster <- cluster[indOrder]
cluster1Ind <- which(cluster==1)
cluster2Ind <- which(cluster==2)
library(RColorBrewer)

pdf("~/nBox/regime_switch_hamfil_thailand/plots/endepiplot.pdf",width=8,height=11)
par(oma=c(0,6,0,6))
plot(y=c(1,76),x=c(0.55,1),col="white",xlab="Regime Persistence Probability",ylab="",yaxt="n")

endCol <- colorRampPalette(brewer.pal(n = 8, name = "Blues"),bias=4)(length(lab))
epiCol <- colorRampPalette(brewer.pal(n = 8, name = "Reds"),bias=4)(length(lab))

for (i in indLeft){
  lines(y=c(i,i),x=c(0,0.8),lty=2,col="grey")
}

for (i in indRight){
  lines(y=c(i,i),x=c(0.8,1.5),lty=2,col="grey")
}

for (i in 1:76){
  lines(y=c(i,i),x=c(endProb[i,c(1,3)]),col=endCol[76])
  
}
for (i in 1:76){
  lines(y=c(i,i),x=c(epiProb[i,c(1,3)]),col=epiCol[76])
}


points(y=cluster1Ind,x=endProb[cluster1Ind,2],pch=16,col=endCol[76])
points(y=cluster1Ind,x=epiProb[cluster1Ind,2],pch=16,col=epiCol[76])

points(y=cluster2Ind,x=endProb[cluster2Ind,2],pch=5,col=endCol[76])
points(y=cluster2Ind,x=epiProb[cluster2Ind,2],pch=5,col=epiCol[76])

axis(side=2,at=indLeft,labels = lab[indLeft],las=1,cex.axis=0.8)
axis(side=4,at=indRight,labels = lab[indRight],las=1,cex.axis=0.8)
mtext("Epidemic Regime",side=3,adj=0,col=epiCol[length(epiCol)])
mtext("Endemic Regime",side=3,adj=1,col=endCol[length(endCol)])
legend("topleft",legend=c("Cluster 1","Cluster 2"),pch=c(16,5),bty='n',cex=0.8)
dev.off()

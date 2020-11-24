rm(list=ls())
library(sf)
library(pROC)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(readxl)
library(viridis)
Counts <- read.csv("nBox/regime_switch_hamfil_thailand/data/merged_ALL.csv")
Counts <- Counts[,-c(1,2,79)]
indClass <- seq(from=nrow(Counts)/3,to=nrow(Counts))
Counts <- Counts[,order(colnames(Counts))]
load("~/nBox/regime_switch_hamfil_thailand/results_mapAll/out_cluster.RData")
load("~/nBox/regime_switch_hamfil_thailand/results_mapAll/to_yiting.RData")
area <- st_read('~/nBox/EVT_thailand/data/shapefile_plot/tha_admbnda_adm1_rtsd_20190221.shp')
library(readxl)
covariates <- read_excel("nBox/regime_switch_hamfil_thailand/results_mapAll/CompiledCSV_covar.xlsx")
covariates <- covariates[-4,]

colcu <- c(rep('#BCD9EB',13),rep('#ABD0E5',1),rep('#98C6DF', 6),rep('#7BB3D5',8),rep('#5EA0CC',18),rep('#428DC2',31))
#preprocess data

#AUC for posthoc/actual classification
posthoc <- lapply(posthoc,function(x)x[-c(1,2)])
shade3<-list()
for (i in 1:length(posthoc)){
  try({
  shade3[[i]] <- auc(roc(response=actual[[i]],predictor=posthoc[[i]],na.rm=T))})
}
indNull <- unlist(lapply(shade3,is.null))
shade3[indNull]  <- mean(unlist(shade3[-indNull]))

#shading
shade1 <- colMeans(Counts)*12
shade2 <- test[[1]]@cluster
shade3 <- unlist(shade3)
shade4 <- covariates$Urban_Land_Percentage
shade5 <- unlist(lapply(pmatStore,function(x)x[1,1]))
shade6 <- unlist(lapply(pmatStore,function(x)x[2,2]))
shade7 <- covariates$sigmamax2
shade8 <- covariates$Incoming_Flights
shade9 <- covariates$Percentage_Municpal
#shadeLab 


#get colours
library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(8, "YlOrRd"))(length(shade1))
shade1c<- mycolors[order(shade1)]
shade2c <- shade2
shade2c[which(shade2c==1)] <- "lightblue"
shade2c[which(shade2c==2)] <- "orangered"
shade3c <- mycolors[order(shade3)]
shade4c <- mycolors[order(shade4)]
shade5c <- mycolors[order(shade5)]
shade6c <- mycolors[order(shade6)]
shade7c <- mycolors[order(shade7)]
shade8c <- mycolors[order(shade8)]
shade9c <- mycolors[order(shade9)]
#remove bungkan colour to black
#append bungkan at position 4
shade1c <- append(shade1c,values="black",after=3)
shade2c <- append(shade2c,values="black",after=3)#cluster
shade3c <- append(shade3c,values="black",after=3)
shade4c <- append(shade4c,values="black",after=3)
shade5c <- append(shade5c,values="black",after=3)
shade6c <- append(shade6c,values="black",after=3)
shade7c <- append(shade7c,values="black",after=3)
shade8c <- append(shade8c,values="black",after=3)
shade9c <- append(shade9c,values="black",after=3)
#col

shade1 <- append(shade1,values=NA,after=3)
shade2 <- append(shade2,values=NA,after=3)#cluster
shade3 <- append(shade3,values=NA,after=3)
shade4 <- append(shade4,values=NA,after=3)
shade5 <- append(shade5,values=NA,after=3)
shade6 <- append(shade6,values=NA,after=3)
shade7 <- append(shade7,values=NA,after=3)
shade8 <- append(shade8,values=NA,after=3)
shade9 <- append(shade9,values=NA,after=3)
#gen graphs
area <- cbind(area,shade1,
              shade2,
              shade3,
              shade4,
              shade5,
              shade6,
              shade7,
              shade8,
              shade9)

plot1 <- ggplot(data = area) +  geom_sf(aes(fill=shade1),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Case Counts/Yr")


plot2 <- ggplot(data = area) +  geom_sf(aes(fill=shade2),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Epidemic Cluster")

plot3 <- ggplot(data = area) +  geom_sf(aes(fill=shade3),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="AUC")

plot4 <- ggplot(data = area) +  geom_sf(aes(fill=shade4),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Urban Land %")


plot5 <- ggplot(data = area) +  geom_sf(aes(fill=shade9),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Urban Pop %")


plot6 <- ggplot(data = area) +  geom_sf(aes(fill=shade5),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Endemic Probability")

plot7 <- ggplot(data = area) +  geom_sf(aes(fill=shade6),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  )  + labs(fill="Epidemic Probability")

plot8 <- ggplot(data = area) +  geom_sf(aes(fill=shade7),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Reporting Rate")

plot9 <- ggplot(data = area) +  geom_sf(aes(fill=shade8),color=NA)+
  scale_fill_viridis(na.value="grey")+
  coord_sf()+ # Empty theme without axis lines and texts
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), #
    plot.background = element_rect(fill = "transparent", colour = NA), #
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.margin = margin(0.1, 0.1, 0.1, 0.1),
    legend.key.size = unit(0.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    plot.caption = element_text(hjust = 0,size = 3),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),legend.text=element_text(size=rel(0.5)),legend.title=element_text(size=rel(0.5)) 
  ) + labs(fill="Incoming Flights")


library(ggpubr)

pdf(file="~/nBox/regime_switch_hamfil_thailand/plotsmap/plot.pdf")
ggarrange(plot1,
          plot2,
          plot3,
          plot4,
          plot5,
          plot6,
          plot7,
          plot8,
          plot9,
          ncol = 3, nrow = 3,
          labels=c('A', 'B', 'C', 'D','E','F','G','H','I'),
          widths = 8.27, heights = 11)

dev.off()

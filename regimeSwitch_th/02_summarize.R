#SUMMARIZE PROVINCE LEVEL REGIME CHARACTERISTICS
#SUMMARIZE PROVINCE LEVEL REGIME CHARACTERISTICS
#SUMMARIZE PROVINCE LEVEL REGIME CHARACTERISTICS
#SUMMARIZE PROVINCE LEVEL REGIME CHARACTERISTICS
rm(list=ls())
setwd("~/nBox/regime_switch_hamfil_thailand/results/")

files <- list.files()

pmatStore <- list()
sStore <- list()
yStore <- list()
epiProb <- list()
endProb <- list()
for (k in 1:length(files)){
  load(files[[k]])
  pmatStore[[k]] <- pmat
  sStore[[k]] <- s1
  yStore[[k]] <- y
  
  end <- unlist(lapply(out2,function(x)x[1,1]))
  end <- quantile(end,probs = c(0.025,0.5,0.975))
  
  epi <- unlist(lapply(out2,function(x)x[2,2]))
  epi <- quantile(epi,probs = c(0.025,0.5,0.975))
  
  epiProb[[k]] <- epi
  endProb[[k]] <- end
}


rm(list=setdiff(ls(), c("pmatStore","sStore","yStore","epiProb","endProb","files")))

save.image("~/nBox/regime_switch_hamfil_thailand/output/summarize.Rdata")

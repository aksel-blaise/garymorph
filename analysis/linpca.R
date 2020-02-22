#exploratory analysis of Gary dart point linear measures
library(devtools)
install_github("vqv/ggbiplot")
install.packages("ggtern")
library(ggbiplot)
library(ggtern)
# set working directory
setwd(getwd())
ppgary<-read.csv("garymorphlm.csv",header = TRUE)
type<-ppgary$frdwbgrp
ppgary.pca<-prcomp(ppgary[c(2:6)],center = TRUE,scale. = TRUE)
summary(ppgary.pca)
#plot pca
ggbiplot(ppgary.pca)
ggbiplot(ppgary.pca,obs.scale = 1, var.scale = 1, ellipse = TRUE,groups = type)

#plot ternary graph
maxl<-ppgary$maxl
maxw<-ppgary$maxw
maxth<-ppgary$maxth
maxstl<-ppgary$maxstl
maxstw<-ppgary$maxstw
context<-ppgary$basiccontx
group<-ppgary$frdwbgrp
plot<-ggtern(data = ppgary,aes(x=maxw,y=maxth,z=maxstl)) + geom_point(aes(fill=group),
  size = 3, shape = 21, color = "black")+
  labs(fill = "Ford and Webb (1956) Type")+
  theme_rgbw()
plot

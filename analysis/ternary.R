# ternary plot of maxl, maxw, and maxth for Gary points from Poverty Point
# install.packages("ggtern")
library(ggtern)
#set working directory
setwd(getwd())
ppgary<-read.csv("garymorphlm.csv",header = TRUE)
maxl<-ppgary$maxl
maxw<-ppgary$maxw
maxth<-ppgary$maxth
maxstl<-ppgary$maxstl
maxstw<-ppgary$maxstw
context<-ppgary$basiccontx
group<-ppgary$frdwbgrp
plot<-ggtern(data = ppgary,aes(x=maxl,y=maxw,z=maxth)) + geom_point(aes(fill=group),
    size = 3, shape = 21, color = "black")+
    labs(fill = "Ford and Webb (1956) Type")+
    theme_rgbw()
  plot
# end of code
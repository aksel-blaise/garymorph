# install required analysis packages
#devtools::install_github("rich-iannone/DiagrammeR")
#devtools::install_github("vqv/ggbiplot")
#devtools::install_github("mlcollyer/RRPP")
#devtools::install_github("tidyverse/ggplot2")
#devtools::install_github("kassambara/ggpubr")
# load libraries
library(DiagrammeR)
library(ggbiplot)
library(RRPP)
library(ggplot2)
library(ggpubr)
library(ggfortify)
library(cluster)

### Set working directory, load data, and define variables
# set working directory
setwd(getwd())
data<-read.csv("garymorphlm.csv",header = TRUE, as.is=TRUE)
# define variables
maxl<-data$maxl # maximum length
maxw<-data$maxw # maximum width
maxth<-data$maxth # maximum thickness
maxstl<-data$maxstl # maximum stem length
maxstw<-data$maxstw # maximum stem width
site<-data$site # site name

#pca
df<-data[c(2:6)]
sitepca<-autoplot(prcomp(df),data = data,colour = 'Site',
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE,
         loadings.label.size = 3, frame = TRUE)
sitepca
```

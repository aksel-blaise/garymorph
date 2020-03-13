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
site<-data$Site # site name


### Principal Components Analysis for _site_
# assign varieties based upon reported metrics in Ford and Webb [1956]
lcheck1 <- data$maxl >= 80 & data$maxl <= 140
lcheck2 <- data$maxl >= 46 & data$maxl < 80
lcheck3 <- data$maxl >= 33 & data$maxl < 46

data$tv1 <- "UID" # = Gary points (no variety)
data$tv1 <- ifelse(lcheck1, "L", data$tv1)
data$tv1 <- ifelse(lcheck2, "M/T", data$tv1)
data$tv1 <- ifelse(lcheck3, "S", data$tv1)

tv1<-data$tv1
tv1

# subset poverty point data
pvptmx<-subset(data,Site=="Pov Pt",select=maxl:tv1)
pvptmx
#pca
df<-pvptmx[c(1:5)]
t1pca<-autoplot(prcomp(df),data = pvptmx, colour = 'tv1',
                loadings = TRUE, loadings.colour = 'blue',
                loadings.label = TRUE,loadings.label.size = 3, 
                frame = TRUE)
#render figure
t1pca
fig.cap = "PCA by Gary type-varieties reported by Ford and Webb (1956)."

pp.maxl<-pvptmx$maxl
pp.tv1<-pvptmx$tv1
# anova = maximum length ~ tv1
t1ml<-lm.rrpp(pp.maxl ~ pp.tv1, SS.type = "I",data = pvptmx,iter = 9999,print.progress = FALSE)
anova(t1ml)
# visualise model predictions for maximum length ~ site
pp.tvDF<-data.frame(pp.tv1 = c("L","M/T","S"))
rownames(pp.tvDF)<-c("L","M/T","S")
ppPreds<-predict(t1ml,pp.tvDF)
plot(ppPreds, col = 1:NROW(pp.tvDF))
# pairwise comparison of LS means = which sites differ?
pp.ml<-pairwise(t1ml, groups = pvptmx$tv1)
summary(pp.ml, confidence = 0.95, test.type = "dist")
# correlation between mean vectors (angles in degrees)
summary(pp.ml, confidence = 0.95, test.type = "VC", angle.type = "deg")
# pairwise distances between variances = standardization?
summary(pp.ml, confidence = 0.95, test.type = "var")


# histogram and density plot by maxl by tv1
ggplot(pvptmx, aes(x=pp.maxl, fill = pp.tv1)) +
  geom_histogram(binwidth = 2, alpha = .45, position = "dodge") +
  geom_density(alpha = .5) +
  scale_color_brewer(palette = "Set1") +
  xlim(30,140)


# anova = maximum length ~ site
siteml<-lm.rrpp(maxl ~ Site, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(siteml)
# pairwise comparison = which sites differ?
site.pwml<-pairwise(siteml, groups = data$Site)
summary(site.pwml, confidence = 0.95, test.type = "dist")
# visualise model predictions for maximum length ~ site
siteDF<-data.frame(Site = c("Cooper","Means","Pov Pt"))
rownames(siteDF)<-c("Cooper","Means","Pov Pt")
sitePreds<-predict(siteml,siteDF)
plot(sitePreds, col = 1:NROW(siteDF))
# anova = maximum width ~ site
sitemw<-lm.rrpp(maxw ~ Site, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(sitemw)
# pairwise comparison = which sites differ?
site.pwmw<-pairwise(siteml, groups = data$Site)
summary(site.pwmw, confidence = 0.95, test.type = "dist")
# visualise model predictions for maximum width ~ site
sitePreds<-predict(sitemw,siteDF)
plot(sitePreds)
# anova = maximum thickness ~ site
sitemth<-lm.rrpp(maxth ~ Site, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(sitemth)
# pairwise comparison = which sites differ?
site.pwmth<-pairwise(siteml, groups = data$Site)
summary(site.pwmth, confidence = 0.95, test.type = "dist")
# visualise model predictions for maximum thickness ~ site
sitePreds<-predict(sitemth,siteDF)
plot(sitePreds)
# anova = maximum stem length ~ site
sitemstl<-lm.rrpp(maxstl ~ Site, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(sitemstl)
# pairwise comparison = which sites differ?
site.pwmstl<-pairwise(siteml, groups = data$Site)
summary(site.pwmstl, confidence = 0.95, test.type = "dist")
# visualise model predictions for maximum stem length ~ site
sitePreds<-predict(sitemstl,siteDF)
plot(sitePreds)
# anova = maximum stem width ~ site
sitemstw<-lm.rrpp(maxstw ~ Site, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(sitemstw)
# pairwise comparison = which sites differ?
site.pwmstw<-pairwise(siteml, groups = data$Site)
summary(site.pwmstw, confidence = 0.95, test.type = "dist")
# visualise model predictions for maximum stem width ~ site
sitePreds<-predict(sitemstw,siteDF)
plot(sitePreds)



## Gary varieties proposed by Ford, Phillips, and Haag [-@RN20899]

The `tv0` argument used within this and the subsequent section articulates with three variants of the Gary type--including _Gary Stemmed_, _Typical Gary Stemmed_, _Broad Gary Stemmed_, _Thin Gary Stemmed_, and _Small Gary Stemmed_, which were assigned using those morphological criteria advanced by Ford, Phillips, and Haag [-@RN20899] at the Jaketown site in west central Mississippi. The _Long Gary Stemmed_ type was omitted from consideration due to the absence of reported metrics.

```{r fphgantt, out.width = "100%", dpi = 200, echo=TRUE}
# Ford, Phillips, and Haag (1955)
fphl<-data.frame(Name=c('var.GaryStemmed','var.TypicalGaryStemmed','var.BroadGaryStemmed','var.ThinGaryStemmed','var.SmallGaryStemmed'),
                 Length=c(45,45,45,55,35), # in mm
                 end=c(70,79,70,75,50) # in mm
)
fphlength<-ggplot(fphl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
fphw<-data.frame(Name=c('var.GaryStemmed','var.TypicalGaryStemmed','var.BroadGaryStemmed','var.ThinGaryStemmed','var.SmallGaryStemmed'),
                 Width=c(22,22,32,30,20), # in mm
                 end=c(31,31,48,36,30) # in mm
)
fphwidth<-ggplot(fphw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
fwebbfig<-ggarrange(fphlength,fphwidth,
                    labels = c("a","b"),
                    ncol = 1, nrow = 2)

fig.cap = "Gantt charts illustrating the range of linear measurements for each type-variety reported by Ford, Phillips, and Haag (1955)."
```

### Assign Gary type-varieties proposed by Ford, Phillips, and Haag [-@RN20899]

Ford, Phillips, and Haag [-@RN20899] used a series of metrics (maximum length) to assign Gary varieties at the Jaketown site. The approach empoyed here enlists all of Ford and Webb's [-@RN20898] reported metrics, and applies them to each of the points. Those points that do not articulate with one of the three criteria were assigned as unidentified (UID), and should be considered as Gary with no type-variety assignment. Due to the degree of overlap in those ranges associated with the Gary type-varieties advanced by Ford, Phillips, and Haag [-@RN20898], it is expected that _Gary Stemmed_ will fall out due to overlap in length with _Broad Gary Stemmed_, and overlap in width with _Typical Gary Stemmed_.

```{r tv0, echo=TRUE}
# assign varieties based upon reported metrics in Ford, Phillips, and Haag [1954]
fphlcheck1 <- data$maxl >= 45 & data$maxl <= 70
fphlcheck2 <- data$maxl >= 45 & data$maxl <= 79
fphlcheck3 <- data$maxl >= 45 & data$maxl <= 70
fphlcheck4 <- data$maxl >= 55 & data$maxl <= 75
fphlcheck5 <- data$maxl >= 35 & data$maxl <= 50

fphwcheck1 <- data$maxw >= 22 & data$maxw <= 31
fphwcheck2 <- data$maxw >= 22 & data$maxw <= 31
fphwcheck3 <- data$maxw >= 32 & data$maxw <= 48
fphwcheck4 <- data$maxw >= 30 & data$maxw <= 36
fphwcheck5 <- data$maxw >= 20 & data$maxw <= 30

data$tv0 <- "UID" # = Gary points (no variety)
data$tv0 <- ifelse(fphlcheck1 & fphwcheck1, "GS", data$tv0)
data$tv0 <- ifelse(fphlcheck2 & fphwcheck2, "TyGS", data$tv0)
data$tv0 <- ifelse(fphlcheck3 & fphwcheck3, "BGS", data$tv0)
data$tv0 <- ifelse(fphlcheck4 & fphwcheck4, "ThGS", data$tv0)
data$tv0 <- ifelse(fphlcheck5 & fphwcheck5, "SGS", data$tv0)

tv0<-data$tv0
tv0
```

### Maximum and minimum values for Ford, Phillips, and Haag's [-@RN20899] type-varieties

The Gary type-varieties defined by Ford, Phillips, and Haag [-@RN20899] only included ranges for maximum length and width. The listing appended below reflects the maximum and minimum values for each orthogonal measurement included in the dataset. These metrics may have utility in positing type-variety assignments to partial specimens.

```{r typevartv0, echo=TRUE}
# subset dataset by tv0 type-varieties
mmtygs<-subset(data,tv0=="TyGS",select=maxl:tv0)
mmbgs<-subset(data,tv0=="BGS",select=maxl:tv0)
mmthgs<-subset(data,tv0=="ThGS",select=maxl:tv0)
mmsgs<-subset(data,tv0=="SGS",select=maxl:tv0)
```

#### Maximum/minimum for Typical Gary Stemmed

```{r, maxmin tygs, echo=TRUE}
# identify maximum/minimum metrics for Typical Gary Stemmed

# max length (mm)
max(mmtygs$maxl)
# min length (mm)
min(mmtygs$maxl)
# max width (mm)
max(mmtygs$maxw)
# min width (mm)
min(mmtygs$maxw)
# max thickness (mm)
max(mmtygs$maxth)
# min thickness (mm)
min(mmtygs$maxth)
# max stem length (mm)
max(mmtygs$maxstl)
# min stem length (mm)
min(mmtygs$maxstl)
# max stem width (mm)
max(mmtygs$maxstw)
# min stem width (mm)
min(mmtygs$maxstw)
```

#### Maximum/minimum for Broad Gary Stemmed

```{r, maxmin bgs, echo=TRUE}
# identify maximum/minimum metrics for Broad Gary Stemmed

# max length (mm)
max(mmbgs$maxl)
# min length (mm)
min(mmbgs$maxl)
# max width (mm)
max(mmbgs$maxw)
# min width (mm)
min(mmbgs$maxw)
# max thickness (mm)
max(mmbgs$maxth)
# min thickness (mm)
min(mmbgs$maxth)
# max stem length (mm)
max(mmbgs$maxstl)
# min stem length (mm)
min(mmbgs$maxstl)
# max stem width (mm)
max(mmbgs$maxstw)
# min stem width (mm)
min(mmbgs$maxstw)
```

#### Maximum/minimum for Thin Gary Stemmed

```{r, maxmin thgs, echo=TRUE}
# identify maximum/minimum metrics for Thin Gary Stemmed

# max length (mm)
max(mmthgs$maxl)
# min length (mm)
min(mmthgs$maxl)
# max width (mm)
max(mmthgs$maxw)
# min width (mm)
min(mmthgs$maxw)
# max thickness (mm)
max(mmthgs$maxth)
# min thickness (mm)
min(mmthgs$maxth)
# max stem length (mm)
max(mmthgs$maxstl)
# min stem length (mm)
min(mmthgs$maxstl)
# max stem width (mm)
max(mmthgs$maxstw)
# min stem width (mm)
min(mmthgs$maxstw)
```

#### Maximum/minimum for Small Gary Stemmed

```{r, maxmin sgs, echo=TRUE}
# identify maximum/minimum metrics for Small Gary Stemmed

# max length (mm)
max(mmsgs$maxl)
# min length (mm)
min(mmsgs$maxl)
# max width (mm)
max(mmsgs$maxw)
# min width (mm)
min(mmsgs$maxw)
# max thickness (mm)
max(mmsgs$maxth)
# min thickness (mm)
min(mmsgs$maxth)
# max stem length (mm)
max(mmsgs$maxstl)
# min stem length (mm)
min(mmsgs$maxstl)
# max stem width (mm)
max(mmsgs$maxstw)
# min stem width (mm)
min(mmsgs$maxstw)
```

### Boxplots for `site` by Ford, Phillips, and Haag's [-@RN20899] type-varieties for Gary dart points from Cooper

```{r boxplot c tv0, out.width = "100%", dpi = 200, echo=TRUE}
# subset cooper data
cprmxl<-subset(data,Site=="Cooper",select=maxl:tv0)
# boxplot of maximum length
cprmaxl<-ggplot(cprmxl,aes(x=tv0,y=maxl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
cprmaxw<-ggplot(cprmxl,aes(x=tv0,y=maxw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxth<-ggplot(cprmxl,aes(x=tv0,y=maxth,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
cprmaxstl<-ggplot(cprmxl,aes(x=tv0,y=maxstl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxstw<-ggplot(cprmxl,aes(x=tv0,y=maxstw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
cprfigure<-ggarrange(cprmaxl,cprmaxw,cprmaxth,cprmaxstl,cprmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
cprfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Cooper site."
```

### Boxplots for `site` by Ford, Phillips, and Haag's [-@RN20899] type-varieties for Gary dart points from Means

```{r boxplot m tv0, out.width = "100%", dpi = 200, echo=TRUE}
# subset means data
mnsmxl<-subset(data,Site=="Means",select=maxl:tv0)
# boxplot of maximum length
mnsmaxl<-ggplot(mnsmxl,aes(x=tv0,y=maxl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
mnsmaxw<-ggplot(mnsmxl,aes(x=tv0,y=maxw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxth<-ggplot(mnsmxl,aes(x=tv0,y=maxth,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
mnsmaxstl<-ggplot(mnsmxl,aes(x=tv0,y=maxstl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxstw<-ggplot(mnsmxl,aes(x=tv0,y=maxstw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
mnsfigure<-ggarrange(mnsmaxl,mnsmaxw,mnsmaxth,mnsmaxstl,mnsmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
mnsfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Means site."
```

### Boxplots for `site` by Ford, Phillips, and Haag's [-@RN20899] type-varieties for Gary dart points from Poverty Point

```{r boxplot pp tv0, out.width = "100%", dpi = 200, echo=TRUE}
# subset poverty point data
pvptmxl<-subset(data,Site=="Pov Pt",select=maxl:tv0)
# boxplot of maximum length
pvptmaxl<-ggplot(pvptmxl,aes(x=tv0,y=maxl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
pvptmaxw<-ggplot(pvptmxl,aes(x=tv0,y=maxw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxth<-ggplot(pvptmxl,aes(x=tv0,y=maxth,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
pvptmaxstl<-ggplot(pvptmxl,aes(x=tv0,y=maxstl,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxstw<-ggplot(pvptmxl,aes(x=tv0,y=maxstw,color=tv0)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
ppfigure<-ggarrange(pvptmaxl,pvptmaxw,pvptmaxth,pvptmaxstl,pvptmaxstw,
                    labels = c("a","b","c","d","e"),
                    ncol = 3, nrow = 2)
ppfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Poverty Point site."
```

### Principal Components Analysis for Ford, Phillips, and Haag's [-@RN20899] type-varieties at all sites

```{r pca tv0, out.width = "100%", dpi = 200, echo=TRUE}
#pca
df<-data[c(2:6)]
tv0pca<-autoplot(prcomp(df),data = data, colour = 'tv0',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE,loadings.label.size = 3, 
                 frame = TRUE)
#render figure
tv0pca
fig.cap = "PCA by Gary type-varieties reported by Ford, Phillips, and Haag (1955)."
```

### Analyses of Variance (ANOVA) for linear variables ~ Ford, Phillips, and Haag's [-@RN20899] type-varieties

```{r anova tv0, echo=TRUE}
# anova = maximum length ~ tv0
t0ml<-lm.rrpp(maxl ~ tv0, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t0ml)
# anova = maximum width ~ tv0
t0mw<-lm.rrpp(maxw ~ tv0, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t0mw)
# anova = maximum thickness ~ tv0
t0mth<-lm.rrpp(maxth ~ tv0, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t0mth)
# anova = maximum stem length ~ tv0
t0mstl<-lm.rrpp(maxstl ~ tv0, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t0mstl)
# anova = maximum stem width ~ tv0
t0mstw<-lm.rrpp(maxstw ~ tv0, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t0mstw)
```


## Gary varieties proposed by Johnson [-@RN2403]

The `tv3` argument used within this and the subsequent section articulates with nine variants of the Gary type--including _var. Kaufman_, _var. Alsa_, _var. Hobson_, _var. Runge_, _var. Emory_, _var. Kemp_, _var. Panna Maria_, _var. Kenedy_, and _var. Colfax_, which were assigned using those morphological criteria advanced by Johnson [-@RN2403] at the Yarbrough and Miller sites in northeast Texas.

```{r johnsongantt, out.width = "100%", dpi = 200, echo=TRUE}
# reported length by variety
jl<-data.frame(Name=c('var.Kaufman','var.Alsa','var.Hobson','var.Runge','var.Emory','var.Kemp','var.PannaMaria','var.Kenedy','var.Colfax'),
               Length=c(34,30,22,31,27,29,39,21,25), # in mm
               end=c(77,55,58,52,35,40,58,31,43) # in mm
)
length<-ggplot(jl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported width by variety
jw<-data.frame(Name=c('var.Kaufman','var.Alsa','var.Hobson','var.Runge','var.Emory','var.Kemp','var.PannaMaria','var.Kenedy','var.Colfax'),
               Width=c(23,30,13,18,14,13,15,15,14), # in mm
               end=c(38,42,30,34,24,18,28,23,23) # in mm
)
width<-ggplot(jw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported thickness by variety
jth<-data.frame(Name=c('var.Kaufman','var.Alsa','var.Hobson','var.Runge','var.Emory','var.Kemp','var.PannaMaria','var.Kenedy','var.Colfax'),
                Thickness=c(5,9,5,6,7,5,7,5,5), # in mm
                end=c(13,12,11,10,9,9,18,7,8) # in mm
)
thickness<-ggplot(jth,aes(x=Thickness,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")

johnsonfig<-ggarrange(length,width,thickness,
                      labels = c("a","b","c"),
                      ncol = 1, nrow = 3)

fig.cap = "Gantt charts illustrating the range of linear measurements for each type-variety reported by Johnson (1961)."
```

### Assign Gary type-varieties proposed by Johnson [-@RN2403]

Johnson [-@RN2403] used a combination of maximum length, width, and thickness to assign Gary varieties at the Yarbrough and Miller sites. The approach empoyed here enlists all of Johnson [-@RN2403] reported metrics. Those points that do not match with one of the three criteria were assigned as unidentified ("UID"), which effectively discriminates a Gary point with no assigned variety.

```{r tv3, echo=TRUE}
# assign varieties based upon reported metrics in Schambach (1998)
jlcheck1 <- data$maxl >= 34 & data$maxl <= 77
jlcheck2 <- data$maxl >= 30 & data$maxl <= 55
jlcheck3 <- data$maxl >= 22 & data$maxl <= 58
jlcheck4 <- data$maxl >= 31 & data$maxl <= 52
jlcheck5 <- data$maxl >= 27 & data$maxl <= 35
jlcheck6 <- data$maxl >= 29 & data$maxl <= 40
jlcheck7 <- data$maxl >= 39 & data$maxl <= 58
jlcheck8 <- data$maxl >= 21 & data$maxl <= 31
jlcheck9 <- data$maxl >= 25 & data$maxl <= 43

jwcheck1 <- data$maxw >= 23 & data$maxw <= 38
jwcheck2 <- data$maxw >= 30 & data$maxw <= 42
jwcheck3 <- data$maxw >= 13 & data$maxw <= 30
jwcheck4 <- data$maxw >= 18 & data$maxw <= 34
jwcheck5 <- data$maxw >= 14 & data$maxw <= 24
jwcheck6 <- data$maxw >= 13 & data$maxw <= 18
jwcheck7 <- data$maxw >= 15 & data$maxw <= 28
jwcheck8 <- data$maxw >= 15 & data$maxw <= 23
jwcheck9 <- data$maxw >= 14 & data$maxw <= 23

jtcheck1 <- data$maxth >= 5 & data$maxth <= 13
jtcheck2 <- data$maxth >= 9 & data$maxth <= 12
jtcheck3 <- data$maxth >= 5 & data$maxth <= 11
jtcheck4 <- data$maxth >= 6 & data$maxth <= 10
jtcheck5 <- data$maxth >= 7 & data$maxth <= 9
jtcheck6 <- data$maxth >= 5 & data$maxth <= 9
jtcheck7 <- data$maxth >= 7 & data$maxth <= 18
jtcheck8 <- data$maxth >= 5 & data$maxth <= 7
jtcheck9 <- data$maxth >= 5 & data$maxth <= 8

data$tv3 <- "UID" # = Gary points (no variety)
data$tv3 <- ifelse(jlcheck1 & jwcheck1 & jtcheck1, "Kf", data$tv3)
data$tv3 <- ifelse(jlcheck2 & jwcheck2 & jtcheck2, "Al", data$tv3)
data$tv3 <- ifelse(jlcheck3 & jwcheck3 & jtcheck3, "Ho", data$tv3)
data$tv3 <- ifelse(jlcheck4 & jwcheck4 & jtcheck4, "Ru", data$tv3)
data$tv3 <- ifelse(jlcheck5 & jwcheck5 & jtcheck5, "Em", data$tv3)
data$tv3 <- ifelse(jlcheck6 & jwcheck6 & jtcheck6, "Km", data$tv3)
data$tv3 <- ifelse(jlcheck7 & jwcheck7 & jtcheck7, "PM", data$tv3)
data$tv3 <- ifelse(jlcheck8 & jwcheck8 & jtcheck8, "Kn", data$tv3)
data$tv3 <- ifelse(jlcheck9 & jwcheck9 & jtcheck9, "Cl", data$tv3)

tv3<-data$tv3
tv3
```

### Maximum and minimum values for Johnson's [-@RN2403] type-varieties

The Gary type-varieties defined by Johnson [-@RN2403] only included ranges for maximum length, width, and thickness. The listing appended below reflects the maximum and minimum values for each measure included in the dataset. These metrics may have utility in positing type-variety assignments to partial specimens.

```{r typevartv3, echo=TRUE}
# subset dataset by tv3 type-varieties
jkf<-subset(data,tv3=="Kf",select=maxl:tv3)
jal<-subset(data,tv3=="Al",select=maxl:tv3)
jho<-subset(data,tv3=="Ho",select=maxl:tv3)
jru<-subset(data,tv3=="Ru",select=maxl:tv3)
jem<-subset(data,tv3=="Em",select=maxl:tv3)
jkm<-subset(data,tv3=="Km",select=maxl:tv3)
jpm<-subset(data,tv3=="PM",select=maxl:tv3)
jkn<-subset(data,tv3=="Kn",select=maxl:tv3)
jcl<-subset(data,tv3=="Cl",select=maxl:tv3)
```

##### Maximum/minimum for _variety Kaufman_

```{r, maxmin jkf, echo=TRUE}
# identify maximum/minimum metrics for variety Kaufman

# max length (mm)
max(jkf$maxl)
# min length (mm)
min(jkf$maxl)
# max width (mm)
max(jkf$maxw)
# min width (mm)
min(jkf$maxw)
# max thickness (mm)
max(jkf$maxth)
# min thickness (mm)
min(jkf$maxth)
# max stem length (mm)
max(jkf$maxstl)
# min stem length (mm)
min(jkf$maxstl)
# max stem width (mm)
max(jkf$maxstw)
# min stem width (mm)
min(jkf$maxstw)
```

#### Maximum/minimum for _variety Alsa_

```{r, maxmin jal, echo=TRUE}
# identify maximum/minimum metrics for variety Alsa

# max length (mm)
max(jal$maxl)
# min length (mm)
min(jal$maxl)
# max width (mm)
max(jal$maxw)
# min width (mm)
min(jal$maxw)
# max thickness (mm)
max(jal$maxth)
# min thickness (mm)
min(jal$maxth)
# max stem length (mm)
max(jal$maxstl)
# min stem length (mm)
min(jal$maxstl)
# max stem width (mm)
max(jal$maxstw)
# min stem width (mm)
min(jal$maxstw)
```

#### Maximum/minimum for _variety Hobson_

```{r, maxmin jho, echo=TRUE}
# identify maximum/minimum metrics for variety Hobson

# max length (mm)
max(jho$maxl)
# min length (mm)
min(jho$maxl)
# max width (mm)
max(jho$maxw)
# min width (mm)
min(jho$maxw)
# max thickness (mm)
max(jho$maxth)
# min thickness (mm)
min(jho$maxth)
# max stem length (mm)
max(jho$maxstl)
# min stem length (mm)
min(jho$maxstl)
# max stem width (mm)
max(jho$maxstw)
# min stem width (mm)
min(jho$maxstw)
```

#### Maximum/minimum for _variety Runge_

```{r, maxmin jru, echo=TRUE}
# identify maximum/minimum metrics for variety Runge

# max length (mm)
max(jru$maxl)
# min length (mm)
min(jru$maxl)
# max width (mm)
max(jru$maxw)
# min width (mm)
min(jru$maxw)
# max thickness (mm)
max(jru$maxth)
# min thickness (mm)
min(jru$maxth)
# max stem length (mm)
max(jru$maxstl)
# min stem length (mm)
min(jru$maxstl)
# max stem width (mm)
max(jru$maxstw)
# min stem width (mm)
min(jru$maxstw)
```

#### Maximum/minimum for _variety Emory_

```{r, maxmin jem, echo=TRUE}
# identify maximum/minimum metrics for variety Emory

# max length (mm)
max(jem$maxl)
# min length (mm)
min(jem$maxl)
# max width (mm)
max(jem$maxw)
# min width (mm)
min(jem$maxw)
# max thickness (mm)
max(jem$maxth)
# min thickness (mm)
min(jem$maxth)
# max stem length (mm)
max(jem$maxstl)
# min stem length (mm)
min(jem$maxstl)
# max stem width (mm)
max(jem$maxstw)
# min stem width (mm)
min(jem$maxstw)
```

#### Maximum/minimum for _variety Kemp_

```{r, maxmin jkm, echo=TRUE}
# identify maximum/minimum metrics for variety Kemp

# max length (mm)
max(jkm$maxl)
# min length (mm)
min(jkm$maxl)
# max width (mm)
max(jkm$maxw)
# min width (mm)
min(jkm$maxw)
# max thickness (mm)
max(jkm$maxth)
# min thickness (mm)
min(jkm$maxth)
# max stem length (mm)
max(jkm$maxstl)
# min stem length (mm)
min(jkm$maxstl)
# max stem width (mm)
max(jkm$maxstw)
# min stem width (mm)
min(jkm$maxstw)
```

#### Maximum/minimum for _variety Panna Maria_

```{r, maxmin jpm, echo=TRUE}
# identify maximum/minimum metrics for variety Panna Maria

# max length (mm)
max(jpm$maxl)
# min length (mm)
min(jpm$maxl)
# max width (mm)
max(jpm$maxw)
# min width (mm)
min(jpm$maxw)
# max thickness (mm)
max(jpm$maxth)
# min thickness (mm)
min(jpm$maxth)
# max stem length (mm)
max(jpm$maxstl)
# min stem length (mm)
min(jpm$maxstl)
# max stem width (mm)
max(jpm$maxstw)
# min stem width (mm)
min(jpm$maxstw)
```

#### Maximum/minimum for _variety Kenedy_

```{r, maxmin jkn, echo=TRUE}
# identify maximum/minimum metrics for variety Kenedy

# max length (mm)
max(jkn$maxl)
# min length (mm)
min(jkn$maxl)
# max width (mm)
max(jkn$maxw)
# min width (mm)
min(jkn$maxw)
# max thickness (mm)
max(jkn$maxth)
# min thickness (mm)
min(jkn$maxth)
# max stem length (mm)
max(jkn$maxstl)
# min stem length (mm)
min(jkn$maxstl)
# max stem width (mm)
max(jkn$maxstw)
# min stem width (mm)
min(jkn$maxstw)
```

#### Maximum/minimum for _variety Colfax_

```{r, maxmin jcl, echo=TRUE}
# identify maximum/minimum metrics for variety Colfax

# max length (mm)
max(jcl$maxl)
# min length (mm)
min(jcl$maxl)
# max width (mm)
max(jcl$maxw)
# min width (mm)
min(jcl$maxw)
# max thickness (mm)
max(jcl$maxth)
# min thickness (mm)
min(jcl$maxth)
# max stem length (mm)
max(jcl$maxstl)
# min stem length (mm)
min(jcl$maxstl)
# max stem width (mm)
max(jcl$maxstw)
# min stem width (mm)
min(jcl$maxstw)
```

### Boxplots for `site` by Johnson's [-@RN2403] type-varieties for Gary dart points from Cooper

```{r boxplot c tv3, out.width = "100%", dpi = 200, echo=TRUE}
# subset cooper data
cprmxl<-subset(data,Site=="Cooper",select=maxl:tv3)
# boxplot of maximum length
cprmaxl<-ggplot(cprmxl,aes(x=tv3,y=maxl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
cprmaxw<-ggplot(cprmxl,aes(x=tv3,y=maxw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxth<-ggplot(cprmxl,aes(x=tv3,y=maxth,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
cprmaxstl<-ggplot(cprmxl,aes(x=tv3,y=maxstl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxstw<-ggplot(cprmxl,aes(x=tv3,y=maxstw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
cprfigure<-ggarrange(cprmaxl,cprmaxw,cprmaxth,cprmaxstl,cprmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
cprfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Cooper site."
```

### Boxplots for `site` by Johnson's [-@RN2403] type-varieties for Gary dart points from Means

```{r boxplot m tv3, out.width = "100%", dpi = 200, echo=TRUE}
# subset means data
mnsmxl<-subset(data,Site=="Means",select=maxl:tv3)
# boxplot of maximum length
mnsmaxl<-ggplot(mnsmxl,aes(x=tv3,y=maxl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
mnsmaxw<-ggplot(mnsmxl,aes(x=tv3,y=maxw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxth<-ggplot(mnsmxl,aes(x=tv3,y=maxth,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
mnsmaxstl<-ggplot(mnsmxl,aes(x=tv3,y=maxstl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxstw<-ggplot(mnsmxl,aes(x=tv3,y=maxstw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
mnsfigure<-ggarrange(mnsmaxl,mnsmaxw,mnsmaxth,mnsmaxstl,mnsmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
mnsfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Means site."
```

### Boxplots for `site` by Johnson's [-@RN2403] type-varieties for Gary dart points from Poverty Point

```{r boxplot pp tv3, out.width = "100%", dpi = 200, echo=TRUE}
# subset poverty point data
pvptmxl<-subset(data,Site=="Pov Pt",select=maxl:tv3)
# boxplot of maximum length
pvptmaxl<-ggplot(pvptmxl,aes(x=tv3,y=maxl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
pvptmaxw<-ggplot(pvptmxl,aes(x=tv3,y=maxw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxth<-ggplot(pvptmxl,aes(x=tv3,y=maxth,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
pvptmaxstl<-ggplot(pvptmxl,aes(x=tv3,y=maxstl,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxstw<-ggplot(pvptmxl,aes(x=tv3,y=maxstw,color=tv3)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
ppfigure<-ggarrange(pvptmaxl,pvptmaxw,pvptmaxth,pvptmaxstl,pvptmaxstw,
                    labels = c("a","b","c","d","e"),
                    ncol = 3, nrow = 2)
ppfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Poverty Point site."
```

### Principal Components Analysis for Johnson's [-@RN2403] type-varieties at all sites

```{r pca tv3, out.width = "100%", dpi = 200, echo=TRUE}
#pca
df<-data[c(2:6)]
tv3pca<-autoplot(prcomp(df),data = data, colour = 'tv3',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE,loadings.label.size = 3, 
                 frame = TRUE)
#render figure
tv3pca
fig.cap = "PCA by Gary type-varieties reported by Johnson (1961)."
```

### Analyses of Variance (ANOVA) for linear variables ~ Johnson's [-@RN2403] type-varieties

```{r anova tv3, echo=TRUE}
# anova = maximum length ~ tv3
t3ml<-lm.rrpp(maxl ~ tv3, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t3ml)
# anova = maximum width ~ tv3
t3mw<-lm.rrpp(maxw ~ tv3, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t3mw)
# anova = maximum thickness ~ tv3
t3mth<-lm.rrpp(maxth ~ tv3, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t3mth)
# anova = maximum stem length ~ tv3
t3mstl<-lm.rrpp(maxstl ~ tv3, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t3mstl)
# anova = maximum stem width ~ tv3
t3mstw<-lm.rrpp(maxstw ~ tv3, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t3mstw)
```

## Gary type-varieties proposed by Shafer [-@RN3682]

```{r shafgantt, out.width = "100%", dpi = 200, echo=TRUE}
# reported length by variety
shafl<-data.frame(Name=c('Group 1','Group2'),
                  Length=c(28,26), # in mm
                  end=c(63,29) # in mm
)
length<-ggplot(shafl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported width by variety
shafw<-data.frame(Name=c('Group 1','Group2'),
                  Width=c(15,15), # in mm
                  end=c(30,19) # in mm
)
width<-ggplot(shafw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported thickness by variety
shafth<-data.frame(Name=c('Group 1','Group2'),
                   Thickness=c(6,7), # in mm
                   end=c(10,8) # in mm
)
thickness<-ggplot(shafth,aes(x=Thickness,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported stem length by variety
shafstl<-data.frame(Name=c('Group 1','Group2'),
                    StemLength=c(10,12), # in mm
                    end=c(17,17) # in mm
)
stemlength<-ggplot(shafstl,aes(x=StemLength,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported stem width by variety
shafstw<-data.frame(Name=c('Group 1','Group 2'),
                    StemWidth=c(11,15), # in mm
                    end=c(21,19) # in mm
)
stemwidth<-ggplot(shafstw,aes(x=StemWidth,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
shaferfig<-ggarrange(length,width,thickness,stemlength,stemwidth,
                     labels = c("a","b"),
                     ncol = 2, nrow = 3)
shaferfig

fig.cap = "Gantt charts illustrating the range of linear measurements for each type-variety reported by Shafer (1973)."
```

### Assign Gary type-varieties proposed by Shafer [-@RN3682]

Shafer [-@RN3682] used maximum length, width, thickness, stem width, and stem length to assign Gary varieties at the George C. Davis site. The approach empoyed here enlists both of Shafer's [-@RN3682] reported metrics, and applies them to each of the points within the aggregated sample. Those points that do not articulate with one of the three criteria were assigned as unidentified (UID), and should be considered as Gary with no type-variety assignment.

```{r tv4, echo=TRUE}
# assign varieties based upon reported metrics in Shafer (1973)
shlcheck1 <- data$maxl >= 28 & data$maxl <= 63
shlcheck2 <- data$maxl >= 26 & data$maxl <= 29

shwcheck1 <- data$maxw >= 15 & data$maxw <= 30
shwcheck2 <- data$maxw >= 15 & data$maxw <= 19

shtcheck1 <- data$maxth >= 6 & data$maxth <= 10
shtcheck2 <- data$maxth >= 7 & data$maxth <= 8

shslcheck1 <- data$maxstl >= 10 & data$maxstl <= 17
shslcheck2 <- data$maxstl >= 12 & data$maxstl <= 17

shswcheck1 <- data$maxstw >= 11 & data$maxstw <= 21
shswcheck2 <- data$maxstw >= 15 & data$maxstw <= 19

data$tv4 <- "UID" # = Gary points (no variety)
data$tv4 <- ifelse(shlcheck1 & shwcheck1 & shtcheck1 & shslcheck1 & shswcheck1, "G1", data$tv4)
data$tv4 <- ifelse(shlcheck2 & shwcheck2 & shtcheck2 & shslcheck2 & shswcheck2, "G2", data$tv4)

tv4<-data$tv4
tv4
```

### Maximum and minimum values for Shafer's [-@RN3682] type-varieties

The Gary type-varieties defined by Shafer [-@RN3682] included ranges for maximum length width, thickness, stem length, and stem width. The listing appended below reflects the maximum and minimum values for each  measure included in the dataset, and extends Shafer's type-varieties beyond the George C. Davis site. These metrics may have utility in positing type-variety assignments to partial specimens.

```{r typevartv4, echo=TRUE}
# subset dataset by tv4 type-varieties
mmg1<-subset(data,tv4=="G1",select=maxl:tv4)
mmg2<-subset(data,tv4=="G2",select=maxl:tv4)
```

##### Maximum/minimum for _Group 1_

```{r, maxmin mmg1, echo=TRUE}
# identify maximum/minimum metrics for Group 1

# max length (mm)
max(mmg1$maxl)
# min length (mm)
min(mmg1$maxl)
# max width (mm)
max(mmg1$maxw)
# min width (mm)
min(mmg1$maxw)
# max thickness (mm)
max(mmg1$maxth)
# min thickness (mm)
min(mmg1$maxth)
# max stem length (mm)
max(mmg1$maxstl)
# min stem length (mm)
min(mmg1$maxstl)
# max stem width (mm)
max(mmg1$maxstw)
# min stem width (mm)
min(mmg1$maxstw)
```

##### Maximum/minimum for _Group 2_

```{r, maxmin mmg2, echo=TRUE}
# identify maximum/minimum metrics for Group 2

# max length (mm)
max(mmg2$maxl)
# min length (mm)
min(mmg2$maxl)
# max width (mm)
max(mmg2$maxw)
# min width (mm)
min(mmg2$maxw)
# max thickness (mm)
max(mmg2$maxth)
# min thickness (mm)
min(mmg2$maxth)
# max stem length (mm)
max(mmg2$maxstl)
# min stem length (mm)
min(mmg2$maxstl)
# max stem width (mm)
max(mmg2$maxstw)
# min stem width (mm)
min(mmg2$maxstw)
```

### Boxplots for `site` by Shafer's [-@RN3682] type-varieties for Gary dart points from Cooper

```{r boxplot c tv4, out.width = "100%", dpi = 200, echo=TRUE}
# subset cooper data
cprmxl<-subset(data,Site=="Cooper",select=maxl:tv4)
# boxplot of maximum length
cprmaxl<-ggplot(cprmxl,aes(x=tv4,y=maxl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
cprmaxw<-ggplot(cprmxl,aes(x=tv4,y=maxw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxth<-ggplot(cprmxl,aes(x=tv4,y=maxth,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
cprmaxstl<-ggplot(cprmxl,aes(x=tv4,y=maxstl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxstw<-ggplot(cprmxl,aes(x=tv4,y=maxstw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
cprfigure<-ggarrange(cprmaxl,cprmaxw,cprmaxth,cprmaxstl,cprmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
cprfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Cooper site."
```

### Boxplots for `site` by Shafer's [-@RN3682] type-varieties for Gary dart points from Means

```{r boxplot m tv4, out.width = "100%", dpi = 200, echo=TRUE}
# subset means data
mnsmxl<-subset(data,Site=="Means",select=maxl:tv4)
# boxplot of maximum length
mnsmaxl<-ggplot(mnsmxl,aes(x=tv4,y=maxl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
mnsmaxw<-ggplot(mnsmxl,aes(x=tv4,y=maxw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxth<-ggplot(mnsmxl,aes(x=tv4,y=maxth,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
mnsmaxstl<-ggplot(mnsmxl,aes(x=tv4,y=maxstl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxstw<-ggplot(mnsmxl,aes(x=tv4,y=maxstw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
mnsfigure<-ggarrange(mnsmaxl,mnsmaxw,mnsmaxth,mnsmaxstl,mnsmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
mnsfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Means site."
```

### Boxplots for `site` by Shafer's [-@RN3682] type-varieties for Gary dart points from Poverty Point

```{r boxplot pp tv4, out.width = "100%", dpi = 200, echo=TRUE}
# subset poverty point data
pvptmxl<-subset(data,Site=="Pov Pt",select=maxl:tv4)
# boxplot of maximum length
pvptmaxl<-ggplot(pvptmxl,aes(x=tv4,y=maxl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
pvptmaxw<-ggplot(pvptmxl,aes(x=tv4,y=maxw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxth<-ggplot(pvptmxl,aes(x=tv4,y=maxth,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
pvptmaxstl<-ggplot(pvptmxl,aes(x=tv4,y=maxstl,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxstw<-ggplot(pvptmxl,aes(x=tv4,y=maxstw,color=tv4)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
ppfigure<-ggarrange(pvptmaxl,pvptmaxw,pvptmaxth,pvptmaxstl,pvptmaxstw,
                    labels = c("a","b","c","d","e"),
                    ncol = 3, nrow = 2)
ppfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Poverty Point site."
```

### Principal Components Analysis for Shafer's [-@RN3682] type-varieties at all sites

```{r pca tv4, out.width = "100%", dpi = 200, echo=TRUE}
#pca
df<-data[c(2:6)]
tv4pca<-autoplot(prcomp(df),data = data, colour = 'tv4',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE,loadings.label.size = 3, 
                 frame = TRUE)
#render figure
tv4pca
fig.cap = "PCA by Gary type-varieties reported by Shafer (1973)."
```

### Analyses of Variance (ANOVA) for linear variables ~ Shafer's [-@RN3682] type-varieties

```{r anova tv4, echo=TRUE}
# anova = maximum length ~ tv4
t4ml<-lm.rrpp(maxl ~ tv4, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t4ml)
# anova = maximum width ~ tv4
t4mw<-lm.rrpp(maxw ~ tv4, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t4mw)
# anova = maximum thickness ~ tv4
t4mth<-lm.rrpp(maxth ~ tv4, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t4mth)
# anova = maximum stem length ~ tv4
t4mstl<-lm.rrpp(maxstl ~ tv4, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t4mstl)
# anova = maximum stem width ~ tv4
t4mstw<-lm.rrpp(maxstw ~ tv4, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t4mstw)
```

## Gary varieties proposed by Schambach [-@RN3132]

```{r schamgantt, out.width = "100%", dpi = 200, echo=TRUE}
# reported length by variety
schambl<-data.frame(Name=c('var.Gary','var.Malvern','var.LeFlore','var.Bodcaw','var.Manice','var.Camden','var.CamdenA'),
                    Length=c(51,43,43,40,36,34,39), # in mm
                    end=c(73,72,80,60,57,73,67) # in mm
)
length<-ggplot(schambl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported width by variety
schambw<-data.frame(Name=c('var.Gary','var.Malvern','var.LeFlore','var.Bodcaw','var.Manice','var.Camden','var.CamdenA','var.CamdenB'),
                    Width=c(31,23,25,21,22,14,16,26), # in mm
                    end=c(45,33,54,36,41,35,27,38) # in mm
)
width<-ggplot(schambw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported thickness by variety
schambth<-data.frame(Name=c('var.Gary','var.Malvern','var.LeFlore','var.Bodcaw','var.Manice','var.Camden','var.CamdenA','var.CamdenB'),
                     Thickness=c(6,7,5,5,6,6,5,7), # in mm
                     end=c(11,13,13,12,9,12,14,11) # in mm
)
thickness<-ggplot(schambth,aes(x=Thickness,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported stem length by variety
schambstl<-data.frame(Name=c('var.Gary','var.Malvern','var.LeFlore','var.Bodcaw','var.Manice','var.Camden','var.CamdenA','var.CamdenB'),
                      StemLength=c(15,11,11,11,10,9,9,12), # in mm
                      end=c(29,23,24,24,17,22,19,18) # in mm
)
stemlength<-ggplot(schambstl,aes(x=StemLength,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# reported stem width by variety
schambstw<-data.frame(Name=c('var.Gary','var.Malvern','var.LeFlore','var.Bodcaw','var.Manice','var.Camden','var.CamdenA'), # var.CamdenB-stemlength not listed in text
                      StemWidth=c(20,17,13,15,9,12,11), # in mm
                      end=c(28,25,33,24,20,24,21) # in mm
)
stemwidth<-ggplot(schambstw,aes(x=StemWidth,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
schambachfig<-ggarrange(length,width,thickness,stemlength,stemwidth,
                        labels = c("a","b","c","d","e"),
                        ncol = 2, nrow = 3)

fig.cap = "Gantt charts illustrating the range of linear measurements for each type-variety reported by Schambach (1998)."
```


```{r tv2, echo=TRUE}
# assign varieties based upon reported metrics in Schambach (1998)
slcheck1 <- data$maxl >= 51 & data$maxl <= 73
slcheck2 <- data$maxl >= 43 & data$maxl <= 72
slcheck3 <- data$maxl >= 43 & data$maxl <= 80
slcheck4 <- data$maxl >= 40 & data$maxl <= 60
slcheck5 <- data$maxl >= 36 & data$maxl <= 57
slcheck6 <- data$maxl >= 34 & data$maxl <= 73
slcheck7 <- data$maxl >= 39 & data$maxl <= 67

swcheck1 <- data$maxw >= 22 & data$maxw <= 31
swcheck2 <- data$maxw >= 23 & data$maxw <= 33
swcheck3 <- data$maxw >= 25 & data$maxw <= 54
swcheck4 <- data$maxw >= 21 & data$maxw <= 36
swcheck5 <- data$maxw >= 22 & data$maxw <= 41
swcheck6 <- data$maxw >= 14 & data$maxw <= 35
swcheck7 <- data$maxw >= 16 & data$maxw <= 27
swcheck8 <- data$maxw >= 26 & data$maxw <= 38

stcheck1 <- data$maxth >= 6 & data$maxth <= 11
stcheck2 <- data$maxth >= 7 & data$maxth <= 13
stcheck3 <- data$maxth >= 5 & data$maxth <= 13
stcheck4 <- data$maxth >= 5 & data$maxth <= 12
stcheck5 <- data$maxth >= 6 & data$maxth <= 9
stcheck6 <- data$maxth >= 6 & data$maxth <= 12
stcheck7 <- data$maxth >= 5 & data$maxth <= 14
stcheck8 <- data$maxth >= 7 & data$maxth <= 11

sslcheck1 <- data$maxstl >= 15 & data$maxstl <= 29
sslcheck2 <- data$maxstl >= 11 & data$maxstl <= 23
sslcheck3 <- data$maxstl >= 11 & data$maxstl <= 24
sslcheck4 <- data$maxstl >= 11 & data$maxstl <= 24
sslcheck5 <- data$maxstl >= 10 & data$maxstl <= 17
sslcheck6 <- data$maxstl >= 9 & data$maxstl <= 22
sslcheck7 <- data$maxstl >= 9 & data$maxstl <= 19
sslcheck8 <- data$maxstl >= 9 & data$maxstl <= 19

sswcheck1 <- data$maxstw >= 20 & data$maxstw <= 28
sswcheck2 <- data$maxstw >= 17 & data$maxstw <= 25
sswcheck3 <- data$maxstw >= 13 & data$maxstw <= 33
sswcheck4 <- data$maxstw >= 15 & data$maxstw <= 24
sswcheck5 <- data$maxstw >= 12 & data$maxstw <= 24
sswcheck6 <- data$maxstw >= 9 & data$maxstw <= 20
sswcheck7 <- data$maxstw >= 11 & data$maxstw <= 21

data$tv2 <- "UID" # = Gary points (no variety)
data$tv2 <- ifelse(slcheck1 & swcheck1 & stcheck1 & sslcheck1 & sswcheck1, "G", data$tv2)
data$tv2 <- ifelse(slcheck2 & swcheck2 & stcheck2 & sslcheck2 & sswcheck2, "Ml", data$tv2)
data$tv2 <- ifelse(slcheck3 & swcheck3 & stcheck3 & sslcheck3 & sswcheck3, "LF", data$tv2)
data$tv2 <- ifelse(slcheck4 & swcheck4 & stcheck4 & sslcheck4 & sswcheck4, "Bd", data$tv2)
data$tv2 <- ifelse(slcheck5 & swcheck5 & stcheck5 & sslcheck5 & sswcheck5, "Mn", data$tv2)
data$tv2 <- ifelse(slcheck6 & swcheck6 & stcheck6 & sslcheck6 & sswcheck6, "Cm", data$tv2)
data$tv2 <- ifelse(slcheck7 & swcheck7 & stcheck7 & sslcheck7 & sswcheck7, "CmA", data$tv2)
data$tv2 <- ifelse(swcheck8 & stcheck8 & sslcheck8, "CmB", data$tv2)

tv2<-data$tv2
tv2
```

### Maximum and minimum values for Schambach's [-@RN3132] type-varieties

The Gary type-varieties defined by Schambach [-@RN3132] included ranges for maximum length width, thickness, stem length, and stem width for most of the types, but maximum length and maximum stem width were missing from _variety Camden B_. The listing appended below reflects the maximum and minimum values for each  measure included in the dataset, and extends Schambach's type-varieties beyond the Cooper and Means sites. These metrics may have utility in positing type-variety assignments to partial specimens.

```{r typevartv2, echo=TRUE}
# subset dataset by tv2 type-varieties
mmg<-subset(data,tv2=="G",select=maxl:tv2)
mmml<-subset(data,tv2=="Ml",select=maxl:tv2)
mmlf<-subset(data,tv2=="LF",select=maxl:tv2)
mmbd<-subset(data,tv2=="Bd",select=maxl:tv2)
mmmn<-subset(data,tv2=="Mn",select=maxl:tv2)
mmcm<-subset(data,tv2=="Cm",select=maxl:tv2)
mmcma<-subset(data,tv2=="CmA",select=maxl:tv2)
mmcmb<-subset(data,tv2=="CmB",select=maxl:tv2)
```

##### Maximum/minimum for _variety Gary_

```{r, maxmin mmg, echo=TRUE}
# identify maximum/minimum metrics for variety Gary

# max length (mm)
max(mmg$maxl)
# min length (mm)
min(mmg$maxl)
# max width (mm)
max(mmg$maxw)
# min width (mm)
min(mmg$maxw)
# max thickness (mm)
max(mmg$maxth)
# min thickness (mm)
min(mmg$maxth)
# max stem length (mm)
max(mmg$maxstl)
# min stem length (mm)
min(mmg$maxstl)
# max stem width (mm)
max(mmg$maxstw)
# min stem width (mm)
min(mmg$maxstw)
```

#### Maximum/minimum for _variety Malvern_

```{r, maxmin mmml, echo=TRUE}
# identify maximum/minimum metrics for variety Malvern

# max length (mm)
max(mmml$maxl)
# min length (mm)
min(mmml$maxl)
# max width (mm)
max(mmml$maxw)
# min width (mm)
min(mmml$maxw)
# max thickness (mm)
max(mmml$maxth)
# min thickness (mm)
min(mmml$maxth)
# max stem length (mm)
max(mmml$maxstl)
# min stem length (mm)
min(mmml$maxstl)
# max stem width (mm)
max(mmml$maxstw)
# min stem width (mm)
min(mmml$maxstw)
```

#### Maximum/minimum for _variety Le Flore_

```{r, maxmin mmlf, echo=TRUE}
# identify maximum/minimum metrics for variety Le Flore

# max length (mm)
max(mmlf$maxl)
# min length (mm)
min(mmlf$maxl)
# max width (mm)
max(mmlf$maxw)
# min width (mm)
min(mmlf$maxw)
# max thickness (mm)
max(mmlf$maxth)
# min thickness (mm)
min(mmlf$maxth)
# max stem length (mm)
max(mmlf$maxstl)
# min stem length (mm)
min(mmlf$maxstl)
# max stem width (mm)
max(mmlf$maxstw)
# min stem width (mm)
min(mmlf$maxstw)
```

#### Maximum/minimum for _variety Bodcaw_

```{r, maxmin mmbd, echo=TRUE}
# identify maximum/minimum metrics for variety Bodcaw

# max length (mm)
max(mmbd$maxl)
# min length (mm)
min(mmbd$maxl)
# max width (mm)
max(mmbd$maxw)
# min width (mm)
min(mmbd$maxw)
# max thickness (mm)
max(mmbd$maxth)
# min thickness (mm)
min(mmbd$maxth)
# max stem length (mm)
max(mmbd$maxstl)
# min stem length (mm)
min(mmbd$maxstl)
# max stem width (mm)
max(mmbd$maxstw)
# min stem width (mm)
min(mmbd$maxstw)
```

#### Maximum/minimum for _variety Manice_

```{r, maxmin mmmn, echo=TRUE}
# identify maximum/minimum metrics for variety Manice

# max length (mm)
max(mmmn$maxl)
# min length (mm)
min(mmmn$maxl)
# max width (mm)
max(mmmn$maxw)
# min width (mm)
min(mmmn$maxw)
# max thickness (mm)
max(mmmn$maxth)
# min thickness (mm)
min(mmmn$maxth)
# max stem length (mm)
max(mmmn$maxstl)
# min stem length (mm)
min(mmmn$maxstl)
# max stem width (mm)
max(mmmn$maxstw)
# min stem width (mm)
min(mmmn$maxstw)
```

#### Maximum/minimum for _variety Camden_

```{r, maxmin mmcm, echo=TRUE}
# identify maximum/minimum metrics for variety Camden

# max length (mm)
max(mmcm$maxl)
# min length (mm)
min(mmcm$maxl)
# max width (mm)
max(mmcm$maxw)
# min width (mm)
min(mmcm$maxw)
# max thickness (mm)
max(mmcm$maxth)
# min thickness (mm)
min(mmcm$maxth)
# max stem length (mm)
max(mmcm$maxstl)
# min stem length (mm)
min(mmcm$maxstl)
# max stem width (mm)
max(mmcm$maxstw)
# min stem width (mm)
min(mmcm$maxstw)
```

#### Maximum/minimum for _variety Camden subgroup A_

```{r, maxmin mmcma, echo=TRUE}
# identify maximum/minimum metrics for variety Camden subgroup A

# max length (mm)
max(mmcma$maxl)
# min length (mm)
min(mmcma$maxl)
# max width (mm)
max(mmcma$maxw)
# min width (mm)
min(mmcma$maxw)
# max thickness (mm)
max(mmcma$maxth)
# min thickness (mm)
min(mmcma$maxth)
# max stem length (mm)
max(mmcma$maxstl)
# min stem length (mm)
min(mmcma$maxstl)
# max stem width (mm)
max(mmcma$maxstw)
# min stem width (mm)
min(mmcma$maxstw)
```

#### Maximum/minimum for _variety Camden subgroup B_

```{r, maxmin mmcmb, echo=TRUE}
# identify maximum/minimum metrics for variety Camden subgroup B

# max length (mm)
max(mmcmb$maxl)
# min length (mm)
min(mmcmb$maxl)
# max width (mm)
max(mmcmb$maxw)
# min width (mm)
min(mmcmb$maxw)
# max thickness (mm)
max(mmcmb$maxth)
# min thickness (mm)
min(mmcmb$maxth)
# max stem length (mm)
max(mmcmb$maxstl)
# min stem length (mm)
min(mmcmb$maxstl)
# max stem width (mm)
max(mmcmb$maxstw)
# min stem width (mm)
min(mmcmb$maxstw)
```

### Boxplots for `site` by Schambach's [-@RN3132] type-varieties for Gary dart points from Cooper

```{r boxplot c tv2, out.width = "100%", dpi = 200, echo=TRUE}
# subset cooper data
cprmxl<-subset(data,Site=="Cooper",select=maxl:tv2)
# boxplot of maximum length
cprmaxl<-ggplot(cprmxl,aes(x=tv2,y=maxl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
cprmaxw<-ggplot(cprmxl,aes(x=tv2,y=maxw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxth<-ggplot(cprmxl,aes(x=tv2,y=maxth,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
cprmaxstl<-ggplot(cprmxl,aes(x=tv2,y=maxstl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
cprmaxstw<-ggplot(cprmxl,aes(x=tv2,y=maxstw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
cprfigure<-ggarrange(cprmaxl,cprmaxw,cprmaxth,cprmaxstl,cprmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
cprfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Cooper site."
```

### Boxplots for `site` by Schambach's [-@RN3132] type-varieties for Gary dart points from Means

```{r boxplot m tv2, out.width = "100%", dpi = 200, echo=TRUE}
# subset means data
mnsmxl<-subset(data,Site=="Means",select=maxl:tv2)
# boxplot of maximum length
mnsmaxl<-ggplot(mnsmxl,aes(x=tv2,y=maxl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
mnsmaxw<-ggplot(mnsmxl,aes(x=tv2,y=maxw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxth<-ggplot(mnsmxl,aes(x=tv2,y=maxth,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
mnsmaxstl<-ggplot(mnsmxl,aes(x=tv2,y=maxstl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
mnsmaxstw<-ggplot(mnsmxl,aes(x=tv2,y=maxstw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
mnsfigure<-ggarrange(mnsmaxl,mnsmaxw,mnsmaxth,mnsmaxstl,mnsmaxstw,
                     labels = c("a","b","c","d","e"),
                     ncol = 3, nrow = 2)
mnsfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Means site."
```

### Boxplots for `site` by Schambach's [-@RN3132] type-varieties for Gary dart points from Poverty Point

```{r boxplot pp tv2, out.width = "100%", dpi = 200, echo=TRUE}
# subset poverty point data
pvptmxl<-subset(data,Site=="Pov Pt",select=maxl:tv2)
# boxplot of maximum length
pvptmaxl<-ggplot(pvptmxl,aes(x=tv2,y=maxl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum width
pvptmaxw<-ggplot(pvptmxl,aes(x=tv2,y=maxw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxth<-ggplot(pvptmxl,aes(x=tv2,y=maxth,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum stem length
pvptmaxstl<-ggplot(pvptmxl,aes(x=tv2,y=maxstl,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# boxplot of maximum thickness
pvptmaxstw<-ggplot(pvptmxl,aes(x=tv2,y=maxstw,color=tv2)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3)+
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
# render figure
ppfigure<-ggarrange(pvptmaxl,pvptmaxw,pvptmaxth,pvptmaxstl,pvptmaxstw,
                    labels = c("a","b","c","d","e"),
                    ncol = 3, nrow = 2)
ppfigure
fig.cap = "Boxplots for maximum length, width, thickness, stem length, and stem width for Gary dart points from the Poverty Point site."
```

### Principal Components Analysis for Schambach's [-@RN3132] type-varieties at all sites

```{r pca tv2, out.width = "100%", dpi = 200, echo=TRUE}
#pca
df<-data[c(2:6)]
tv2pca<-autoplot(prcomp(df),data = data, colour = 'tv2',
                 loadings = TRUE, loadings.colour = 'blue',
                 loadings.label = TRUE,loadings.label.size = 3, 
                 frame = TRUE)
#render figure
tv2pca
fig.cap = "PCA by Gary type-varieties reported by Schambach (1998)."
```

### Analyses of Variance (ANOVA) for linear variables ~ Schambach's [-@RN3132] type-varieties

```{r anova tv2, echo=TRUE}
# anova = maximum length ~ tv2
t2ml<-lm.rrpp(maxl ~ tv2, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t2ml)
# anova = maximum width ~ tv2
t2mw<-lm.rrpp(maxw ~ tv2, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t2mw)
# anova = maximum thickness ~ tv2
t2mth<-lm.rrpp(maxth ~ tv2, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t2mth)
# anova = maximum stem length ~ tv2
t2mstl<-lm.rrpp(maxstl ~ tv2, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t2mstl)
# anova = maximum stem width ~ tv2
t2mstw<-lm.rrpp(maxstw ~ tv2, SS.type = "I",data = data,iter = 9999,print.progress = FALSE)
anova(t2mstw)
```


## Acknowledgments

Funding for this project was provided to RZS by the United States Forest Service, National Forests and Grasslands in Texas (15-PA-11081300-033 and 20-PA-11081300-074), and components of this analytical work flow were developed and funded by a Preservation Technology and Training grant (P14AP00138) to RZS from the National Center for Preservation Technology and Training.

I extend my gratitude to the Poverty Point World Heritage Site, and The University Museum in the J. William Fulbright College of Arts and Sciences at the University of Arkansas. Personal thanks go to Diana Greenlee and Mary C. Suter  for their help and guidance regarding permissions and access to the collections. Additional thanks to Jared S. Pebworth and Ernest Gann for their help with aggregating the Gary dart points, and with locating and sorting through the various associated site records for the collections from the Cooper and Means sites.

## References cited



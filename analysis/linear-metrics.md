Traditional linear metrics for Gary dart points
================
Robert Z. Selden, Jr.
February 22, 2020

### Introduction

This document includes supplemental materials for the article, “A
quantitative assessment of intraspecific morphological variation in Gary
dart points: An exercise in archaeological epistemology and heuristics.”
The traditional linear measures of maximum length, width, and thickness
were combined with stem length and stem width for this study. These
variables represent the full suite of metrics used to characterise Gary
points in previous studies, while also providing those data needed to
examine whether specimens included in this study may transcend the
threshold between dart and arrow points (Hildebrandt and King 2012).

Others have noted a gradual diminution in the size of Gary dart points
through time (Ford and Webb 1956; Schambach 1998; Densmore 2007), thus
it was necessary to begin by asking whether the size of those points
aggregated for this study may fall below the arbitrary 11.8 mm
dart-arrow index (DAI) threshold posited by Hildebrandt and King (2012).
The temporal span associated with morphologically-diagnostic Gary dart
points is thought to transcend the cultural shift from hunter-gatherer
to emergent horticulturalist, and overlaps temporally with the advent
and flourescence of the bow and arrow. The dart-arrow index was
calculated for all specimens of Gary dart points used in this study, and
none fell below the dart-arrow threshold posited by Hildebrandt and King
(2012). It is noteworthy that others (Erlandson, Watts, and Jew 2014)
have found that the DAI may not be universally applicable; however, the
DAI and the threshold identified by Hildebrandt and King (2012) is a
useful heuristic in this instance.

### Load packages for analysis

``` r
# install required analysis packages
#devtools::install_github("vqv/ggbiplot")
#devtools::install_github("mlcollyer/RRPP")
#devtools::install_github("tidyverse/ggplot2")
#devtools::install_github("kassambara/ggpubr")
# load libraries
library(ggbiplot)
```

    ## Loading required package: ggplot2

    ## Loading required package: plyr

    ## Loading required package: scales

    ## Loading required package: grid

``` r
library(RRPP)
library(ggplot2)
library(ggpubr)
```

    ## Loading required package: magrittr

    ## 
    ## Attaching package: 'ggpubr'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     mutate

### Set working directory, load data, and define variables

``` r
# set working directory
setwd(getwd())
ppgary<-read.csv("garymorphlm.csv",header = TRUE)
# define variables
maxl<-ppgary$maxl # maximum length
maxw<-ppgary$maxw # maximum width
maxth<-ppgary$maxth # maximum thickness
maxstl<-ppgary$maxstl # maximum stem length
maxstw<-ppgary$maxstw # maximum stem width
dai<-ppgary$dai # dart-arrow index value
```

### Gary type-variety linear metrics

``` r
# Ford, Phillips, and Haag (1955)
fphl<-data.frame(Name=c('var.GaryStemmed-length','var.TypicalGaryStemmed-length','var.BroadGaryStemmed-length','var.ThinGaryStemmed-length','var.SmallGaryStemmed-length'),
           Length=c(45,45,45,55,35), # in mm
           end=c(70,79,70,75,50) # in mm
)
fphlength<-ggplot(fphl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
fphw<-data.frame(Name=c('var.GaryStemmed-width','var.TypicalGaryStemmed-width','var.BroadGaryStemmed-width','var.ThinGaryStemmed-width','var.SmallGaryStemmed-width'),
           Width=c(22,22,32,30,20), # in mm
           end=c(31,31,48,36,30) # in mm
)
fphwidth<-ggplot(fphw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# render figure
fwebbfig<-ggarrange(fphlength,fphwidth,
                  labels = c("A","B"),
                  ncol = 1, nrow = 2)
annotate_figure(fwebbfig,
                top=text_grob("Gary type-variety linear metrics per Ford, Phillips, and Haag (1955)")
)
```

![](linear-metrics_files/figure-gfm/gantt-1.png)<!-- -->

``` r
# Ford and Webb (1956)
fwebbl<-data.frame(Name=c('var.GaryLarge-length','var.GaryMed/Typical-length','var.GarySmall-length'),
           Length=c(80,46,33), # in mm
           end=c(140,79,45) # in mm
)
fwebblength<-ggplot(fwebbl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
fwebbw<-data.frame(Name=c('var.GaryLarge-width','var.GaryMed/Typical-width','var.GarySmall-width'),
           Width=c(30,20,19), # in mm
           end=c(42,45,32) # in mm
)
fwebbwidth<-ggplot(fwebbw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# thickness values were not precisely reported for GaryLarge and GaryTypical, and are included here for reference only
fwebbth<-data.frame(Name=c('var.GaryLarge-width','var.GaryMed/Typical-width','var.GarySmall-width'),
           Thickness=c(13,9,5), # in mm
           end=c(13,10,10) # in mm
)
fwebbthickness<-ggplot(fwebbth,aes(x=Thickness,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# render figure
fwebbfig<-ggarrange(fwebblength,fwebbwidth,fwebbthickness,
                  labels = c("A","B","C"),
                  ncol = 1, nrow = 3)
annotate_figure(fwebbfig,
                top=text_grob("Gary type-variety linear metrics per Ford and Webb (1956)")
)
```

![](linear-metrics_files/figure-gfm/gantt-2.png)<!-- -->

``` r
# Schambach (1998)
schambl<-data.frame(Name=c('var.Gary-length','var.Malvern-length','var.LeFlore-length','var.Bodcaw-length','var.Manice-length','var.CamdenA-length','var.CamdenB-length'),
           Length=c(51,43,43,40,36,39,50), # in mm
           end=c(73,72,80,60,57,67,80) # in mm
)
length<-ggplot(schambl,aes(x=Length,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
schambw<-data.frame(Name=c('var.Gary-width','var.Malvern-width','var.LeFlore-width','var.Bodcaw-width','var.Manice-width','var.CamdenA-width','var.CamdenB-width'),
           Width=c(31,23,25,21,22,16,26), # in mm
           end=c(45,33,54,36,41,27,38) # in mm
)
width<-ggplot(schambw,aes(x=Width,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
schambth<-data.frame(Name=c('var.Gary-thickness','var.Malvern-thickness','var.LeFlore-thickness','var.Bodcaw-thickness','var.Manice-thickness','var.CamdenA-thickness','var.CamdenB-thickness'),
           Thickness=c(6,7,5,5,6,5,7), # in mm
           end=c(11,13,13,12,9,14,11) # in mm
)
thickness<-ggplot(schambth,aes(x=Thickness,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
schambstl<-data.frame(Name=c('var.Gary-stemlength','var.Malvern-stemlength','var.LeFlore-stemlength','var.Bodcaw-stemlength','var.Manice-stemlength','var.CamdenA-stemlength','var.CamdenB-stemlength'),
           StemLength=c(15,11,11,11,10,9,12), # in mm
           end=c(29,23,24,24,17,19,18) # in mm
)
stemlength<-ggplot(schambstl,aes(x=StemLength,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
schambstw<-data.frame(Name=c('var.Gary-stemlength','var.Malvern-stemlength','var.LeFlore-stemlength','var.Bodcaw-stemlength','var.Manice-stemlength','var.CamdenA-stemlength'), # var.CamdenB-stemlength not listed in text
           StemWidth=c(20,17,13,15,12,11), # in mm
           end=c(28,25,33,24,24,21) # in mm
)
stemwidth<-ggplot(schambstw,aes(x=StemWidth,xend=end,y=Name,yend=Name,color=Name)) +
  geom_segment(size=2) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# render figure
schambachfig<-ggarrange(length,width,thickness,stemlength,stemwidth,
                  labels = c("A","B","C","D","E"),
                  ncol = 2, nrow = 3)
annotate_figure(schambachfig,
                top=text_grob("Gary type-variety linear metrics per Schambach (1998)")
)
```

![](linear-metrics_files/figure-gfm/gantt-3.png)<!-- -->

### Functions used to assign Gary type-varieties

## Gary varieties proposed by Ford and Webb (1956)

The `type1` argument used within this and the subsequent section
articulates with three variants of the Gary type (`Large`, `Typical`,
and `Small`), which were assigned using those morphological criteria
first advanced by Ford, Phillips, and Haag (1955) at the Jaketown site,
and later refined by Ford and Webb (1956) at Poverty Point.

Each of the three type-varieties proposed by Ford and Webb (1956) was
based upon a suite of morphological criteria that can be systematically
replicated, and a function was used to assign each of the Gary points to
the correct type-variety. Gary type varieties range between 80 and 140
mm in maxl, and between 30 and 42 mm in maxw for *Gary Large*; between
46 to 79 mm in maxl, and between 20 and 45 mm in maxw for *Gary
Medium/Typical*; and between 33 to 45 mm in maxl, 19 to 32 mm in maxw,
and five to 10 mm in maxth for *Gary Small* (Ford and Webb 1956).

### Boxplots for `variable` by `type1`

``` r
# boxplot of maximum length ~ type
t1maxl<-ggplot(ppgary,aes(x=type1,y=maxl,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# boxplot of maximum width ~ type
t1maxw<-ggplot(ppgary,aes(x=type1,y=maxw,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# boxplot of maximum thickness ~ type
t1maxth<-ggplot(ppgary,aes(x=type1,y=maxth,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# boxplot of stem length ~ type
t1maxstl<-ggplot(ppgary,aes(x=type1,y=maxstl,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# boxplot of stem width ~ type
t1maxstw<-ggplot(ppgary,aes(x=type1,y=maxstw,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# boxplot of dart-arrow index ~ type
t1bdai<-ggplot(ppgary,aes(x=type1,y=dai,color=type1)) + geom_boxplot() +
  geom_dotplot(binaxis = 'y',stackdir = 'center',dotsize = 0.3) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")
# render figure
t1figure<-ggarrange(t1maxl,t1maxw,t1maxth,t1maxstl,t1maxstw,t1bdai,
                  labels = c("A","B","C","D","E","F"),
                  ncol = 3, nrow = 2)
```

    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bindot()` using `bins = 30`. Pick better value with `binwidth`.

``` r
t1figure
```

![](linear-metrics_files/figure-gfm/boxplot-1.png)<!-- -->

### Principal Components Analysis for `type1`

``` r
#pca
ppgary.pca<-prcomp(ppgary[c(2:6)],center = TRUE,scale. = TRUE)
summary(ppgary.pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4     PC5
    ## Standard deviation     1.5521 1.0832 0.8842 0.6925 0.39530
    ## Proportion of Variance 0.4818 0.2347 0.1564 0.0959 0.03125
    ## Cumulative Proportion  0.4818 0.7165 0.8729 0.9688 1.00000

``` r
t1pca<-ggbiplot(ppgary.pca,obs.scale = 1,var.scale = 1,ellipse = TRUE,groups = type1) +
  scale_color_brewer(name = "Type-Variety",palette = "Dark2") +
  theme(legend.position = "right")
#render figure
t1pca
```

![](linear-metrics_files/figure-gfm/pca-1.png)<!-- -->

### Analyses of Variance (ANOVA) for `variable` \~ `type1`

``` r
# anova = maximum length ~ type
t1ml<-lm.rrpp(maxl ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1ml)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df     SS      MS     Rsq     F      Z Pr(>F)    
    ## type1      2 2925.7 1462.87 0.49302 29.66 3.2263  1e-04 ***
    ## Residuals 61 3008.6   49.32 0.50698                        
    ## Total     63 5934.4                                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = maxl ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

``` r
# anova = maximum width ~ type
t1mw<-lm.rrpp(maxw ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1mw)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS     MS     Rsq      F      Z Pr(>F)  
    ## type1      2  123.57 61.787 0.10497 3.5769 1.4664  0.033 *
    ## Residuals 61 1053.69 17.274 0.89503                       
    ## Total     63 1177.26                                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = maxw ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

``` r
# anova = maximum thickness ~ type
t1mth<-lm.rrpp(maxth ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1mth)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS      MS     Rsq      F      Z Pr(>F)  
    ## type1      2  25.741 12.8704 0.09701 3.2768 1.3278 0.0464 *
    ## Residuals 61 239.594  3.9278 0.90299                       
    ## Total     63 265.335                                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = maxth ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

``` r
# anova = maximum stem length ~ type
t1mstl<-lm.rrpp(maxstl ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1mstl)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df     SS     MS     Rsq      F      Z Pr(>F)   
    ## type1      2 141.07 70.534 0.17904 6.6514 1.8069  0.002 **
    ## Residuals 61 646.87 10.604 0.82096                        
    ## Total     63 787.94                                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = maxstl ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

``` r
# anova = maximum stem width ~ type
t1mstw<-lm.rrpp(maxw ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1mstw)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS     MS     Rsq      F      Z Pr(>F)  
    ## type1      2  123.57 61.787 0.10497 3.5769 1.4664  0.033 *
    ## Residuals 61 1053.69 17.274 0.89503                       
    ## Total     63 1177.26                                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = maxw ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

``` r
# anova = dart-arrow index ~ type
t1dai<-lm.rrpp(dai ~ type1, SS.type = "I",data = ppgary,iter = 9999,print.progress = FALSE)
anova(t1dai)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS      MS    Rsq      F      Z Pr(>F)    
    ## type1      2  276.44 138.222 0.2432 9.8015 2.2253  2e-04 ***
    ## Residuals 61  860.23  14.102 0.7568                         
    ## Total     63 1136.68                                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Call: lm.rrpp(f1 = dai ~ type1, iter = 9999, SS.type = "I", data = ppgary,  
    ##     print.progress = FALSE)

## Acknowledgments

Funding for this project was provided to RZS by the United States Forest
Service, National Forests and Grasslands in Texas (15-PA-11081300-033
and 20-PA-11081300-074), and components of this analytical work flow
were developed and funded by a Preservation Technology and Training
grant (P14AP00138) to RZS from the National Center for Preservation
Technology and Training.

## References cited

<div id="refs" class="references">

<div id="ref-RN20874">

Densmore, Julie A. 2007. “A Detailed Analysis of the Variation in
Morphology of the Gary Dart Point.” *Lithic Technology* 32 (1): 7–16.
<https://doi.org/10.1080/01977261.2007.11721040>.

</div>

<div id="ref-RN20880">

Erlandson, Jon M., Jack L. Watts, and Nicholas P. Jew. 2014. “Darts,
Arrows, and Archaeologists: Distinguishing Dart and Arrow Points in the
Archaeological Record.” *American Antiquity* 79 (1): 162–69.
<https://doi.org/10.7183/0002-7316.79.1.162>.

</div>

<div id="ref-RN20899">

Ford, James A., Philip Phillips, and William G. Haag. 1955. *The
Jaketown Site in West-Central Mississippi*. Vol. 45: Part 1.
Anthropological Papers of the American Museum of Natural History. New
York: American Museum of Natural History.
<http://digitallibrary.amnh.org/handle/2246/90>.

</div>

<div id="ref-RN20898">

Ford, James A., and Clarence H. Webb. 1956. *Poverty Point, a Late
Archaic Site in Louisiana*. Vol. 46. Anthropological Papers of the
American Museum of Natural History. New York: American Museum of Natural
History. <http://digitallibrary.amnh.org/handle/2246/108>.

</div>

<div id="ref-RN20881">

Hildebrandt, William R., and Jerome H. King. 2012. “Distinguishing
Between Darts and Arrows in the Archaeological Record: Implications for
Technological Change in the American West.” *American Antiquity* 77 (4):
789–99. <https://doi.org/10.7183/0002-7316.77.4.789>.

</div>

<div id="ref-RN3132">

Schambach, Frank F. 1998. *Pre-Caddoan Cultures in the Trans-Mississippi
South: A Beginning Sequence*. Fayetteville: Research Series 53, Arkansas
Archeological Survey.

</div>

</div>

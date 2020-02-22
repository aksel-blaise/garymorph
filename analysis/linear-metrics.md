Traditional linear metrics for Gary dart points
================
Robert Z. Selden, Jr.
February 22, 2020

## Introduction

The traditional linear measures of maximum length, width, and thickness
were combined with stem length and stem width for this study. These
variables represent the full suite of metrics used to characterise Gary
points in previous studies, while also providing those data necessary to
examine whether those specimens included in this study may transcend the
threshold between dart and arrow points (Hildebrandt and King 2012).

Others have noted a gradual diminution in the size of Gary dart points
through time (Ford and Webb 1956; Schambach 1998; Densmore 2007), thus
it was necessary to begin by asking whether the size of those points
aggregated for this study may fall below the arbitrary 11.8 mm
dart-arrow index (DAI) threshold posited by Hildebrandt and King (2012).
The temporal span associated with morphologically-diagnostic Gary dart
points is thought to transcend the cultural shift from hunter-gatherer
to emergent horticulturalist, during the time thought to articulate with
the advent and flourescence of the bow and arrow. The dart-arrow index
was calculated for all specimens of Gary dart points used in this study,
and none fell below the dart-arrow threshold posited by Hildebrandt and
King (2012). It is noteworthy that others (Erlandson, Watts, and Jew
2014) have found that the DAI may not be universally applicable;
however, the DAI and the threshold identified by Hildebrandt and King
(2012) is a useful heuristic in this instance.

### Load packages

``` r
# load required analysis packages
library(devtools)
```

    ## Loading required package: usethis

``` r
install_github("vqv/ggbiplot")
```

    ## Skipping install of 'ggbiplot' from a github remote, the SHA1 (7325e880) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(ggbiplot)
```

    ## Loading required package: ggplot2

    ## Loading required package: plyr

    ## Loading required package: scales

    ## Loading required package: grid

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Principal Components Analysis

The `type` argument used in this and the subsequent section articulates
with three variants of the Gary type (`Large`, `Typical`, and `Small`),
which were assigned using those morphological criteria first advanced by
Ford, Phillips, and Haag (1955) at the Jaketown site, and later refined
by Ford and Webb (1956) at Poverty Point. The `type2` argument used in
this and the subsequent section articulates with a modified approach to
the classification system developed by Densmore (2007) that leverages
shoulder classes, which is detailed in at the end of the [landmarking
protocol](landmarking-protocol.md).

``` r
# set working directory
setwd(getwd())
ppgary<-read.csv("garymorphlm.csv",header = TRUE)
type<-ppgary$frdwbgrp
ppgary.pca<-prcomp(ppgary[c(2:6)],center = TRUE,scale. = TRUE)
summary(ppgary.pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4     PC5
    ## Standard deviation     1.5521 1.0832 0.8842 0.6925 0.39530
    ## Proportion of Variance 0.4818 0.2347 0.1564 0.0959 0.03125
    ## Cumulative Proportion  0.4818 0.7165 0.8729 0.9688 1.00000

``` r
#plot pca
ggbiplot(ppgary.pca,obs.scale = 1, var.scale = 1, ellipse = TRUE,groups = type)
```

![](linear-metrics_files/figure-gfm/pca-1.png)<!-- -->

### Multivariate Analysis of Variance

``` r
#compute manova
res.man<-manova(cbind(maxl,maxth,maxw,maxstl,maxstw) ~ type, data = ppgary)
summary(res.man)
```

    ##           Df  Pillai approx F num Df den Df    Pr(>F)    
    ## type       2 0.60362   5.0144     10    116 4.859e-06 ***
    ## Residuals 61                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#which differ
summary.aov(res.man)
```

    ##  Response maxl :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## type         2 2925.8 1462.87   29.66 1.005e-09 ***
    ## Residuals   61 3008.6   49.32                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response maxth :
    ##             Df  Sum Sq Mean Sq F value  Pr(>F)  
    ## type         2  25.741 12.8704  3.2768 0.04449 *
    ## Residuals   61 239.594  3.9278                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response maxw :
    ##             Df  Sum Sq Mean Sq F value  Pr(>F)  
    ## type         2  123.57  61.787  3.5769 0.03397 *
    ## Residuals   61 1053.69  17.274                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response maxstl :
    ##             Df Sum Sq Mean Sq F value   Pr(>F)   
    ## type         2 141.07  70.534  6.6514 0.002437 **
    ## Residuals   61 646.87  10.604                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response maxstw :
    ##             Df Sum Sq Mean Sq F value   Pr(>F)   
    ## type         2 147.67  73.833  7.1622 0.001607 **
    ## Residuals   61 628.83  10.309                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#end of code
```

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

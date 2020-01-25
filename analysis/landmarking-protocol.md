3D landmarking protocol for Gary dart points
================
Robert Z. Selden, Jr.
January 25, 2020

This document includes supplemental materials for the article, “A
Quantitative Assessment of Intraspecific Morphological Variation in Gary
Dart Points: An Exercise in Archaeological Epistemology and Heuristics.”
The study asks whether the morphologically-derived temporal framework
advanced by Densmore (2007) can be extended to Gary dart points
generally, and whether those specimens assigned to each temporal
category exhibit unique morphological characteristics. The landmarking
protocol developed for this project bears some visual similarities with
the configuration used in the [previous 2D study of basal
morphology](https://scholarworks.sfasu.edu/cgi/viewcontent.cgi?article=1280&context=crhr),
as well as other two-dimensional geometric morphometric analyses
(Buchanan and Collard 2010; Buchanan et al. 2011; Ragan and Buchanan
2018); however, it differs in some fundamental ways. The first and most
obvious difference is that this is a three-dimensional study. The second
is the method of landmark placement, where *Geomagic Design X (Build
Version 2019.0.2 \[Build Number: 78\])* was used to generate a spline
around the periphery of each dart point, and to populate the landmarks
and equidistant semilandmarks in a replicable manner using
mathematically-defined criteria.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="Gary dart point from 41TN20. \\label{figTN20}"
```

Inclusion of the third (z-dimension) provides a means of capturing those
morphological characteristics associated with axial twisting introduced
through the practice of bifacial beveling and/or resharpening. This
landmarking protocol represents an intermediate iteration between the
previous 2D analysis, and the forthcoming protocol that includes
semilandmarks placed on a series of equidistant cross-sections. The
cross-sections increase the coverage of semilandmarks across the mesh
topology, and provide for greater precision in the analysis of gross
morphology. The evolution of this landmarking protocol represents a
concerted effort to better comprehend the vagaries of morphological
similarities and differences among and between sites where Gary dart
points have been found. While true that some landmarking protocols can
be—and often are—recycled as new specimens are added, this particular
research programme endeavours to achieve greater analytical accuracy and
precision in each subsequent iteration.

### Generating the spline

Unlike the [previous
study](https://scholarworks.sfasu.edu/cgi/viewcontent.cgi?article=1280&context=crhr),
where the outline of Gary dart point bases were projected onto a 2D
plane, this effort is based upon a spline extracted from the surface
geometry of the 3D mesh. That spline is generated with the `extract
contour curves` command that detects and extracts 3D contour curves
along high-curvature areas of the mesh (i.e., edges). In
reverse-engineering, `extract contour curves` is regularly employed as
the first step in building a `patch network` used to generate a surface.
The extracted feature curve is rendered as a spline, and follows the
highest curvature contours around the sinuous edge of each point. The
remainder of the landmarking protocol is based upon this spline, which
is subsequently split at six mathematically-defined locations.

``` r
knitr::include_graphics('images/extractspline.png')
```

![](images/extractspline.png)<!-- -->

``` r
fig.cap="Spline extracted along the highest contours of the projectile. \\label{figspline}"
```

### Splitting the spline

*A few definitions are warranted before proceeding. `Reference
geometries` are used in the assistance of creating other features. These
include basic geometric entities, such as `planes`, `vectors`,
`coordinates`, `points`, and `polygons`. A `reference point` is a
virtual point and is used to mark a specific position on a model or in
3D space. A `reference plane` is a virtual plane that has a normal
direction and an infinite size. A `reference plane` is not a surface
body, and is used to create other features.*

The characteristic points and tangents developed for this landmarking
protocol were inspired by the work of Birkhoff (1933). The first
landmark (LM1) is placed at the horizontal tangent at the tip (proximal
end) of each Gary dart point. The second and third splits (LM2 and LM3)
enlist vertical tangents on either side of each point at the junction of
the blade and shoulder, and LM2 is always split on the right side of the
point when oriented in 3D space following the alignment output of
*auto3dgm*. The fourth and fifth landmarks (LM4 and LM5) occur at points
of highest curvature at the junction of the shoulder and base. The final
landmark (LM6) is placed by calculating the linear distance between LM4
and LM5, projecting a reference point equidistant between the landmarks,
and using that point to place the reference plane that splits the spline
at the location of LM6 (centre base).

#### Spline split at location of LM1 (tip/proximal point)

The `horizontal tangent` is calculated by drawing a horizontal line
above the tip of the Gary dart point using the tangent as a `common
constraint`, and the horizontal as the `independent constraint`. To
split the 3D spline at the location of the horizontal tangent, a
`reference point` was inserted at the location of the `tangent` in the
sketch (light blue point; below, left), followed by a `reference plane`
(in white; below, left and right) using the `pick point and normal axis`
function where the `reference point` (h-tangent) was used as the `pick
point`, and the `Right plane` as the `normal axis` (below, left). The 3D
spline was then cut at the location where the `reference plane`
intersects with the spline (below image, right).

``` r
knitr::include_graphics('images/lm1.png')
```

![](images/lm1.png)<!-- -->

``` r
fig.cap="Identify horizontal tangent, insert reference point and reference plane (left). Use reference plane to cut spline at the location of the horizontal tangent (right). \\label{figlm1}"
```

#### Spline split at locations of LM2 and LM3 (junction of blade and shoulder)

The `vertical tangent` is calculated by drawing a vertical line to the
left or right of the Gary dart point using the tangent as a `common
constraint`, and the vertical as the `independent constraint`. To split
the 3D spline at the location of the vertical tangent, a `reference
point` was inserted at the location of the `tangent` in the 2D sketch
(light blue point; below, left), followed by a `reference plane` (in
white; below, left and right) using the `pick point and normal axis`
function where the `reference point` (v-tangent) was used as the `pick
point`, and the `Top plane` as the `normal axis`. The 3D spline was cut
at the location where the `reference plane` intersected with the spline.
The alignment and orientation of each point was dictated by the
*auto3dgm* output, and the landmarking protocol follows the mesh
orientation in that figure, where LM2 was always placed on the right
side of the basal edge, and LM3 on the left.

#### Spline Split at locations of LM4 and LM5 (junction of shoulder and base)

The point of highest curvature between the shoulder and base was
calculated using the `curvature function` in the Accuracy Analyser. This
function displays the curvature flow as a continuous colour plot across
the area of the curve. In this instance, *curvature* is defined as the
amount by which a geometric shape deviates from being flat or straight
in the case of a line. The curvature is displayed in different colours
according to the local radius, and is calculated in only one direction
(U or V) along the curve. Using this tool, the two points of highest
curvature were located between the edges of the shoulder and base on
either side of each Gary dart point where the local radius measure was
largest.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="Identify points of hightest curvature (light blue) at left/right intersection of lateral and basal edges. \\label{figsplinesplitlr}"
```

#### Spline split at location of LM6 (centre base)

One additional landmark (LM4) was placed at the centre of the base. The
location of this landmark was identified by calculating the linear
distance between LM2 and LM3, and projecting a `reference point`
(ctrl-div; below) equidistant between the two. A `reference plane` was
added using the ctrl-div as the pick point, and the `Right plane` as the
`normal axis`. The spline was then split at the intersection of the
`reference plane` and the basal spline.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="Calculate linear distance between LM2 and LM3, insert reference plane coplanar to Right plane equidistant between LM2 and LM3, and use the reference plane to cut the spline.  \\label{figlm4}"
```

#### Final spline

Through the preceding protocol, the initial spline was split into four
discrete splines. These splines articulate with components of bifacial
morphology that can be compartmentalised in the subsequent analyses
(i.e., left/right \[directional\] asymmetry, blade/base morphological
integration, etc.). The primary analytical gain achieved through this
exercise is the requisite foundation needed to carry out replicable
analyses of Gahagan biface morphology in three dimensions, further
increasing the precision of the geometric morphometric analysis.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="Result of spline splits include four discrete splines, each articulating with a potential region of analytical interest. \\label{figsplinesplit-frbl}"
```

### Landmark and semilandmark placement

Landmarks 1-4 were placed at the location of each spline split (blue
points, below). Equidistant semilandmarks were then added to each of the
four splines; 20 between LM1 and LM2, five between LM2 and LM4, five
between LM4 and LM3, and 20 between LM3 and LM1.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="3D spline with landmarks (blue) and equidistant semilandmarks (white) applied, with top, right, and front planes. Semilandmarks are renumbered in post.  \\label{figlmslm-all}"
```

Superficially, this constellation of landmarks and semilandmarks appears
similar to those used in recent 2D studies. However, the rigourous
protocol used in the application of landmarks and semilandmarks aids in
capturing morphological variation that articulates with axial twisting
introduced by knappers through the practice of beveling. Thus, this
constellation of landmarks and semilandmarks provides for greater
precision in the geometric morphometric analysis, marking a substantive
advancement in those analytical protocols used to achieve a more
complete and holistic analysis of Gahagan biface morphology.

``` r
knitr::include_graphics('images/41TN20-base.png')
```

![](images/41TN20-base.png)<!-- -->

``` r
fig.cap="Mesh for Gahagan biface 545 from Gahagan Mound with landmarks and equidistant semilandmarks applied. \\label{figbevlm}"
```

### Acknowledgments

I extend my gratitude to Christian S. Hoggard and David K. Thulman for
their thoughtful comments and constructive criticisms on an earlier
draft of this landmarking protocol. The current iteration of the
landmarking protocol was developed using the `digit3DLand` package in R
(code available in this repository); however, the capacity to populate
replicable reference geometry across the sample in *Geomagic Design X*
made it a better option for the dynamic design process.

### References cited

<div id="refs" class="references">

<div id="ref-RN11786">

Birkhoff, George D. 1933. *Aesthetic Measure*. Cambridge: Harvard
University Press.

</div>

<div id="ref-RN1754">

Buchanan, Briggs, and Mark Collard. 2010. “A Geometric
Morphometrics-Based Assessment of Blade Shape Differences among
Paleoindian Projectile Point Types from Western North America.” *Journal
of Archaeological Science* 37 (2): 350–59.
<https://doi.org/10.1016/j.jas.2009.09.047>.

</div>

<div id="ref-RN1736">

Buchanan, Briggs, Mark Collard, Marcus J. Hamilton, and Michael J.
O’Brien. 2011. “Points and Prey: A Quantitative Test of the Hypothesis
that Prey Size Influences early Paleoindian Projectile Point Form.”
*Journal of Archaeological Science* 38 (4): 852–64.
<https://doi.org/10.1016/j.jas.2010.11.007>.

</div>

<div id="ref-RN20874">

Densmore, Julie A. 2007. “A Detailed Analysis of the Variation in
Morphology of the Gary Dart Point.” *Lithic Technology* 32 (1): 7–16.
<https://doi.org/10.1080/01977261.2007.11721040>.

</div>

<div id="ref-RN11731">

Ragan, Kathryn, and Briggs Buchanan. 2018. “Assessing Collector Bias: A
Geometric Morphometric Analysis of a Collection of Isolated Clovis
Points from the Midcontinent.” *Midcontinental Journal of Archaeology*
43 (2): 91–111. <https://doi.org/10.1080/01461109.2018.1426430>.

</div>

</div>

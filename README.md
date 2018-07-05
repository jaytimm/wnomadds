wnomadds
========

An R package that provides (very unofficial) add-on functionality to the `wnominate` package. Functions included in the package extract/output the coordinates and angles of roll call cutting lines, ie, the data underyling plots generated by `wnominate::plot.cutlines()` and `wnominate::plot.angles()`, respectively.

`wnomadds` analogue functions are dubbed `get_cutlines()` and `get_angles()`, and in their core functions are based exclusively on existing `wnominate` code, tweaked only to output data frames as opposed to base R plots. However, `wnomadds::get_cutlines()` includes the added functionality of allowing users to add arrows at the end of cutting lines in the direction of roll call Yea's. Depending on the analysis, visualizing roll call polarity can be super useful.

Functions ulimately facilitate more control over plot aesthetics within the `ggplot` framework

Table of Contents
-----------------

-   [Installation](#installation)
-   [Usage](#usage)
-   [Sources](#sources)

------------------------------------------------------------------------

Installation
------------

``` r
devtools::install_github("jaytimm/wnomadds")
```

Usage
-----

To demonstrate the functionality of `wnomadds`, we first build a `wnominate` model using roll call data from the **53rd Congress of the New Mexico State Legislature** made available via the `nmlegisdatr` package.

``` r
library(wnomadds)
library(nmlegisdatr)#devtools::install_github("jaytimm/nmlegisdatr")
library(tidyverse)
library(wnominate)
library(pscl)
```

------------------------------------------------------------------------

### Prepare data & run `wnominate` model

#### Reshape data for Senate roll calls

``` r
datFile <- nmlegisdatr::nml_rollcall  %>%
  filter(Chamber =='Senate' & !grepl('^LT', Representative)) %>%
  dplyr::select(Representative, Bill_Code, Rep_Vote) %>%
  spread(key= Bill_Code, value = Rep_Vote)
```

#### Create rollcall object

``` r
datRC <- pscl::rollcall(datFile [,-1], 
                        yea='Yea',
                        nay='Nay',
                        missing=c('Excused', 'Absent', 'Rec'),
                        vote.names = colnames(datFile)[2:ncol(datFile)], 
                        legis.names = datFile$Representative)
```

``` r
#Swap yea/nay/not voting to 1/6/9 --  easier downstream.
datRC$votes [datRC$votes == 'Excused' | datRC$votes == 'Absent' | datRC$votes == 'Rec'] <- 9
datRC$votes [datRC$votes == 'Yea'] <- 1
datRC$votes [datRC$votes == 'Nay'] <- 6 
```

#### Build model

``` r
resultd2 <- wnominate::wnominate (datRC, 
                       ubeta=15, 
                       uweights=0.5, 
                       dims=2, 
                       minvotes=20,
                       lop=0.025,trials=3, 
                       polarity=c(1,3),
                       verbose=FALSE)
## 
## Preparing to run W-NOMINATE...
## 
##  Checking data...
## 
##      All members meet minimum vote requirements.
## 
##      Votes dropped:
##      ... 516 of 740 total votes dropped.
## 
##  Running W-NOMINATE...
## 
##      Getting bill parameters...
##      Getting legislator coordinates...
##      Starting estimation of Beta...
##      Getting bill parameters...
##      Getting legislator coordinates...
##      Starting estimation of Beta...
##      Getting bill parameters...
##      Getting legislator coordinates...
##      Getting bill parameters...
##      Getting legislator coordinates...
##      Estimating weights...
##      Getting bill parameters...
##      Getting legislator coordinates...
##      Estimating weights...
##      Getting bill parameters...
##      Getting legislator coordinates...
## 
## 
## W-NOMINATE estimation completed successfully.
## W-NOMINATE took 5.43 seconds to execute.
```

``` r
row.names(resultd2$rollcalls) <- colnames(datFile)[2:ncol(datFile)]
```

#### Extract legislators coordinates from `nomObj` object

``` r
house_data <- resultd2$legislators %>% 
  bind_cols(nml_legislators %>% 
              filter(Chamber == 'Senate' & !grepl('^LT', Representative))) 
```

#### Plot two-dimensional model

``` r
house_data%>%
  ggplot(aes(x=coord1D, y=coord2D)) +
  geom_point(aes(color = Party),
             size= 3, 
             shape= 17) +
  nml_color_party()+ 
  theme(legend.position = 'bottom') +
  geom_text(aes(label=Representative), 
            size=2.5, 
            check_overlap = TRUE, 
            hjust = "inward",
            nudge_y = -0.03)+
  coord_fixed(ratio=1)  +
  labs(title="New Mexico's 53rd Congress - Upper Chamber",
       subtitle = 'Legislator coordinates') 
```

![](figure-markdown_github/unnamed-chunk-10-1.png)

------------------------------------------------------------------------

### `wnomadds::get_cutlines()`

The `get_cutlines()` function returns a data frame of cutting line coordinates. The function takes a `nomObj` object and a roll call object (from call to `pscl::rollcall`). If the `add_arrows` parameter is set to `TRUE`, additionally included in the data frame are coordinates of points perpendicular to cutting line ends in order to draw arrows in the direction of vote consensus. Arrow length can be specified by the `arrow_length` parameter.

``` r
with_cuts <- wnomadds::wnom_adds_get_cutlines(resultd2, 
                                              rollcall_obj = datRC, 
                                              add_arrows = TRUE,
                                              arrow_length = 0.05)
```

#### Sample output

Output effectively contains four sets of points:

-   x\_1, y\_1: cutting line start
-   x\_2, y\_2: cutting line end
-   x\_1a, y\_1a: point perpendicular to cutting line start, `arrow_length` away in the direction of max Yea's
-   x\_2a, y\_2a: point perpendicular to cutting line end, `arrow_length` away in the direction of max Yea's

``` r
head(with_cuts)
##      Bill_Code       x_1         y_1         x_2        y_2      x_1a
## 1:  R17_HB0001 0.7611899  0.64852911  0.66757317 -0.7445442 0.6915362
## 2: R17_HB00012 0.7611899  0.64852911  0.66757317 -0.7445442 0.6915362
## 3:  R17_HB0002 0.9781846  0.20773749 -0.21362665 -0.9769154 0.9189520
## 4:  R17_HB0063 0.7662343 -0.64256127  0.47683292  0.8789940 0.6901565
## 5:  R17_HB0080 0.9974388  0.07152501 -0.66671682 -0.7453111 0.9565970
## 6:  R17_HB0086 0.6445311 -0.76457809 -0.01234074  0.9999239 0.5563060
##          y_1a       x_2a       y_2a
## 1:  0.6532099  0.5979195 -0.7398634
## 2:  0.6532099  0.5979195 -0.7398634
## 3:  0.2673281 -0.2728593 -0.9173248
## 4: -0.6570313  0.4007552  0.8645239
## 5:  0.1547328 -0.7075586 -0.6621033
## 6: -0.7974217 -0.1005658  0.9670803
```

#### Plot legislators coordinates & cutting lines with arrows indicating polarity

The four sets of points included in the output of `wnomadds::get_angles` can be used to create three line segments via `geom_plot`: cutting start to cutting end, cutting start to opposite arrow, and cutting end to opposite arrow.

``` r
ggplot () + 
  nml_color_party() +
  theme(legend.position = 'bottom') +
  geom_point(data=house_data, 
               aes(x=coord1D, y=coord2D,color = Party),
               size= 3, 
               shape= 17) +
  geom_segment(data=with_cuts, 
               aes(x = x_1, y = y_1, xend = x_2, yend = y_2)) + #cutting start to end
  geom_segment(data=with_cuts, 
               aes(x = x_2, y = y_2, xend = x_2a, yend = y_2a), #cutting end to opposite arrow
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(data=with_cuts, 
               aes(x = x_1, y = y_1, xend = x_1a, yend = y_1a), #cutting start to opposite arrow
               arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=with_cuts, 
               aes(x = x_1a, y = y_1a, label = Bill_Code), 
               size=2.5, 
               nudge_y = 0.03,
               check_overlap = TRUE) +
  coord_fixed(ratio=1) + 
  labs(title="New Mexico's 53rd Congress - Upper Chamber",
       subtitle = 'Cutting lines & legislator coordinates')
```

![](figure-markdown_github/unnamed-chunk-13-1.png)

#### Select single cutting line

``` r
select_cuts <- c('R18_SB0018')

sub <- nmlegisdatr::nml_rollcall %>%
  filter(Bill_Code %in% select_cuts) %>%
  inner_join(house_data)

cut_sub <- subset(with_cuts, Bill_Code %in% select_cuts)
```

#### Plot single cutting line

``` r
sub %>%
ggplot(aes(x=coord1D, y=coord2D)) +
  geom_point(aes(color = Party_Vote, shape= Party_Vote, fill = Party_Vote),
             size= 2.5) +
  nmlegisdatr::nml_color_vote() +
  nmlegisdatr::nml_fill_vote() +
  nmlegisdatr::nml_shape_vote()+
  theme(legend.position = 'bottom') +
  geom_text(aes(label=Representative), 
            size=2.5, 
            check_overlap = TRUE, 
            hjust = "inward",
            nudge_y = -0.03)+
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_2, yend = y_2)) +
  geom_segment(data=cut_sub, 
               aes(x = x_2, y = y_2, xend = x_2a, yend = y_2a), 
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_1a, yend = y_1a), 
               arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=cut_sub, 
               aes(x = x_1a, y = y_1a, label = Bill_Code), 
               size=2.5, 
               nudge_y = 0.03,
               check_overlap = TRUE) +
  coord_equal(ratio=1) +
  labs(title="New Mexico's 53rd Congress - Upper Chamber",
       subtitle = '2nd Regular Session - Senate Bill 18') 
```

![](figure-markdown_github/unnamed-chunk-15-1.png)

#### Select multiple cutting lines

``` r
#select_cuts <- with_cuts$Bill_Code[112:123]
select_cuts <- c('R18_HB0129', 'R18_SB0018', 'R18_SB0176', 'R18_SM023',
                 'R18_SB0040', 'S17_HB0002', 'R18_SM003', 'R17_SB0188', 
                 'R18_HB0079', 'R17_HB0442', 'R17_SB0158', 'R17_SB0140')

sub <- nmlegisdatr::nml_rollcall %>%
  filter(Bill_Code %in% select_cuts) %>%
  inner_join(house_data)

cut_sub <- subset(with_cuts, Bill_Code %in% select_cuts)
```

#### Facet plot of multiple cutting lines

``` r
sub %>%
ggplot(aes(x=coord1D, y=coord2D)) +
  geom_point(aes(color = Party_Vote, shape= Party_Vote, fill = Party_Vote),
             size= 1.5) +
  nmlegisdatr::nml_color_vote() +
  nmlegisdatr::nml_fill_vote() +
  nmlegisdatr::nml_shape_vote()+
  theme(legend.position = 'bottom') +
  geom_text(aes(label=Representative), 
            size=1.5, 
            check_overlap = TRUE, 
            hjust = "inward",
            nudge_y = -0.03)+
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_2, yend = y_2)) +
  geom_segment(data=cut_sub, 
               aes(x = x_2, y = y_2, xend = x_2a, yend = y_2a), 
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_1a, yend = y_1a), 
               arrow = arrow(length = unit(0.2,"cm")))+
  coord_equal(ratio=1)+
  labs(title="New Mexico's 53rd Congress - Upper Chamber",
       subtitle = 'Selected roll calls & cutting lines') +
  facet_wrap(~Bill_Code, ncol = 4)
```

![](figure-markdown_github/unnamed-chunk-17-1.png)

------------------------------------------------------------------------

### `wnomadds::get_angles()`

Cutting line angles can be extracted from a `nomObj` object using `wnomadds::get_angles()`. Output is a simple data frame.

``` r
angles <- wnomadds::wnom_adds_get_angles(resultd2)
head(angles)
##     Bill_Code     angle
## 1  R17_HB0001  86.15542
## 2 R17_HB00012  86.15542
## 3  R17_HB0002  44.82741
## 4  R17_HB0063 100.76908
## 5  R17_HB0080  26.14371
## 6  R17_HB0086 110.41881
```

``` r
library(wnomadds)#devtools::install_github("jaytimm/wnomadds")
library(nmlegisdatr)#devtools::install_github("jaytimm/nmlegisdatr")
```

``` r
library(tidyverse)
library(wnominate)
library(pscl)
```

``` r
datFile <- nmlegisdatr::nml_rollcall  %>%
  filter(Chamber =='House' & !grepl('^LT', Representative)) %>%
  dplyr::select(Representative, Bill_ID, Rep_Vote) %>%
  spread(key= Bill_ID, value = Rep_Vote)
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

``` r
datRC <- pscl::rollcall(datFile [,-1], 
                        yea='Yea',
                        nay='Nay',
                        missing=c('Excused', 'Absent', 'Rec'),
                        vote.names = colnames(datFile)[2:ncol(datFile)], 
                        legis.names = datFile$Representative)
```

Swap yea/nay to 1, 6, 9

``` r
datRC$votes [datRC$votes == 'Excused' | datRC$votes == 'Absent' | datRC$votes == 'Rec'] <- 9
datRC$votes [datRC$votes == 'Yea'] <- 1
datRC$votes [datRC$votes == 'Nay'] <- 6
```

``` r
resultd2 <- wnominate::wnominate (datRC, 
                       ubeta=15, 
                       uweights=0.5, 
                       dims=2, 
                       minvotes=20,
                       lop=0.025,trials=3, 
                       polarity=c(1,1),
                       verbose=FALSE)
## 
## Preparing to run W-NOMINATE...
## 
##  Checking data...
## 
##      All members meet minimum vote requirements.
## 
##      Votes dropped:
##      ... 234 of 290 total votes dropped.
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
## W-NOMINATE took 2.49 seconds to execute.
```

``` r
row.names(resultd2$rollcalls) <- colnames(datFile)[2:ncol(datFile)]
```

Extract legislators from `nom` object. And add legislator details from `nmlegisdatr`.

``` r
house_data <- resultd2$legislators %>% #May need to remove NAs
  bind_cols(nml_legislators %>% 
              filter(Chamber == 'House' & !grepl('^LT', Representative))) 
```

``` r
library(ggthemes)

house_data%>%
  ggplot(aes(x=coord1D, y=coord2D)) +
  geom_point(aes(color = Party),
             size= 2.5, 
             shape= 17) +
  scale_colour_stata() + 
  #theme_fivethirtyeight() +
  theme(legend.position = 'bottom', 
        plot.title = element_text(size=13), 
        axis.title = element_text(size=10)) +
  geom_text(aes(label=Representative), 
            size=2.5, 
            check_overlap = TRUE, 
            hjust = 0, 
            nudge_x = 0.03)+
  labs(title="New Mexico 53rd House Roll Call - 2nd Session") +
  coord_equal(ratio=1) 
```

![](figure-markdown_github/unnamed-chunk-10-1.png)

Demonstrate how to extract cutting lines using `wnom_adds`.

``` r
with_cuts <- wnomadds::wnom_adds_get_cutlines(resultd2, rollcall_obj = datRC)
## Warning in FUN(newX[, i], ...): Couldn't solve for points on the unit circle!

## Warning in FUN(newX[, i], ...): Couldn't solve for points on the unit circle!
#Suppress warnings
```

``` r
head(with_cuts)
##          Bill_ID        x_1         x_2        y_1        y_2 pols
## 1:        HB0002  0.8715717  0.67587271 -0.4902681  0.7370184   -1
## 2: HB0002HCONCUR  0.9903836 -0.47974198 -0.1383488  0.8774096   -1
## 3:        HB0003  0.8715717  0.67587271 -0.4902681  0.7370184   -1
## 4:        HB0026 -0.3645179 -0.97765640 -0.9311964 -0.2102093    1
## 5:        HB0027 -0.3375543 -0.35960803 -0.9413061  0.9331035    1
## 6:        HB0032  0.3891161 -0.01486458  0.9211887 -0.9998895   -1
##          x_1a        y_1a       x_2a       y_2a
## 1:  0.9329360 -0.48048319  0.7372370  0.7468033
## 2:  1.0411715 -0.06484255 -0.4289541  0.9509159
## 3:  0.9329360 -0.48048319  0.7372370  0.7468033
## 4: -0.4005673 -0.96185329 -1.0137057 -0.2408663
## 5: -0.4312748 -0.94240875 -0.4533285  0.9320008
## 6:  0.2930622  0.94138776 -0.1109185 -0.9796905
```

Plot legislators with cutting lines.

``` r
ggplot () + 
  scale_colour_stata() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size=13),
        legend.position = 'bottom') +
  geom_point(data=house_data, 
               aes(x=coord1D, y=coord2D,color = Party),
               size= 2.5, 
               shape= 17) +
  geom_segment(data=with_cuts, 
               aes(x = x_1, y = y_1, xend = x_2, yend = y_2)) +
  geom_segment(data=with_cuts, 
               aes(x = x_2, y = y_2, xend = x_2a, yend = y_2a), 
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(data=with_cuts, 
               aes(x = x_1, y = y_1, xend = x_1a, yend = y_1a), 
               arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=with_cuts, 
               aes(x = x_1a, y = y_1a, label = Bill_ID), 
               size=2.5, 
               nudge_y = 0.03,
               check_overlap = TRUE) +
  coord_fixed(ratio=1) + 
  labs(title="Cutting lines for New Mexico's 53rd Lower Chamber - 2nd Session")
```

![](figure-markdown_github/unnamed-chunk-13-1.png)

Cutting line selections.

``` r
select_cuts <- c("HM054")

sub <- nmlegisdatr::nml_rollcall %>%
  filter(Bill_ID %in% select_cuts) %>%
  inner_join(house_data)
## Joining, by = c("Chamber", "Representative")

cut_sub <- subset(with_cuts, Bill_ID %in% select_cuts)
```

``` r
sub %>%
ggplot(aes(x=coord1D, y=coord2D)) +
  geom_point(aes(color = Party_Vote, shape= Party_Vote, fill = Party_Vote),
             size= 2.5) +
  nmlegisdatr::nml_color_vote() +
  nmlegisdatr::nml_fill_vote() +
  nmlegisdatr::nml_shape_vote()+
  theme(legend.position = 'bottom', 
        plot.title = element_text(size=13), 
        axis.title = element_text(size=10)) +
  geom_text(aes(label=Representative), 
            size=2.5, 
            check_overlap = TRUE, 
            hjust = 0, 
            nudge_x = 0.03)+
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_2, yend = y_2)) +
  geom_segment(data=cut_sub, 
               aes(x = x_2, y = y_2, xend = x_2a, yend = y_2a), 
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(data=cut_sub, 
               aes(x = x_1, y = y_1, xend = x_1a, yend = y_1a), 
               arrow = arrow(length = unit(0.2,"cm")))+
  geom_text(data=cut_sub, 
               aes(x = x_1a, y = y_1a, label = Bill_ID), 
               size=2.5, 
               nudge_y = 0.03,
               check_overlap = TRUE) +
  labs(title="New Mexico 53rd House Roll Call - 2nd Session") +
  coord_equal(ratio=1) 
```

![](figure-markdown_github/unnamed-chunk-15-1.png)

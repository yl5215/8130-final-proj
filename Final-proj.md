Final project
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(patchwork)
library(gt)
library(leaps)
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(readxl)
library(patchwork)
library(olsrr)
```

    ## 
    ## Attaching package: 'olsrr'
    ## 
    ## The following object is masked from 'package:datasets':
    ## 
    ##     rivers

``` r
body_density_df = read_excel("data/body_density_data.xlsx") %>%
  rename(outcome = bodyfat_brozek) %>% 
  select(-bodyfat_siri & -body_density)
head(body_density_df)
```

    ## # A tibble: 6 × 15
    ##      id outcome   age weight height  neck chest abdomen   hip thigh  knee ankle
    ##   <dbl>   <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1     1    12.6    23   154.   67.8  36.2  93.1    85.2  94.5  59    37.3  21.9
    ## 2     2     6.9    22   173.   72.2  38.5  93.6    83    98.7  58.7  37.3  23.4
    ## 3     3    24.6    22   154    66.2  34    95.8    87.9  99.2  59.6  38.9  24  
    ## 4     4    10.9    26   185.   72.2  37.4 102.     86.4 101.   60.1  37.3  22.8
    ## 5     5    27.8    24   184.   71.2  34.4  97.3   100   102.   63.2  42.2  24  
    ## 6     6    20.6    24   210.   74.8  39   104.     94.4 108.   66    42    25.6
    ## # … with 3 more variables: bicep <dbl>, forearm <dbl>, wrist <dbl>

Descriptive statistics:

``` r
body_density_df %>%
  gtsummary::tbl_summary() %>%
  gtsummary::bold_labels()
```

<div id="fgefxguvqv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fgefxguvqv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fgefxguvqv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fgefxguvqv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fgefxguvqv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fgefxguvqv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fgefxguvqv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fgefxguvqv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fgefxguvqv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fgefxguvqv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fgefxguvqv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fgefxguvqv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fgefxguvqv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#fgefxguvqv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fgefxguvqv .gt_from_md > :first-child {
  margin-top: 0;
}

#fgefxguvqv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fgefxguvqv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fgefxguvqv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#fgefxguvqv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fgefxguvqv .gt_row_group_first td {
  border-top-width: 2px;
}

#fgefxguvqv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fgefxguvqv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fgefxguvqv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fgefxguvqv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fgefxguvqv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fgefxguvqv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fgefxguvqv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fgefxguvqv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fgefxguvqv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fgefxguvqv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fgefxguvqv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fgefxguvqv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fgefxguvqv .gt_left {
  text-align: left;
}

#fgefxguvqv .gt_center {
  text-align: center;
}

#fgefxguvqv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fgefxguvqv .gt_font_normal {
  font-weight: normal;
}

#fgefxguvqv .gt_font_bold {
  font-weight: bold;
}

#fgefxguvqv .gt_font_italic {
  font-style: italic;
}

#fgefxguvqv .gt_super {
  font-size: 65%;
}

#fgefxguvqv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#fgefxguvqv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fgefxguvqv .gt_indent_1 {
  text-indent: 5px;
}

#fgefxguvqv .gt_indent_2 {
  text-indent: 10px;
}

#fgefxguvqv .gt_indent_3 {
  text-indent: 15px;
}

#fgefxguvqv .gt_indent_4 {
  text-indent: 20px;
}

#fgefxguvqv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col"><strong>N = 252</strong><sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left" style="font-weight: bold;">id</td>
<td class="gt_row gt_center">126 (64, 189)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">outcome</td>
<td class="gt_row gt_center">19 (13, 25)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">age</td>
<td class="gt_row gt_center">43 (36, 54)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">weight</td>
<td class="gt_row gt_center">176 (159, 197)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">height</td>
<td class="gt_row gt_center">70.00 (68.25, 72.25)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">neck</td>
<td class="gt_row gt_center">38.00 (36.40, 39.42)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">chest</td>
<td class="gt_row gt_center">100 (94, 105)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">abdomen</td>
<td class="gt_row gt_center">91 (85, 99)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">hip</td>
<td class="gt_row gt_center">99 (96, 104)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">thigh</td>
<td class="gt_row gt_center">59.0 (56.0, 62.3)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">knee</td>
<td class="gt_row gt_center">38.50 (36.98, 39.92)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">ankle</td>
<td class="gt_row gt_center">22.80 (22.00, 24.00)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">bicep</td>
<td class="gt_row gt_center">32.05 (30.20, 34.32)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">forearm</td>
<td class="gt_row gt_center">28.70 (27.30, 30.00)</td></tr>
    <tr><td class="gt_row gt_left" style="font-weight: bold;">wrist</td>
<td class="gt_row gt_center">18.30 (17.60, 18.80)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><sup class="gt_footnote_marks">1</sup> Median (IQR)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
summary(body_density_df)
```

    ##        id            outcome           age            weight     
    ##  Min.   :  1.00   Min.   : 0.00   Min.   :22.00   Min.   :118.5  
    ##  1st Qu.: 63.75   1st Qu.:12.80   1st Qu.:35.75   1st Qu.:159.0  
    ##  Median :126.50   Median :19.00   Median :43.00   Median :176.5  
    ##  Mean   :126.50   Mean   :18.94   Mean   :44.88   Mean   :178.9  
    ##  3rd Qu.:189.25   3rd Qu.:24.60   3rd Qu.:54.00   3rd Qu.:197.0  
    ##  Max.   :252.00   Max.   :45.10   Max.   :81.00   Max.   :363.1  
    ##      height           neck           chest           abdomen      
    ##  Min.   :64.00   Min.   :31.10   Min.   : 79.30   Min.   : 69.40  
    ##  1st Qu.:68.25   1st Qu.:36.40   1st Qu.: 94.35   1st Qu.: 84.58  
    ##  Median :70.00   Median :38.00   Median : 99.65   Median : 90.95  
    ##  Mean   :70.31   Mean   :37.99   Mean   :100.82   Mean   : 92.56  
    ##  3rd Qu.:72.25   3rd Qu.:39.42   3rd Qu.:105.38   3rd Qu.: 99.33  
    ##  Max.   :77.75   Max.   :51.20   Max.   :136.20   Max.   :148.10  
    ##       hip            thigh            knee           ankle          bicep      
    ##  Min.   : 85.0   Min.   :47.20   Min.   :33.00   Min.   :19.1   Min.   :24.80  
    ##  1st Qu.: 95.5   1st Qu.:56.00   1st Qu.:36.98   1st Qu.:22.0   1st Qu.:30.20  
    ##  Median : 99.3   Median :59.00   Median :38.50   Median :22.8   Median :32.05  
    ##  Mean   : 99.9   Mean   :59.41   Mean   :38.59   Mean   :23.1   Mean   :32.27  
    ##  3rd Qu.:103.5   3rd Qu.:62.35   3rd Qu.:39.92   3rd Qu.:24.0   3rd Qu.:34.33  
    ##  Max.   :147.7   Max.   :87.30   Max.   :49.10   Max.   :33.9   Max.   :45.00  
    ##     forearm          wrist      
    ##  Min.   :21.00   Min.   :15.80  
    ##  1st Qu.:27.30   1st Qu.:17.60  
    ##  Median :28.70   Median :18.30  
    ##  Mean   :28.66   Mean   :18.23  
    ##  3rd Qu.:30.00   3rd Qu.:18.80  
    ##  Max.   :34.90   Max.   :21.40

Exploratory plots

``` r
body_density_df %>%
  relocate(outcome) %>%
  ggpairs()
```

![](Final-proj_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Boxplot

``` r
hist1 = body_density_df %>% 
  ggplot(aes(x = age)) +
  geom_histogram()
hist2 = body_density_df %>% 
  ggplot(aes(x = weight)) +
  geom_histogram()
hist3 = body_density_df %>% 
  ggplot(aes(x = height)) +
  geom_histogram()

hist1 + hist2 + hist3
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-proj_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
hist4 = body_density_df %>% 
  ggplot(aes(x = neck)) +
  geom_histogram()
hist5 = body_density_df %>% 
  ggplot(aes(x = chest)) +
  geom_histogram()
hist6 = body_density_df %>% 
  ggplot(aes(x = abdomen)) +
  geom_histogram()

hist4 + hist5 + hist6
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-proj_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
hist7 = body_density_df %>% 
  ggplot(aes(x = hip)) +
  geom_histogram()
hist8 = body_density_df %>% 
  ggplot(aes(x = thigh)) +
  geom_histogram()
hist9 = body_density_df %>% 
  ggplot(aes(x = knee)) +
  geom_histogram()

hist7 + hist8 + hist9
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-proj_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
hist10 = body_density_df %>% 
  ggplot(aes(x = ankle)) +
  geom_histogram()
hist11 = body_density_df %>% 
  ggplot(aes(x = bicep)) +
  geom_histogram()
hist12 = body_density_df %>% 
  ggplot(aes(x = forearm)) +
  geom_histogram()

hist10 + hist11 + hist12
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-proj_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
hist13 = body_density_df %>% 
  ggplot(aes(x = wrist)) +
  geom_histogram()

hist13
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-proj_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

## Fitting a full model and checking diagnostic plots

``` r
full_model = lm(outcome ~ ., data = body_density_df)
summary(full_model)
```

    ## 
    ## Call:
    ## lm(formula = outcome ~ ., data = body_density_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.0409  -2.7156  -0.1523   2.7601   9.5656 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -18.291346  20.578647  -0.889  0.37498    
    ## id           -0.002652   0.003772  -0.703  0.48275    
    ## age           0.062977   0.030343   2.076  0.03902 *  
    ## weight       -0.087260   0.057382  -1.521  0.12967    
    ## height       -0.031670   0.165869  -0.191  0.84874    
    ## neck         -0.441342   0.218444  -2.020  0.04447 *  
    ## chest        -0.013458   0.095824  -0.140  0.88843    
    ## abdomen       0.889422   0.083732  10.622  < 2e-16 ***
    ## hip          -0.190632   0.135097  -1.411  0.15953    
    ## thigh         0.240269   0.135653   1.771  0.07781 .  
    ## knee         -0.002369   0.230352  -0.010  0.99180    
    ## ankle         0.155961   0.207110   0.753  0.45218    
    ## bicep         0.149300   0.160096   0.933  0.35199    
    ## forearm       0.435202   0.184820   2.355  0.01935 *  
    ## wrist        -1.516844   0.494546  -3.067  0.00241 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.996 on 237 degrees of freedom
    ## Multiple R-squared:  0.749,  Adjusted R-squared:  0.7342 
    ## F-statistic: 50.52 on 14 and 237 DF,  p-value: < 2.2e-16

``` r
par(mfrow = c(2,2))
plot(full_model)
```

![](Final-proj_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Checking VIF in full model

``` r
## ols_coll_diag(full_model)
```

The diagnostics plots look good. No need for Box-Cox transformation

## Stepwise Selection

``` r
##step(lm(outcome ~ .,data = body_density_df),direction = "both")
```

``` r
## fit_stepwise = lm(outcome ~ ., data = body_density_df)
## ols_step_both_p(fit_stepwise)
```

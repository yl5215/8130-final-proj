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
```

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

<div id="lbxbkmtqum" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lbxbkmtqum .gt_table {
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

#lbxbkmtqum .gt_heading {
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

#lbxbkmtqum .gt_title {
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

#lbxbkmtqum .gt_subtitle {
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

#lbxbkmtqum .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbxbkmtqum .gt_col_headings {
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

#lbxbkmtqum .gt_col_heading {
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

#lbxbkmtqum .gt_column_spanner_outer {
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

#lbxbkmtqum .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lbxbkmtqum .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lbxbkmtqum .gt_column_spanner {
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

#lbxbkmtqum .gt_group_heading {
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

#lbxbkmtqum .gt_empty_group_heading {
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

#lbxbkmtqum .gt_from_md > :first-child {
  margin-top: 0;
}

#lbxbkmtqum .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lbxbkmtqum .gt_row {
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

#lbxbkmtqum .gt_stub {
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

#lbxbkmtqum .gt_stub_row_group {
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

#lbxbkmtqum .gt_row_group_first td {
  border-top-width: 2px;
}

#lbxbkmtqum .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbxbkmtqum .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lbxbkmtqum .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lbxbkmtqum .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbxbkmtqum .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbxbkmtqum .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lbxbkmtqum .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lbxbkmtqum .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lbxbkmtqum .gt_footnotes {
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

#lbxbkmtqum .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbxbkmtqum .gt_sourcenotes {
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

#lbxbkmtqum .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lbxbkmtqum .gt_left {
  text-align: left;
}

#lbxbkmtqum .gt_center {
  text-align: center;
}

#lbxbkmtqum .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lbxbkmtqum .gt_font_normal {
  font-weight: normal;
}

#lbxbkmtqum .gt_font_bold {
  font-weight: bold;
}

#lbxbkmtqum .gt_font_italic {
  font-style: italic;
}

#lbxbkmtqum .gt_super {
  font-size: 65%;
}

#lbxbkmtqum .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#lbxbkmtqum .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lbxbkmtqum .gt_indent_1 {
  text-indent: 5px;
}

#lbxbkmtqum .gt_indent_2 {
  text-indent: 10px;
}

#lbxbkmtqum .gt_indent_3 {
  text-indent: 15px;
}

#lbxbkmtqum .gt_indent_4 {
  text-indent: 20px;
}

#lbxbkmtqum .gt_indent_5 {
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

The diagnostics plots look good. No need for Box-Cox transformation

## Forward selection

``` r
intercept_only <- lm(outcome ~ 1, data = body_density_df)
step(intercept_only, direction = "forward", scope = formula(full_model))
```

    ## Start:  AIC=1033.09
    ## outcome ~ 1
    ## 
    ##           Df Sum of Sq     RSS     AIC
    ## + abdomen  1    9984.1  5094.9  761.66
    ## + chest    1    7449.8  7629.3  863.40
    ## + hip      1    5903.4  9175.6  909.91
    ## + weight   1    5669.1  9409.9  916.26
    ## + thigh    1    4750.5 10328.5  939.74
    ## + knee     1    3888.1 11190.9  959.94
    ## + bicep    1    3665.4 11413.6  964.91
    ## + neck     1    3642.5 11436.5  965.41
    ## + forearm  1    1990.0 13089.0  999.43
    ## + wrist    1    1821.6 13257.4 1002.65
    ## + age      1    1260.9 13818.1 1013.08
    ## + ankle    1    1073.2 14005.8 1016.49
    ## + id       1     185.6 14893.4 1031.97
    ## <none>                 15079.0 1033.09
    ## + height   1       9.1 15069.9 1034.94
    ## 
    ## Step:  AIC=761.66
    ## outcome ~ abdomen
    ## 
    ##           Df Sum of Sq    RSS    AIC
    ## + weight   1    853.60 4241.3 717.45
    ## + wrist    1    601.95 4493.0 731.97
    ## + neck     1    521.22 4573.7 736.46
    ## + height   1    500.85 4594.1 737.58
    ## + hip      1    467.43 4627.5 739.41
    ## + knee     1    279.91 4815.0 749.42
    ## + ankle    1    197.47 4897.5 753.70
    ## + chest    1    167.55 4927.4 755.23
    ## + age      1    164.67 4930.3 755.38
    ## + thigh    1    142.97 4952.0 756.48
    ## + bicep    1    117.61 4977.3 757.77
    ## + forearm  1     43.24 5051.7 761.51
    ## <none>                 5094.9 761.66
    ## + id       1      2.17 5092.8 763.55
    ## 
    ## Step:  AIC=717.45
    ## outcome ~ abdomen + weight
    ## 
    ##           Df Sum of Sq    RSS    AIC
    ## + wrist    1   133.146 4108.2 711.41
    ## + thigh    1    74.090 4167.2 715.01
    ## + neck     1    73.386 4167.9 715.05
    ## + forearm  1    60.592 4180.7 715.82
    ## + bicep    1    52.085 4189.2 716.33
    ## <none>                 4241.3 717.45
    ## + id       1    11.097 4230.2 718.79
    ## + knee     1     6.356 4235.0 719.07
    ## + height   1     6.285 4235.0 719.07
    ## + age      1     2.416 4238.9 719.30
    ## + ankle    1     1.369 4240.0 719.37
    ## + chest    1     0.024 4241.3 719.45
    ## + hip      1     0.013 4241.3 719.45
    ## 
    ## Step:  AIC=711.41
    ## outcome ~ abdomen + weight + wrist
    ## 
    ##           Df Sum of Sq    RSS    AIC
    ## + forearm  1   113.872 3994.3 706.33
    ## + bicep    1    72.826 4035.4 708.90
    ## + thigh    1    38.053 4070.1 711.06
    ## <none>                 4108.2 711.41
    ## + neck     1    21.188 4087.0 712.11
    ## + age      1    15.435 4092.7 712.46
    ## + knee     1    14.604 4093.6 712.51
    ## + ankle    1    12.957 4095.2 712.61
    ## + hip      1     8.100 4100.1 712.91
    ## + height   1     5.114 4103.1 713.10
    ## + id       1     4.583 4103.6 713.13
    ## + chest    1     0.942 4107.2 713.35
    ## 
    ## Step:  AIC=706.33
    ## outcome ~ abdomen + weight + wrist + forearm
    ## 
    ##          Df Sum of Sq    RSS    AIC
    ## + neck    1    43.683 3950.6 705.55
    ## <none>                3994.3 706.33
    ## + age     1    29.483 3964.8 706.46
    ## + bicep   1    26.140 3968.2 706.67
    ## + thigh   1    25.904 3968.4 706.69
    ## + ankle   1    15.757 3978.6 707.33
    ## + knee    1    14.034 3980.3 707.44
    ## + id      1     3.831 3990.5 708.08
    ## + hip     1     3.071 3991.2 708.13
    ## + height  1     1.434 3992.9 708.24
    ## + chest   1     0.560 3993.8 708.29
    ## 
    ## Step:  AIC=705.55
    ## outcome ~ abdomen + weight + wrist + forearm + neck
    ## 
    ##          Df Sum of Sq    RSS    AIC
    ## + age     1    37.251 3913.4 705.17
    ## + bicep   1    35.937 3914.7 705.25
    ## <none>                3950.6 705.55
    ## + thigh   1    23.995 3926.6 706.02
    ## + hip     1     9.494 3941.1 706.95
    ## + ankle   1     9.297 3941.3 706.96
    ## + knee    1     6.780 3943.8 707.12
    ## + height  1     5.684 3944.9 707.19
    ## + id      1     3.023 3947.6 707.36
    ## + chest   1     0.000 3950.6 707.55
    ## 
    ## Step:  AIC=705.17
    ## outcome ~ abdomen + weight + wrist + forearm + neck + age
    ## 
    ##          Df Sum of Sq    RSS    AIC
    ## + thigh   1    60.163 3853.2 703.26
    ## + bicep   1    37.661 3875.7 704.73
    ## <none>                3913.4 705.17
    ## + ankle   1    12.670 3900.7 706.35
    ## + id      1    12.129 3901.2 706.38
    ## + height  1     7.835 3905.5 706.66
    ## + knee    1     4.067 3909.3 706.91
    ## + hip     1     3.050 3910.3 706.97
    ## + chest   1     0.826 3912.6 707.11
    ## 
    ## Step:  AIC=703.26
    ## outcome ~ abdomen + weight + wrist + forearm + neck + age + thigh
    ## 
    ##          Df Sum of Sq    RSS    AIC
    ## + hip     1    33.229 3820.0 703.08
    ## <none>                3853.2 703.26
    ## + bicep   1    18.816 3834.4 704.03
    ## + ankle   1    10.913 3842.3 704.55
    ## + id      1     6.728 3846.5 704.82
    ## + height  1     0.676 3852.5 705.22
    ## + chest   1     0.607 3852.6 705.22
    ## + knee    1     0.223 3853.0 705.25
    ## 
    ## Step:  AIC=703.08
    ## outcome ~ abdomen + weight + wrist + forearm + neck + age + thigh + 
    ##     hip
    ## 
    ##          Df Sum of Sq    RSS    AIC
    ## <none>                3820.0 703.08
    ## + bicep   1   14.9103 3805.1 704.09
    ## + id      1   10.9040 3809.1 704.36
    ## + ankle   1    9.9018 3810.1 704.43
    ## + height  1    2.5369 3817.4 704.91
    ## + knee    1    0.0583 3819.9 705.08
    ## + chest   1    0.0036 3820.0 705.08

    ## 
    ## Call:
    ## lm(formula = outcome ~ abdomen + weight + wrist + forearm + neck + 
    ##     age + thigh + hip, data = body_density_df)
    ## 
    ## Coefficients:
    ## (Intercept)      abdomen       weight        wrist      forearm         neck  
    ##   -20.06213      0.87721     -0.08414     -1.40487      0.48255     -0.43189  
    ##         age        thigh          hip  
    ##     0.05922      0.28644     -0.18641

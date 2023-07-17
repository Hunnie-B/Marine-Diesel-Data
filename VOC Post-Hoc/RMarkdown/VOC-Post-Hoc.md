---
title: "VOC Post-Hoc"
author: "BH"
date: "2023-05-23"
output: 
  html_document:
    keep_md: true
---





```r
library(rstatix)
```

```
## 
## Attaching package: 'rstatix'
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.2     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks rstatix::filter(), stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following object is masked from 'package:rstatix':
## 
##     make_clean_names
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(here)
```

```
## here() starts at /Users/blake/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CEOS Technician/R/VOC Post-Hoc
```



```r
first_test_data <- read_csv(here("data", "first_test.csv")) %>%
  clean_names()
```

```
## Rows: 168 Columns: 1
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (1): data
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
second_test_data <- read_csv(here("data", "second_test.csv")) %>%
  clean_names()
```

```
## Rows: 42 Columns: 4
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (4): 5ppm FY, 50ppm FN, 50ppm FY, 500ppm FY
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
third_test_data <- read_csv(here("data", "third_test.csv")) %>%
  clean_names()
```

```
## Rows: 168 Columns: 2
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): timepoint
## dbl (1): value
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```



```r
shapiro.test(first_test_data$data)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  first_test_data$data
## W = 0.62554, p-value < 2.2e-16
```



```r
kruskal.test(second_test_data)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  second_test_data
## Kruskal-Wallis chi-squared = 118.26, df = 3, p-value < 2.2e-16
```



```r
dunn_test_results <- dunn_test(third_test_data, value~timepoint, p.adjust.method = "bonferroni") %>%
  write_csv(here("Saved Files", "dunn_test_results.csv"))

dunn_test_results
```

```
## # A tibble: 66 × 9
##    .y.   group1       group2    n1    n2 statistic        p   p.adj p.adj.signif
##  * <chr> <chr>        <chr>  <int> <int>     <dbl>    <dbl>   <dbl> <chr>       
##  1 value T=0 500ppm … T=0 5…    14    14    -2.17  2.98e- 2 1   e+0 ns          
##  2 value T=0 500ppm … T=0 5…    14    14    -2.38  1.75e- 2 1   e+0 ns          
##  3 value T=0 500ppm … T=0 5…    14    14    -5.83  5.62e- 9 3.71e-7 ****        
##  4 value T=0 500ppm … T=13 …    14    14    -0.103 9.18e- 1 1   e+0 ns          
##  5 value T=0 500ppm … T=13 …    14    14    -2.69  7.23e- 3 4.77e-1 ns          
##  6 value T=0 500ppm … T=13 …    14    14    -2.81  5.00e- 3 3.30e-1 ns          
##  7 value T=0 500ppm … T=13 …    14    14    -6.28  3.29e-10 2.17e-8 ****        
##  8 value T=0 500ppm … T=E 5…    14    14    -0.508 6.12e- 1 1   e+0 ns          
##  9 value T=0 500ppm … T=E 5…    14    14    -3.17  1.51e- 3 9.98e-2 ns          
## 10 value T=0 500ppm … T=E 5…    14    14    -5.31  1.10e- 7 7.23e-6 ****        
## # ℹ 56 more rows
```


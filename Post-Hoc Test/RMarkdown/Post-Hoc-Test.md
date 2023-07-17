---
title: "Post-Hoc Test"
author: "BH"
date: "2023-05-22"
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
## here() starts at /Users/blake/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CEOS Technician/R/Post-Hoc Test
```



```r
first_test_data <- read_csv(here("data", "first_test.csv")) %>%
  clean_names()
```

```
## Rows: 408 Columns: 1
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
## Rows: 102 Columns: 4
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
## Rows: 408 Columns: 2
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
## W = 0.22604, p-value < 2.2e-16
```



```r
kruskal.test(second_test_data)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  second_test_data
## Kruskal-Wallis chi-squared = 71.299, df = 3, p-value = 2.25e-15
```



```r
dunn_test_results <- dunn_test(third_test_data, value~timepoint, p.adjust.method = "bonferroni") %>%
  write_csv(here("Saved Files", "dunn_test_results.csv"))

dunn_test_results
```

```
## # A tibble: 66 × 9
##    .y.   group1        group2    n1    n2 statistic       p   p.adj p.adj.signif
##  * <chr> <chr>         <chr>  <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
##  1 value T=0 500ppm FY T=0 5…    34    34    -0.460 6.45e-1 1       ns          
##  2 value T=0 500ppm FY T=0 5…    34    34    -0.719 4.72e-1 1       ns          
##  3 value T=0 500ppm FY T=0 5…    34    34    -3.40  6.77e-4 0.0447  *           
##  4 value T=0 500ppm FY T=13 …    34    34    -0.101 9.19e-1 1       ns          
##  5 value T=0 500ppm FY T=13 …    34    34    -1.19  2.36e-1 1       ns          
##  6 value T=0 500ppm FY T=13 …    34    34    -1.50  1.35e-1 1       ns          
##  7 value T=0 500ppm FY T=13 …    34    34    -4.17  3.07e-5 0.00203 **          
##  8 value T=0 500ppm FY T=E 5…    34    34     0.167 8.68e-1 1       ns          
##  9 value T=0 500ppm FY T=E 5…    34    34    -2.04  4.14e-2 1       ns          
## 10 value T=0 500ppm FY T=E 5…    34    34    -1.78  7.54e-2 1       ns          
## # ℹ 56 more rows
```


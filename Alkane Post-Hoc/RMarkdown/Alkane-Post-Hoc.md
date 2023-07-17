---
title: "Alkane Post-Hoc"
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
## here() starts at /Users/blake/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CEOS Technician/R/Alkane Post-Hoc
```



```r
first_test_data <- read_csv(here("data", "first_test.csv")) %>%
  clean_names()
```

```
## Rows: 324 Columns: 1
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
## Rows: 81 Columns: 4
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
## Rows: 324 Columns: 2
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
## W = 0.58432, p-value < 2.2e-16
```



```r
kruskal.test(second_test_data)
```

```
## 
## 	Kruskal-Wallis rank sum test
## 
## data:  second_test_data
## Kruskal-Wallis chi-squared = 112.45, df = 3, p-value < 2.2e-16
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
##  1 value T=0 500ppm FY T=0 5…    27    27    -2.70  7.03e-3 4.64e-1 ns          
##  2 value T=0 500ppm FY T=0 5…    27    27    -1.87  6.11e-2 1   e+0 ns          
##  3 value T=0 500ppm FY T=0 5…    27    27    -4.36  1.30e-5 8.59e-4 ***         
##  4 value T=0 500ppm FY T=13 …    27    27     0.307 7.59e-1 1   e+0 ns          
##  5 value T=0 500ppm FY T=13 …    27    27    -1.16  2.47e-1 1   e+0 ns          
##  6 value T=0 500ppm FY T=13 …    27    27    -1.63  1.03e-1 1   e+0 ns          
##  7 value T=0 500ppm FY T=13 …    27    27    -5.80  6.67e-9 4.40e-7 ****        
##  8 value T=0 500ppm FY T=E 5…    27    27     0.990 3.22e-1 1   e+0 ns          
##  9 value T=0 500ppm FY T=E 5…    27    27    -1.46  1.45e-1 1   e+0 ns          
## 10 value T=0 500ppm FY T=E 5…    27    27    -3.06  2.21e-3 1.46e-1 ns          
## # ℹ 56 more rows
```


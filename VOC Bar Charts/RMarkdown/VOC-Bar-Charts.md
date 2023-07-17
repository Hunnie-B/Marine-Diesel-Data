---
title: "VOC Bar Charts"
author: "BH"
date: "2023-04-30"
output: 
  html_document:
    keep_md: true
---





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
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
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
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(here)
```

```
## here() starts at /Users/blake/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CEOS Technician/R/VOC Bar Charts
```

```r
library(gt)
library(broom) 
library(fs)
library(viridis)
```

```
## Loading required package: viridisLite
```



```r
voc_data <- read_csv(here("data", "tidy_voc_data.csv")) %>% #loading in csv. file dataset
  pivot_longer(cols = c(Benz,Ebenz, Tol, mXyl, pXyl, IPBenz, oXyl, nPBenz, x135TMBenz, x124TMBenz, secBBenz, pIPTol, nBBenz, Naph), names_to = "parameter", values_to = "measure") %>% #converting numerous columns into fewer
  mutate(oil_concentration = as.character(oil_concentration))
```

```
## Rows: 12 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): fertilized, sample_id
## dbl (15): oil_concentration, Benz, Ebenz, Tol, mXyl, pXyl, IPBenz, oXyl, nPB...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
voc_data  
```

```
## # A tibble: 168 × 5
##    oil_concentration fertilized sample_id parameter  measure
##    <chr>             <chr>      <chr>     <chr>        <dbl>
##  1 5                 y          5FT0      Benz          0   
##  2 5                 y          5FT0      Ebenz         3.21
##  3 5                 y          5FT0      Tol           8.69
##  4 5                 y          5FT0      mXyl          6.15
##  5 5                 y          5FT0      pXyl          6.15
##  6 5                 y          5FT0      IPBenz        1.09
##  7 5                 y          5FT0      oXyl          8.00
##  8 5                 y          5FT0      nPBenz        4.93
##  9 5                 y          5FT0      x135TMBenz   10.2 
## 10 5                 y          5FT0      x124TMBenz   41.9 
## # ℹ 158 more rows
```




```r
nested_data <- voc_data %>% #assigning the dataset to an object
  group_by(oil_concentration, fertilized) %>% #grouping the data by 3 variables: site, sample depth and sample type
  nest() #nesting the grouped data
nested_data #calling the nested data
```

```
## # A tibble: 4 × 3
## # Groups:   oil_concentration, fertilized [4]
##   oil_concentration fertilized data             
##   <chr>             <chr>      <list>           
## 1 5                 y          <tibble [42 × 3]>
## 2 50                n          <tibble [42 × 3]>
## 3 50                y          <tibble [42 × 3]>
## 4 500               y          <tibble [42 × 3]>
```




```r
nested_plots <- nested_data %>% #assigning the nested data to an object
  mutate(plot = pmap(list(oil_concentration, fertilized, data), #creating individual objects to automatically generate plots from
                     ~ ggplot() + #creating plots
                       geom_col(data = ..3, aes(x = parameter, y = measure, fill = sample_id), colour = "black", position = "dodge") + #sequentially creating bar plots from the nested data
                       labs(y = "Percent of total (%)") + #labelling the y-axis
                       scale_x_discrete(limits = c("Benz", "Ebenz", "Tol", "mXyl", "pXyl", "IPBenz", "oXyl", "nPBenz", "x135TMBenz", "x124TMBenz", "secBBenz", "pIPTol", "nBBenz", "Naph")) + #ordering the observations on the x-axis
                       theme_minimal() + #assigning the tidyverse minimal theme to the plot
                       scale_fill_viridis(discrete = TRUE) + #changing the colour palette 
                       theme(axis.title.x = element_blank(), #removing the title from the x-axis
                             axis.title.y = element_text(size = 14), #changing the size of the y-axis title
                             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.9), #changing the angle of the x-axis text
                             axis.text = element_text(size = 14), #changing the overall size of axis text
                             legend.title = element_blank(), #removing the title from the legend
                             legend.text = element_text(size = 14), #setting the size of the legend text
                             axis.ticks.x = element_line(size = 0.25), #changing the size of the x-axis tick marks
                             panel.grid.major.x = element_blank(), #removing the panelling
                             legend.position = "bottom")), #changing the position of the legend
                    # labs(title = str_c(..1, ..2, ..3, "sediments", sep = ", "))), #sequentially assigning the titles for each plot
         filename = str_c(oil_concentration, fertilized, "plot.png", sep = "_")) #assigning a filename to each plot
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `plot = pmap(...)`.
## ℹ In group 1: `oil_concentration = "5"`, `fertilized = "y"`.
## Caused by warning:
## ! The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
## ℹ Please use the `linewidth` argument instead.
```

```r
nested_plots #calling the nested plots
```

```
## # A tibble: 4 × 5
## # Groups:   oil_concentration, fertilized [4]
##   oil_concentration fertilized data              plot   filename      
##   <chr>             <chr>      <list>            <list> <chr>         
## 1 5                 y          <tibble [42 × 3]> <gg>   5_y_plot.png  
## 2 50                n          <tibble [42 × 3]> <gg>   50_n_plot.png 
## 3 50                y          <tibble [42 × 3]> <gg>   50_y_plot.png 
## 4 500               y          <tibble [42 × 3]> <gg>   500_y_plot.png
```




```r
nested_plots_save <- nested_plots %>% #assigning the nested plots to an object
  ungroup() %>% #must ungroup the nested data to save the individual files
  select(filename, plot) %>% #choosing the parameters to save each file from
  pwalk(ggsave, path =  here("figures"), width = 190, height = 220, units = "mm") #sequentially saving each plot based on the two above variables
```


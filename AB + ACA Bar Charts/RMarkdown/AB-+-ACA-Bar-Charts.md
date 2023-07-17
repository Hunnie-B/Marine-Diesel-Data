---
title: "AB + ACA Bar Charts"
author: "BH"
date: "2023-05-29"
output: 
  html_document:
    keep_md: true
---








```r
ab_aca_data <- read_csv(here("data", "tidy_ab_aca_data.csv")) %>% 
  #clean_names() %>%
  pivot_longer(cols = c(nC6CH, nC7CH, nC8CH, nC9CH, nC10CH, nC11CH, nC12CH, nC13CH, nC14CH, nC4B, nC5B, nC6B, nC7B, nC8B, nC9B, nC10B, nC11B, nC12B, nC13B, nC14B, nC15B, nC16B, nC17B, nC18B, nC19B), names_to = "parameter", values_to = "measure") %>%
  #pivot_longer(cols = c(n_hexylcyclohexane, n_heptylcyclohexane, n_octylcyclohexane, n_nonylcyclohexane, n_decylcyclohexane, n_undecylcyclohexane, n_dodecylcyclohexane, n_tridecylcyclohexane, n_tetradecylcyclohexane, n_butylbenzene, n_pentylbenzene, n_hexylbenzene, n_heptylbenzene, n_octylbenzene, n_nonylbenzene, n_decylbenzene, n_undecylbenzene, n_dodecylbenzene, n_tridecylbenzene, n_tetradecylbenzene, n_pentadecylbenzene, n_hexadecylbenzene, n_heptadecylbenzene, n_octadecylbenzene, n_nonadecylbenzene), names_to = "parameter", values_to = "measure") %>%
  mutate(oil_concentration = as.character(oil_concentration))
```

```
## Rows: 12 Columns: 28
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): fertilized, sample_id
## dbl (26): oil_concentration, nC6CH, nC7CH, nC8CH, nC9CH, nC10CH, nC11CH, nC1...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
ab_aca_data  
```

```
## # A tibble: 300 × 5
##    oil_concentration fertilized sample_id parameter measure
##    <chr>             <chr>      <chr>     <chr>       <dbl>
##  1 5                 y          5FT0      nC6CH       14.3 
##  2 5                 y          5FT0      nC7CH       20.3 
##  3 5                 y          5FT0      nC8CH       21.1 
##  4 5                 y          5FT0      nC9CH       15.2 
##  5 5                 y          5FT0      nC10CH      16.0 
##  6 5                 y          5FT0      nC11CH       4.59
##  7 5                 y          5FT0      nC12CH       2.07
##  8 5                 y          5FT0      nC13CH       3.21
##  9 5                 y          5FT0      nC14CH       3.23
## 10 5                 y          5FT0      nC4B        26.6 
## # ℹ 290 more rows
```



```r
nested_data <- ab_aca_data %>% #assigning the dataset to an object
  group_by(oil_concentration, fertilized) %>% #grouping the data by 3 variables: site, sample depth and sample type
  nest() #nesting the grouped data
nested_data #calling the nested data
```

```
## # A tibble: 4 × 3
## # Groups:   oil_concentration, fertilized [4]
##   oil_concentration fertilized data             
##   <chr>             <chr>      <list>           
## 1 5                 y          <tibble [75 × 3]>
## 2 50                n          <tibble [75 × 3]>
## 3 50                y          <tibble [75 × 3]>
## 4 500               y          <tibble [75 × 3]>
```



```r
nested_plots <- nested_data %>% #assigning the nested data to an object
  mutate(plot = pmap(list(oil_concentration, fertilized, data), #creating individual objects to automatically generate plots from
                     ~ ggplot() + #creating plots
                       geom_col(data = ..3, aes(x = parameter, y = measure, fill = sample_id), colour = "black", position = "dodge") + #sequentially creating bar plots from the nested data
                       labs(y = "Percent of total (%)") + #labelling the y-axis
                       scale_x_discrete(limits = c("nC6CH", "nC7CH", "nC8CH", "nC9CH", "nC10CH", "nC11CH", "nC12CH", "nC13CH", "nC14CH", "nC4B", "nC5B", "nC6B", "nC7B", "nC8B", "nC9B", "nC10B", "nC11B", "nC12B", "nC13B", "nC14B", "nC15B", "nC16B", "nC17B", "nC18B", "nC19B")) +
                       #scale_x_discrete(limits = c("n_hexylcyclohexane", "n_heptylcyclohexane", "n_octylcyclohexane", "n_nonylcyclohexane", "n_decylcyclohexane", "n_undecylcyclohexane", "n_dodecylcyclohexane", "n_tridecylcyclohexane", "n_tetradecylcyclohexane", "n_butylbenzene", "n_pentylbenzene", "n_hexylbenzene", "n_heptylbenzene", "n_octylbenzene", "n_nonylbenzene", "n_decylbenzene", "n_undecylbenzene", "n_dodecylbenzene", "n_tridecylbenzene", "n_tetradecylbenzene", "n_pentadecylbenzene", "n_hexadecylbenzene", "n_heptadecylbenzene", "n_octadecylbenzene", "n_nonadecylbenzene")) + #ordering the observations on the x-axis
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
## 1 5                 y          <tibble [75 × 3]> <gg>   5_y_plot.png  
## 2 50                n          <tibble [75 × 3]> <gg>   50_n_plot.png 
## 3 50                y          <tibble [75 × 3]> <gg>   50_y_plot.png 
## 4 500               y          <tibble [75 × 3]> <gg>   500_y_plot.png
```



```r
nested_plots_save <- nested_plots %>% #assigning the nested plots to an object
  ungroup() %>% #must ungroup the nested data to save the individual files
  select(filename, plot) %>% #choosing the parameters to save each file from
  pwalk(ggsave, path =  here("figures"), width = 190, height = 220, units = "mm") #sequentially saving each plot based on the two above variables
```














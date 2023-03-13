---
title: "Takoudes Independent Problem 1"
author: "Alex Takoudes"
date: "2023-03-12"
output:
  pdf_document: default
  html_document: default
  word_document: default
---




```r
dist1940 <- read.csv(file="1940DistVal.csv", header=T)
dist2020 <- read.csv(file="2020DistVal.csv", header=T)
median1940 <- read.csv(file="1940MajorValue.csv", header=T)
median2020 <- read.csv(file="2020MajorValue.csv", header=T)

library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.2 --
## v ggplot2 3.4.0      v purrr   1.0.1 
## v tibble  3.1.8      v dplyr   1.0.10
## v tidyr   1.2.1      v stringr 1.5.0 
## v readr   2.1.3      v forcats 0.5.2 
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
median1940 <- median1940 %>%
  mutate(avgmedValu = round(avgmedValu, 2)) %>%
  select(-cntmedValu) %>%
  select(iso_major, featCount, avgmedValu)

median2020 <- median2020 %>%
  mutate(avgmedValu = round(avgmedValu, 2)) %>%
  select(-cntmedValu) %>%
  select(iso_major, featCount, avgmedValu)

median_table <- left_join(median1940, median2020, by = "iso_major")

dist1940 <- dist1940 %>%
  mutate(avgmedValu = round(avgmedValu, 2)) %>%
  select(-c(cntmedValu, featCount)) %>%
  pivot_wider(
    names_from = iso_major, 
    values_from = avgmedValu,
  ) %>%
  arrange(by=as.integer(substr(distgrp, 1, 2)))



dist2020 <- dist2020 %>%
  mutate(avgmedValu = round(avgmedValu, 2)) %>%
  select(-c(cntmedValu, featCount)) %>%
  pivot_wider(
    names_from = iso_major, 
    values_from = avgmedValu,
  ) %>%
  arrange(by=as.integer(substr(distgrp, 1, 2)))
```


```r
library(gt)
gt_median <- gt(median_table) %>%
  tab_header(title = "Average of median home value for owner-occupied houses in St. Louis, MO in 1940 and 2020") %>%
  cols_label(
    iso_major = "Majority Group",
    featCount.x = "Feature Count (1940)",
    avgmedValu.x = "Average Median Home Value (1940)", 
    featCount.y = "Feature Count (2020)", 
    avgmedValu.y = "Average Median Home Value (2020)"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  fmt_currency(
    columns = c(avgmedValu.x, avgmedValu.y),
    currency = "USD"
  )

gt_dist1940 <- gt(dist1940) %>%
  tab_header(title = "Average of median home value for owner-occupied houses in St. Louis, MO in 1940") %>%
  cols_label(
    distgrp = "Distance from CBD (km)",
    count = "Feature Count",
  ) %>%
  cols_align(align = "center", columns = everything())%>%
  fmt_currency(
    columns = c(3:6),
    currency = "USD"
  ) %>%
  cols_move(
    columns = 6,
    after = 4
  )

gt_dist2020 <- gt(dist2020) %>%
  tab_header(title = "Average of median home value for owner-occupied houses in St. Louis, MO in 2020") %>%
  cols_label(
    distgrp = "Distance from CBD (km)",
    count = "Feature Count",
  ) %>%
  cols_align(align = "center", columns = everything())%>%
  fmt_currency(
    columns = c(3:6),
    currency = "USD"
  ) %>%
  cols_move(
    columns = 6,
    after = 4
  )

gt_median
```

\begin{longtable}{ccccc}
\caption*{
{\large Average of median home value for owner-occupied houses in St. Louis, MO in 1940 and 2020}
} \\ 
\toprule
Majority Group & Feature Count (1940) & Average Median Home Value (1940) & Feature Count (2020) & Average Median Home Value (2020) \\ 
\midrule
Isolated Black & 9 & $\text{\$}1,357.89$ & 71 & $\text{\$}75,926.76$ \\ 
Isolated White & 198 & $\text{\$}4,144.90$ & 144 & $\text{\$}274,204.17$ \\ 
Mixed Black & 3 & $\text{\$}1,956.33$ & 36 & $\text{\$}127,780.56$ \\ 
Mixed White & 6 & $\text{\$}1,378.67$ & 69 & $\text{\$}186,571.01$ \\ 
\bottomrule
\end{longtable}

```r
gt_dist1940
```

\begin{longtable}{cccccc}
\caption*{
{\large Average of median home value for owner-occupied houses in St. Louis, MO in 1940}
} \\ 
\toprule
Distance from CBD (km) & Feature Count & Isolated Black & Isolated White & Mixed White & Mixed Black \\ 
\midrule
0 to 3 & 24 & $\text{\$}904.83$ & $\text{\$}1,331.85$ & $\text{\$}1,405.00$ & $\text{\$}1,959.50$ \\ 
3 to 5 & 25 & $\text{\$}2,798.00$ & $\text{\$}2,716.09$ & NA & NA \\ 
5 to 10 & 77 & NA & $\text{\$}4,116.66$ & $\text{\$}2,601.00$ & NA \\ 
10 to 15 & 45 & NA & $\text{\$}5,281.66$ & NA & $\text{\$}1,950.00$ \\ 
15 or more & 45 & $\text{\$}1,196.00$ & $\text{\$}4,658.29$ & $\text{\$}728.00$ & NA \\ 
\bottomrule
\end{longtable}

```r
gt_dist2020
```

\begin{longtable}{cccccc}
\caption*{
{\large Average of median home value for owner-occupied houses in St. Louis, MO in 2020}
} \\ 
\toprule
Distance from CBD (km) & Feature Count & Isolated Black & Isolated White & Mixed White & Mixed Black \\ 
\midrule
0 to 3 & 11 & $\text{\$}152,475.00$ & $\text{\$}179,600.00$ & $\text{\$}208,166.67$ & $\text{\$}186,700.00$ \\ 
3 to 5 & 21 & $\text{\$}55,328.57$ & NA & $\text{\$}245,825.00$ & $\text{\$}184,550.00$ \\ 
5 to 10 & 65 & $\text{\$}67,704.35$ & $\text{\$}213,957.89$ & $\text{\$}210,923.53$ & $\text{\$}108,466.67$ \\ 
10 to 15 & 69 & $\text{\$}64,321.74$ & $\text{\$}257,885.29$ & $\text{\$}213,600.00$ & $\text{\$}122,316.67$ \\ 
15 or more & 154 & $\text{\$}96,928.57$ & $\text{\$}295,425.84$ & $\text{\$}154,714.29$ & $\text{\$}108,418.75$ \\ 
\bottomrule
\end{longtable}

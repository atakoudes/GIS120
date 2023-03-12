dist1940 <- read.csv(file="Problem1/1940DistVal.csv", header=T)
dist2020 <- read.csv(file="Problem1/2020DistVal.csv", header=T)
median1940 <- read.csv(file="Problem1/1940MajorValue.csv", header=T)
median2020 <- read.csv(file="Problem1/2020MajorValue.csv", header=T)

library(tidyverse)

median1940 <- median1940 %>%
  select(-cntmedValu) %>%
  select(iso_major, featCount, avgmedValu)

median2020 <- median2020 %>%
  select(-cntmedValu) %>%
  select(iso_major, featCount, avgmedValu)

median_table <- left_join(median1940, median2020, by = "iso_major")

dist1940 <- dist1940 %>%
  select(-c(cntmedValu, featCount)) %>%
  pivot_wider(
    names_from = iso_major, 
    values_from = avgmedValu,
  ) %>%
  arrange(by=as.integer(substr(distgrp, 1, 2)))

dist2020 <- dist2020 %>%
  select(-c(cntmedValu, featCount)) %>%
  pivot_wider(
    names_from = iso_major, 
    values_from = avgmedValu,
  ) %>%
  arrange(by=as.integer(substr(distgrp, 1, 2)))
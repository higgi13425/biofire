## create table 1

library(desctable)
library(pander)
library(tidyverse)
library(rmarkdown)
library(kable)
library(kableExtra)
library(knitr)
# read in data
biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")

#try table by groups
biofire_rect %>% 
  select(group, age, sex, smoking, duration_of_ibd_yr, type_of_ibd) %>% 
  group_by(group) %>% 
  desctable(stats = stats_auto) %>% 
  datatable


#try table
biofire_rect %>% 
  select(group, age, sex, smoking, duration_of_ibd_yr) %>% 
  desctable(stats = stats_auto) %>% 
  datatable

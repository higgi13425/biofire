# bar graphs for infection counts by group, percentage infected


library(desctable)
library(pander)
library(tidyverse)
library(rmarkdown)
#library(kable)
library(kableExtra)
library(knitr)
library(ggstance)
library(ggthemes)
library(forcats)
library(magrittr)
library(extrafont)
library(janitor)
 
quietly(fonts())
quietly(fonttable())


# read in data
biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")

# which infections in active on vedo?
biofire_rect %>% 
  filter(vedolizumab == 1) %>% 
  filter(inf_count >0) %>% 
  #filter(!is.na(subject_id)) %>% 
  filter(group %in% c("Active CD", "Active UC")) %>% 
  select(subject_id, group, vedolizumab, tacrolimus, oral_prednisone, oral_budesonide,
         inf_count, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  gather(key = infection, value = pos, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  filter(pos == 1)

# any other meds (steroids?) with vedo?

# which infections in active on tacro?
biofire_rect %>% 
  filter(tacrolimus == 1) %>% 
  filter(inf_count >0) %>% 
  #filter(!is.na(subject_id)) %>% 
  filter(group %in% c("Active CD", "Active UC")) %>% 
  select(subject_id, group, tacrolimus, vedolizumab, oral_prednisone, oral_budesonide,
         inf_count, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  gather(key = infection, value = pos, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  filter(pos == 1)

# which infections occur in healthy controls (likely to be colonized)
biofire_rect %>% 
  filter(group %in% c("Healthy Control", "IBS-D")) %>% 
  select(subject_id, group, 
         inf_count, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  gather(key = infection, value = pos, `Adenovirus F 40/41`:`E. coli O157`) %>% 
  filter(pos == 1) %>% 
  arrange(group) %>% 
  print(n=Inf)

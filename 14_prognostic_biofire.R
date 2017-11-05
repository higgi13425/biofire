# bar graphs for infection counts by group, percentage infected


library(desctable)
library(pander)
library(tidyverse)
library(rmarkdown)
library(kable)
library(kableExtra)
library(knitr)
library(ggstance)
library(ggthemes)
library(forcats)
library(magrittr)
library(extrafont)
library(janitor)
font_import()
fonts()
fonttable()


# read in data
biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")

biofire_rect$esc_count <- biofire_rect$f_steroid + biofire_rect$f_biologics + 
  biofire_rect$f_immunomodulator
biofire_rect$escalate <- 0
biofire_rect$escalate[biofire_rect$esc_count>0] <- 1

## dichotomize surgery - replace NA with 0
biofire_rect$f_surg_dichot <- 0
biofire_rect$f_surg_dichot[biofire_rect$f_surgery == 1] <- 1
#now make into factor
biofire_rect$f_surg_fact <- as.factor(biofire_rect$f_surg_dichot)

biofire_rect <- biofire_rect %>% mutate(f_surg_fact = fct_recode(
  f_surg_fact,
  "No Surgery" = "0",
  "Surgery" = "1"
))

#set up inf as a factor variable
biofire_rect$inf <- as.factor(biofire_rect$inf)

biofire_rect <- biofire_rect %>% mutate(inf = fct_recode(
  inf,
  "No Infection" = "0",
  "Stool Infection" = "1"
))



biofire_rect %>% 
  select(group, inf, f_surg_dichot, f_surgery) %>% 
  filter(group %in% c("Active CD", "Inactive CD")) %>% 
  group_by(inf) %>% 
  summarise(sur_pct= 100*mean(f_surg_dichot), count= n()) 


%>% 
  ggplot(., aes(f_surg_fact, sur_pct, fill = sur_pct)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(x=0.3, y= f_surg_fact, label = group), color="black",
            family="Arial-ItalicMT", size=5, hjust=0) +
  geom_text(aes(x=26, y= f_surg_dichot, label = round(inf_pct, digits=1)), color="black",
            family="Arial", size=5, hjust=0) +
  coord_flip() +
  theme(strip.text = element_text(hjust=0, family = "Arial", size = 12)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x= "Presence of Infection", title ="Need for Surgery in CD by Infection Status", 
       y="Percent requiring surgery within 90 days")



biofire_rect %>% 
  select(group, inf, f_surg_dichot, f_surgery) %>% 
  filter(group %in% c("Active UC", "Inactive UC")) %>% 
  group_by(inf) %>% 
  summarise(sur_pct= 100*mean(f_surg_dichot), count= n()) 


biofire_rect %>% 
  select(group, inf, escalate) %>% 
  filter(group %in% c("Active CD", "Inactive CD")) %>% 
  group_by(inf) %>% 
  summarise(esc_pct= 100*mean(escalate), count= n()) 


biofire_rect %>% 
  select(group, inf, escalate) %>% 
  filter(group %in% c("Active UC", "Inactive UC")) %>% 
  group_by(inf) %>% 
  summarise(esc_pct= 100*mean(escalate), count= n()) 

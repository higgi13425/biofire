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
font_import()
fonts()
fonttable()


# read in data
biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")


biofire_rect %>% 
  select(group, inf, inf_count) %>% 
  group_by(group) %>% 
  summarise(inf_pct= 100*mean(inf), count= n()) %>% 
  ggplot(., aes(group, inf_pct, fill="blue")) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme(strip.text = element_text(hjust=0, family = "Roboto-Bold", size = 12)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, title ="Prevalence of Infectious Agents by Group", 
       y="Percent with Infectious Agent")

biofire_rect %>% 
  select(group, inf, inf_count) %>% 
  group_by(group) %>% 
  summarise(inf_pct= 100*mean(inf), count= n()) %>% 
  ggplot(., aes(inf_pct, fct_reorder(group, inf_pct), fill=inf_pct)) +
  geom_barh(stat="identity", alpha=0.8) +
  theme_tufte(base_family = "Arial") +
  geom_text(aes(x=0.3, y= group, label = group), color="black",
            family="Arial-ItalicMT", size=5, hjust=0) +
    theme(plot.title=element_text(family="Arial-BoldMT")) +
            scale_fill_gradient(low="darkslategray3", high="turquoise4") +
            theme(legend.position = "none") +
            theme(axis.ticks = element_blank()) +
            scale_x_continuous(expand=c(0,0)) +
            theme(axis.text.y=element_blank()) +
  labs(x=NULL, title ="Prevalence of Infectious Agents by Group", 
       y="Percent with Infectious Agent")


biofire_rect %>% 
  select(group, inf, inf_count) %>% 
  group_by(group) %>% 
  summarise(inf_pct= 100*mean(inf), count= n()) %>% 
  ggplot(., aes(inf_pct, group, fill=inf_pct)) +
  geom_barh(stat="identity", alpha=0.8) +
  theme_tufte(base_family = "Arial") +
  geom_text(aes(x=0.3, y= group, label = group), color="black",
            family="Arial-ItalicMT", size=5, hjust=0) +
  geom_text(aes(x=26, y= group, label = round(inf_pct, digits=1)), color="black",
            family="Arial", size=5, hjust=0) +
  theme(plot.title=element_text(family="Arial-BoldMT")) +
  scale_fill_gradient(low="darkslategray3", high="turquoise4") +
  theme(legend.position = "none") +
  theme(axis.ticks = element_blank()) +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.text.y=element_blank()) +
  labs(y=NULL, title ="Prevalence of Infectious Agents by Group (Percent)", 
       x="Percent with Infectious Agent")

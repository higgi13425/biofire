# graph counts of infectious agents

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
  select(group, `Adenovirus F 40/41`: `E. coli O157`) %>% 
  gather('Agent', 'Detected', 2:23) %>% 
  group_by(Agent) %>% 
  summarise(pos_pct= 100*mean(Detected), count= n()) %>% 
  ggplot(., aes(pos_pct, Agent, fill=pos_pct)) +
  geom_barh(stat="identity", alpha=0.8) +
  theme_tufte(base_family = "Arial") +
  geom_text(aes(x=0.1, y= Agent, label = Agent), color="black",
            family="Arial-ItalicMT", size=5, hjust=0) +
  geom_text(aes(x=6, y= Agent, label = round(pos_pct, digits=1)), color="black",
            family="Arial", size=5, hjust=0) +
  theme(plot.title=element_text(family="Arial-BoldMT")) +
  scale_fill_gradient(low="darkslategray3", high="turquoise4") +
  theme(legend.position = "none") +
  theme(axis.ticks = element_blank()) +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.text.y=element_blank()) +
  labs(y=NULL, title ="Prevalence of Infectious Agents (Percent)", 
       x="Percent with Infectious Agent")


biofire_rect %>% 
  select(group, `Adenovirus F 40/41`: `E. coli O157`) %>% 
  gather('Agent', 'Detected', 2:23) %>% 
  group_by(group, Agent) %>% 
  summarise(pos_pct= 100*mean(Detected), count= n()) %>% 
  ggplot(., aes(pos_pct, Agent, fill=pos_pct)) +
  geom_barh(stat="identity", alpha=0.8) +
  theme_tufte(base_family = "Arial") +
  facet_wrap(~group, nrow=3) +
  geom_text(aes(x=0.3, y= Agent, label = Agent), color="black",
            family="Arial-ItalicMT", size=2.5, hjust=0) +
  geom_text(aes(x=6, y= Agent, label = round(pos_pct, digits=1)), color="black",
            family="Arial", size=2.5, hjust=0) +
  theme(plot.title=element_text(family="Arial-BoldMT")) +
  scale_fill_gradient(low="darkslategray3", high="turquoise4") +
  theme(legend.position = "none") +
  theme(axis.ticks = element_blank()) +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.text.y=element_blank()) +
  labs(y=NULL, title ="Prevalence of Infectious Agents (Percent)", 
       x="Percent with Infectious Agent")

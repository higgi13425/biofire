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
library(multipanelfigure)
#font_import()
fonts()
#fonttable()

# read in data
biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")

# set palette
legend_ord <- levels(with(df, reorder(Labels, Percent)))

# By Agent for CD
biofire_rect %>% 
  select(group, `Adenovirus F 40/41`: `E. coli O157`) %>% 
  gather('Agent', 'Detected', 2:23) %>% 
  mutate(Agent = as.factor(Agent)) %>% 
  group_by(Agent, group) %>% 
  filter(group %in% c("Active CD", "Inactive CD")) %>% 
  filter(Agent != "Astrovirus") %>% 
  filter(Agent != "Shigella/Enteroinvasive E. coli (EIEC)") %>% 
  filter(Agent != "Shiga-like toxin-producing E. coli (STEC) stx1/stx2") %>% 
  filter(Agent != "Plesiomonas shigelloides") %>% 
  filter(Agent != "Enterotoxigenic E. coli (ETEC) lt/st") %>% 
  filter(Agent != "E. coli O157") %>% 
  filter(Agent != "Giardia lamblia") %>% 
  filter(Agent != "Entamoeba histolytica") %>% 
  filter(Agent != "Cyclospora cayetanensis") %>%
  summarise(pos_pct= 100*mean(Detected), count= n()) %>% 
  ggplot(., aes(x = fct_reorder(Agent, pos_pct), y = pos_pct, 
                group = fct_rev(group), fill=group)) + # could add col =24 for black outline
  scale_fill_manual(values=c("red", "gray70"), name = "Group") +
  geom_col(position = "dodge") +
  theme_tufte(base_family = "Arial") +
  geom_text(aes(label=round(pos_pct, digits = 1)), hjust=-0.1, 
            color="black", size=5, position = position_dodge(width = 0.9)) +
  ylab("Percent Positive") +
  xlab("Infectious Agent") +
  ylim(0,15) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size=12)) +
  theme(legend.position = c(0.6,0.6)) +
  theme(legend.text = element_text(size=16)) +
  theme(legend.title = element_text(size=18, face = "bold")) +
  coord_flip()

# By Agent for UC
biofire_rect %>% 
  select(group, `Adenovirus F 40/41`: `E. coli O157`) %>% 
  gather('Agent', 'Detected', 2:23) %>% 
  group_by(Agent, group) %>% 
  filter(group %in% c("Active UC", "Inactive UC")) %>% 
  filter(Agent != "Astrovirus") %>% 
  filter(Agent != "Shigella/Enteroinvasive E. coli (EIEC)") %>% 
  filter(Agent != "Shiga-like toxin-producing E. coli (STEC) stx1/stx2") %>% 
  filter(Agent != "Plesiomonas shigelloides") %>% 
  filter(Agent != "Enterotoxigenic E. coli (ETEC) lt/st") %>% 
  filter(Agent != "E. coli O157") %>% 
  filter(Agent != "Giardia lamblia") %>% 
  filter(Agent != "Entamoeba histolytica") %>% 
  filter(Agent != "Cyclospora cayetanensis") %>%
  summarise(pos_pct= 100*mean(Detected), count= n()) %>% 
  ggplot(., aes(x = fct_reorder(Agent, pos_pct), y = pos_pct, 
                group = fct_rev(group), fill=group)) + # could add col =24 for black outline
  scale_fill_manual(values=c("red", "gray70"), name = "Group") +
  geom_col(position = "dodge") +
  theme_tufte(base_family = "Arial") +
  geom_text(aes(label=round(pos_pct, digits = 1)), hjust=-0.1, 
            color="black", size=5, position = position_dodge(width = 0.9)) +
  ylab("Percent Positive") +
  xlab("Infectious Agent") +
  ylim(0,18) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size=12)) +
  theme(legend.position = c(0.6,0.6)) +
  theme(legend.text = element_text(size=16)) +
  theme(legend.title = element_text(size=18, face = "bold")) +
  coord_flip()
  
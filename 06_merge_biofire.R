####
# NOTE: still need to merge in cdtox data
####


library(dplyr)

### read in files for merging
cdr_biofire <- readRDS("~/Documents/Rcode/biofire/cdr_biofire.rds")

jul_biofire <- readRDS("~/Documents/Rcode/biofire/jul_biofire.rds")

lab_biofire <- read_rds("/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire.rds")


biofire2 <- inner_join(jul_biofire, cdr_biofire, by = c("mrn", "stool_date"))

biofire3 <- inner_join(jul_biofire, lab_biofire, by = "subject_id")

biofire4 <- rbind(biofire2, biofire3)
#clean up names
biofire5 <- clean_names(biofire4)

# count infections
biofire6 <- biofire5 %>% select(adenovirus_f_40_41:e_coli_o157) %>% 
  mutate(inf_count = rowSums(.)) %>% 
  select(inf_count) 
biofire7 <- cbind(biofire5, biofire6)
#generate a dichotmous marker variable
biofire7$inf <- 0
biofire7$inf[biofire7$inf_count>0] <- 1

# select only the inf and inf_count
biofire8 <- biofire7 %>% select(inf, inf_count)
#merge these back to biofire4, which has the 'nice' pathogen names
biofire9 <- bind_cols(biofire4, biofire8)
#checking
# crosstab(biofire7$inf, biofire7$inf_count) - matches up

#problem - some inf=1 have fc= NA

saveRDS(biofire9, "~/Documents/Rcode/biofire/biofire_rect.rds")

biofire_rect <- read_rds("~/Documents/Rcode/biofire/biofire_rect.rds")

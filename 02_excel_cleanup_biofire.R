## Prepare jul_biofire.rds   =====================================
##
##

library(stringr)
library(readxl)
library(janitor)
library(tidyverse)
library(lubridate)
library(readr)
library(pdftools)
library(forcats)
library(dataMaid)
library(shiny)
library(rmarkdown)

jul1 <- read_excel("~/Documents/Rcode/biofire/jul_biofire/BIOFIRE_Michigan_full (FINAL).xlsx")
jul1 <- jul1 %>% clean_names() 

#jul1 <- jul1[1:456, ] # note that this is a workaround as filtering out NA for number is not working
# assumes all should have number
#note - eligible cases have both jul_number and subject_id 
jul1$jul_number <- jul1$number #change varname to jul_number
jul1$number <-NULL  #drop number
jul1$eligible <- 0 #initialize eligible variable
jul1$eligible[!is.na(jul1$jul_number)] <- 1 #define eligible
jul2 <- jul1[jul1$eligible==1,]

#pad mrn with leading 0
jul2$mrn <- str_pad(jul2$mrn, width=9, side="left", pad="0")
# fix dates from POSIXct to Date
jul2 <- jul2 %>% rename(stool_date = date_stool)
jul2$stool_date <- ymd(jul2$stool_date) 
jul2$dob <- ymd(jul2$dob)
jul2$date_ibd_dx <- ymd(jul2$date_ibd_dx)
jul2$sur_date <- ymd(jul2$sur_date)
jul2$hos_date <- ymd(jul2$hos_date)
jul2$last_seen_date <- ymd(jul2$last_seen_date)
jul2$x90_d <- ymd(jul2$x90_d)
jul2$final_sur <- ymd(jul2$final_sur)
jul2$final_s_t <- ymd(jul2$final_s_t)

# could possibly do this as a function
#dates <- c('stool_date', 'dob', 'date_ibd_dx', 'sur_date', 'hos_date', 'last_seen_date',
           #'x90_d', 'final_sur', 'final_s_t')
# fixdate <- function(date){
#   date <- ymd(date)
# }

#check jul2
dim(jul2)
names(jul2)
glimpse(jul2)
#sum(is.na(jul2))
#summary(jul2)
# --------------------
#set up groups as a factor variable
jul2$group <- as.factor(jul2$group)
jul2 <- jul2 %>% mutate(group = fct_recode(
  group,
  "Active CD" = "1",
  "Inactive CD" = "2",
  "Active UC" = "3",
  "Inactive UC" = "4",
  "IBS" = "5",
  "Healthy Control" = "6"
)) 
# note all have a group

#set up smoking as a factor variable
jul2$smoking <- as.factor(jul2$smoking)

jul2 <- jul2 %>% mutate(smoking = fct_recode(
  smoking,
  "Never" = "0",
  "Ex-smoker" = "1",
  "Current smoker" = "2"
))

#set up type of IBD as a factor variable
jul2$type_of_ibd <- as.factor(jul2$type_of_ibd)

jul2 <- jul2 %>% mutate(type_of_ibd = fct_recode(
  type_of_ibd,
  "Crohn's disease" = "1",
  "Ulcerative colitis" = "2"
))

#set up sex as a factor variable
jul2$sex <- as.factor(jul2$sex)

#fix a few missing sex
jul2$sex[jul2$jul_number=="321"] <- "1"
jul2$sex[jul2$jul_number=="351"] <- "1"
jul2$sex[jul2$jul_number=="354"] <- "1"
jul2$sex[jul2$jul_number=="492"] <- "1"
jul2$sex[jul2$jul_number=="493"] <- "1"
jul2$sex[jul2$jul_number=="494"] <- "0"
jul2$sex[jul2$jul_number=="495"] <- "1"
jul2$sex[jul2$jul_number=="496"] <- "1"
jul2$sex[jul2$jul_number=="497"] <- "0"

jul2 <- jul2 %>% mutate(sex = fct_recode(
  sex,
  "Male" = "0",
  "Female" = "1"
)) 
# note several missing sex - jul_numbers of 
# 301 406 410 424  all missing sex - even in MiChart
#checking -------------
sum(is.na(jul2$jul_number)) #none missing
sum(is.na(jul2$subject_id)) # note 176 missing a subject_id 
#subject_id means done in our lab. Missing subj id = biofire done clinically
sum(is.na(jul2$dob)) # note 69 missing a dob !!!
sum(is.na(jul2$stool_date)) # no missing stool_date(s)

##
## fill in missing ages
jul2$age <- (jul2$stool_date - jul2$dob)/365.25

# ------------------
#clean up one duplicate MRN
jul2 %>% filter(!is.na(jul2$mrn)) %>% get_dupes(mrn) #duplicated mrn, consecutive days.
# note one duplicate subject = MRN 100328668, on Jan 23 and Jan 24 2017. Not given subject_id for Jan24
# need to remove Jan 24 version, jul_number 213
juldupe <- jul2 %>% filter(jul2$mrn == "100328668" & jul2$jul_number==213)
jul2 <- anti_join(jul2,juldupe)
#jul2 %>%  filter(!is.na(jul2$mrn)) %>% get_dupes(jul2$mrn)
# now no duplicate MRNs!!!

janitor::tabyl(jul2$group) # six groups
janitor::tabyl(jul2$sex) # note 13 NA for sex

tabyl(jul2$presentation_wk)
tabyl(jul2$type_of_ibd)
#crosstab(jul1$type_of_ibd, jul1$group, show_na = T, percent = 'none')


# find that the following dates need to be changed in jul_biofire
# to match CDR dates for stool tests
# mrn 021682381, date 2016-07-14 to 2016-07-15 
# mrn 100617086, date 2016-07-08 to 2016-07-02 
# mrn 100499379, date 2016-06-16 to 2016-06-20
# mrn 017564377, date 2016-11-23 to 2016-11-24
# mrn 039437570, date 2017-02-18 to 2017-02-20
# mrn 037037771, date 2017-01-23 to 2017-01-24
# mrn 031332060, date 2016-07-18 to 2016-07-28 
# mrn 100411432, date 2016-09-03 to 2016-09-04
# mrn 039946208, date 2016-07-08 to 2016-07-10
jul2$stool_date[jul2$mrn == "021682381"] <- as.Date("2016-07-15")
jul2$stool_date[jul2$mrn == "100617086"] <- as.Date("2016-07-02")
jul2$stool_date[jul2$mrn == "100499379"] <- as.Date("2016-06-20")
jul2$stool_date[jul2$mrn == "017564377"] <- as.Date("2016-11-24")
jul2$stool_date[jul2$mrn == "039437570"] <- as.Date("2017-02-20")
jul2$stool_date[jul2$mrn == "037037771"] <- as.Date("2017-01-24")
jul2$stool_date[jul2$mrn == "031332060"] <- as.Date("2016-07-28")
jul2$stool_date[jul2$mrn == "100411432"] <- as.Date("2016-09-04")
jul2$stool_date[jul2$mrn == "039946208"] <- as.Date("2016-07-10")

# fix some variable names
colnames(jul2)[30] <- "oral_prednisone"
colnames(jul2)[31] <- "oral_budesonide"
colnames(jul2)[45] <- "Hgb"

#make age numeric
jul2$age <- as.numeric(jul2$age)

saveRDS(jul2,"~/Documents/Rcode/biofire/jul_biofire.rds")
jul_biofire <- readRDS("~/Documents/Rcode/biofire/jul_biofire.rds")

##
##
#rm(list=ls())
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)
library(fuzzyjoin)
library(dplyr)
library(janitor)
# for data from CDR query for GIPAN

dfclin <- read_csv("/Users/peterhiggins/Documents/Rcode/biofire/CDR_biofire_results/Biofire_CDRQ_08_03_17.csv")
#note no NA for result-text

#clean up and pad mrn
dfclin$mrn <- as.character(dfclin$mrn)
dfclin$mrn <- str_pad(dfclin$mrn, width=9, side="left", pad="0")
dfclin$stool_date <- dmy(dfclin$stool_date) #218 unique dates
length(dfclin %>% distinct(dfclin$mrn) %>% pull()) #229 distinct mrns out of 335 (106 unnecessary dupes)

# bring in jul_biofire
jul_biofire <- readRDS("~/Documents/Rcode/biofire/jul_biofire/jul_biofire.rds")
# use semijoin to remove duplicate mrns with wrong stool date
# i.e. biofire sent more than once
dfclin2 <- semi_join(dfclin, jul_biofire, by = c("mrn", "stool_date"))
#clean up duplicates - 7 with same MRN, same date from CDR
cdr_clin <- dfclin2 %>% distinct(mrn, PATIENT_CDR_ID, stool_date, .keep_all = TRUE)
# now down to 229 distinct obs


# now start to extract data from CDR reported BioFire
# unpack clinical biofire result text
## note some names will be slightly wrong - will replace from lab_biofire
cdr_clin$RESULT_TEXT <- str_extract(cdr_clin$RESULT_TEXT, "Campylobacter [\\s\\S]*Sapovirus [\\s\\S]*etected")
# odd warning for this one - trying to find column 'result'

#cleans out junk - now only results
#clean out "\r\r" characters
cdr_clin$RESULT_TEXT <- str_replace_all(cdr_clin$RESULT_TEXT, "\r\r", "\n")

cdr_clin$RESULT_TEXT <- str_replace_all(cdr_clin$RESULT_TEXT, "\n \n", " \n ")
# cleans out extra newline after Campylobacter

cdr_clin$RESULT_TEXT <- str_replace_all(cdr_clin$RESULT_TEXT, "Detected", "NDetected")
#attach N to beginning of detected, so can do split

cdr_clin<- cdr_clin %>% separate_rows(RESULT_TEXT, sep="\n") 
# splits on newlines, then unnests

cdr_clin$RESULT_TEXT <- str_trim(cdr_clin$RESULT_TEXT) 
# trim extra spaces for Norovirus, others

cdr_clin <- separate(cdr_clin, RESULT_TEXT, c("bug","detect"), sep=" N") 
#separate into two columns #note watch out for Norovirus!! with capital N

#note two problem lines
# tabyl(dfclin$detect)
# which(dfclin$detect == "ot applicable") row 2676
# which(dfclin$detect == "Detected - E.coli non-O157") row 2678
# fix manually
cdr_clin[2676,10] <- "ot detected"
cdr_clin[2678,10] <- "Detected"
# now have 79 "Detected", 4730 "ot detected"

#clean up bug text
cdr_clin$detect <- str_replace_all(cdr_clin$detect, "ot", "Not") # add back N to Not
cdr_clin$bug <- str_trim(cdr_clin$bug) #trims off extra spaces

#set up result column as 0,1
cdr_clin$result <- 0
cdr_clin$result[cdr_clin$detect=="Not Detected"]<- 0
cdr_clin$result[cdr_clin$detect=="Detected"]<- 1

#select columns, change to pathogen, result
cdr_clin <- filter(cdr_clin, bug != "NA") %>% select(c(mrn, stool_date, pathogen = bug, result))

#now spread to wide - one record per stool sample 
cdr_clin %>% spread(pathogen, result) -> dfclin_spread
#retest after spread
#clean up problem names
# adeno, EAEC, EPEC, ETEC, Shig/Enter, Vibrio chol
colnames(dfclin_spread) [3] <- "Adenovirus F 40/41" 
colnames(dfclin_spread) [10] <- "Enteroaggregative E. coli (EAEC)" 
colnames(dfclin_spread) [11] <- "Enteropathogenic E. coli (EPEC)" 
colnames(dfclin_spread) [12] <- "Enterotoxigenic E. coli (ETEC) lt/st" 
colnames(dfclin_spread) [19] <- "Shiga-like toxin-producing E. coli (STEC) stx1/stx2"
colnames(dfclin_spread) [20] <- "Shigella/Enteroinvasive E. coli (EIEC)" 
colnames(dfclin_spread) [22] <- "Vibrio cholerae" 
#plan to add "E. coli O157" added as a column at the end
dfclin_spread$o157 <- 0
colnames(dfclin_spread) [24] <- "E. coli O157" 
#check that colnames match between dfclin_spread and lab_biofire
colnames(lab_biofire) [2:23] == colnames(dfclin_spread) [3:24]

#check dfclin_spread
dim(dfclin_spread)
names(dfclin_spread)
glimpse(dfclin_spread)
sum(is.na(dfclin_spread))
summary(dfclin_spread)

saveRDS(dfclin_spread, "~/Documents/Rcode/biofire/cdr_biofire.rds")

cdr_biofire <- readRDS("~/Documents/Rcode/biofire/cdr_biofire.rds")

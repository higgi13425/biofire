#read in BIOFIRE RESULTS txt data & parse
#start with packages_functions_1
# this is script 2, import_merge_2




## load libraries and functions  ----------------------------------------------
##
##
##
#Packages and functions for analysis of BioFire data.
rm(list=ls())

#Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)


#function reorder, for levels in a bar graph
reorder <- function(x) {
  factor(x, levels = names(sort(table(x))))
}

#function clean_biofire, for cleaning .txt files from biofire machine
#test file is here:
# file <- "~/Documents/Rcode/biofire/lab_biofire_results_txt/FilmArray_Run_Date_2017_08_04_Sample_3113_SN_09937650.txt"
#requires packages lubridate, dplyr, tidyr, stringr
clean_biofire <- function(file){
  lns<- readLines(file)
  df <- read.csv(text=lns, stringsAsFactors=FALSE, skip = 8L, strip.white = TRUE)
  id <- df[1,]
  df$sampid <- as.numeric(str_trim(str_sub(id,15,18))) 
  df$rundate <- dmy(str_sub(id,-11,-1))
  df$Run.Summary[grepl("N/A", df$Run.Summary)]<- "Not Detected E. coli O157"
  df$result <- !grepl("Not Detected", df$Run.Summary)
  numpos <- (sum(df$result)-13)/2
  delrows<-numpos +4
  df<-df[delrows:nrow(df),] # keeps from delrows to end
  df <-filter(df, grepl("Detected ", Run.Summary))
  df <-df%>% separate(Run.Summary, c("detect","bug"), sep="ed ")
  df$detect<- paste0(df$detect, "ed")
  df$pathogen <-str_trim(df$bug)
  df <-df%>% select(sampid, rundate, pathogen, result, detect) 
  df
}


#function clean_biofire_pdf, for cleaning pdf files from biofire machine
#requires packages pdftools, stringr, tidyr
#test file is here:
# file <- "~/Documents/Rcode/biofire/lab_biofire_results_pdf/FilmArray_Run_Date_2017_06_27_Sample_153_SN_09937928.pdf"
clean_biofire_pdf <- function(file){
  pdtext<- pdf_text(file)
  text2<- str_extract(pdtext, "Bacteria\n[\\s\\S]*Sapovirus")
  text2 <- read.csv(text=text2, stringsAsFactors=FALSE, strip.white = TRUE)
  text2$Bacteria[grepl("N/A", text2$Bacteria)] <- "Not Detected E. coli O157"
  text2 <- as.data.frame(text2[str_detect(text2$Bacteria, "Detect"),])
  names(text2) <- "Bacteria"
  text2 <- text2 %>% separate(Bacteria, c("detect","bug"), sep="ed ")
  text2$detect<- paste0(text2$detect, "ed")
  text2$bug<- str_trim(text2$bug)
  text2  
}




## prepare lab_biofire.rds  ----------------------------------------------
## 
##
##

# read list of files into a vector to establish length
files <- list.files(path = "/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire_results_txt", pattern = "*.txt")

# read list of files into a vector
path = "/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire_results_txt/"
setwd("/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire_results_txt")
files <- list.files(path , pattern = "*.txt")

#now create dataframe to be built
fulldata <- NULL #create fulldata file, empty

for(file in files){
  df <- clean_biofire(file)
  fulldata <- rbind(fulldata, df)
}
df<- NULL #remove small df
# iterate over for(i in 1:length(vector)) {read file, process, clean up, rbind to big datafile, empty df}

#adjust for cases whith sample ID > 1000 to put in correct group.
fulldata <- fulldata %>% mutate(group = case_when(.$sampid<1000 ~ round(sampid/100),
                              .$sampid >=1000 ~ round (sampid/1000)))

#check fulldata
library(dataMaid)
library(janitor)
dim(fulldata)
names(fulldata)
glimpse(fulldata)
sum(is.na(fulldata))
summary(fulldata)
janitor::tabyl(fulldata$pathogen)
janitor::tabyl(fulldata$result)
janitor::tabyl(fulldata$detect)
janitor::tabyl(fulldata$group) #note 7 groups
fulldata$result <- as.numeric(fulldata$result)
fulldata$subject_id <- fulldata$sampid

# need a fix
# note one (extra) repeated O157 for 1 subject on line 3089
# pathogen (column 3) should have been EPEC
fulldata[3089,3]<- "Enteropathogenic E. coli (EPEC)"

#now spread data so that there is one observation per subject
fulldata %>% select(subject_id, pathogen, result) %>% spread(pathogen, result) -> fulldata_spread
#retest after spread
sum(is.na(fulldata))
#now save file
saveRDS(fulldata_spread, "/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire.rds")
#test it
lab_biofire <- read_rds("/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire.rds")
##
# lab_biofire.rds now clean, locked, and saved.
## 


## prepare clin_biofire ----------------------------------------------
##
##
#rm(list=ls())
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)
library(fuzzyjoin)
# for data from CDR query for GIPAN
 
dfclin <- read_csv("/Users/peterhiggins/Documents/Rcode/biofire/CDR_biofire_results/Biofire_CDRQ_08_03_17.csv")
#note no NA for result-text

#clean up and pad mrn
dfclin$mrn <- as.character(dfclin$mrn)
dfclin$mrn <- str_pad(dfclin$mrn, width=9, side="left", pad="0")
dfclin$stool_date <- dmy(dfclin$stool_date) #218 unique dates
length(dfclin %>% distinct(dfclin$mrn) %>% pull()) #229 distinct mrns out of 335 (106 unnecessary dupes)

#clean up duplicates - same MRN, diff dates
dupes <- get_dupes(dfclin, mrn)
length(dupes %>% distinct(dupes$mrn) %>% pull()) #65 unique among the 171 dupes (106 unnecessary dupes)
nrow(dupes) # lots, total of 171 on diff stool_dates
#these 65 need to be filtered down to the right date for each of these 65 mrns

#set up fuzzyjoin - find cases with close dates
jul_biofire <- readRDS("~/Documents/Rcode/biofire/jul_biofire/jul_biofire.rds")



full_join(dfclin, jul2, by = 'mrn') %>% mutate(datediff = as.integer(stool_date.x - as.Date(stool_date.y))) %>% 
  arrange(datediff) %>% 
  select(mrn, stool_date.x, stool_date.y, datediff) %>% print(n=462)

#fix one date
dfclin$stool_date[dfclin$mrn=="100328668"]<-"2017-01-24"


# do a semijoin (aka filtering join) with jul_biofire
jul7 <-readRDS("/Users/peterhiggins/Documents/Rcode/biofire/jul_biofire.rds")

length(jul7 %>% distinct(stool_date) %>% pull()) #255 unique dates
length(jul7 %>% distinct(mrn) %>% pull()) #419 unique mrns
length(jul7 %>% distinct(subject_id) %>% pull()) #281 unique subject_id



dfclin2 <- semi_join(dfclin, jul7, by = c("mrn", "stool_date")) # note filtering join
#filters out mrns that do not have a matching stool date in jul7
# ideally keeps only those rows in dfclin with a match in jul_biofire by mrn and stool_date
# end up with 226 records (expected 229)
nrow(dfclin2) #226
length(dfclin %>% distinct(mrn) %>% pull()) #229
#now find MRNs that did not match by stool_date
anti_join(dfclin, dfclin2, by="mrn") #get 20 lines, some dupes
#find all one by one in jul2
# i.e. jul2[,mrn="017564377"] %>% select(mrn, stool_date)
# find that the following dates need to be changed in jul2
# mrn 021682381, date 2016-07-14 to 2016-07-15
# mrn 100617086, date 2016-07-08 to 2016-07-02
# mrn 100499379, date 2016-06-16 to 2016-06-20
# mrn 017564377, date 2016-11-23 to 2016-11-24
# mrn 039437570, date 2017-02-18 to 2017-02-20
# mrn 037037771, date 2017-01-23 to 2017-01-24
# mrn 031332060, date 2016-07-18 to 2016-07-25
# mrn 100411432, date 2016-09-03 to 2016-09-04
# mrn 039946208, date 2016-07-08 to 2016-07-10

#make changes to 9 records in jul2
jul2 %>% 
  mutate(stool_date = replace(stool_date, which(stool_date=="2016-07-14" & mrn=="021682381"), "2016-07-15")) %>% 
  filter(mrn=="021682381") %>% 
  select(mrn, stool_date) #shows result

jul2 %>% 
  mutate(stool_date = replace(stool_date, which(stool_date=="2016-07-08" & mrn=="100617086"), "2016-07-02")) %>% 
  filter(mrn=="100617086") %>% 
  select(mrn, stool_date) #shows result


dfclin <- dfclin[1:nrow(dfclin),] 
dfclin$RESULT_TEXT <- str_extract(dfclin$RESULT_TEXT, "Campylobacter [\\s\\S]*Sapovirus [\\s\\S]*etected")
#cleans out junk - now only results
#clean out "\r\r" characters
dfclin$RESULT_TEXT <- str_replace_all(dfclin$RESULT_TEXT, "\r\r", "\n")

dfclin$RESULT_TEXT <- str_replace_all(dfclin$RESULT_TEXT, "\n \n", " \n ")
# cleans out extra newline after Campylobacter

#problem case 214 with \n after C diff - fix by replacing with another negative result
dfclin$RESULT_TEXT[214] <- dfclin$RESULT_TEXT[213]

dfclin$RESULT_TEXT <- str_replace_all(dfclin$RESULT_TEXT, "Detected", "NDetected")
#attach N to beginning of detected, so can do split
 
dfclin<- dfclin %>% separate_rows(RESULT_TEXT, sep="\n") 
# splits on newlines, then unnests

dfclin$RESULT_TEXT <- str_trim(dfclin$RESULT_TEXT) # trim for Norovirus

dfclin <- separate(dfclin, RESULT_TEXT, c("bug","detect"), sep=" N") #note watch out for Norovirus!!
#separate into two columns

#make stool_date into a proper date
dfclin$stool_date <- dmy(dfclin$stool_date)

#unpack clinical result text
dfclin$detect <- str_replace_all(dfclin$detect, "ot", "Not") # add back N to Not
dfclin$bug <- str_trim(dfclin$bug) #trims off extra spaces
dfclin$result <- 0
dfclin$result[dfclin$detect=="Not Detected"]<- 0
dfclin$result[dfclin$detect=="Not applicable"]<- 0
dfclin$result[dfclin$detect=="Detected"]<- 1
dfclin$result[dfclin$detect=="Detected - E.coli non-O157"]<- 1

dfclin <- filter(dfclin, bug != "NA") %>% select(c(mrn, date_stool, pathogen = bug, result))

#check dfclin
dim(dfclin)
names(dfclin)
glimpse(dfclin)
sum(is.na(dfclin))
summary(dfclin)
janitor::tabyl(dfclin$pathogen)
janitor::tabyl(dfclin$result)

#check for and eliminate duplicates



#now spread
dfclin %>% spread(pathogen, result) -> dfclin
#need to create O157 ==0 results column for each mrn

#retest after spread
sum(is.na(dfclin))

saveRDS(dfclin, "cdr_biofire.rds")


## Prepare jul_biofire.rds   =====================================
##
##
#rm(list=ls())
library(stringr)
library(readxl)
library(janitor)
library(tidyverse)
library(lubridate)
library(readr)
library(pdftools)

jul1 <- read_excel("~/Documents/Rcode/biofire/jul_biofire/BIOFIRE_Michigan_full (FINAL).xlsx")
jul1 <- jul1 %>% clean_names() 

#jul1 <- jul1[1:456, ] # note that this is a workaround as filtering out NA for number is not working
# assumes all should have number
jul1 <- jul1 %>% filter(!is.na(number))

jul1$mrn <- str_pad(jul1$mrn, width=9, side="left", pad="0")
#jul1$stool_date = jul1$date_stool #note that this is a workaround as rename not working
jul1 <- jul1 %>% rename(stool_date = date_stool)
jul1$stool_date <- dmy(jul1$stool_date) #convert to date

#check jul1
dim(jul1)
names(jul1)
glimpse(jul1)
#sum(is.na(jul1))
#summary(jul1)

sum(is.na(jul1$number)) #none missing
sum(is.na(jul1$subject_id)) # note 176 missing a subject_id !!!
sum(is.na(jul1$dob)) # note 69 missing a dob !!!
sum(is.na(jul1$stool_date)) # no missing stool_date

jul1 %>% filter(!is.na(mrn)) %>% get_dupes(mrn) #now not working
#jul1 %>% get_dupes(mrn)
# note one duplicate subject = MRN 100328668, on Jan 23 and Jan 24 2017. Not given subject_id for Jan24
juldupe <- jul1 %>% filter(mrn == "100328668" & number==213)
jul3 <- anti_join(jul1,juldupe)
jul3 %>%  filter(!is.na(mrn)) %>% get_dupes(mrn)
# now no duplicate MRNs!!!

janitor::tabyl(jul3$group) # six groups
janitor::tabyl(jul3$sex) # note 15 NA for sex

tabyl(jul3$presentation_wk)
tabyl(jul3$type_of_ibd)
#crosstab(jul1$type_of_ibd, jul1$group, show_na = T, percent = 'none')

saveRDS(jul3,"~/Documents/Rcode/biofire/jul_biofire/jul_biofire.rds")
jul_biofire <- readRDS("~/Documents/Rcode/biofire/jul_biofire/jul_biofire.rds")

# Merging -----------------------------------------------------------------
#start by adding lab generated biofire results as row at rightward end of jul_biofire
left_join(jul_biofire, lab_biofire, by = "subject_id")

## Analysis --------------------

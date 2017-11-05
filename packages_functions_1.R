#Packages and functions for analysis of BioFire data.

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



#Annotated version of clean_biofire

lns<- readLines(file)
#print (lns)

#read file to dataframe
df <- read.csv(text=lns, stringsAsFactors=FALSE, skip = 8L, strip.white = TRUE)

# extract Sample ID
id <- df[1,]
df$sampid <- as.numeric(str_trim(str_sub(id,15,18))) 
#captures 4 digits as some are 3, some are 4, then trims and makes numeric

# Get RUNDATE, put in new col
df$rundate <- dmy(str_sub(id,-11,-1))
# subset last 11 chars, convert to date with dmy in lubridate

#replace N/A if present in Ecoli 0157:H7 row with grep logical
df$Run.Summary[grepl("N/A", df$Run.Summary)]<- "Not Detected E. coli O157"

# extract result to result column - TRUE if not equal to "Not Detected"
df$result <- !grepl("Not Detected", df$Run.Summary)

#determine how many header rows to drop - TRUE results - dummy rows, then divide by 2 as pos results listed twice = numpos
#then add 4 to numpos to get # of header rows
numpos <- (sum(df$result)-13)/2
delrows<-numpos +4

#drop header rows
df<-df[delrows:nrow(df),] # keeps from delrows to end

#filter out rows with no results
df <-filter(df, grepl("Detected ", Run.Summary)) #filters to keep only those with the word "Detected"

df <-df%>% separate(Run.Summary, c("detect","bug"), sep="ed ")
df$detect<- paste0(df$detect, "ed")
df$pathogen <-str_trim(df$bug)
#clean up Run.Summary (pathogen) column - removed detect/not detct, trim spaces
#df$pathogen <-df$Run.Summary %>% str_replace("Detected ", " ") %>% str_replace("Not ", " ") %>% str_trim()
df <-df%>% select(sampid, rundate, pathogen, result, detect) #keep only interesting columns, in order


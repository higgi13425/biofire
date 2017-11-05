## load libraries and functions  ----------------------------------------------
##
##
##
#Packages and functions for analysis of BioFire data.
#clean up the environment first
rm(list=ls())

#Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)


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


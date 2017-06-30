#read in BIOFIRE RESULTS txt data & parse
#file.choose to pick txt file
library(stringr)
library(qdap)
library (tidyverse)

# note - have all files in a single folder
# read list of files into a vector
# iterate over for(i in 1:nrow(vector)) {read, process, clean up, save rds files}


file <- "FilmArray_Run_Date_2017_06_27_Sample_153_SN_09937928.txt"
lns<- readLines(file)
#print (lns)

#read file to dataframe
df <- read.csv(text=lns,  stringsAsFactors=FALSE, skip = 8L, strip.white = TRUE)

# extract Sample ID
id <- head (df,1)
print(id)
sampid <- substr (id, 15, 17)
print (sampid)
df$sampid <- sampid


# Get RUNDATE, put in new col
rundate <- sub('.*:', '', id)
print (rundate) # ARRRGH has extra quotes!!!!
df$rundate <- rundate

#replace N/A if present in Ecoli 0157:H7 row
df$Run.Summary[grepl("N/A", df$Run.Summary)]<- "Not Detected E. coli O157"

# extract result to result column
df$result <- !grepl("Not Detected", df$Run.Summary)

#determine how many header rows to drop
numpos <- (sum(df$result)-13)/2
delrows<-numpos +4

#drop header rows
df<-df[delrows:nrow(df),] # NOTE - this only works if 2 positives. Need to make robust to varying # of positives

#filter out rows with no results
df <-filter(df, grepl("Detected ", Run.Summary))

#clean up Run.Summary (pathogen) column
df$pathogen <-df$Run.Summary %>% str_replace("Detected ", " ") %>% str_replace("Not ", " ") %>% str_trim()
df <-df%>% select(sampid, rundate, pathogen, result)





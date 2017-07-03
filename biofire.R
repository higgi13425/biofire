#read in BIOFIRE RESULTS txt data & parse
#start with libraries
library(tidyverse)
library(stringr)
library(lubridate)

# note for future task - have all .txt files in a single folder
# read list of files into a vector
files <- list.files(path = "/Users/peterhiggins/Documents/Rcode/biofire", pattern = "*.txt")
# iterate over for(i in 1:length(vector)) {read file, process, clean up, rbind to big datafile, empty df}

#two test files
file <- "FilmArray_Run_Date_2017_06_27_Sample_153_SN_09937928.txt"
#file <- "FilmArray_Run_Date_2017_06_28_Sample_301_SN_09937893.txt"
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





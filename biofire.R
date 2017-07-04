#read in BIOFIRE RESULTS txt data & parse
#start with libraries
library(tidyverse)
library(stringr)
library(lubridate)

# note for future task - have all .txt files in a single folder

# read list of files into a vector to establish length
files <- list.files(path = "/Users/peterhiggins/Documents/Rcode/biofire", pattern = "*.txt")

#create clean_biofire function
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

# #set up data frame - allocate space
# fulldata <- function(n){
#   sampid <- numeric(n)
#   rundate <- Date(n)
#   pathogen <-character(n)
#   result <- logical(n)
#   detect <- character(n)
#   for i in 1:files{
#     fulldata$sampid[i]<-i
#     fulldata$rundate[i]<-as.Date(i)
#     fulldata$pathogen[i]<- as.character(i)
#     fulldata$result[i]<- as.logical(i)
#     fulldata$detect[i]<-as.character(i)
#   }
#   data.frame(sampid, rundate, pathogen, result, detect, stringsAsFactors = F)
# }

# read list of files into a vector
files <- list.files(path = "/Users/peterhiggins/Documents/Rcode/biofire", pattern = "*.txt")

#now overwrite dataframe
fulldata <- NULL #create fulldata file, empty

for(file in files){
  df <- clean_biofire(file)
  fulldata <- rbind(fulldata, df)
}
df<- NULL #remove small df
# iterate over for(i in 1:length(vector)) {read file, process, clean up, rbind to big datafile, empty df}

fulldata %>% mutate(group = case_when(.$sampid<1000 ~ round(sampid/100),
                              .$sampid >=1000 ~ round (sampid/1000)))



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





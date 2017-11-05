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

# remove O157, put at end to align with cdr_biofire
#all are zero results anyway
fulldata_spread$`E. coli O157` <- NULL
fulldata_spread$o157 <-0
colnames(fulldata_spread)[23] <- "E. coli O157"

#now save file
saveRDS(fulldata_spread, "/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire.rds")
#test it
lab_biofire <- read_rds("/Users/peterhiggins/Documents/Rcode/biofire/lab_biofire.rds")
##
# lab_biofire.rds now clean, locked, and saved.
## 

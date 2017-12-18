# 05_cdtox_biofire.R
# to bring in, then unnest, then cleanup cdtox data from the cdr
# work to do to find, fix exceptions in this one.
# used str_detect and then move them to true columns
# for antigen vs toxin vs. toxB dna vs. toxBgene

library(tidyverse)
library(readxl)
library(stringr)
library(janitor)

#read in data
biofire_cdtox <- read_excel("Biofire_Cdiff_tox.xlsx")

biofire_cdtox$result <- str_extract(biofire_cdtox$RESULT_TEXT, 
        "RESULT: [\\s\\S]*-----")

sum(is.na(biofire_cdtox$result))
#get 21 NAs


biofire_cdtox <- separate(biofire_cdtox, result, 
            c("result1", "result2", "result3", "result4"), sep = "\n") 
#tested - result5 and >5 were all junk - but it will warn you about additional lines


#inspect what is in these - some junk - hard to inspect
#note lots of problems here - columns mixing antigen and tox, tox B gene vs. toxin

tabyl(biofire_cdtox$result1)
tabyl(biofire_cdtox$result2)
tabyl(biofire_cdtox$result3)
tabyl(biofire_cdtox$result4)
#now to put positives into right variables
# overall_result
# cd_ag_gdh_present
# cd_tox_gene_present
# cd_toxin_present

# set up overall_result
biofire_cdtox$overall_result <- 0
biofire_cdtox$overall_result[str_detect(biofire_cdtox$result1,"RESULT:  \\*\\*POSITIVE")] <- 1
biofire_cdtox$overall_result[str_detect(biofire_cdtox$result1,"RESULT: \\*\\* POSITIVE")] <- 1
biofire_cdtox$overall_result[str_detect(biofire_cdtox$result1,"RESULT: \\*\\*\\* POSITIVE")] <- 1
biofire_cdtox$overall_result[str_detect(biofire_cdtox$result1,"RESULT: POSITIVE,")] <- 1
biofire_cdtox$overall_result[is.na(biofire_cdtox$overall_result)] <- 0

#set up cd_ag_gdh_present
biofire_cdtox$cd_ag_gdh_present <- 0
biofire_cdtox$cd_ag_gdh_present[str_detect(biofire_cdtox$result2,
              "Clostridium difficile antigen PRESENT")] <-1 ## 13 added
biofire_cdtox$cd_ag_gdh_present[str_detect(biofire_cdtox$result2,
              "CLOSTRIDIUM DIFFICILE ANTIGEN PRESENT")] <-1 ## 14 added
biofire_cdtox$cd_ag_gdh_present[str_detect(biofire_cdtox$result3,
              "Clostridium difficile Antigen PRESENT\r$")] <-1 ## 137 added
biofire_cdtox$cd_ag_gdh_present[str_detect(biofire_cdtox$result3,
              "Clostridium dificile Antigen PRESENT\r")] <-1 ## 7 added
biofire_cdtox$cd_ag_gdh_present[str_detect(biofire_cdtox$result3,
              "CLOSTRIDIUM DIFFICILE ANTIGEN PRESENT\r$")] <-1 ## 39 added

#set up cd_tox_gene_present
biofire_cdtox$cd_tox_gene_present <- 0
biofire_cdtox$cd_tox_gene_present[str_detect(biofire_cdtox$result1,
            ", CLOSTRIDIUM DIFFICILE TOXIN B GENE")] <-1
biofire_cdtox$cd_tox_gene_present[str_detect(biofire_cdtox$result2,
            "Clostridium difficile Toxin B Gene PRESENT")] <-1 ## 79
biofire_cdtox$cd_tox_gene_present[str_detect(biofire_cdtox$result2,
            "CLOSTRIDIUM DIFFICILE TOXIN B GENE PRESENT\r$")] <-1 ## 23
biofire_cdtox$cd_tox_gene_present[str_detect(biofire_cdtox$result4,
            "difficile Toxin B DNA DETECTED BY PCR")] <-1 ## should add 79?? not clear why not
biofire_cdtox$cd_tox_gene_present[str_detect(biofire_cdtox$result4,
            "CLOSTRIDIUM DIFFICILE TOXIN B DNA DETECTED BY PCR")] <-1

#set up cd_toxin_present
biofire_cdtox$cd_toxin_present <- 0
biofire_cdtox$cd_toxin_present[str_detect(biofire_cdtox$result1,
          ", CLOSTRIDIUM DIFFICILE TOXIN PRESENT")] <-1 #adds 3
biofire_cdtox$cd_toxin_present[str_detect(biofire_cdtox$result3,
          "Clostridium difficile toxin PRESENT\r$")] <-1  #adds 13
biofire_cdtox$cd_toxin_present[str_detect(biofire_cdtox$result3,
          "  CLOSTRIDIUM DIFFICILE TOXIN PRESENT\r$")] <-1  #adds 3?? should add 6

#now check with crosstabs - total overall result - 1030 negative, 125 pos
crosstab(biofire_cdtox$overall_result, biofire_cdtox$cd_ag_gdh_present)
# 125 pos, all have GDH pos, another 85 are GDH pos but overall neg

crosstab(biofire_cdtox$overall_result, biofire_cdtox$cd_toxin_present)
#cd toxin only found by EIA in 19 of the 125 positives

crosstab(biofire_cdtox$overall_result, biofire_cdtox$cd_tox_gene_present)
#cd tox gene found in the other 106 considered pos

crosstab(biofire_cdtox$cd_toxin_present, biofire_cdtox$cd_tox_gene_present)
# no overlap between groups - makes sense

# clinical question - is there a clinical difference between
# group of 19 with toxin present
# group of 106 with only tcdB gene present
# and how do these overlap with Biofire C diff positives??




# previous attempt
# biofire_cdtox$result <- sapply(biofire_cdtox$result_split, "[", 1)
# biofire_cdtox$toxB_gene <- sapply(biofire_cdtox$result_split,  "[", 2)
# biofire_cdtox$antigen <- sapply(biofire_cdtox$result_split, "[", 3)
# biofire_cdtox$toxB_dna <- sapply(biofire_cdtox$result_split, "[", 4)

# To Do List

# Clean, test, rename vars in fulldata from lab, spread to wide -> lab_biofire.rds
#DONE 

# clean, test, rename vars in CDR data, spread to wide -> cdr_biofire.rds

# merge lab_biofire with BIOFIRE_Michigan_full(FINAL).xls from M+Box, match on sampid = subjectID

# merge cdr_biofire with BIOFIRE_Michigan_full(FINAL).xls from M+Box, match on mrn

#rbind CDR data with lab -> biofire_data

#start analysis Aims

# 1.       Proportions:  What proportion of IBD patients have detectable stool infectious agents, vs IBS and HC?
#see nice barplot code in Julia Silge blog post on gobbledegook
# a.       What proportion of CD vs. UC vs IBS vs HC
# b.      What proportion of Active UC vs inactive UC
# c.       What proportion of Active CD vs inactive CD
# d.      Are proportions different for inpatients vs. outpatients?
# e.      Are proportions (a-d) diff for C diff, coliform bacteria (grouped), viruses (grouped)
# f. note no parasites, no O157
# g. note number with multiple infections, distribution (# with 0,1,2,3)
# h. which infections are present (mosaic plot) in each group?


# 2.       Outcomes: Is presence of an infectious agent in the stool associated with outcomes in IBD?
# a.       Positive associated with steroids, hospitalization, surgery within 90d (vs. negative test) in active IBD?
# (individual outcomes vs. composite of 3 combined as dichotomous outcome)
# i.      In active IBD
# ii.      In inactive IBD
# iii.      C diff alone
# iv.      Other bacteria (grouped)
# v.      Viruses (grouped)
# b.      When there are infectious agents in inactive pts, does that predict future outcome better/worse?
# c.       What were infections in HC group – are these clinically not meaningful?
# d.      Which E coli subtypes are associated with bad outcomes, and which are clinically not meaningful?

# 3.       Is presence of an infectious agent in the stool associated with timing of presentation in IBD?
# a.       Acute (<7 days of Sx) vs. chronic Sx

# 4.       Is presence of an infectious agent in the stool associated with timing of presentation in IBS?
# a.       Acute (<7 days of Sx) vs. chronic Sx

# 5.       Is presence of infectious agent assoc with FCP?
# a.       Can we run FCP on most/all? – we can probably get supplies from Buhlmann
# b.      Does calpro predict outcomes, does infectious agent independently affect prediction after control for FCP?
# c.      is low FCP infection truly colonization - are outcomes less likely vs. high FCP infection?
#             is this true for some infections, not others?

# 6.       Were norovirus cases in a particular time period?
# a.       Did this represent a local pandemic at a period of time, or seasonal?

#7. Does Cdiff biofire PCR pos differ from C diff toxin pos - when discordant?
# a. diff FCP if discordant?
# b. diff outcome if discordant?




############
# The data will come out of the BioFire machine and be cleaned to a tidy (tall) format.
# spread to wide - one observation per subject
# We will then do merge (join) to
# 1.       The MiChart derived BioFire results,
# 2.       the clinical data in the excel file and
# 3.       the CDTOX results that Laura extracted from CDW
# 
# Then we can run the analyses, to include
# 
# 1.       Proportions:  What proportion of IBD patients have detectable stool infectious agents, vs IBS and HC?
#   a.       What proportion of CD vs. UC vs IBS vs HC
#   b.      What proportion of Active UC vs inactive UC
#   c.       What proportion of Active CD vs inactive CD
#   d.      Are proportions different for inpatients vs. outpatients?
#   e.      Are proportions (a-d) diff for C diff, coliform bacteria (grouped), viruses (grouped)
# 2.       Outcomes: Is presence of an infectious agent in the stool associated with outcomes in IBD?
#   a.       Positive associated with steroids, hospitalization, surgery within 90d (vs. negative test) in active IBD?
#   (individual outcomes vs. composite of 3 combined as dichotomous outcome)
#     i.      In active IBD
#     ii.      In inactive IBD
#     iii.      C diff alone
#     iv.      Other bacteria (grouped)
#     v.      Viruses (grouped)
#   b.      When there are infectious agents in inactive pts, does that predict future outcome better/worse?
#   c.       What were infections in HC group – are these clinically not meaningful?
#   d.      Which E coli subtypes are associated with bad outcomes, and which are clinically not meaningful?
# 3.       Is presence of an infectious agent in the stool associated with timing of presentation in IBD?
#   a.       Acute (<7 days of Sx) vs. chronic Sx
# 4.       Is presence of an infectious agent in the stool associated with timing of presentation in IBS?
#   a.       Acute (<7 days of Sx) vs. chronic Sx
# 5.       Is presence of infectious agent assoc with high FCP?
#   a.       Can we run FCP on most/all? – we can probably get supplies from Buhlmann
#   b.   is high FCP assoc with symptoms / low FCP assoc with carrier state?
#   b.      Does calpro predict outcomes, does infectious agent independently affect prediction?
# 6.       Were norovirus cases in a particular time period?
#   a.       Did this represent a local pandemic at a period of time, or seasonal?

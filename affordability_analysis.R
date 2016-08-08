########## Whos missed out analysis###########
### Characteristics of households who can't afford different housing### 
### options using Family Resources Survey#####
### Author: sara_mahmoud@shelter.org.uk####


## Make flatfiles for each year###
source("make_flatfile.R")
curr_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/Family Resources Survey/"

affdt_1314 <- make.flatfile(curr_dir,"2013-14")
affdt_1415 <- make.flatfile(curr_dir,"2014-15")

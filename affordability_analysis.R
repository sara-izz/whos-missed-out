########## Whos missed out analysis###########
### Characteristics of households who can't afford different housing### 
### options using Family Resources Survey#####
### Author: sara_mahmoud@shelter.org.uk####

#If packages not found, do install.packages("foo")
install.packages("devtools")
require("devtools")
install.packages("sqldf")
#install.packages("data.table")
install_github("Rdatatable/data.table") #Use dev version on Github
install.packages("dplyr")

## Make flatfiles for each year###
source("make_flatfile.R")
curr_data_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/"
results_dir <- "S:/@Communications, Policy and Campaigns/Research/RESEARCH/More Affordable Homes/Social housing attitudes and needs/Needs/"

affdt_1314 <- make.flatfile(curr_data_dir,"1314")
affdt_1415 <- make.flatfile(curr_data_dir,"1415")

## Add year marker to each file
affdt_1314[ , year := 1314]
affdt_1415[ , year := 1415]

## append datasets together, creating missing value variables for each file where columns are missing (set argument fill=TRUE on rbindlist())
l <- list(affdt_1314, affdt_1415)
affdt_1315 <- rbindlist(l, fill = TRUE)

write_sav(affdt_1415, paste0(results_dir, "flatfile_1415.sav"))
write_sav(affdt_1314, paste0(results_dir, "flatfile_1314.sav"))
write_sav(affdt_1315, paste0(results_dir, "flatfile_1315.sav"))

#Get rid of non-England rows
#setkey(affdt_1415, GVTREGN)
# eng_regn <- c(112000001, 112000002, 112000003, 112000004, 112000005, 112000006, 112000007, 112000008, 112000009)
# affdt_1415 <- affdt_1415[.(eng_regn), ] 

#Grab latest ownership thresholds
owner_thresh <- read.csv(paste0(results_dir,"20160815_UdatedOwnershipThresholds.csv"), stringsAsFactors = FALSE)
owner_thresh <- data.table(owner_thresh)
#Recode to match new govt region codes
owner_thresh$region[owner_thresh$region == "South East"]<- as.numeric(112000008)
owner_thresh$region[owner_thresh$region == "North East"]<- as.numeric(112000001)
owner_thresh$region[owner_thresh$region == "North West"]<- as.numeric(112000002)
owner_thresh$region[owner_thresh$region == "Yorkshire and Humber"]<- as.numeric(112000003)
owner_thresh$region[owner_thresh$region == "East Midlands"]<- as.numeric(112000004)
owner_thresh$region[owner_thresh$region == "West Midlands"]<- as.numeric(112000005)
owner_thresh$region[owner_thresh$region == "East"]<- as.numeric(112000006)
owner_thresh$region[owner_thresh$region == "London"]<- as.numeric(112000007)
owner_thresh$region[owner_thresh$region == "South West"]<- as.numeric(112000009)
owner_thresh$region[owner_thresh$region == "England"]<- as.numeric(92000001)

#rename region 
setnames(owner_thresh, "region", "GVTREGN")
#owner_thresh[ ,GVTREGN := lapply(owner_thresh[ ,GVTREGN], as.numeric)]


write_sav(owner_thresh, paste0(results_dir, "ownership_thresholds.sav"))

# setkey(owner_thresh, GVTREGN)
# setkey(affdt_1415, GVTREGN)
# 
# affdt_1415[owner_thresh, market_thresh := i.market]#, starterm_thresh = i.starter_median, starterlq_thresh = i.starter_lq,
#                               #htb_thresh = i.help_to_buy, shared_thresh = i.shared)]
# 


# #deflate SPI'd gross hhold income to average of survey year
affdt_1315[ year == '1415',hh_gross_inc := ESGINCHH * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1315[ year == '1314',hh_gross_inc := ESGINCHH * BHCDEF * 52 * (100/99)]

#Calculate number of bedrooms needed according to overcrowding definition
#number of cohabiting couples in household requiring rooms
affdt_1315[ , num_cohabita := 0L]
affdt_1315[ , num_cohabitc := 0L]
affdt_1315[ , num_cohabita := sum(COHABITa == '1'), by=.(SERNUM, year)]
affdt_1315[ , num_cohabitc := sum(COHABITc == '1'), by=.(SERNUM, year)]
affdt_1315[ , cohab_rooms :=  (num_cohabita + num_cohabitc)/2]  #doesn't work at the moment because num_cohabitc empty

#Number of single over (here 24, should be 21 but only have banded ages)
affdt_1315[ , single21_rooms := sum((HDAGE > '1' & COHABITa =='2')), by=.(SERNUM, year)]

#Number of pairs of kids under 10 
#Recode AGEc for adults from NA to 999
affdt_1315[ is.na(AGEc), AGEc := 999]
affdt_1315[ , num_kids10 := sum(AGEc <= 10), by=.(SERNUM, year)]
affdt_1315[ , kids10_rooms := floor(num_kids10/2), by=.(SERNUM, year)]

#

# #Attach starter homes threshold (median NB)
# affdt_1415[, startm_thresh := owner_thresh [affdt_1415[,GVTREGN], starter_median]]
# #Create can afford starter homes flag
# affdt_1415[ hh_gross_inc < owner_thresh[affdt_1415[,GVTREGN], ], naffstartm_hhgross := TRUE]
#   
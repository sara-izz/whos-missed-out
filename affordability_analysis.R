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
install.packages("sjPlot")
require(sjPlot)

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

#clean up
rm(affdt_1314)
rm(affdt_1415)

# write_sav(affdt_1415, paste0(results_dir, "flatfile_1415.sav"))
# write_sav(affdt_1314, paste0(results_dir, "flatfile_1314.sav"))
# write_sav(affdt_1315, paste0(results_dir, "flatfile_1315.sav"))

#Get rid of non-England rows
#setkey(affdt_1415, GVTREGN)
# eng_regn <- c(112000001, 112000002, 112000003, 112000004, 112000005, 112000006, 112000007, 112000008, 112000009)
# affdt_1415 <- affdt_1415[.(eng_regn), ] 

#Grab latest ownership thresholds
# owner_thresh <- read.csv(paste0(results_dir,"20160815_UdatedOwnershipThresholds.csv"), stringsAsFactors = FALSE)
# owner_thresh <- data.table(owner_thresh)
# #Recode to match new govt region codes
# owner_thresh$region[owner_thresh$region == "South East"]<- as.numeric(112000008)
# owner_thresh$region[owner_thresh$region == "North East"]<- as.numeric(112000001)
# owner_thresh$region[owner_thresh$region == "North West"]<- as.numeric(112000002)
# owner_thresh$region[owner_thresh$region == "Yorkshire and Humber"]<- as.numeric(112000003)
# owner_thresh$region[owner_thresh$region == "East Midlands"]<- as.numeric(112000004)
# owner_thresh$region[owner_thresh$region == "West Midlands"]<- as.numeric(112000005)
# owner_thresh$region[owner_thresh$region == "East"]<- as.numeric(112000006)
# owner_thresh$region[owner_thresh$region == "London"]<- as.numeric(112000007)
# owner_thresh$region[owner_thresh$region == "South West"]<- as.numeric(112000009)
# owner_thresh$region[owner_thresh$region == "England"]<- as.numeric(92000001)
# 
# #rename region 
# setnames(owner_thresh, "region", "GVTREGN")
# #owner_thresh[ ,GVTREGN := lapply(owner_thresh[ ,GVTREGN], as.numeric)]
# 
# 
# write_sav(owner_thresh, paste0(results_dir, "ownership_thresholds.sav"))

# setkey(owner_thresh, GVTREGN)
# setkey(affdt_1415, GVTREGN)
# 
# affdt_1415[owner_thresh, market_thresh := i.market]#, starterm_thresh = i.starter_median, starterlq_thresh = i.starter_lq,
#                               #htb_thresh = i.help_to_buy, shared_thresh = i.shared)]
# 



#Calculate number of bedrooms needed according to LHA definition
#see http://england.shelter.org.uk/get_advice/housing_benefit_and_local_housing_allowance/what_is_housing_benefit/local_housing_allowance
#number of couples in household requiring rooms

#recode NA relationship variables to 999
affdt_1315[ is.na(R01), R01 := 999 ]
affdt_1315[ is.na(R02), R02 := 999 ]
affdt_1315[ is.na(R03), R03 := 999 ]
affdt_1315[ is.na(R04), R04 := 999 ]
affdt_1315[ is.na(R05), R05 := 999 ]
affdt_1315[ is.na(R06), R06 := 999 ]
affdt_1315[ is.na(R07), R07 := 999 ]
affdt_1315[ is.na(R08), R08 := 999 ]
affdt_1315[ is.na(R09), R09 := 999 ]
affdt_1315[ is.na(R10), R10 := 999 ]
affdt_1315[ is.na(R11), R11 := 999 ]
affdt_1315[ is.na(R12), R12 := 999 ]
affdt_1315[ is.na(R13), R13 := 999 ]
affdt_1315[ is.na(R14), R14 := 999 ]
#flag if relationship with anyone in the hhld is spouse (1) or cohabitee (2)
affdt_1315[ , ispart_couple := ifelse((R01 == 1 | R02 == 1 | R03 == 1 | R04 == 1 | R05 == 1 | R06 == 1 | R07 == 1 | R08 == 1 |
                                         R09 == 1 | R10 == 1 | R11 == 1 | R12 == 1 | R13 == 1 | R14 == 1 |R01 == 2 | R02 == 2 | 
                                         R03 == 2 | R04 == 2 | R05 == 2 | R06 == 2 | R07 == 2 | R08 == 2 | R09 == 2 | R10 == 2 | 
                                         R11 == 2 | R12 == 2 | R13 == 2 | R14 == 2 ), 1, 0)]

affdt_1315[ , num_cohabit := 0L]
affdt_1315[ , num_cohabit := sum(ispart_couple == '1'), by=.(SERNUM, year)]
affdt_1315[ , cohab_rooms :=  num_cohabit/2]  


#Number of single over 16
affdt_1315[ , single16_rooms := sum(( (IsChild == 'FALSE' | AGEc >=16) & ispart_couple =='0')), by=.(SERNUM, year)]

#Number of pairs of kids under 10 
#Recode AGEc for adults from NA to 999
affdt_1315[ is.na(AGEc), AGEc := 999]
affdt_1315[ , num_kids10 := sum(AGEc < 10), by=.(SERNUM, year)]
affdt_1315[ , kids10_rooms := floor(num_kids10/2), by=.(SERNUM, year)]

#Flag if left over kid
affdt_1315[ , spare_kid10 := FALSE]
affdt_1315[ , spare_kid10 := ifelse (num_kids10 %% 2 != 0, TRUE, FALSE)]

#Number of pairs of girls 10-16
affdt_1315[ , num_girls1016 := sum( (AGEc >= 10 & AGEc < 16) & SEXc == '2'), by=.(SERNUM, year)]
affdt_1315[ , girls1016_rooms := floor(num_girls1016/2), by=.(SERNUM, year)]

#Flag if left over girl 10-16
affdt_1315[ , spare_girl1016 := FALSE]
affdt_1315[ , spare_girl1016 := ifelse (num_girls1016 %% 2 != 0, TRUE, FALSE)]

#Number of pairs of boys 10-16
affdt_1315[ , num_boys1016 := sum( (AGEc >= 10 & AGEc < 16) & SEXc == '1'), by=.(SERNUM, year)]
affdt_1315[ , boys1016_rooms := floor(num_boys1016/2), by=.(SERNUM, year)]

#Flag if left over boy 10-16
affdt_1315[ , spare_boy1016 := FALSE]
affdt_1315[ , spare_boy1016 := ifelse (num_boys1016 %% 2 != 0, TRUE, FALSE)]

#### Extra rooms for leftover kids
affdt_1315[ , num_extrarooms := 0]

# Sexes of kids under 10
affdt_1315[ ,isgirl_under10 := ifelse((SEXc == '2' & AGEc < 10), 1, 0)]
affdt_1315[ ,isboy_under10 := ifelse((SEXc == '1' & AGEc < 10), 1, 0)]
affdt_1315[ , num_girlunder10 := sum(isgirl_under10), by =.(SERNUM, year)]
affdt_1315[ , num_boyunder10 := sum(isboy_under10), by =.(SERNUM, year)]

## First try and assign spare 10-16 to spare kids under 10, add on room for them if can't
#Cases where there is a spare kid under 10, spare girl 10-16 and one of kids under 10 isn't a girl -> 1 extra room for spare girl 10-16
affdt_1315[ (spare_kid10 == 'TRUE' & spare_girl1016 == 'TRUE'), 
            num_extrarooms := ifelse( num_girlunder10 == 0, num_extrarooms + 1, num_extrarooms), 
            by=.(SERNUM, year)]
#Cases where there is a spare kid under 10, spare boy 10-16 and one of kids under 10 isn't a boy -> 1 extra room for spare girl 10-16
affdt_1315[ (spare_kid10 == 'TRUE' & spare_boy1016 == 'TRUE'), 
            num_extrarooms := ifelse(num_boyunder10 == 0, num_extrarooms + 1, num_extrarooms), 
            by=.(SERNUM, year)]


## Now add extra rooms for cases where can match spare 10-16 to spare kids under 10
affdt_1315[ (spare_kid10 == 'TRUE' & spare_girl1016 == 'TRUE'), 
            num_extrarooms := ifelse( num_girlunder10 > 0, num_extrarooms + 1, num_extrarooms), 
            by=.(SERNUM, year)]


affdt_1315[ (spare_kid10 == 'TRUE' & spare_boy1016 == 'TRUE'), 
            num_extrarooms := ifelse( num_boyunder10 > 0, num_extrarooms + 1, num_extrarooms), 
            by=.(SERNUM, year)]


## Now add extra rooms for cases where can't match spare kid under 10 to spare girl or boy 10-16
affdt_1315[ (spare_kid10 == 'TRUE' & (num_girlunder10 == 0)), 
            num_extrarooms := ifelse((spare_boy1016 == FALSE & spare_girl1016 == TRUE), num_extrarooms + 1, num_extrarooms),
            by=.(SERNUM, year)]


affdt_1315[ (spare_kid10 == 'TRUE' & (num_boyunder10 == 0)), 
            num_extrarooms := ifelse((spare_girl1016 == FALSE & spare_boy1016 == TRUE), num_extrarooms + 1, num_extrarooms),
            by=.(SERNUM, year)]


affdt_1315[ ( spare_kid10 == 'TRUE' & spare_girl1016 == 0 & spare_boy1016 == 0 ), 
            num_extrarooms := num_extrarooms + 1,
            by=.(SERNUM, year)]


### Add all rooms together
affdt_1315[ , bedrooms_needed := cohab_rooms + single16_rooms + kids10_rooms + girls1016_rooms + boys1016_rooms + num_extrarooms]

############# Attach 14/15 private rent for number of bedrooms needed
#recode number of bedrooms needed to match
affdt_1315[ bedrooms_needed == 1, bedrooms_rent := 1]
affdt_1315[ bedrooms_needed == 2, bedrooms_rent := 2]
affdt_1315[ bedrooms_needed == 3, bedrooms_rent := 3]
affdt_1315[ bedrooms_needed >= 4, bedrooms_rent := 4]

voa_rents_1415 <- read.csv(paste0(results_dir,"VOA_privaterents_1415.csv"))
voa_rents_1415 <- data.table(voa_rents_1415)


rent_keycols = c("GVTREGN", "bedrooms_rent")
voa_cols = c("GVTREGN", "bedrooms_rent","rent_mean", "rent_lq", "rent_med", "rent_uq")

setkeyv(affdt_1315, rent_keycols)
setkeyv(voa_rents_1415, rent_keycols)

affdt_1315 <- merge(affdt_1315 , voa_rents_1415[ , .SD, .SDcols = voa_cols], by = rent_keycols, all.x = TRUE)

############# Flag for households with more than 1 benefit unit
affdt_1315[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
affdt_1315[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]

############# Calculate deflated household incomes #######################
## simple gross household income
#deflate SPI'd gross hhold income to average of survey year
affdt_1315[ year == '1415',hh_grossinc := ESGINCHH * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1315[ year == '1314',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/99)]

## gross household income minus HB (following definiton only suitable for households with 1 ben unit)
#deflate SPI'd gross hhold income to average of survey year
affdt_1315[ (year == '1415' & multiple_benu == 0), hh_grossinc_nohb := (ESGINCHH - HBENBU) * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1315[ (year == '1314' & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/99)]

############# Flags for afford rent ########################

#select England only
affdt_1315 <- affdt_1315[ (GVTREGN != 299999999 & GVTREGN != 399999999 & GVTREGN != 499999999),  ]

#flag for don't have HBAI variables
affdt_1315[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]

# flag if can afford median market rent, using gross household income
affdt_1315[ , amount_canaff_g := (hh_grossinc*0.3)/12]
affdt_1315[ nohbai_inc == 0, aff_rentmed_g := ifelse(   amount_canaff_g >= rent_med, 1, 0 )]
affdt_1315[ nohbai_inc == 0, aff_rentlq_g := ifelse(   amount_canaff_g >= rent_lq, 1, 0 )]

# flag is can afford median market rent, using gross household income
affdt_1315[ multiple_benu == 0, amount_canaff_gnohb := (hh_grossinc_nohb*0.3)/12]
affdt_1315[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed_gnohb := ifelse(   amount_canaff_gnohb >= rent_med, 1, 0 )]
affdt_1315[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq, 1, 0 )]

############ Table fun! #################################
#Restrict to houses with only one benefit unit so can use GS_NEWHH to weight
affdt_1315_1BU <- affdt_1315[ multiple_benu == 0, ]

#Add 2 year hhd weight
affdt_1315_1BU[ , hhweight_2 := GS_NEWHH/2]

#Private renters who can't afford median market rent

#with HB
affdt_1315_1BU[ ( PTENTYP2 == 3 | PTENTYP2 == 4 ), sjt.frq(aff_rentmed_g, weight.by = hhweight_2)]
affdt_1315_1BU[ ( PTENTYP2 == 3 | PTENTYP2 == 4 ), sjt.frq(aff_rentlq_g, weight.by = hhweight_2)]

#without HB
affdt_1315_1BU[ ( PTENTYP2 == 3 | PTENTYP2 == 4 ), sjt.frq(aff_rentmed_gnohb, weight.by = hhweight_2)]
affdt_1315_1BU[ ( PTENTYP2 == 3 | PTENTYP2 == 4 ), sjt.frq(aff_rentlq_gnohb, weight.by = hhweight_2)]



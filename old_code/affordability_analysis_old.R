########## Whos missed out analysis###########
### Characteristics of households who can't afford ownership, with a focus on shared ownership### 
### Uses Family Resources Survey#####
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
require(ggplot2)
require(RColorBrewer)
install.packages("reshape2")
library("reshape2")
install.packages("survey")
require("survey")


## Make flatfiles for each year####
source("make_flatfile.R")
curr_data_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/"
derived_data_dir <- "~/R analysis/whos_missed_out/derived data sets/"
results_dir <- "S:/@Communications, Policy and Campaigns/Research/RESEARCH/More Affordable Homes/Social housing attitudes and needs/Needs/"


affdt_1314 <- make.flatfile(curr_data_dir,"1314")
affdt_1415 <- make.flatfile(curr_data_dir,"1415")
affdt_1213 <- make.flatfile(curr_data_dir,"1213")

## Add year marker to each file
affdt_1314[ , year := 1314]
affdt_1415[ , year := 1415]
affdt_1213[ , year := 1213]

## append datasets together, creating missing value variables for each file where columns are missing (set argument fill=TRUE on rbindlist())
l <- list(affdt_1314, affdt_1415)
affdt_1315 <- rbindlist(l, fill = TRUE)

l_more <- list(affdt_1213, affdt_1314, affdt_1415)
affdt_1215 <- rbindlist(l_more, fill = TRUE)

#clean up
rm(affdt_1213)
rm(affdt_1314)
rm(affdt_1415)
rm(l)
rm(l_more)



############ Attach 14/15 ownership product thresholds####
owner_thresh <- read.csv(paste0(derived_data_dir,"20160815_UdatedOwnershipThresholds.csv"), stringsAsFactors = FALSE)
owner_thresh <- data.table(owner_thresh)

own_keycols = c("GVTREGN")
own_cols = c("GVTREGN", "starter_med", "starter_lq", "helptobuy", "shared", "starter_mdeposit",  "starter_lqdeposit", 
             "helptobuy_deposit", "shared_deposit" )

setkeyv(affdt_1215, own_keycols)
setkeyv(owner_thresh, own_keycols)

affdt_1215 <- merge(affdt_1215 , owner_thresh[ , .SD, .SDcols = own_cols], by = own_keycols, all.x = TRUE)

## select England only, stuff to make weights work ####
affdt_1215 <- affdt_1215[ (GVTREGN != 299999999 & GVTREGN != 399999999 & GVTREGN != 499999999),  ]

#flag for don't have HBAI variables
affdt_1215[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]
## Flag for households with more than 1 benefit unit
affdt_1215[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
affdt_1215[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]

#Flag for is HRP - can also be used to collapse dataset down to household level for household level variables
affdt_1215[ , is_HRP := ifelse( PERSON == HRPNUM, 1, 0) ]

#Add 2 year hhd weight
#affdt_1315_1BU[ , hhweight_2 := GS_NEWHH/2]
affdt_1215[ , hhweight_3 := GS_NEWHH/3]

## Tidy up region, create grouped regions ####
affdt_1215[ , GVTREGN := factor(GVTREGN, labels = c("North East", "North West", "Yorkshire and Humber", "East Midlands",
                                                        "West Midlands", "East", "London", "South East", "South West"))]

#Create some grouped regions to get decent sample breakdowns
affdt_1215[ , grpd_region1 := 0.0]
affdt_1215[ , grpd_region1 := ifelse(GVTREGN=="North East" | GVTREGN=="North West" | GVTREGN=="Yorkshire and Humber", 
                                         1.0, grpd_region1 )]
affdt_1215[ , grpd_region1 := ifelse(GVTREGN=="West Midlands" | GVTREGN=="South West" , 
                                         2.0, grpd_region1 )]
affdt_1215[ , grpd_region1 := ifelse(GVTREGN=="East Midlands" | GVTREGN=="East" , 
                                         3.0, grpd_region1 )]
affdt_1215[ , grpd_region1 := ifelse(GVTREGN=="London" | GVTREGN=="South East" , 
                                         4.0, grpd_region1 )]

affdt_1215[ , grpd_region1 := factor(grpd_region1, labels = c("North", "West", "East", "London & S. East"))]


#### Logical flags for later analysis##########

##Has at least one school age kids
affdt_1215[ ,  has_kids16 := ifelse( (sum(AGEc <= 16) >0) , 1, 0 ), by=.(SERNUM, year)]

## Lone parent flag
affdt_1215[ , single_parent := 0]
affdt_1215[ , single_parent := ifelse((BUKIDS == 5 | BUKIDS == 6 | BUKIDS == 7 | BUKIDS == 8), 1, 0)]
affdt_1215[ , single_parent := ifelse(DEPCHLDH == 0, 0, single_parent)]

## single mum flag
affdt_1215[ , single_mum := 0]
affdt_1215[ , single_mum := ifelse((single_parent==1 & PERSON == HRPNUM & SEXa==2), 1, 0)]

## Has at least one disabled adult
affdt_1215[ ,  has_disabledad := ifelse( DISCHHA1 > 0 , 1, 0 ), by=.(SERNUM, year)]
## Has at least one disabled child
affdt_1215[ ,  has_disabledch := ifelse( DISCHHC1 > 0 , 1, 0 ), by=.(SERNUM, year)]
## Has at least one pensioner
affdt_1215[ ,  has_pensioner := FALSE]
affdt_1215[ ,  has_pensioner := ifelse( sum(HDAGE==6) >0 , TRUE, FALSE ), by=.(SERNUM, year)]
## Head of household is pensioner
affdt_1215[ ,  head_pensioner := ifelse( (PERSON==HRPNUM & HDAGE==6) , 1, 0 ), by=.(SERNUM, year)]

#flag someone working
affdt_1215[ is.na(ECOBU), ECOBU := 999]
affdt_1215[ ,  has_working := 0]
affdt_1215[ ,  has_working := ifelse( (ECOBU != 6 & ECOBU != 7 & ECOBU != 8) , 1, 0 ), by=.(SERNUM, year)]

#Caring responsibilities
affdt_1215[ is.na(WHOLOO01), WHOLOO01 := 999]
affdt_1215[ is.na(WHOLOO02), WHOLOO02 := 999]
affdt_1215[ is.na(WHOLOO03), WHOLOO03 := 999]
affdt_1215[ is.na(WHOLOO04), WHOLOO04 := 999]
affdt_1215[ is.na(WHOLOO05), WHOLOO05 := 999]
affdt_1215[ is.na(WHOLOO06), WHOLOO06 := 999]
affdt_1215[ is.na(WHOLOO07), WHOLOO07 := 999]
affdt_1215[ is.na(WHOLOO08), WHOLOO08 := 999]
affdt_1215[ is.na(WHOLOO09), WHOLOO09 := 999]
affdt_1215[ is.na(WHOLOO10), WHOLOO10 := 999]
affdt_1215[ is.na(WHOLOO11), WHOLOO11 := 999]
affdt_1215[ is.na(WHOLOO12), WHOLOO12 := 999]
affdt_1215[ is.na(WHOLOO13), WHOLOO13 := 999]
affdt_1215[ is.na(WHOLOO14), WHOLOO14 := 999]

affdt_1215[ , has_carer := FALSE]
affdt_1215[ , has_carer := ifelse (sum(WHOLOO01 == 1 | WHOLOO02 == 1 | WHOLOO03 == 1 | WHOLOO04 == 1 | WHOLOO05 == 1 | WHOLOO06 == 1 | 
                                         WHOLOO07 == 1 | WHOLOO08 == 1 |WHOLOO09 == 1 | WHOLOO10 == 1 | WHOLOO11 == 1 | WHOLOO12 == 1 | 
                                         WHOLOO13 == 1 | WHOLOO14 == 1 ) > 0 , TRUE, FALSE), by=.(SERNUM, year)]

##Flags to try and establish need to be in area
#Kids looked after by someone outside household -simpler version
affdt_1215[ is.na(R01), R01 := 999]
affdt_1215[ is.na(R02), R02 := 999]
affdt_1215[ is.na(R03), R03 := 999]
affdt_1215[ is.na(R04), R04 := 999]
affdt_1215[ is.na(R05), R05 := 999]
affdt_1215[ is.na(R06), R06 := 999]
affdt_1215[ is.na(R07), R07 := 999]
affdt_1215[ is.na(R08), R08 := 999]
affdt_1215[ is.na(R09), R09 := 999]
affdt_1215[ is.na(R10), R10 := 999]
affdt_1215[ is.na(R11), R11 := 999]
affdt_1215[ is.na(R12), R12 := 999]
affdt_1215[ is.na(R13), R13 := 999]
affdt_1215[ is.na(R14), R14 := 999]
affdt_1215[ , has_resident_grandparent := ifelse (sum(R01 == 16 | R02 == 16 | R03 == 16 | R04 == 16 | R05 == 16 | R06 == 16 | R07 == 16 |
                                                        R08 == 16 | R09 == 16 | R10 == 16 | R11 == 16 | R12 == 16 | R13 == 16 | 
                                                        R14 == 16) > 0 , 1, 0), by=.(SERNUM, year)]


#kid looked after by non-res parent or ex, friends or neighbours or non-resident grandparent
affdt_1215[ , kid_care_nonres := FALSE]
affdt_1215[ , kid_care_nonres := ifelse( (CHLOOK == 14 | CHLOOK == 19 | (CHLOOK == 13 & has_resident_grandparent == 0)), TRUE, FALSE), 
            by=.(SERNUM, year)]

#recode NA in DEBTFRE variables to 0
affdt_1215[ is.na(DEBTFRE1), DEBTFRE1 := 0]
affdt_1215[ is.na(DEBTFRE2), DEBTFRE2 := 0]
affdt_1215[ is.na(DEBTFRE3), DEBTFRE3 := 0]

#### Calculate deflated household incomes #######################
## simple gross household income
#deflate SPI'd gross hhold income to average of survey year
affdt_1215[ year == '1415',hh_grossinc := ESGINCHH * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1215[ year == '1314',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/99)]
#inflate 1213 to 1415 prices
affdt_1215[ year == '1213',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/96.8)]

## gross household income minus HB (following definiton only suitable for households with 1 ben unit)
#deflate SPI'd gross hhold income to average of survey year
affdt_1215[ (year == '1415' & multiple_benu == 0), hh_grossinc_nohb := (ESGINCHH - HBENBU) * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1215[ (year == '1314' & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/99)]
#inflate 1213 to 1415 prices
affdt_1215[ (year == '1213' & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/96.8)]

## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
affdt_1215[ (year == '1415'), hh_grossinc_noincben := (HHINC - HHIRBEN) * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1215[ (year == '1314'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/99)]
#inflate 1213 to 1415 prices
affdt_1215[ (year == '1213'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/96.8)]




############# Flags for afford sale ########################
#Flag for renter
affdt_1215[ , prs_renter := ifelse(PTENTYP2 == 3 | PTENTYP2 == 4, 1, 0)]

#Flag for eligible for Starter Homes
affdt_1215[ , eligible_starter := ifelse(HDAGE <4, 1, 0)]

# flag if can afford ownership, using gross household income minus income related benefits

affdt_1215[ , aff_starterm_noincben := 0]
affdt_1215[ , aff_starterm_noincben := ifelse( (hh_grossinc_noincben >= starter_med & HDAGE < 4), 1, 0 )]
affdt_1215[ , aff_starterm_noincben := ifelse( HDAGE >3, 2, aff_starterm_noincben)]
#Make into factors
# affdt_1215[ , aff_starterm_noincben := factor(aff_starterm_noincben, levels = c(0, 1, 2), 
#                                               labels = c("Can't afford", "Can afford", "Ineligible"))]

affdt_1215[ , aff_starterlq_noincben := 0]
affdt_1215[ , aff_starterlq_noincben := ifelse( (hh_grossinc_noincben >= starter_lq & HDAGE < 4) , 1, 0 )]
affdt_1215[ , aff_starterlq_noincben := ifelse( HDAGE >3, 2, aff_starterlq_noincben)]
#Make into factors
# affdt_1215[ , aff_starterlq_noincben := factor(aff_starterlq_noincben, levels = c(0, 1, 2), 
#                                               labels = c("Can't afford", "Can afford", "Ineligible"))]

affdt_1215[ , aff_htb_noincben := ifelse( hh_grossinc_noincben >= helptobuy, 1, 0 )]
#Make into factors
# affdt_1215[ , aff_htb_noincben := factor(aff_htb_noincben, levels = c(0, 1), 
#                                                labels = c("Can't afford", "Can afford"))]
affdt_1215[ , aff_shared_noincben := ifelse( hh_grossinc_noincben >= shared, 1, 0 )]
#Make into factors
# affdt_1215[ , aff_shared_noincben := factor(aff_shared_noincben, levels = c(0, 1), 
#                                          labels = c("Can't afford", "Can afford"))]
##Add has deposit variables
affdt_1215[ , has_starterlqdep := ifelse(TOTCAPB3 >= starter_lqdeposit, 1, 0 )]
affdt_1215[ , has_startermdep := ifelse(TOTCAPB3 >= starter_mdeposit, 1, 0 )]
affdt_1215[ , has_htbdep := ifelse(TOTCAPB3 >= helptobuy_deposit, 1, 0 )]
affdt_1215[ , has_shareddep := ifelse(TOTCAPB3 >= shared_deposit, 1, 0 )]

##Add definitely can't save for it in 5 years
# affdt_1215[ , cant_starterlqdep := ifelse( (TOTCAPB3 < (starter_lqdeposit - 600)) & ADDMON == 2, 1, 0 )]
# affdt_1215[ , cant_startermdep := ifelse( (TOTCAPB3 < (starter_mdeposit - 600)) & ADDMON == 2, 1, 0 )]
# affdt_1215[ , cant_htbdep := ifelse( (TOTCAPB3 < (helptobuy_deposit - 600)) & ADDMON == 2, 1, 0 )]
# affdt_1215[ , cant_shareddep := ifelse( (TOTCAPB3 < (shared_deposit - 600)) & ADDMON == 2, 1, 0 )]

#Difference between deposit and savings
affdt_1215[ , diff_starterlqdep := (starter_lqdeposit - TOTCAPB3)]
affdt_1215[ , diff_startermdep := (starter_mdeposit - TOTCAPB3)]
affdt_1215[ , diff_htbdep := (helptobuy_deposit - TOTCAPB3)]
affdt_1215[ , diff_shareddep := (shared_deposit - TOTCAPB3)]

#Flag for financial stress; behind on two debts (rent, utilities, loans) or behind twice or more on any of them individually
affdt_1215[ , behind_debts := ifelse( DEBTFRE1 == 2 | DEBTFRE2 == 2 | DEBTFRE3 == 2 | (DEBTFRE1 == 1 & DEBTFRE2 == 1) | 
                                        (DEBTFRE1 == 1 & DEBTFRE3 == 1) | (DEBTFRE2 == 1 & DEBTFRE3 == 1 ), 1, 0 )]


#Flag for don't have deposit and financial stress
affdt_1215[ , cant_starterlqdep := ifelse( (TOTCAPB3 < (starter_lqdeposit - 1000)) & (ADDMON == 2 | behind_debts==1), 1, 0 )]
affdt_1215[ , cant_startermdep := ifelse( (TOTCAPB3 < (starter_mdeposit - 1000)) & (ADDMON == 2 | behind_debts==1), 1, 0 )]
affdt_1215[ , cant_htbdep := ifelse( (TOTCAPB3 < (helptobuy_deposit - 1000)) & (ADDMON == 2 | behind_debts==1), 1, 0 )]
affdt_1215[ , cant_shareddep := ifelse( (TOTCAPB3 < (shared_deposit - 1000)) & (ADDMON == 2 | behind_debts==1), 1, 0 )]

#flag for shared ownership situation
affdt_1215[ , affanddep_shared := 0]
affdt_1215[ , affanddep_shared := ifelse(aff_shared_noincben==1 & cant_shareddep==1, 1 , 0)]
affdt_1215[ , affanddep_shared := ifelse(aff_shared_noincben==1 & cant_shareddep==0, 2 , affanddep_shared)]
affdt_1215[ , affanddep_shared := ifelse(aff_shared_noincben==0 , 0 , affanddep_shared)]

affdt_1215[ , affanddep_shared := factor(affanddep_shared, levels = c(0, 1, 2), labels = c("Can't afford", "Can afford but can't save deposit", "Can afford and can save or has deposit"))]

############ Table fun! #################################

#Create survey object from data table, limiting to HRP so household weights work. Need to restrict to cases with non-missing weights
deshhd_1215 <- svydesign(ids = ~1, weights = ~ hhweight_3, data = affdt_1215[affdt_1215$is_HRP & 
                                                                               (is.na(affdt_1215$hhweight_3) == F & 
                                                                                  multiple_benu == 0), ])


#Private renters ages
ftable(svyby(formula = ~factor(HDAGE), by = ~GVTREGN, design = subset(deshhd_1215, prs_renter == 1), FUN = svymean))
barplot(svyby(formula = ~factor(HDAGE), by = ~grpd_region1, design = subset(deshhd_1215, prs_renter == 1), FUN = svymean))

barplot(svyby(formula = ~factor(HDAGE), by = ~grpd_region1, design = subset(deshhd_1215, prs_renter == 1), FUN = svymean))

barplot(svymean(~factor(HDAGE), design = subset(deshhd_1215, prs_renter == 1)))
svymean(~factor(HDAGE), design = subset(deshhd_1215, prs_renter == 1))

#### Results for working private renters ####

## Data table and survey of working private renters - defined as head of household employed according to ILO
affdt_1215_prswork <- affdt_1215[ (!is.na(hhweight_3) & multiple_benu == 0 & is_HRP & prs_renter == 1 & DVIL04A == 1 ), ]
des_prs_work <- subset(deshhd_1215, (prs_renter == 1 & DVIL04A == 1))

ggplot(affdt_1215_prswork[GVTREGN == "London"], 
       aes(x = hh_grossinc_noincben, weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_freqpoly(binwidth = 5000) + ggtitle("Gross household income - struggling PRS households") +
  xlim(-100, 100000) #+


ftable(svyby(formula = ~factor(aff_starterlq_noincben), by = ~GVTREGN, design = des_prs_work, FUN = svymean, na.rm = TRUE))
ftable(svyby(formula = ~factor(aff_htb_noincben), by = ~GVTREGN, design = des_prs_work, FUN = svymean, na.rm = TRUE))
ftable(svyby(formula = ~factor(aff_shared_noincben), by = ~GVTREGN, design = des_prs_work, FUN = svymean, na.rm = TRUE))

ftable(svyby(formula = ~factor(aff_shared_noincben), by = ~GVTREGN, design = des_prs_work, FUN = svytotal, na.rm = TRUE))


ftable(svyby(formula = ~factor(aff_shared_noincben), by = ~GVTREGN, design = subset(deshhd_1215, prs_renter==1), FUN = svytotal, na.rm = TRUE))

##Working Private renters who can't afford ownership numbers and proportions ####

affdt_1215_prswork[ , sjt.xtab(aff_starterlq_noincben, GVTREGN,
                                        var.labels=c("Can afford LQ Starter", "Region"), 
                                        weight.by = hhweight_3, 
                                        show.col.prc = TRUE,
                                        use.viewer = FALSE)
                    ]

affdt_1215_prswork[ , sjt.xtab(aff_starterm_noincben, GVTREGN,
                                        var.labels=c("Can afford median Starter", "Region"), 
                                        weight.by = hhweight_3, 
                                        show.col.prc = TRUE,
                                        use.viewer = FALSE)
                    ]



affdt_1215_prswork[ , sjt.xtab((aff_htb_noincben), GVTREGN,
                               var.labels=c("Can afford Help to Buy", "Region"), 
                               value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               use.viewer = FALSE)
                    ]

affdt_1215_prswork[ , sjt.xtab(aff_shared_noincben, GVTREGN,
                               var.labels=c("Can afford Shared Ownership", "Region"), 
                               value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               use.viewer = FALSE)
                    ]


#can't afford anything 

affdt_1215_prswork[ , sjt.xtab(((HDAGE < 5 & aff_starterlq_noincben==1) | aff_htb_noincben==1 | aff_shared_noincben==1), GVTREGN,
                               var.labels=c("Can afford anything", "Region"), 
                               value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               use.viewer = FALSE)
                    ]


#can afford other stuff if can afford shared ownership 
affdt_1215_prswork[ aff_shared_noincben==1 , sjt.xtab(((HDAGE < 5 & aff_starterlq_noincben==1) | aff_htb_noincben==1), GVTREGN,
                               var.labels=c("Can afford anything", "Region"), 
                               value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               use.viewer = FALSE)
                    ]

#income distributions by region ####
ggplot(affdt_1215_prswork, 
       aes(x = hh_grossinc_noincben, colour=factor(aff_shared_noincben), weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
  geom_freqpoly(binwidth = 1000) +  
  xlim(-100, 100000) +
  facet_grid(grpd_region1 ~ .)

svyquantile(~hh_grossinc, design = subset(des_prs_work, aff_shared_noincben ==0), quantile = 0.5, ci = TRUE)
svyquantile(~hh_grossinc, design = subset(des_prs_work, aff_shared_noincben ==1), quantile = 0.5, ci = TRUE)

#incomes all England
svyquantile(~hh_grossinc_noincben, design = subset(des_prs_work, aff_shared_noincben ==0), quantile = 0.5)
svyquantile(~hh_grossinc_noincben, design = subset(des_prs_work, aff_shared_noincben ==1), quantile = 0.5)
svyquantile(~hh_grossinc_noincben, design = des_prs_work, quantile = 0.5)

#incomes by region
t_grossincnoben_naff <- as.data.frame(svyby(~hh_grossinc_noincben, by = ~GVTREGN, 
                                            design = subset(des_prs_work, aff_shared_noincben ==0), FUN = svyquantile, 
                                            quantiles = 0.5, ci = TRUE))

t_grossinc_naff <- as.data.frame(svyby(~hh_grossinc, by = ~GVTREGN, design = subset(des_prs_work, aff_shared_noincben ==0), FUN = svyquantile, 
                                 quantiles = 0.5, ci = TRUE))

t_grossincnoben_aff <- as.data.frame(svyby(~hh_grossinc_noincben, by = ~GVTREGN, 
                                             design = subset(des_prs_work, aff_shared_noincben ==1), FUN = svyquantile, 
                                 quantiles = 0.5, ci = TRUE))

t_grossinc_aff <- as.data.frame(svyby(~hh_grossinc, by = ~GVTREGN, 
                                 design = subset(des_prs_work, aff_shared_noincben ==1), FUN = svyquantile, 
                                 quantiles = 0.5, ci = TRUE))

t_grossincnoben_all <- as.data.frame(svyby(~hh_grossinc_noincben, by = ~GVTREGN, design = des_prs_work, FUN = svyquantile, 
                                 quantiles = 0.5, ci = TRUE))

t_grossinc_all <- as.data.frame(svyby(~hh_grossinc, by = ~GVTREGN, design = des_prs_work, FUN = svyquantile, 
                                 quantiles = 0.5, ci = TRUE))

#Age of households who can't afford shared ####
des_prs_work <-update(des_prs_work, HDAGE = factor(HDAGE))

svymean(~HDAGE, design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~HDAGE, design = des_prs_work)

t_age_prsshare_naff <- as.data.frame( ftable( svyby(formula = ~HDAGE, by = ~grpd_region1, 
                                               design = subset(des_prs_work, aff_shared_noincben == 0), 
                                               FUN = svymean, na.rm = TRUE))
)


t_age_prsshare_naff <- dcast(t_age_prsshare_naff, grpd_region1 + Var3 ~ Var2)
t_age_prsshare_naff$Var3 <- factor(t_age_prsshare_naff$Var3, levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                              labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64",
                                         "65 and over"))

t_age_prs <- as.data.frame( ftable( svyby(formula = ~HDAGE, by = ~grpd_region1, 
                                                    design = des_prs_work, 
                                                    FUN = svymean, na.rm = TRUE))
)


t_age_prs <- dcast(t_age_prs, grpd_region1 + Var3 ~ Var2)
t_age_prs$Var3 <- factor(t_age_prs$Var3, levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                                   labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64",
                                              "65 and over"))

t_age_prsshare_naff$status <- "Can't afford shared ownership"
t_age_prs$status <- "All working PRS"
l <- list(t_age_prsshare_naff, t_age_prs)
t_age <- rbindlist(l, fill = TRUE)


#Plot as points with error bars
ggplot(na.omit(t_age_prsshare_naff), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Prop in category") + ylab("Age") +
  ggtitle("Age of head for PRS households under 55 who can't afford Shared Ownership") + facet_grid(grpd_region1 ~ .)



ggplot(na.omit(t_age), aes(x = Var3, y = svymean, fill = status)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
  geom_bar(stat="identity", position = position_dodge()) +  
  scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
  guides(fill=guide_legend(title=NULL)) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
  xlab("Age") + ylab("Proportion") +
  facet_grid(grpd_region1 ~ .)

##Employment - can't afford shared ownership ####
#Self-reported employment of HRP ####
des_prs_work <-update(des_prs_work, SELFDEMP = factor(SELFDEMP, 
                                                      labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                                                 "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other")))
svymean(~SELFDEMP, design = subset(des_prs_work, aff_shared_noincben == 0))
t_emply_prsshare <- as.data.frame( ftable( svyby(formula = ~SELFDEMP, by = ~grpd_region1, 
                                                 design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                 FUN = svymean, na.rm = TRUE))
)


t_emply_prsshare <- dcast(t_emply_prsshare, grpd_region1 + Var3 ~ Var2)
t_emply_prsshare$Var3 <- factor(t_emply_prsshare$Var3, levels = c("SELFDEMPFull-time", "SELFDEMPPart-time", "SELFDEMPFT self-employed",
                                                                  "SELFDEMPPT self-employed", "SELFDEMPUnemployed", "SELFDEMPStudent", "SELFDEMPFamily home", 
                                                                  "SELFDEMPDisabled", "SELFDEMPRetired", "SELFDEMPOther"),
                                labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                           "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"))
#Plot proportions as stacked bar
ggplot(na.omit(t_emply_prsshare), aes(x = grpd_region1,  y = svymean , ymin = svymean-SE, ymax = svymean+SE, fill = Var3)) +
  geom_bar(stat="identity") +
  #geom_segment( aes(x = GVTREGN, xend = GVTREGN, y = svymean-SE, yend=svymean-SE)) +
  xlab("region") + ylab("Prop in category") +
  ggtitle("Self-reported employment of head for PRS households under 55 who can't afford Shared Ownership") #+ facet_grid(GVTREGN ~ .)

#Plot as points with error bars
ggplot(na.omit(t_emply_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Self-reported situation of head  for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

ggplot(na.omit(t_emply_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Prop in category") + ylab("Employment status") +
  ggtitle("Self-reported employment of head for PRS households under 55 who can't afford Shared Ownership") + facet_grid(grpd_region1 ~ .)


# Employment of whole household ####
des_prs_work <-update(des_prs_work, ECOBU = factor(ECOBU)) 
svymean(~ECOBU, design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~ECOBU, design = des_prs_work)

des_prs_work <-update(des_prs_work, DVIL04A = factor(DVIL04A)) 
svymean(~DVIL04A, design = subset(des_prs_work, aff_shared_noincben == 0))

t_emplhh_prsshare_naff <- as.data.frame( ftable( svyby(formula = ~ECOBU, by = ~grpd_region1, 
                                                    design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                    FUN = svymean, na.rm = TRUE))
)


t_emplhh_prsshare_naff <- dcast(t_emplhh_prsshare_naff, grpd_region1 + Var3 ~ Var2)
t_emplhh_prsshare_naff$Var3 <- factor(t_emplhh_prsshare_naff$Var3, levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5"),
                                      labels = c("Self employed", "All FT", 
                                                 "1 FT, 1 PT",  "1 FT, 1 no work", 
                                                 "No FT, PT"))

t_emplhh_prs <- as.data.frame( ftable( svyby(formula = ~ECOBU, by = ~grpd_region1, 
                                          design = des_prs_work, 
                                          FUN = svymean, na.rm = TRUE))
)


t_emplhh_prs <- dcast(t_emplhh_prs, grpd_region1 + Var3 ~ Var2)
t_emplhh_prs$Var3 <- factor(t_emplhh_prs$Var3, levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5"),
                            labels = c("Self employed", "All FT", 
                                       "1 FT, 1 PT",  "1 FT, 1 no work", 
                                       "No FT, PT"))

t_emplhh_prsshare_naff$status <- "Can't afford shared ownership"
t_emplhh_prs$status <- "All working PRS"
l <- list(t_emplhh_prsshare_naff, t_emplhh_prs)
t_emplhh <- rbindlist(l, fill = TRUE)


ggplot(na.omit(t_emplhh), aes(x = Var3, y = svymean, fill = status)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
  geom_bar(stat="identity", position = position_dodge()) +  
  scale_fill_manual(values=c("#A7A8AA","#FF0000")) +
  guides(fill=guide_legend(title=NULL)) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white") ) +
  xlab("Employment of household") + ylab("Proportion")# +
  #facet_grid(grpd_region1 ~ .)

ggplot(na.omit(t_emplhh_prsshare_naff), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, 
                                            colour = grpd_region1, shape = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  scale_colour_manual(values=c("grey19","firebrick3", "cyan4", "darkorange1")) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"), 
         panel.grid.major = element_line(colour = "grey85")) +
  xlab("Prop in category") + guides(colour=FALSE, shape = FALSE) + ylab("Grouped region")+
  ggtitle("Household employment for working PRS households \nwho can't afford shared ownership") + facet_grid( Var3~ .)


# t_emplhh_prsshare <- as.data.frame( ftable( svyby(formula = ~ECOBU, by = ~grpd_region1, 
#                                                   design = subset(des_prs_work, aff_shared_noincben == 0), 
#                                                   FUN = svymean, na.rm = TRUE))
# )


# t_emplhh_prsshare <- dcast(t_emplhh_prsshare, grpd_region1 + Var3 ~ Var2)
# t_emplhh_prsshare$Var3 <- factor(t_emplhh_prsshare$Var3, levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
#                                                                     "ECOBU8"),
#                                  labels = c("1+ self employed", "Sing/couple all FT", 
#                                             "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
#                                             "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
#                                             "Workless, head/spouse unemployed", "Workless, inactive" ))
# 
# #Plot proportions as grouped bar
# ggplot(na.omit(t_emplhh_prsshare), aes(x = Var3,  y = svymean , ymin = svymean-SE, ymax = svymean+SE)) +
#   geom_bar(stat="identity") +
#   #geom_segment( aes(x = GVTREGN, xend = GVTREGN, y = svymean-SE, yend=svymean-SE)) +
#   xlab("region") + ylab("Prop in category") +
#   ggtitle("Age of head for PRS households under 55 who can't afford Shared Ownership") + facet_grid(. ~ grpd_region1)
# 
# #Plot as points with error bars
# #by region
# ggplot(na.omit(t_emplhh_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
#   geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
#   xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
#   ggtitle("Household employment for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)



#Pretty employment tables

affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(SELFDEMP, grpd_region1,
                                                       var.labels=c("Self-reported employment of HRP", "Region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(DVIL04A, grpd_region1,
                                                       var.labels=c("ILO employment", "Region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(ECOBU, grpd_region1,
                                                       var.labels=c("Household employment status", "Region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

## Children and single parents ####
# School age children 

svytotal(~factor(has_kids16), design = subset(des_prs_work, aff_shared_noincben == 0))
svytotal(~factor(has_kids16), design = des_prs_work)

svymean(~factor(has_kids16), design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~factor(has_kids16), design = des_prs_work)
#can't afford shared
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = des_prs_work, 
              FUN = svymean, na.rm = TRUE))



##single parents
svytotal(~factor(single_parent), design = subset(des_prs_work, aff_shared_noincben == 0))
svytotal(~factor(single_parent), design = des_prs_work)

svymean(~factor(single_parent), design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~factor(single_parent), design = des_prs_work)

#can't afford shared
ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))
#cf all prs working
ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
              design = des_prs_work, 
              FUN = svymean, na.rm = TRUE))

# #can't afford shared - prs working
# ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
#               design = subset(deshhd_1215, (aff_shared_noincben == 0 & prs_renter==1 & has_working == 1 
#               FUN = svymean, na.rm = TRUE))
# #cf all prs working
# ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
#               design = subset(deshhd_1215, ( prs_renter==1 & has_working == 1 
#               FUN = svymean, na.rm = TRUE))

#new stats requested - sort out where to put this
# affdt_1215_1BU[ (  prs_renter & is_HRP& has_working == 1 & aff_shared_noincben == 0), sjt.xtab(single_parent, GVTREGN,
#                                                                    var.labels=c("Single parent", "Region"),
#                                                                    #value.labels = c("Can't afford", "Can afford"),
#                                                                    weight.by = hhweight_3,
#                                                                    show.col.prc = TRUE,
#                                                                    use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & is_HRP& has_working == 1 & aff_shared_noincben == 0), sjt.xtab(has_disabledad, GVTREGN,
#                                                                                               var.labels=c("Single parent", "Region"), 
#                                                                                               #value.labels = c("Can't afford", "Can afford"),
#                                                                                               weight.by = hhweight_3, 
#                                                                                               show.col.prc = TRUE,
#                                                                                               use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & is_HRP& has_working == 1 & aff_shared_noincben == 0), sjt.xtab(has_disabledch | has_disabledad, GVTREGN,
#                                                                                               var.labels=c("Disabled adult or child", "Region"), 
#                                                                                               #value.labels = c("Can't afford", "Can afford"),
#                                                                                               weight.by = hhweight_3, 
#                                                                                               show.col.prc = TRUE,
#                                                                                               use.viewer = FALSE)
#                 ]
# 
# 
 # affdt_1215_1BU[ ( prs_renter & is_HRP& has_working == 1 & aff_shared_noincben == 0), sjt.xtab((ADDMON == 2 | behind_debts==1), GVTREGN,
 #                                                                                               var.labels=c("Financially precarious", "Region"), 
 #                                                                                               #value.labels = c("Can't afford", "Can afford"),
 #                                                                                               weight.by = hhweight_3, 
 #                                                                                               show.col.prc = TRUE,
 #                                                                                               use.viewer = FALSE)
 #                 ]

#by working

affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(factor(single_parent), DVIL04A,
                                                       var.labels=c("Single Parent?", "Employment"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       #show.col.prc = TRUE,
                                                       show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]


ftable( svyby(formula = ~factor(single_mum), by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))

##Disabled adults and children ####
#adults and children
svytotal(~factor(has_disabledad | has_disabledch), design = subset(des_prs_work, aff_shared_noincben == 0))
svytotal(~factor(has_disabledad | has_disabledch), design = des_prs_work)

svymean(~factor(has_disabledad | has_disabledch), design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~factor(has_disabledad | has_disabledch), design = des_prs_work)

#can't afford shared
ftable( svyby(formula = ~factor(has_disabledad | has_disabledch), by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~factor(has_disabledad | has_disabledch), by = ~grpd_region1, 
              design = des_prs_work, 
              FUN = svymean, na.rm = TRUE))

affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(factor(has_disabledad), grpd_region1,
                                                       var.labels=c("Disabled adult?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                     ]

#children
affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(factor(has_disabledch), grpd_region1,
                                                       var.labels=c("Disabled child?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]


##Caring reponsibilities ####
svytotal(~has_carer, design = subset(des_prs_work, aff_shared_noincben == 0), na.rm = TRUE)
svytotal(~has_carer, design = des_prs_work, na.rm = TRUE)

svymean(~has_carer, design = subset(des_prs_work, aff_shared_noincben == 0), na.rm = TRUE)
svymean(~has_carer, design = des_prs_work, na.rm = TRUE)

#can't afford shared
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = des_prs_work, 
              FUN = svymean, na.rm = TRUE))


#care given outside hhld
svytotal(~factor(GIVEHELP), design = subset(des_prs_work, aff_shared_noincben == 0))
svytotal(~factor(GIVEHELP), design = des_prs_work)

svymean(~factor(GIVEHELP), design = subset(des_prs_work, aff_shared_noincben == 0))
svymean(~factor(GIVEHELP), design = des_prs_work)

#can't afford shared
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 0), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = des_prs_work, 
              FUN = svymean, na.rm = TRUE))



#kids looked after by someone outside household - doesn't work; come back to it

# 
# svytotal(~kid_care_nonres, design = subset(des_prs_work, aff_shared_noincben == 0), na.rm = TRUE)
# svytotal(~kid_care_nonres, design = des_prs_work, na.rm = TRUE)
# 
# svymean(~kid_care_nonres, design = subset(des_prs_work, aff_shared_noincben == 0), na.rm = TRUE)
# svymean(~kid_care_nonres, design = des_prs_work, na.rm = TRUE)
# 
# #can't afford shared
# ftable( svyby(formula = ~kid_care_nonres, by = ~grpd_region1, 
#               design = subset(des_prs_work, aff_shared_noincben == 0), 
#               FUN = svymean, na.rm = TRUE))
# #cf all prs under 55
# ftable( svyby(formula = ~kid_care_nonres, by = ~grpd_region1, 
#               design = des_prs_work, 
#               FUN = svymean, na.rm = TRUE))

#Ability to save/behind on debts - can't afford shared ownership ####


#average savings
svyquantile(~TOTCAPB3, design = subset(des_prs_work, aff_shared_noincben ==0), quantile = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE)
svymean(~TOTCAPB3, design = subset(des_prs_work, aff_shared_noincben ==0), ci = TRUE)

#% no savings can't afford
affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(TOTCAPB3 <=0, grpd_region1,
                                                       var.labels=c("No savings?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

#all PRS working
affdt_1215_prswork[ , sjt.xtab(TOTCAPB3 <=0, grpd_region1,
                               var.labels=c("No savings? All PRS", "Grouped region"), 
                               #value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               #show.row.prc = TRUE,
                               use.viewer = FALSE)
                    ]

#Ability to save at least £10 a month
des_prs_work <-update(des_prs_work, ADDMON = factor(ADDMON)) 
t_save10_prsshare <- as.data.frame( ftable( svyby(formula = ~ADDMON, by = ~grpd_region1, 
                                                  design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                  FUN = svymean, na.rm = TRUE))
)


t_save10_prsshare <- dcast(t_save10_prsshare, grpd_region1 + Var3 ~ Var2)
t_save10_prsshare$Var3 <- factor(t_save10_prsshare$Var3, levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                                 labels = c("Do this", "Like to but can't afford", 
                                            "Don't want to",  "Does not apply" ))

#Plot as points with error bars
#by region
ggplot(na.omit(t_save10_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(ADDMON, grpd_region1,
                                                       var.labels=c("Save £10?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]
#all PRS working
affdt_1215_prswork[, sjt.xtab(ADDMON, grpd_region1,
                              var.labels=c("Save £10?", "Grouped region"), 
                              #value.labels = c("Can't afford", "Can afford"),
                              weight.by = hhweight_3, 
                              show.col.prc = TRUE,
                              #show.row.prc = TRUE,
                              use.viewer = FALSE)
                   ]

#Struggling with housing costs
des_prs_work <-update(des_prs_work, BURDEN = factor(BURDEN)) 
t_rentburden_prsshare <- as.data.frame( ftable( svyby(formula = ~BURDEN, by = ~grpd_region1, 
                                                      design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                      FUN = svymean, na.rm = TRUE))
)


t_rentburden_prsshare <- dcast(t_rentburden_prsshare, grpd_region1 + Var3 ~ Var2)
t_rentburden_prsshare$Var3 <- factor(t_rentburden_prsshare$Var3, levels = c("BURDEN1", "BURDEN2", "BURDEN3"),
                                     labels = c("Heavy burden", "Slight burden", 
                                                "Not a burden"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_rentburden_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(BURDEN, grpd_region1,
                                                       var.labels=c("Rent a burden?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

#all PRS under 55
affdt_1215_1BU[ ( prs_renter & is_HRP& (HDAGE < 5) ), sjt.xtab(BURDEN, grpd_region1,
                                                               var.labels=c("Rent a burden? All PRS", "Grouped region"), 
                                                               #value.labels = c("Can't afford", "Can afford"),
                                                               weight.by = hhweight_3, 
                                                               show.col.prc = TRUE,
                                                               #show.row.prc = TRUE,
                                                               use.viewer = FALSE)
                ]

#Behind on rent/mortgage, utility bills or other debts
des_prs_work <-update(des_prs_work, DEBTFRE1 = factor(DEBTFRE1)) 
des_prs_work <-update(des_prs_work, DEBTFRE2 = factor(DEBTFRE2))
des_prs_work <-update(des_prs_work, DEBTFRE3 = factor(DEBTFRE3)) 

t_debt1_prsshare <- as.data.frame( ftable( svyby(formula = ~DEBTFRE1, by = ~grpd_region1, 
                                                 design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                 FUN = svymean, na.rm = TRUE))
)
t_debt2_prsshare <- as.data.frame( ftable( svyby(formula = ~DEBTFRE2, by = ~grpd_region1, 
                                                 design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                 FUN = svymean, na.rm = TRUE))
)
t_debt3_prsshare <- as.data.frame( ftable( svyby(formula = ~DEBTFRE3, by = ~grpd_region1, 
                                                 design = subset(des_prs_work, aff_shared_noincben == 0), 
                                                 FUN = svymean, na.rm = TRUE))
)


t_debt1_prsshare <- dcast(t_debt1_prsshare, grpd_region1 + Var3 ~ Var2)
t_debt2_prsshare <- dcast(t_debt2_prsshare, grpd_region1 + Var3 ~ Var2)
t_debt3_prsshare <- dcast(t_debt3_prsshare, grpd_region1 + Var3 ~ Var2)

t_debt1_prsshare$Var3 <- factor(t_debt1_prsshare$Var3, levels = c("DEBTFRE10", "DEBTFRE11", "DEBTFRE12"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))
t_debt2_prsshare$Var3 <- factor(t_debt2_prsshare$Var3, levels = c("DEBTFRE20", "DEBTFRE21", "DEBTFRE22"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))
t_debt3_prsshare$Var3 <- factor(t_debt3_prsshare$Var3, levels = c("DEBTFRE30", "DEBTFRE31", "DEBTFRE32"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_debt1_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with rent for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt2_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with utility bills for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt3_prsshare), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with other bills for PRS households under 55 who can't afford Shared Ownership") + facet_grid( Var3~ .)

#table
svymean(~DEBTFRE1, design = subset(des_prs_work, aff_shared_noincben == 0), na.rm = TRUE, ci = TRUE)
#all PRS under 55
svymean(~DEBTFRE1, design = des_prs_work, na.rm = TRUE, ci = TRUE)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 0, sjt.xtab(OAEXPNS, grpd_region1,
                                                       var.labels=c("£200 expense?", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]





#### Finances of those who *can* afford shared ownership ####
#average savings
svyquantile(~TOTCAPB3, design = subset(des_prs_work, aff_shared_noincben ==1), quantile = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE)
svymean(~TOTCAPB3, design = subset(des_prs_work, aff_shared_noincben ==1), ci = TRUE)

#% no savings can afford
affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(TOTCAPB3 <=0, grpd_region1,
                                                       var.labels=c("No savings? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

#average difference between savings and deposit
svyquantile(~diff_shareddep, design = subset(des_prs_work, aff_shared_noincben ==1), quantile = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE)
ftable( svyby(formula = ~diff_shareddep, by = ~grpd_region1, 
              design = subset(des_prs_work, aff_shared_noincben == 1), 
              FUN = svyquantile, quantiles = 0.5, ci = TRUE))


#Ability to save at least £10 a month
t_save10_prssharecan <- as.data.frame( ftable( svyby(formula = ~ADDMON, by = ~grpd_region1, 
                                                     design = subset(des_prs_work, aff_shared_noincben == 1), 
                                                     FUN = svymean, na.rm = TRUE))
)


t_save10_prssharecan <- dcast(t_save10_prssharecan, grpd_region1 + Var3 ~ Var2)
t_save10_prssharecan$Var3 <- factor(t_save10_prssharecan$Var3, levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                                    labels = c("Do this", "Like to but can't afford", 
                                               "Don't want to",  "Does not apply" ))

#Plot as points with error bars
#by region
ggplot(na.omit(t_save10_prssharecan), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for PRS households under 55 who can afford Shared Ownership") + facet_grid( Var3~ .)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(ADDMON, grpd_region1,
                                                       var.labels=c("Save £10? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]


#Struggling with housing costs
t_rentburden_prssharecan <- as.data.frame( ftable( svyby(formula = ~BURDEN, by = ~grpd_region1, 
                                                         design = subset(des_prs_work, aff_shared_noincben == 1), 
                                                         FUN = svymean, na.rm = TRUE))
)


t_rentburden_prssharecan <- dcast(t_rentburden_prssharecan, grpd_region1 + Var3 ~ Var2)
t_rentburden_prssharecan$Var3 <- factor(t_rentburden_prssharecan$Var3, levels = c("BURDEN1", "BURDEN2", "BURDEN3"),
                                        labels = c("Heavy burden", "Slight burden", 
                                                   "Not a burden"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_rentburden_prssharecan), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for PRS households under 55 who can afford Shared Ownership") + facet_grid( Var3~ .)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(BURDEN, grpd_region1,
                                                       var.labels=c("Rent a burden? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]



#Behind on rent/mortgage, utility bills or other debts


t_debt1_prssharecan <- as.data.frame( ftable( svyby(formula = ~DEBTFRE1, by = ~grpd_region1, 
                                                    design = subset(des_prs_work, aff_shared_noincben == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)
t_debt2_prssharecan <- as.data.frame( ftable( svyby(formula = ~DEBTFRE2, by = ~grpd_region1, 
                                                    design = subset(des_prs_work, aff_shared_noincben == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)
t_debt3_prssharecan <- as.data.frame( ftable( svyby(formula = ~DEBTFRE3, by = ~grpd_region1, 
                                                    design = subset(des_prs_work, aff_shared_noincben == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)


t_debt1_prssharecan <- dcast(t_debt1_prssharecan, grpd_region1 + Var3 ~ Var2)
t_debt2_prssharecan <- dcast(t_debt2_prssharecan, grpd_region1 + Var3 ~ Var2)
t_debt3_prssharecan <- dcast(t_debt3_prssharecan, grpd_region1 + Var3 ~ Var2)

t_debt1_prssharecan$Var3 <- factor(t_debt1_prssharecan$Var3, levels = c("DEBTFRE10", "DEBTFRE11", "DEBTFRE12"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))
t_debt2_prssharecan$Var3 <- factor(t_debt2_prssharecan$Var3, levels = c("DEBTFRE20", "DEBTFRE21", "DEBTFRE22"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))
t_debt3_prssharecan$Var3 <- factor(t_debt3_prssharecan$Var3, levels = c("DEBTFRE30", "DEBTFRE31", "DEBTFRE32"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_debt1_prssharecan), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with rent for PRS households under 55 who can afford Shared Ownership") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt2_prssharecan), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with utility bills for PRS households under 55 who can afford Shared Ownership") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt3_prssharecan), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with other bills for PRS households under 55 who can afford Shared Ownership") + facet_grid( Var3~ .)

#table
svymean(~DEBTFRE1, design = subset(des_prs_work, aff_shared_noincben == 1), na.rm = TRUE, ci = TRUE)
#all PRS under 55
svymean(~DEBTFRE1, design = des_prs_work, na.rm = TRUE, ci = TRUE)

#pretty table
affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(DEBTFRE1, grpd_region1,
                                                       var.labels=c("Behind on rent? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(DEBTFRE2, grpd_region1,
                                                       var.labels=c("Behind on utilities? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]

affdt_1215_prswork[ aff_shared_noincben == 1, sjt.xtab(DEBTFRE3, grpd_region1,
                                                       var.labels=c("Behind on other loans? Can afford", "Grouped region"), 
                                                       #value.labels = c("Can't afford", "Can afford"),
                                                       weight.by = hhweight_3, 
                                                       show.col.prc = TRUE,
                                                       #show.row.prc = TRUE,
                                                       use.viewer = FALSE)
                    ]



## Additional numbers who can't access shared ownership because can't save ####
#Number out of all PRS who can technically afford shared but are at least £1000 off deposit and can't save/are in financial difficulty


affdt_1215_prswork[ , sjt.xtab(affanddep_shared, GVTREGN,
                               var.labels=c("Can afford and can deposit shared? All PRS 55", "Grouped region"),
                               #value.labels = c("Can't afford", "Can afford"),
                               weight.by = hhweight_3, 
                               show.col.prc = TRUE,
                               #show.row.prc = TRUE,
                               use.viewer = FALSE)
                    ]
     
# ######Affordability of shared ownership, private renters who are eligible for Starter Homes
# des_prsel <- subset(deshhd_1215, (prs_renter == 1 & eligible_starter ==1))
# ftable(svyby(formula = ~aff_shared_noincben, by = ~GVTREGN, design = des_prsel, 
#              FUN = svymean))
# 
# 
# #deshhd_1215 <- update(deshhd_1215, aff_shared_double = as.double(aff_shared_noincben))
# 
# des_prselshare <- subset(deshhd_1215, (prs_renter == 1 & eligible_starter ==1 & aff_shared_noincben == 0))
# des_prselshare <-update(des_prselshare, SELFDEMP = factor(SELFDEMP, 
#                                                           labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
#                                                                      "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other")))
# #data frame 
# t_emply_prselshare <- as.data.frame( ftable( svyby(formula = ~SELFDEMP, by = ~GVTREGN, 
#                                                    design = des_prselshare, 
#                                                    FUN = svymean, na.rm = TRUE))
# )
# 
# #Feed table in to data fram to make for easy plotting
# # t_shared_afford_el <- as.data.frame (svyby(formula = ~aff_shared_noincben, by = ~GVTREGN, design = subset(deshhd_1215, (prs_renter == 1 & eligible_starter ==1)), 
# #                     FUN = svymean))
# 
# t_emply_prselshare_wide <- dcast(t_emply_prselshare, GVTREGN + Var3 ~ Var2)
# t_emply_prselshare_wide$Var3 <- factor(t_emply_prselshare_wide$Var3, levels = c("SELFDEMPFull-time", "SELFDEMPPart-time", "SELFDEMPFT self-employed",
#                                         "SELFDEMPPT self-employed", "SELFDEMPUnemployed", "SELFDEMPStudent", "SELFDEMPFamily home", 
#                                        "SELFDEMPDisabled", "SELFDEMPRetired", "SELFDEMPOther"),
#                                        labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
#                                                   "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"))
# 
# #Plot proportions as stacked bar
# ggplot(na.omit(t_emply_prselshare_wide), aes(x = GVTREGN,  y = svymean , ymin = svymean-SE, ymax = svymean+SE, fill = Var3)) +
#   geom_bar(stat="identity") + 
#   #geom_segment( aes(x = GVTREGN, xend = GVTREGN, y = svymean-SE, yend=svymean-SE)) +
#     xlab("region") + ylab("Prop in category") +
#   ggtitle("Self-reported employment of head for PRS households who are eligible for Starter Homes 
#                           and can't afford Shared Ownership") #+ facet_grid(GVTREGN ~ .)
# 
# #Plot as points with error bars
# ggplot(na.omit(t_emply_prselshare_wide), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3)) +
#   geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
#   xlab("Prop in category") + ylab("Employment status") +
#   ggtitle("Self-reported employment of head for PRS households who are eligible for Starter Homes 
#           and can't afford Shared Ownership") + facet_grid(GVTREGN ~ .)
# 
# 

# #############Stats on young PRS renters eligible for Starter Homes
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_starterlq_noincben, 
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford LQ Starter and eligible", "Region"), 
#                                                                     weight.by = hhweight_3, 
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_starterm_noincben,
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford median Starter and eligible", "Region"),
#                                                                   
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab((aff_htb_noincben),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Help to Buy", "Region"),
#                                                                     
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben,
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Shared Ownership", "Region"),
#                                                                    
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# #Can't afford any
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab((aff_shared_noincben | aff_starterlq_noincben | aff_htb_noincben),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford at least one of the options", "Region"),
#                                                              
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# ### CAN afford AND have deposit OR not in situation where at least £600 off deposit AND can't save at least £10 a month (ADDMON == 2);
# ### implies will take longer than 5 years to save deposit
# 
# 
# 
# # #HtB and shared who can afford and not impossible deposit
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_htb_noincben == 1 & (has_htbdep ==1 |
#                                                                                                cant_htbdep == 0),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Help to Buy & not impossible deposit",
#                                                                                  "Region"),
#                                                                     
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
#                                                                                                   cant_shareddep == 0),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford shared ownership & not impossible deposit",
#                                                                                  "Region"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# #starter homes - can't afford or can afford and impossible deposit
# 
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP ), sjt.xtab(aff_starterlq_noincben == 0 | (aff_starterlq_noincben == 1 & (has_starterlqdep == 0 &
#                                                                                                                                      cant_starterlqdep == 1)),
#                                                                      GVTREGN,
#                                                                      var.labels=c("Can't or Can afford LQ Starter & impossible deposit",
#                                                                                   "Region"),
#                                                                      weight.by = hhweight_3,
#                                                                      show.col.prc = TRUE,
#                                                                      use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP ), sjt.xtab(aff_starterm_noincben == 0 | (aff_starterm_noincben == 1 & (has_startermdep == 0 &
#                                                                                                                                    cant_startermdep == 1)),
#                                                                      GVTREGN,
#                                                                      var.labels=c("Can't or Can afford median Starter & impossible deposit",
#                                                                                   "Region"),
#                                                                      weight.by = hhweight_3,
#                                                                      show.col.prc = TRUE,
#                                                                      use.viewer = FALSE)
#                 ]
# 
# 
# #############Stats on older PRS renters eligible for Starter Homes
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab((aff_htb_noincben),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Help to Buy", "Region"),
#                                                                     
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# affdt_1215_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_shared_noincben,
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Shared Ownership", "Region"),
#                                                                     
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# #Can't afford any
# affdt_1215_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab((aff_shared_noincben | aff_htb_noincben),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford at least one of the options", "Region"),
#                                                                     
#                                                                     value.labels = c("Can't afford", "Can afford"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# # #HtB and shared who can afford and not impossible deposit
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_htb_noincben == 1 & (has_htbdep ==1 |
#                                                                                                cant_htbdep == 0),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford Help to Buy & not impossible deposit",
#                                                                                  "Region"),
#                                                                     
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
#                                                                                                   cant_shareddep == 0),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford shared ownership & not impossible deposit",
#                                                                                  "Region"),
#                                                                     weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
# 
# 
# #################### Characteristics of those who can't afford shared ownership
# 
# affdt_1215_1BU[ ( prs_renter & is_HRP & eligible_starter == 1), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
#                                                                                                        cant_shareddep == 0),
#                                                                          GVTREGN,
#                                                                          var.labels=c("Can afford shared ownership",
#                                                                                       "Region"),
#                                                                          #weight.by = hhweight_3,
#                                                                          show.col.prc = TRUE,
#                                                                          use.viewer = FALSE)
#                 ]
# 
# affdt_1215_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
#                                                                                                   cant_shareddep == 0),
#                                                                     GVTREGN,
#                                                                     var.labels=c("Can afford shared ownership & not impossible deposit",
#                                                                                  "Region"),
#                                                                     #weight.by = hhweight_3,
#                                                                     show.col.prc = TRUE,
#                                                                     use.viewer = FALSE)
#                 ]
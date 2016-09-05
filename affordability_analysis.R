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

# write_sav(affdt_1415, paste0(results_dir, "flatfile_1415.sav"))
# write_sav(affdt_1314, paste0(results_dir, "flatfile_1314.sav"))
# write_sav(affdt_1315, paste0(results_dir, "flatfile_1315.sav"))

#Calculate number of bedrooms needed according to LHA definition
#see http://england.shelter.org.uk/get_advice/housing_benefit_and_local_housing_allowance/what_is_housing_benefit/local_housing_allowance
source("calculate_bedrooms.R")

calculate.bedrooms(affdt_1315)
calculate.bedrooms(affdt_1215)

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

############ Attach 14/15 ownership product thresholds
owner_thresh <- read.csv(paste0(derived_data_dir,"20160815_UdatedOwnershipThresholds.csv"), stringsAsFactors = FALSE)
owner_thresh <- data.table(owner_thresh)

own_keycols = c("GVTREGN")
own_cols = c("GVTREGN", "starter_med", "starter_lq", "helptobuy", "shared", "starter_mdeposit",  "starter_lqdeposit", 
             "helptobuy_deposit", "shared_deposit" )

setkeyv(affdt_1315, own_keycols)
setkeyv(owner_thresh, own_keycols)

affdt_1315 <- merge(affdt_1315 , owner_thresh[ , .SD, .SDcols = own_cols], by = own_keycols, all.x = TRUE)


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

## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
affdt_1315[ (year == '1415'), hh_grossinc_noincben := (HHINC - HHIRBEN) * BHCDEF * 52]
#inflate 1314 to 1415 prices
affdt_1315[ (year == '1314'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/99)]

############## select England only
affdt_1315 <- affdt_1315[ (GVTREGN != 299999999 & GVTREGN != 399999999 & GVTREGN != 499999999),  ]

#flag for don't have HBAI variables
affdt_1315[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]

############# Flags for afford sale ########################
# flag if can afford ownership, using gross household income minus income related benefits

affdt_1315[ , aff_starterm_noincben := 0]
affdt_1315[ , aff_starterm_noincben := ifelse( (hh_grossinc_noincben >= starter_med & HDAGE < 4), 1, 0 )]
affdt_1315[ , aff_starterm_noincben := ifelse( HDAGE >3, 2, aff_starterm_noincben)]

affdt_1315[ , aff_starterlq_noincben := 0]
affdt_1315[ , aff_starterlq_noincben := ifelse( (hh_grossinc_noincben >= starter_lq & HDAGE < 4) , 1, 0 )]
affdt_1315[ , aff_starterlq_noincben := ifelse( HDAGE >3, 2, aff_starterlq_noincben)]

affdt_1315[ , aff_htb_noincben := ifelse( hh_grossinc_noincben >= helptobuy, 1, 0 )]
affdt_1315[ , aff_shared_noincben := ifelse( hh_grossinc_noincben >= shared, 1, 0 )]




############ Table fun! #################################
#Restrict to houses with only one benefit unit so can use GS_NEWHH to weight
affdt_1315_1BU <- affdt_1315[ multiple_benu == 0, ]

#Flag for is HRP - can also be used to collapse dataset down to household level for household level variables
affdt_1315_1BU[ , is_HRP := ifelse( PERSON == HRPNUM, 1, 0) ]

#Add 2 year hhd weight
affdt_1315_1BU[ , hhweight_2 := GS_NEWHH/2]

affdt_1315_1BU[ , GVTREGN := factor(GVTREGN, labels = c("North East", "North West", "Yorkshire and Humber", "East Midlands",
                                                        "West Midlands", "East", "London", "South East", "South West"))]

#Flag for renter
affdt_1315_1BU[ , prs_renter := ifelse(PTENTYP2 == 3 | PTENTYP2 == 4, 1, 0)]

#Flag for eligible for Starter Homes
affdt_1315_1BU[ , eligible_starter := ifelse(HDAGE <4, 1, 0)]

#Add has deposit variables
affdt_1315_1BU[ , has_starterlqdep := ifelse(TOTCAPB3 >= starter_lqdeposit, 1, 0 )]
affdt_1315_1BU[ , has_startermdep := ifelse(TOTCAPB3 >= starter_mdeposit, 1, 0 )]
affdt_1315_1BU[ , has_htbdep := ifelse(TOTCAPB3 >= helptobuy_deposit, 1, 0 )]
affdt_1315_1BU[ , has_shareddep := ifelse(TOTCAPB3 >= shared_deposit, 1, 0 )]

#Add definitely can't save for it in 5 years
affdt_1315_1BU[ , cant_starterlqdep := ifelse( (TOTCAPB3 < (starter_lqdeposit - 600)) & ADDMON == 2, 1, 0 )]
affdt_1315_1BU[ , cant_startermdep := ifelse( (TOTCAPB3 < (starter_mdeposit - 600)) & ADDMON == 2, 1, 0 )]
affdt_1315_1BU[ , cant_htbdep := ifelse( (TOTCAPB3 < (helptobuy_deposit - 600)) & ADDMON == 2, 1, 0 )]
affdt_1315_1BU[ , cant_shareddep := ifelse( (TOTCAPB3 < (shared_deposit - 600)) & ADDMON == 2, 1, 0 )]

#### Private renters who can't afford ownership
affdt_1315_1BU[ ( prs_renter & is_HRP), sjt.xtab(aff_starterlq_noincben, GVTREGN,
                                                                      var.labels=c("Can afford LQ Starter", "Region"), 
                                                                      weight.by = hhweight_2, 
                                                                      show.col.prc = TRUE,
                                                                      use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & is_HRP), sjt.xtab(aff_starterm_noincben, GVTREGN,
                                                                      var.labels=c("Can afford median Starter", "Region"), 
                                                                      weight.by = hhweight_2, 
                                                                      show.col.prc = TRUE,
                                                                      use.viewer = FALSE)
                ]


affdt_1315_1BU[ ( prs_renter & is_HRP), sjt.xtab((aff_htb_noincben), GVTREGN,
                                                                      var.labels=c("Can afford Help to Buy", "Region"), 
                                                                      value.labels = c("Can't afford", "Can afford"),
                                                                      weight.by = hhweight_2, 
                                                                      show.col.prc = TRUE,
                                                                      use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & is_HRP), sjt.xtab(aff_shared_noincben, GVTREGN,
                                                                      var.labels=c("Can afford Shared Ownership", "Region"), 
                                                                      value.labels = c("Can't afford", "Can afford"),
                                                                      weight.by = hhweight_2, 
                                                                      show.col.prc = TRUE,
                                                                      use.viewer = FALSE)
                ]


#############Stats on young PRS renters eligible for Starter Homes

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_starterlq_noincben, 
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford LQ Starter and eligible", "Region"), 
                                                                    weight.by = hhweight_2, 
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_starterm_noincben,
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford median Starter and eligible", "Region"),
                                                                  
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab((aff_htb_noincben),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Help to Buy", "Region"),
                                                                    
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]
affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben,
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Shared Ownership", "Region"),
                                                                   
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


#Can't afford any
affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab((aff_shared_noincben | aff_starterlq_noincben | aff_htb_noincben),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford at least one of the options", "Region"),
                                                             
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


### CAN afford AND have deposit OR not in situation where at least £600 off deposit AND can't save at least £10 a month (ADDMON == 2);
### implies will take longer than 5 years to save deposit



# #HtB and shared who can afford and not impossible deposit

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_htb_noincben == 1 & (has_htbdep ==1 |
                                                                                               cant_htbdep == 0),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Help to Buy & not impossible deposit",
                                                                                 "Region"),
                                                                    
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
                                                                                                  cant_shareddep == 0),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford shared ownership & not impossible deposit",
                                                                                 "Region"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]

#starter homes - can't afford or can afford and impossible deposit


affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP ), sjt.xtab(aff_starterlq_noincben == 0 | (aff_starterlq_noincben == 1 & (has_starterlqdep == 0 &
                                                                                                                                     cant_starterlqdep == 1)),
                                                                     GVTREGN,
                                                                     var.labels=c("Can't or Can afford LQ Starter & impossible deposit",
                                                                                  "Region"),
                                                                     weight.by = hhweight_2,
                                                                     show.col.prc = TRUE,
                                                                     use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP ), sjt.xtab(aff_starterm_noincben == 0 | (aff_starterm_noincben == 1 & (has_startermdep == 0 &
                                                                                                                                   cant_startermdep == 1)),
                                                                     GVTREGN,
                                                                     var.labels=c("Can't or Can afford median Starter & impossible deposit",
                                                                                  "Region"),
                                                                     weight.by = hhweight_2,
                                                                     show.col.prc = TRUE)#,
                #use.viewer = FALSE)
                ]


#############Stats on older PRS renters eligible for Starter Homes

affdt_1315_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab((aff_htb_noincben),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Help to Buy", "Region"),
                                                                    
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]
affdt_1315_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_shared_noincben,
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Shared Ownership", "Region"),
                                                                    
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


#Can't afford any
affdt_1315_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab((aff_shared_noincben | aff_htb_noincben),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford at least one of the options", "Region"),
                                                                    
                                                                    value.labels = c("Can't afford", "Can afford"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


# #HtB and shared who can afford and not impossible deposit

affdt_1315_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_htb_noincben == 1 & (has_htbdep ==1 |
                                                                                               cant_htbdep == 0),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford Help to Buy & not impossible deposit",
                                                                                 "Region"),
                                                                    
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & eligible_starter == 0 & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
                                                                                                  cant_shareddep == 0),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford shared ownership & not impossible deposit",
                                                                                 "Region"),
                                                                    weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]


#################### Characteristics of those who can't afford shared ownership

affdt_1315_1BU[ ( prs_renter & is_HRP), sjt.xtab(aff_shared_noincben == 1 ,
                                                                         GVTREGN,
                                                                         var.labels=c("Can afford shared ownership",
                                                                                      "Region"),
                                                                         #weight.by = hhweight_2,
                                                                         show.col.prc = TRUE,
                                                                         use.viewer = FALSE)
                ]

affdt_1315_1BU[ ( prs_renter & eligible_starter & is_HRP), sjt.xtab(aff_shared_noincben == 1 & (has_shareddep ==1 |
                                                                                                  cant_shareddep == 0),
                                                                    GVTREGN,
                                                                    var.labels=c("Can afford shared ownership & not impossible deposit",
                                                                                 "Region"),
                                                                    #weight.by = hhweight_2,
                                                                    show.col.prc = TRUE,
                                                                    use.viewer = FALSE)
                ]
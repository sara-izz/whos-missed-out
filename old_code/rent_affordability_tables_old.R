quantinstall.packages("devtools")
require("devtools")
install.packages("sqldf")
#install.packages("data.table")
install_github("Rdatatable/data.table") #Use dev version on Github
install.packages("dplyr")
install.packages("sjPlot")
require(sjPlot)
require(ggplot2)
install.packages("reshape2")
library("reshape2")
install.packages("survey")
require("survey")

## Make flatfiles for each year#####
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

#Calculate number of bedrooms needed according to LHA definition#####
#see http://england.shelter.org.uk/get_advice/housing_benefit_and_local_housing_allowance/what_is_housing_benefit/local_housing_allowance
source("calculate_bedrooms.R")

calculate.bedrooms(affdt_1315)
calculate.bedrooms(affdt_1215)


############# Attach 14/15 private rent for number of bedrooms needed #####
#recode number of bedrooms needed to match
affdt_1215[ bedrooms_needed == 1, bedrooms_rent := 1]
affdt_1215[ bedrooms_needed == 2, bedrooms_rent := 2]
affdt_1215[ bedrooms_needed == 3, bedrooms_rent := 3]
affdt_1215[ bedrooms_needed >= 4, bedrooms_rent := 4]

voa_rents_1415 <- read.csv(paste0(derived_data_dir,"VOA_privaterents_1415.csv"))
voa_rents_1415 <- data.table(voa_rents_1415)


rent_keycols = c("GVTREGN", "bedrooms_rent")
voa_cols = c("GVTREGN", "bedrooms_rent","rent_mean", "rent_lq", "rent_med", "rent_uq")

setkeyv(affdt_1215, rent_keycols)
setkeyv(voa_rents_1415, rent_keycols)

affdt_1215 <- merge(affdt_1215 , voa_rents_1415[ , .SD, .SDcols = voa_cols], by = rent_keycols, all.x = TRUE)

#clean up
rm(rent_keycols)
rm(voa_cols)

#Add 80% market rent values
affdt_1215[ , rent_mean80 := rent_mean*0.8]
affdt_1215[ , rent_lq80 := rent_lq*0.8]
affdt_1215[ , rent_med80 := rent_med*0.8]
affdt_1215[ , rent_uq80 := rent_uq*0.8]

###### Logical flags for later analysis #########
## Flag for households with more than 1 benefit unit
affdt_1215[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
affdt_1215[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]

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



############# Calculate deflated household incomes #######################
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



# select England only, stuff to make weights work ####
affdt_1215 <- affdt_1215[ (GVTREGN != 299999999 & GVTREGN != 399999999 & GVTREGN != 499999999),  ]

#flag for don't have HBAI variables
affdt_1215[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]

#Flag for is HRP - can also be used to collapse dataset down to household level for household level variables
affdt_1215[ , is_HRP := ifelse( PERSON == HRPNUM, 1, 0) ]

#Add 2 year hhd weight
#affdt_1315_1BU[ , hhweight_2 := GS_NEWHH/2]
affdt_1215[ , hhweight_3 := GS_NEWHH/3]


## Tidy up tenure ####
#Flags for tenure
affdt_1215[ , prs_renter := ifelse(PTENTYP2 == 3 | PTENTYP2 == 4, 1, 0)]
affdt_1215[ , council_renter := ifelse(PTENTYP2 == 1, 1, 0)]
affdt_1215[ , ha_renter := ifelse(PTENTYP2 == 2, 1, 0)]
affdt_1215[ , social_renter := ifelse(PTENTYP2 == 1 | PTENTYP2 == 2, 1, 0)]
affdt_1215[ , outright_owner := ifelse(PTENTYP2 == 5 , 1, 0)]
affdt_1215[ , mortgaged_owner := ifelse(PTENTYP2 == 6, 1, 0)]

#grouped tenure, 1=PRS, 2=social, 3=outright owner, 4=mortgaged owner
affdt_1215[ , grpd_tenure := ifelse(prs_renter == 1, 1, 0)]
affdt_1215[ , grpd_tenure := ifelse(council_renter == 1 | ha_renter == 1, 2, grpd_tenure)]
affdt_1215[ , grpd_tenure := ifelse(outright_owner == 1, 3, grpd_tenure)]
affdt_1215[ , grpd_tenure := ifelse(mortgaged_owner == 1, 4, grpd_tenure)]

## Tidy up regions ####
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

affdt_1215[ , grpd_region1 := factor(grpd_region1, labels = c("North", "West", "East", "London and South East"))]



#Struggling according to Bramley 2011 definition; more than 25% gross income AND self-reported burden ###########
affdt_1215[ , struggling_housing_g1 := ifelse(GBHSCOST > (0.25*(hh_grossinc/52)) & (BURDEN == 1 | BURDEN == 2), 1, 0) ]
#try equivalised income
#try without HB
affdt_1215[ , struggling_housing_nohb := ifelse(GBHSCOST > (0.25*(hh_grossinc_nohb/52)) & (BURDEN == 1 | BURDEN == 2), 1, 0) ]

#try simple struggling based on can't afford now
affdt_1215[ , struggling_housing_g2 := ifelse(GBHSCOST > (0.25*(hh_grossinc/52)), 1, 0) ]

#Add housing cost to income ratios
#gross
affdt_1215[ , housing_cost_gross := (GBHSCOST*52)/hh_grossinc ]
#without hb
affdt_1215[ , housing_cost_gnohb := (GBHSCOST*52)/hh_grossinc_nohb ]

# Flags for afford rent ########################

# flag if can afford median market rent, using gross household income
affdt_1215[ , amount_canaff_g := (hh_grossinc*0.3)/12]
affdt_1215[ nohbai_inc == 0, aff_rentmed_g := ifelse(   amount_canaff_g >= rent_med, 1, 0 )]
affdt_1215[ nohbai_inc == 0, aff_rentlq_g := ifelse(   amount_canaff_g >= rent_lq, 1, 0 )]

# flag is can afford median market rent, using gross household income
affdt_1215[ multiple_benu == 0, amount_canaff_gnohb := (hh_grossinc_nohb*0.3)/12]
affdt_1215[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed_gnohb := ifelse(   amount_canaff_gnohb >= rent_med, 1, 0 )]
affdt_1215[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq, 1, 0 )]

#flags for 80% market rent
affdt_1215[ nohbai_inc == 0, aff_rentmed80_g := ifelse(   amount_canaff_g >= rent_med80, 1, 0 )]
affdt_1215[ nohbai_inc == 0, aff_rentlq80_g := ifelse(   amount_canaff_g >= rent_lq80, 1, 0 )]
affdt_1215[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed80_gnohb := ifelse(   amount_canaff_gnohb >= rent_med80, 1, 0 )]
affdt_1215[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq80_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq80, 1, 0 )]


#######Create survey object from data table, limiting to HRP & households with only 1 benefit units so household weights work.####### 
#Need to restrict to cases with non-missing weights
deshhd_1215 <- svydesign(ids = ~1, weights = ~ hhweight_3, data = affdt_1215[affdt_1215$is_HRP & 
                                                                               (is.na(affdt_1215$hhweight_3) == F & 
                                                                                  multiple_benu == 0), ])

#survey object of private renters only
deshhd1215_prs <- subset(deshhd_1215, prs_renter == 1)

#survey object of social renters only
deshhd1215_social <- subset(deshhd_1215, council_renter == 1 | ha_renter == 1)

#struggling households all tenure distributions

affdt1215_strug <- affdt_1215[(struggling_housing_g1 == 1 & affdt_1215$is_HRP & !is.na(affdt_1215$hhweight_3) & 
                                 multiple_benu == 0), ]

############### Who is currently struggling with housing costs #################
#Data table of struggling households - for use with sjt.xtab
affdt1215_1BUHRP <- affdt_1215[multiple_benu == 0 & !is.na(hhweight_3) & is_HRP, ]

#How many by tenure
deshhd_1215 <-update(deshhd_1215, PTENTYP2 = factor(PTENTYP2))
deshhd_1215 <-update(deshhd_1215, grpd_tenure = factor(grpd_tenure))

ftable(svyby(formula = ~PTENTYP2, by = ~GVTREGN, design = subset(deshhd_1215, struggling_housing_g1 == 1), FUN = svytotal, na.rm = TRUE))


t_struggling_tenure <- as.data.frame( ftable(svyby(formula = ~grpd_tenure, by = ~GVTREGN, 
                                                   design = subset(deshhd_1215, struggling_housing_g1 == 1), 
                                                   FUN = svytotal, na.rm = TRUE))
)


t_struggling_tenure <- dcast(t_struggling_tenure, GVTREGN + Var3 ~ Var2)
t_struggling_tenure$Var3 <- factor( t_struggling_tenure$Var3, levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                                    labels = c("PRS", "social", "outright owner", "mortgaged owner")) 


#Plot as points with error bars
ggplot(na.omit(t_struggling_tenure), aes(x = svytotal, xmin = svytotal-SE, xmax = svytotal+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svytotal-SE, xend = svytotal+SE, y = Var3, yend=Var3)) +
  xlab("Number struggling") + guides(colour=FALSE) + ylab("Tenure")+
  ggtitle("Struggling households by tenure by region") + facet_grid( GVTREGN~ .)

affdt_1215[ , grpd_tenure := factor(grpd_tenure,levels = c("1", "2", "3", "4"), 
                                     labels = c("PRS", "social", "outright owner", "mortgaged owner"))]
affdt_1215[ (affdt_1215$is_HRP & !is.na(affdt_1215$hhweight_3) & multiple_benu == 0 & struggling_housing_g1 == 1 ) ,
            sjt.xtab(GVTREGN, grpd_tenure, 
                     title = "Tenure by region, struggling with housing (gross, def1)",
                     var.labels=c("Region", "Tenure"), 
                     #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
                     weight.by = hhweight_3, 
                     show.col.prc = TRUE,
                     show.row.prc = TRUE,
                     use.viewer = FALSE)
                ]

deshhd_1215 <- update(deshhd_1215, struggling_housing_g1 = factor(struggling_housing_g1))
ftable(svyby(formula = ~GVTREGN, by = ~struggling_housing_g1, 
             design = deshhd_1215, 
             FUN = svymean, na.rm = TRUE))

ftable(svyby(formula = ~grpd_tenure, by = ~GVTREGN, 
             design = subset(deshhd_1215, struggling_housing_g1 == 1), 
             FUN = svymean, na.rm = TRUE))

# ftable(svyby(formula = ~GBHSCOST, by = ~grpd_region1, 
#              design = subset(deshhd_1215, struggling_housing == 1, outright_owner ==1), 
#              FUN = svymean, na.rm = TRUE))




#gross household income
ggplot(affdt1215_strug, 
       aes(x = hh_grossinc, colour=GVTREGN, weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  + geom_freqpoly(binwidth = 1000) + ggtitle("Gross household income - all struggling households") +
  xlim(-100, 50000) #+
  #facet_grid(grpd_region1 ~ .)

struggling_inc_table <- as.data.frame(svyby(~hh_grossinc, by = ~GVTREGN, design = subset(deshhd_1215, struggling_housing_g1 == 1), 
                                 FUN = svyquantile, 
                                 quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE))
  
#housing cost / gross household income ratio
ggplot(affdt1215_strug, 
       aes(x = housing_cost_gross, colour=GVTREGN, weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))+
  geom_freqpoly(binwidth = 0.1) + ggtitle("Gross household income - all struggling households") +
  xlim(-0.1, 1.1) #+
#facet_grid(grpd_region1 ~ .)

struggling_costtoinc_table <- as.data.frame(svyby(~housing_cost_gross, by = ~GVTREGN, design = subset(deshhd_1215, struggling_housing_g1 == 1), 
                                            FUN = svyquantile, 
                                            quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE))



#################################### Strugling PRS households####

deshhd1215_prs <- update(deshhd1215_prs, receives_hb = ifelse(HBENBU>0, 1, 0) )
deshhd1215_prs <- update(deshhd1215_prs, receives_hb = factor(receives_hb) )

#% of PRS households struggling
t_prs_strug <- as.data.frame( ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~GVTREGN, 
                                                   deshhd1215_prs, 
                                                   FUN = svymean, na.rm = TRUE))
)


t_prs_strug <- dcast(t_prs_strug, GVTREGN + Var3 ~ Var2)
t_prs_strug$Var3 <- factor( t_prs_strug$Var3, levels = c("factor(struggling_housing_g1)0", "factor(struggling_housing_g1)1"), 
                                    labels = c("Not struggling", "Struggling")) 


#Plot as points with error bars
ggplot(na.omit(t_prs_strug), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Proportion of PRS households") + guides(colour=FALSE) + ylab("Struggling or not")+
  ggtitle("PRS households by struggling by region") + facet_grid( GVTREGN~ .)

# table, % prs households struggling
affdt1215_1BUHRP[ prs_renter == 1 ,
                 sjt.xtab(GVTREGN, struggling_housing_g1, 
                          title = "Struggling (gross, def1) by region, PRS households",
                          var.labels=c("Region", "Struggling with housing?"), 
                          #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
                          weight.by = hhweight_3, 
                          show.col.prc = TRUE,
                          show.row.prc = TRUE,
                          use.viewer = FALSE)
                 ]



## PRS struggling households stats
affdt1215_strug_prs <- affdt1215_strug[prs_renter == 1,]

#gross household income
ggplot(affdt1215_strug_prs, 
       aes(x = hh_grossinc, colour=grpd_region1, weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_freqpoly(binwidth = 1000) + ggtitle("Gross household income - struggling PRS households") +
  xlim(-100, 50000) #+

ftable(svyby(~hh_grossinc, by = ~GVTREGN, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
      FUN = svyquantile, 
      quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

# On housing benefit? PRS struggling households
t_prsstrug_hbstatus <- as.data.frame( ftable(svyby(formula = ~receives_hb, by = ~GVTREGN, 
                                                   design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                   FUN = svymean, na.rm = TRUE))
)


t_prsstrug_hbstatus <- dcast(t_prsstrug_hbstatus, GVTREGN + Var3 ~ Var2)
t_prsstrug_hbstatus$Var3 <- factor( t_prsstrug_hbstatus$Var3, levels = c("receives_hb0", "receives_hb1"), 
                                   labels = c("No HB", "Receives HB")) 


#Plot as points with error bars
ggplot(na.omit(t_prsstrug_hbstatus), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Proportion of struggling PRS households") + guides(colour=FALSE) + ylab("HB status")+
  ggtitle("Struggling PRS households by HB status by region") + facet_grid( GVTREGN~ .)


#table of whether on HB or not
affdt1215_strug[ prs_renter == 1 ,
            sjt.xtab(GVTREGN, (HBENBU>0), 
                     title = "Housing benefit by region, PRS households struggling with housing (gross, def1)",
                     var.labels=c("Region", "Household in receipt of HB"), 
                     #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
                     weight.by = hhweight_3, 
                     show.col.prc = TRUE,
                     show.row.prc = TRUE,
                     use.viewer = FALSE)
            ]

#justifies incomes without HB
ftable(svyby(~hh_grossinc_nohb, by = ~GVTREGN, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#% gross income (including HB) spent on housing
#all PRS renters
ftable(svyby(~housing_cost_gross, by = ~GVTREGN, design = deshhd1215_prs, 
      FUN = svyquantile, 
      quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#struggling renters on HB
t_strugglingprs_costtoinc_hb <- as.data.frame(svyby(~housing_cost_gross, by = ~GVTREGN, 
                                                    design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & receives_hb == 1), 
                                                    FUN = svyquantile,
                                                    quantiles = 0.5, ci = TRUE, na.rm = TRUE))

#struggling renters not on HB
t_strugglingprs_costtoinc_nohb <- as.data.frame(svyby(~housing_cost_gross, by = ~GVTREGN, 
                                                      design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & receives_hb == 0),
                                                      FUN = svyquantile,
                                                      quantiles = 0.5, ci = TRUE, na.rm = TRUE))


#Age of struggling PRS households
deshhd1215_prs <-update(deshhd1215_prs, HDAGE = factor(HDAGE)) 
t_age_prsstruggle <- as.data.frame( ftable( svyby(formula = ~HDAGE, by = ~grpd_region1, 
                                               design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                               FUN = svymean, na.rm = TRUE))
)


t_age_prsstruggle <- dcast(t_age_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_age_prsstruggle$Var3 <- factor(t_age_prsstruggle$Var3, levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                              labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"))

#Plot as points with error bars
ggplot(na.omit(t_age_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + ylab("Age") +
  ggtitle("Age of head for struggling PRS households") + facet_grid(Var3~ .)

affdt1215_strug_prs[ , sjt.xtab(HDAGE, grpd_region1,
                                var.labels=c("Age of HRP", "Region"),
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]

affdt1215_1BUHRP[ prs_renter == 1 , sjt.xtab(HDAGE, grpd_region1,
                                var.labels=c("Age of HRP", "Region"),
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


#Self-reported employment of HRP
deshhd1215_prs <-update(deshhd1215_prs, SELFDEMP = factor(SELFDEMP, 
                                                            labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                                                       "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other")))
svymean(~SELFDEMP, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
t_emply_prsstruggle <- as.data.frame( ftable( svyby(formula = ~SELFDEMP, by = ~grpd_region1, 
                                                 design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                 FUN = svymean, na.rm = TRUE))
)


t_emply_prsstruggle <- dcast(t_emply_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_emply_prsstruggle$Var3 <- factor(t_emply_prsstruggle$Var3, levels = c("SELFDEMPFull-time", "SELFDEMPPart-time", "SELFDEMPFT self-employed",
                                                                  "SELFDEMPPT self-employed", "SELFDEMPUnemployed", "SELFDEMPStudent", "SELFDEMPFamily home", 
                                                                  "SELFDEMPDisabled", "SELFDEMPRetired", "SELFDEMPOther"),
                                labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                           "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"))

#Plot as points with error bars
ggplot(na.omit(t_emply_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Self-reported situation of head  for struggling PRS households ") + facet_grid( Var3~ .)

affdt1215_strug_prs[ , sjt.xtab(SELFDEMP, grpd_region1,
                                var.labels=c("Self-reported employment of HRP", "Region"),
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


# Employment of whole household
deshhd1215_prs <-update(deshhd1215_prs, ECOBU = factor(ECOBU)) 
t_emplhh_prsstruggle <- as.data.frame( ftable( svyby(formula = ~ECOBU, by = ~grpd_region1, 
                                                  design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                  FUN = svymean, na.rm = TRUE))
)


t_emplhh_prsstruggle <- dcast(t_emplhh_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_emplhh_prsstruggle$Var3 <- factor(t_emplhh_prsstruggle$Var3, levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
                                                                    "ECOBU8"),
                                 labels = c("1+ self employed", "Sing/couple all FT", 
                                            "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                            "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                            "Workless, head/spouse unemployed", "Workless, inactive" ))


#Plot as points with error bars
#by region
ggplot(na.omit(t_emplhh_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Household employment for struggling PRS households") + facet_grid( Var3~ .)

affdt1215_strug_prs[ , sjt.xtab(ECOBU, grpd_region1,
                                var.labels=c("Household employment status", "Region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


#ILO definition employment
affdt1215_strug_prs[ , sjt.xtab(DVIL04A, grpd_region1,
                                var.labels=c("ILO employment", "Region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                ]



##School age children

svytotal(~factor(has_kids16), design = subset(deshhd1215_prs, struggling_housing_g1 == 1))
svytotal(~factor(has_kids16), design = deshhd1215_prs)

svymean(~factor(has_kids16), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(has_kids16), design = deshhd1215_prs, na.rm = TRUE)

#struggling households
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all prs 
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = deshhd1215_prs, 
              FUN = svymean, na.rm = TRUE))

##single parents
svytotal(~factor(single_parent), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(single_parent), design = deshhd1215_prs, na.rm = TRUE)

svymean(~factor(single_parent), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)

t_singlep_prs<- as.data.frame( ftable(svymean(~factor(single_parent), design = deshhd1215_prs, na.rm = TRUE)))




#struggling households
t_singlep_prsstruggle <- as.data.frame( ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE)))
t_singlep_prsstruggle <- dcast(t_singlep_prsstruggle, grpd_region1 + Var3 ~ Var2)

#cf all prs 
t_singlep_prs <- as.data.frame(ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
              design = deshhd1215_prs, 
              FUN = svymean, na.rm = TRUE)))
t_singlep_prs<- dcast(t_singlep_prs, grpd_region1 + Var3 ~ Var2)

t_singlep_prsstruggle <- t_singlep_prsstruggle[t_singlep_prsstruggle$Var3 == "factor(single_parent)1" , ]
t_singlep_prsstruggle$Var3 <- NULL
t_singlep_prsstruggle$status <- "Struggling"

t_singlep_prs <- t_singlep_prs[t_singlep_prs$Var3 == "factor(single_parent)1" , ]
t_singlep_prs$Var3 <- NULL
t_singlep_prs$status <- "All PRS"

#append % of struggling single vs all in to one data frame for plotting

l <- list(t_singlep_prsstruggle, t_singlep_prs)
t_singlep_prsstruggle <- rbindlist(l, fill = TRUE)

#plot as points with error bars
ggplot(na.omit(t_singlep_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y= status, colour = status)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y=status, yend=status)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Single parents; struggling vs all PRS households") + facet_grid( grpd_region1~ .)

#by working

affdt1215_strug_prs[ , sjt.xtab(factor(single_parent), DVIL04A,
                                var.labels=c("Single Parent?", "Employment"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                #show.col.prc = TRUE,
                                show.row.prc = TRUE,
                                use.viewer = FALSE)
                ]


ftable( svyby(formula = ~factor(single_mum), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))

##Disabled adults and children
#adults
svytotal(~factor(has_disabledad), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(has_disabledad), design = deshhd1215_prs, na.rm = TRUE)

svymean(~factor(has_disabledad), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(has_disabledad), design = deshhd1215_prs, na.rm = TRUE)

#struggling PRS
ftable( svyby(formula = ~factor(has_disabledad), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~factor(has_disabledad), by = ~grpd_region1, 
              design = deshhd1215_prs, 
              FUN = svymean, na.rm = TRUE))

affdt1215_strug_prs[ , sjt.xtab(factor(has_disabledad), grpd_region1,
                                var.labels=c("Disabled adult?", "Grouped region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                ]

#children
affdt1215_strug_prs[ , sjt.xtab(factor(has_disabledch), grpd_region1,
                                var.labels=c("Disabled child?", "Grouped region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                ]

#pensioners - doesn't work for household weighting as need to restrict to 1 BU and if restrict to under 55 
#unlikely to have pensioner in same BU


##Caring reponsibilities
svytotal(~has_carer, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~has_carer, design = deshhd1215_prs, na.rm = TRUE)

svymean(~has_carer, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~has_carer, design = deshhd1215_prs, na.rm = TRUE)


#struggling PRS
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = deshhd1215_prs, 
              FUN = svymean, na.rm = TRUE))


#care given outside hhld
svytotal(~factor(GIVEHELP), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(GIVEHELP), design = deshhd1215_prs, na.rm = TRUE)

svymean(~factor(GIVEHELP), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(GIVEHELP), design = deshhd1215_prs, na.rm = TRUE)

#struggling
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all prs under 55
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = deshhd1215_prs, 
              FUN = svymean, na.rm = TRUE))

#Savings and ability to save at least £10 a month
svyquantile(~TOTCAPB3, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), quantile = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE, na.rm = TRUE)
svymean(~(TOTCAPB3==0), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), ci = TRUE, na.rm = TRUE)
svyby(~(TOTCAPB3==0), by = ~grpd_region1, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), FUN= svymean, ci = TRUE, na.rm = TRUE)
svymean(~(TOTCAPB3==0), design = deshhd1215_prs, ci = TRUE, na.rm = TRUE)
svyby(~(TOTCAPB3==0), by = ~grpd_region1, deshhd1215_prs, FUN= svymean, ci = TRUE, na.rm = TRUE)

deshhd1215_prs <-update(deshhd1215_prs, ADDMON = factor(ADDMON)) 
svymean(~factor(ADDMON), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(ADDMON), design = deshhd1215_prs, na.rm = TRUE)

t_save10_prsstruggle <- as.data.frame( ftable( svyby(formula = ~ADDMON, by = ~grpd_region1, 
                                                  design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                  FUN = svymean, na.rm = TRUE))
)


t_save10_prsstruggle <- dcast(t_save10_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_save10_prsstruggle$Var3 <- factor(t_save10_prsstruggle$Var3, levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                                 labels = c("Do this", "Like to but can't afford", 
                                            "Don't want to",  "Does not apply" ))

#Plot as points with error bars
#by region
ggplot(na.omit(t_save10_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for struggling PRS households") + facet_grid( Var3~ .)

#Behind on rent, utilities, other debts
deshhd1215_prs <-update(deshhd1215_prs, DEBTFRE1 = factor(DEBTFRE1)) 
deshhd1215_prs <-update(deshhd1215_prs, DEBTFRE2 = factor(DEBTFRE2))
deshhd1215_prs <-update(deshhd1215_prs, DEBTFRE3 = factor(DEBTFRE3)) 

t_debt1_prsstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE1, by = ~grpd_region1, 
                                                 design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                 FUN = svymean, na.rm = TRUE))
)
t_debt2_prsstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE2, by = ~grpd_region1, 
                                                 design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                 FUN = svymean, na.rm = TRUE))
)
t_debt3_prsstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE3, by = ~grpd_region1, 
                                                 design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
                                                 FUN = svymean, na.rm = TRUE))
)


t_debt1_prsstruggle <- dcast(t_debt1_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_debt2_prsstruggle <- dcast(t_debt2_prsstruggle, grpd_region1 + Var3 ~ Var2)
t_debt3_prsstruggle <- dcast(t_debt3_prsstruggle, grpd_region1 + Var3 ~ Var2)

t_debt1_prsstruggle$Var3 <- factor(t_debt1_prsstruggle$Var3, levels = c("DEBTFRE10", "DEBTFRE11", "DEBTFRE12"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))
t_debt2_prsstruggle$Var3 <- factor(t_debt2_prsstruggle$Var3, levels = c("DEBTFRE20", "DEBTFRE21", "DEBTFRE22"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))
t_debt3_prsstruggle$Var3 <- factor(t_debt3_prsstruggle$Var3, levels = c("DEBTFRE30", "DEBTFRE31", "DEBTFRE32"),
                                labels = c("Not behind", "Behind once", 
                                           "Behind twice or more"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_debt1_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with rent for struggling PRS households") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt2_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with utility bills for struggling PRS households") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt3_prsstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with other bills for struggling PRS households") + facet_grid( Var3~ .)

#table
#rent
svymean(~DEBTFRE1, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all PRS under 55
svymean(~DEBTFRE1, design = deshhd1215_prs, na.rm = TRUE, ci = TRUE)

#utilities
svymean(~DEBTFRE2, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all PRS under 55
svymean(~DEBTFRE2, design = deshhd1215_prs, na.rm = TRUE, ci = TRUE)

#other bills
svymean(~DEBTFRE3, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all PRS under 55
svymean(~DEBTFRE3, design = deshhd1215_prs, na.rm = TRUE, ci = TRUE)

#able to afford £200?
svymean(~factor(OAEXPNS), design = subset(deshhd1215_prs, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
ftable( svyby(formula = ~factor(OAEXPNS), by = ~grpd_region1, 
              design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))

#how to afford it
#own income and cut back on essentials
svymean(~factor(OAHOWPY1), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#own income and not cut back
svymean(~factor(OAHOWPY2), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#savings
svymean(~factor(OAHOWPY3), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#credit (card or loan)
svymean(~factor(OAHOWPY4), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#family/friends
svymean(~factor(OAHOWPY5), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#other
svymean(~factor(OAHOWPY6), design = subset(deshhd1215_prs, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)




###################### Struggling social renters ############################
deshhd1215_social <- update(deshhd1215_social, receives_hb = ifelse(HBENBU>0, 1, 0) )
deshhd1215_social <- update(deshhd1215_social, receives_hb = factor(receives_hb) )


#% of social households struggling #
t_social_strug <- as.data.frame( ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~GVTREGN, 
                                           deshhd1215_social, 
                                           FUN = svymean, na.rm = TRUE))
)


t_social_strug <- dcast(t_social_strug, GVTREGN + Var3 ~ Var2)
t_social_strug$Var3 <- factor( t_social_strug$Var3, levels = c("factor(struggling_housing_g1)0", "factor(struggling_housing_g1)1"), 
                            labels = c("Not struggling", "Struggling")) 


#Plot as points with error bars
ggplot(na.omit(t_social_strug), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Proportion of social households") + guides(colour=FALSE) + ylab("Struggling or not")+
  xlim(0, 1) +
  ggtitle("social households by struggling by region") + facet_grid( GVTREGN~ .)

# table, % social households struggling
affdt1215_1BUHRP[ social_renter == 1 ,
                  sjt.xtab(GVTREGN, struggling_housing_g1, 
                           title = "Struggling (gross, def1) by region, social households",
                           var.labels=c("Region", "Struggling with housing?"), 
                           #value.labels = c("social", "social", "outright owner", "mortgaged owner"),
                           weight.by = hhweight_3, 
                           show.col.prc = TRUE,
                           show.row.prc = TRUE,
                           use.viewer = FALSE)
                  ]


## struggling by HA/council####
ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
             deshhd1215_social, 
             FUN = svymean, na.rm = TRUE))

ftable(svyby(formula = ~factor(PTENTYP2), by = ~factor(struggling_housing_g1), 
             deshhd1215_social, 
             FUN = svymean, na.rm = TRUE))

#landlord by region for struggling social households
t_socstrug_landlord <- as.data.frame(ftable(svyby(formula = ~factor(PTENTYP2), by = ~GVTREGN, 
             design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
             FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord <- dcast(t_socstrug_landlord, GVTREGN + Var3 ~ Var2)
t_socstrug_landlord$Var3 <- factor(t_socstrug_landlord$Var3, levels = c("factor(PTENTYP2)1", "factor(PTENTYP2)2"),
                                   labels = c("council", "HA"))

ggplot(na.omit(t_socstrug_landlord), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Proportion of social households") + guides(colour=FALSE) + ylab("Struggling or not")+
  xlim(0, 1) +
  ggtitle("Landlords of struggling social households by region") + facet_grid( GVTREGN~ .)

affdt1215_1BUHRP[ social_renter == 1 &  struggling_housing_g1,
                  sjt.xtab( PTENTYP2, GVTREGN,
                           title = "Landlord (gross, def1) by region, struggling social households",
                           var.labels=c("Region", "Landlord"), 
                           #value.labels = c("social", "social", "outright owner", "mortgaged owner"),
                           weight.by = hhweight_3, 
                           #show.col.prc = TRUE,
                           show.row.prc = TRUE,
                           use.viewer = FALSE)
                  ]


affdt1215_1BUHRP[ social_renter == 1 & GVTREGN == "South West" ,
                  sjt.xtab( PTENTYP2, struggling_housing_g1,
                            title = "Landlord (gross, def1) by struggling social households, South West",
                            var.labels=c("Region", "Landlord"), 
                            #value.labels = c("social", "social", "outright owner", "mortgaged owner"),
                            weight.by = hhweight_3, 
                            #show.col.prc = TRUE,
                            show.row.prc = TRUE,
                            use.viewer = FALSE)
                  ]



#North East North West Yorkshire and Humber East Midlands West Midlands East London South East South West
t_socstrug_landlord_NE <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                  design = subset(deshhd1215_social, GVTREGN == "North East" ), 
                                                  FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_NE<- dcast(t_socstrug_landlord_NE, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_NE <- t_socstrug_landlord_NE[t_socstrug_landlord_NE$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_NE$Var3 <- NULL
t_socstrug_landlord_NE$region <- "North East"

t_socstrug_landlord_NW <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "North West" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_NW<- dcast(t_socstrug_landlord_NW, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_NW <- t_socstrug_landlord_NW[t_socstrug_landlord_NW$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_NW$Var3 <- NULL
t_socstrug_landlord_NW$region <- "North West"

t_socstrug_landlord_YH <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "Yorkshire and Humber" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_YH<- dcast(t_socstrug_landlord_YH, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_YH <- t_socstrug_landlord_YH[t_socstrug_landlord_YH$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_YH$Var3 <- NULL
t_socstrug_landlord_YH$region <- "Yorks & Humber"

t_socstrug_landlord_EM <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "East Midlands" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_EM<- dcast(t_socstrug_landlord_EM, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_EM <- t_socstrug_landlord_EM[t_socstrug_landlord_EM$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_EM$Var3 <- NULL
t_socstrug_landlord_EM$region <- "East Midlands"

t_socstrug_landlord_WM <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "West Midlands" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_WM<- dcast(t_socstrug_landlord_WM, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_WM <- t_socstrug_landlord_WM[t_socstrug_landlord_WM$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_WM$Var3 <- NULL
t_socstrug_landlord_WM$region <- "West Midlands"

t_socstrug_landlord_E <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "East" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_E<- dcast(t_socstrug_landlord_E, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_E <- t_socstrug_landlord_E[t_socstrug_landlord_E$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_E$Var3 <- NULL
t_socstrug_landlord_E$region <- "East"

t_socstrug_landlord_L <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                    design = subset(deshhd1215_social, GVTREGN == "London" ), 
                                                    FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_L<- dcast(t_socstrug_landlord_L, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_L <- t_socstrug_landlord_L[t_socstrug_landlord_L$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_L$Var3 <- NULL
t_socstrug_landlord_L$region <- "London"

t_socstrug_landlord_SE <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                    design = subset(deshhd1215_social, GVTREGN == "South East" ), 
                                                    FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_SE<- dcast(t_socstrug_landlord_SE, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_SE <- t_socstrug_landlord_SE[t_socstrug_landlord_SE$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_SE$Var3 <- NULL
t_socstrug_landlord_SE$region <- "South East"

t_socstrug_landlord_SW <- as.data.frame(ftable(svyby(formula = ~factor(struggling_housing_g1), by = ~factor(PTENTYP2), 
                                                     design = subset(deshhd1215_social, GVTREGN == "South West" ), 
                                                     FUN = svymean, na.rm = TRUE)))
t_socstrug_landlord_SW<- dcast(t_socstrug_landlord_SW, factor.PTENTYP2. + Var3 ~ Var2)

t_socstrug_landlord_SW <- t_socstrug_landlord_SW[t_socstrug_landlord_SW$Var3 == "factor(struggling_housing_g1)1" , ]
t_socstrug_landlord_SW$Var3 <- NULL
t_socstrug_landlord_SW$region <- "South West"

#append % of struggling by landlord for each region in to one data frame for plotting

l <- list(t_socstrug_landlord_NE, t_socstrug_landlord_NW, t_socstrug_landlord_YH, t_socstrug_landlord_EM, t_socstrug_landlord_WM,
          t_socstrug_landlord_E, t_socstrug_landlord_L, t_socstrug_landlord_SE, t_socstrug_landlord_SW)
t_socstrug_landlord_reg <- rbindlist(l, fill = TRUE)

#tidy up
rm(t_socstrug_landlord_NE, t_socstrug_landlord_NW, t_socstrug_landlord_YH, t_socstrug_landlord_EM, t_socstrug_landlord_WM,
   t_socstrug_landlord_E, t_socstrug_landlord_L, t_socstrug_landlord_SE, t_socstrug_landlord_SW)

t_socstrug_landlord_reg$factor.PTENTYP2. <- factor(t_socstrug_landlord_reg$factor.PTENTYP2., levels = c("1", "2"),
                                                   labels = c("council", "HA"))
t_socstrug_landlord_reg$landlord <-t_socstrug_landlord_reg$factor.PTENTYP2.
t_socstrug_landlord_reg$factor.PTENTYP2. <- NULL

ggplot(na.omit(t_socstrug_landlord_reg), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = landlord, colour = landlord)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = landlord, yend=landlord)) +
  xlab("Proportion of households struggling") + guides(colour=FALSE) + ylab("Landlord")+
  xlim(0, 1) +
  ggtitle("% struggling by landlord by region") + facet_grid( region~ .)


## social struggling households stats ####
affdt1215_strug_social <- affdt1215_strug[social_renter == 1,]

#gross household income
ggplot(affdt1215_strug_social, 
       aes(x = hh_grossinc, colour=grpd_region1, weight = hhweight_3)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_freqpoly(binwidth = 1000) + ggtitle("Gross household income - struggling social households") +
  xlim(-100, 50000) #+

social<- as.data.frame(ftable(svyby(~hh_grossinc, by = ~GVTREGN, design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.5), ci = TRUE, na.rm = TRUE)))
prs<- as.data.frame(ftable(svyby(~hh_grossinc, by = ~GVTREGN, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.5), ci = TRUE, na.rm = TRUE)))

# On housing benefit? social struggling households
t_socialstrug_hbstatus <- as.data.frame( ftable(svyby(formula = ~receives_hb, by = ~GVTREGN, 
                                                   design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                   FUN = svymean, na.rm = TRUE))
)


t_socialstrug_hbstatus <- dcast(t_socialstrug_hbstatus, GVTREGN + Var3 ~ Var2)
t_socialstrug_hbstatus$Var3 <- factor( t_socialstrug_hbstatus$Var3, levels = c("receives_hb0", "receives_hb1"), 
                                    labels = c("No HB", "Receives HB")) 


#Plot as points with error bars
ggplot(na.omit(t_socialstrug_hbstatus), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = Var3, colour = Var3)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = Var3, yend=Var3)) +
  xlab("Proportion of struggling social households") + guides(colour=FALSE) + ylab("HB status")+
  ggtitle("Struggling social households by HB status by region") + facet_grid( GVTREGN~ .)


#table of whether on HB or not
affdt1215_strug[ social_renter == 1 ,
                 sjt.xtab(GVTREGN, (HBENBU>0), 
                          title = "Housing benefit by region, social households struggling with housing (gross, def1)",
                          var.labels=c("Region", "Household in receipt of HB"), 
                          #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
                          weight.by = hhweight_3, 
                          show.col.prc = TRUE,
                          show.row.prc = TRUE,
                          use.viewer = FALSE)
                 ]

#justifies incomes without HB
ftable(svyby(~hh_grossinc_nohb, by = ~GVTREGN, design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#compare no_hb median income for social and private
t_grossnohb_socialstr <- as.data.frame(ftable(svyby(~hh_grossinc_nohb, by = ~GVTREGN, design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.5), ci = TRUE, na.rm = TRUE)))

t_grossnohb_prsstr <- as.data.frame(ftable(svyby(~hh_grossinc_nohb, by = ~GVTREGN, design = subset(deshhd1215_prs, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.5), ci = TRUE, na.rm = TRUE)))

ftable(svyby(~(HBENBU*52), by = ~GVTREGN, design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
             FUN = svyquantile, 
             quantiles = c(0.5), ci = TRUE, na.rm = TRUE))


#% gross income (including HB) spent on housing
#all social renters
ftable(svyby(~housing_cost_gross, by = ~GVTREGN, design = deshhd1215_social, 
             FUN = svyquantile, 
             quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#struggling renters on HB
t_strugglingsocial_costtoinc_hb <- as.data.frame(svyby(~housing_cost_gross, by = ~GVTREGN, 
                                                    design = subset(deshhd1215_social, struggling_housing_g1 == 1 & receives_hb == 1), 
                                                    FUN = svyquantile,
                                                    quantiles = 0.5, ci = TRUE, na.rm = TRUE))

#struggling renters not on HB
t_strugglingsocial_costtoinc_nohb <- as.data.frame(svyby(~housing_cost_gross, by = ~GVTREGN, 
                                                      design = subset(deshhd1215_social, struggling_housing_g1 == 1 & receives_hb == 0),
                                                      FUN = svyquantile,
                                                      quantiles = 0.5, ci = TRUE, na.rm = TRUE))


#Age of struggling social households
deshhd1215_social <-update(deshhd1215_social, HDAGE = factor(HDAGE)) 
t_age_socialstruggle <- as.data.frame( ftable( svyby(formula = ~HDAGE, by = ~grpd_region1, 
                                                  design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                  FUN = svymean, na.rm = TRUE))
)


t_age_socialstruggle <- dcast(t_age_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_age_socialstruggle$Var3 <- factor(t_age_socialstruggle$Var3, levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                                 labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"))

#Plot as points with error bars
ggplot(na.omit(t_age_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + ylab("Age") +
  ggtitle("Age of head for struggling social households") + facet_grid(Var3~ .)

affdt1215_strug_social[ , sjt.xtab(HDAGE, grpd_region1,
                                var.labels=c("Age of HRP", "Region"),
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]

affdt1215_1BUHRP[ social_renter == 1 , sjt.xtab(HDAGE, grpd_region1,
                                             var.labels=c("Age of HRP", "Region"),
                                             #value.labels = c("Can't afford", "Can afford"),
                                             weight.by = hhweight_3, 
                                             show.col.prc = TRUE,
                                             #show.row.prc = TRUE,
                                             use.viewer = FALSE)
                  ]


#Self-reported employment of HRP
deshhd1215_social <-update(deshhd1215_social, SELFDEMP = factor(SELFDEMP, 
                                                          labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                                                     "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other")))
svymean(~SELFDEMP, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
t_emply_socialstruggle <- as.data.frame( ftable( svyby(formula = ~SELFDEMP, by = ~grpd_region1, 
                                                    design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)


t_emply_socialstruggle <- dcast(t_emply_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_emply_socialstruggle$Var3 <- factor(t_emply_socialstruggle$Var3, levels = c("SELFDEMPFull-time", "SELFDEMPPart-time", "SELFDEMPFT self-employed",
                                                                        "SELFDEMPPT self-employed", "SELFDEMPUnemployed", "SELFDEMPStudent", "SELFDEMPFamily home", 
                                                                        "SELFDEMPDisabled", "SELFDEMPRetired", "SELFDEMPOther"),
                                   labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                              "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"))

#Plot as points with error bars
ggplot(na.omit(t_emply_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Self-reported situation of head  for struggling social households ") + facet_grid( Var3~ .)

affdt1215_strug_social[ , sjt.xtab(SELFDEMP, grpd_region1,
                                var.labels=c("Self-reported employment of HRP", "Region"),
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


# Employment of whole household
deshhd1215_social <-update(deshhd1215_social, ECOBU = factor(ECOBU)) 
t_emplhh_socialstruggle <- as.data.frame( ftable( svyby(formula = ~ECOBU, by = ~grpd_region1, 
                                                     design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                     FUN = svymean, na.rm = TRUE))
)


t_emplhh_socialstruggle <- dcast(t_emplhh_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_emplhh_socialstruggle$Var3 <- factor(t_emplhh_socialstruggle$Var3, levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
                                                                          "ECOBU8"),
                                    labels = c("1+ self employed", "Sing/couple all FT", 
                                               "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                               "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                               "Workless, head/spouse unemployed", "Workless, inactive" ))


#Plot as points with error bars
#by region
ggplot(na.omit(t_emplhh_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Household employment for struggling social households") + facet_grid( Var3~ .)

affdt1215_strug_social[ , sjt.xtab(ECOBU, grpd_region1,
                                var.labels=c("Household employment status", "Region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


#ILO definition employment
affdt1215_strug_social[ , sjt.xtab(DVIL04A, grpd_region1,
                                var.labels=c("ILO employment", "Region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]



##School age children

svytotal(~factor(has_kids16), design = subset(deshhd1215_social, struggling_housing_g1 == 1))
svytotal(~factor(has_kids16), design = deshhd1215_social)

svymean(~factor(has_kids16), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(has_kids16), design = deshhd1215_social, na.rm = TRUE)

#struggling households
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all social 
ftable( svyby(formula = ~factor(has_kids16), by = ~grpd_region1, 
              design = deshhd1215_social, 
              FUN = svymean, na.rm = TRUE))

##single parents
svytotal(~factor(single_parent), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(single_parent), design = deshhd1215_social, na.rm = TRUE)

svymean(~factor(single_parent), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)

t_singlep_social<- as.data.frame( ftable(svymean(~factor(single_parent), design = deshhd1215_social, na.rm = TRUE)))




#struggling households
t_singlep_socialstruggle <- as.data.frame( ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
                                                      design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                      FUN = svymean, na.rm = TRUE)))
t_singlep_socialstruggle <- dcast(t_singlep_socialstruggle, grpd_region1 + Var3 ~ Var2)

#cf all social 
t_singlep_social <- as.data.frame(ftable( svyby(formula = ~factor(single_parent), by = ~grpd_region1, 
                                             design = deshhd1215_social, 
                                             FUN = svymean, na.rm = TRUE)))
t_singlep_social<- dcast(t_singlep_social, grpd_region1 + Var3 ~ Var2)

t_singlep_socialstruggle <- t_singlep_socialstruggle[t_singlep_socialstruggle$Var3 == "factor(single_parent)1" , ]
t_singlep_socialstruggle$Var3 <- NULL
t_singlep_socialstruggle$status <- "Struggling"

t_singlep_social <- t_singlep_social[t_singlep_social$Var3 == "factor(single_parent)1" , ]
t_singlep_social$Var3 <- NULL
t_singlep_social$status <- "All social"

#append % of struggling single vs all in to one data frame for plotting

l <- list(t_singlep_socialstruggle, t_singlep_social)
t_singlep_socialstruggle <- rbindlist(l, fill = TRUE)

#plot as points with error bars
ggplot(na.omit(t_singlep_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y= status, colour = status)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y=status, yend=status)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Single parents; struggling vs all social households") + facet_grid( grpd_region1~ .)

#by working

affdt1215_strug_social[ , sjt.xtab(factor(single_parent), DVIL04A,
                                var.labels=c("Single Parent?", "Employment"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                #show.col.prc = TRUE,
                                show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]


ftable( svyby(formula = ~factor(single_mum), by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))

##Disabled adults and children
#adults
svytotal(~factor(has_disabledad), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(has_disabledad), design = deshhd1215_social, na.rm = TRUE)

svymean(~factor(has_disabledad), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(has_disabledad), design = deshhd1215_social, na.rm = TRUE)

#struggling social
ftable( svyby(formula = ~factor(has_disabledad), by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all social under 55
ftable( svyby(formula = ~factor(has_disabledad), by = ~grpd_region1, 
              design = deshhd1215_social, 
              FUN = svymean, na.rm = TRUE))

affdt1215_strug_social[ , sjt.xtab(factor(has_disabledad), grpd_region1,
                                var.labels=c("Disabled adult?", "Grouped region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]

#children
affdt1215_strug_social[ , sjt.xtab(factor(has_disabledch), grpd_region1,
                                var.labels=c("Disabled child?", "Grouped region"), 
                                #value.labels = c("Can't afford", "Can afford"),
                                weight.by = hhweight_3, 
                                show.col.prc = TRUE,
                                #show.row.prc = TRUE,
                                use.viewer = FALSE)
                     ]

#pensioners - doesn't work for household weighting as need to restrict to 1 BU and if restrict to under 55 
#unlikely to have pensioner in same BU


##Caring reponsibilities
svytotal(~has_carer, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~has_carer, design = deshhd1215_social, na.rm = TRUE)

svymean(~has_carer, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~has_carer, design = deshhd1215_social, na.rm = TRUE)


#struggling social
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all social under 55
ftable( svyby(formula = ~has_carer, by = ~grpd_region1, 
              design = deshhd1215_social, 
              FUN = svymean, na.rm = TRUE))


#care given outside hhld
svytotal(~factor(GIVEHELP), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svytotal(~factor(GIVEHELP), design = deshhd1215_social, na.rm = TRUE)

svymean(~factor(GIVEHELP), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(GIVEHELP), design = deshhd1215_social, na.rm = TRUE)

#struggling
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))
#cf all social under 55
ftable( svyby(formula = ~factor(GIVEHELP), by = ~grpd_region1, 
              design = deshhd1215_social, 
              FUN = svymean, na.rm = TRUE))

#Savings and ability to save at least £10 a month
svyquantile(~TOTCAPB3, design = subset(deshhd1215_social, struggling_housing_g1 == 1), quantile = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), ci = TRUE, na.rm = TRUE)
svymean(~(TOTCAPB3==0), design = subset(deshhd1215_social, struggling_housing_g1 == 1), ci = TRUE, na.rm = TRUE)
svyby(~(TOTCAPB3==0), by = ~grpd_region1, design = subset(deshhd1215_social, struggling_housing_g1 == 1), FUN= svymean, ci = TRUE, na.rm = TRUE)
svymean(~(TOTCAPB3==0), design = deshhd1215_social, ci = TRUE, na.rm = TRUE)
svyby(~(TOTCAPB3==0), by = ~grpd_region1, deshhd1215_social, FUN= svymean, ci = TRUE, na.rm = TRUE)

deshhd1215_social <-update(deshhd1215_social, ADDMON = factor(ADDMON)) 
svymean(~factor(ADDMON), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE)
svymean(~factor(ADDMON), design = deshhd1215_social, na.rm = TRUE)

t_save10_socialstruggle <- as.data.frame( ftable( svyby(formula = ~ADDMON, by = ~grpd_region1, 
                                                     design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                     FUN = svymean, na.rm = TRUE))
)


t_save10_socialstruggle <- dcast(t_save10_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_save10_socialstruggle$Var3 <- factor(t_save10_socialstruggle$Var3, levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                                    labels = c("Do this", "Like to but can't afford", 
                                               "Don't want to",  "Does not apply" ))

#Plot as points with error bars
#by region
ggplot(na.omit(t_save10_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Ability to save £10 for struggling social households") + facet_grid( Var3~ .)

#Behind on rent, utilities, other debts
deshhd1215_social <-update(deshhd1215_social, DEBTFRE1 = factor(DEBTFRE1)) 
deshhd1215_social <-update(deshhd1215_social, DEBTFRE2 = factor(DEBTFRE2))
deshhd1215_social <-update(deshhd1215_social, DEBTFRE3 = factor(DEBTFRE3)) 

t_debt1_socialstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE1, by = ~grpd_region1, 
                                                    design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)
t_debt2_socialstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE2, by = ~grpd_region1, 
                                                    design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)
t_debt3_socialstruggle <- as.data.frame( ftable( svyby(formula = ~DEBTFRE3, by = ~grpd_region1, 
                                                    design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
                                                    FUN = svymean, na.rm = TRUE))
)


t_debt1_socialstruggle <- dcast(t_debt1_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_debt2_socialstruggle <- dcast(t_debt2_socialstruggle, grpd_region1 + Var3 ~ Var2)
t_debt3_socialstruggle <- dcast(t_debt3_socialstruggle, grpd_region1 + Var3 ~ Var2)

t_debt1_socialstruggle$Var3 <- factor(t_debt1_socialstruggle$Var3, levels = c("DEBTFRE10", "DEBTFRE11", "DEBTFRE12"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))
t_debt2_socialstruggle$Var3 <- factor(t_debt2_socialstruggle$Var3, levels = c("DEBTFRE20", "DEBTFRE21", "DEBTFRE22"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))
t_debt3_socialstruggle$Var3 <- factor(t_debt3_socialstruggle$Var3, levels = c("DEBTFRE30", "DEBTFRE31", "DEBTFRE32"),
                                   labels = c("Not behind", "Behind once", 
                                              "Behind twice or more"))

#Plot as points with error bars
#by region
ggplot(na.omit(t_debt1_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with rent for struggling social households") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt2_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with utility bills for struggling social households") + facet_grid( Var3~ .)

ggplot(na.omit(t_debt3_socialstruggle), aes(x = svymean, xmin = svymean-SE, xmax = svymean+SE, y = grpd_region1, colour = grpd_region1)) +
  geom_point() + geom_segment( aes(x = svymean-SE, xend = svymean+SE, y = grpd_region1, yend=grpd_region1)) +
  xlab("Prop in category") + guides(colour=FALSE) + ylab("Grouped region")+
  ggtitle("Times behind with other bills for struggling social households") + facet_grid( Var3~ .)

#table
#rent
svymean(~DEBTFRE1, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all social under 55
svymean(~DEBTFRE1, design = deshhd1215_social, na.rm = TRUE, ci = TRUE)

#utilities
svymean(~DEBTFRE2, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all social under 55
svymean(~DEBTFRE2, design = deshhd1215_social, na.rm = TRUE, ci = TRUE)

#other bills
svymean(~DEBTFRE3, design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
#all social under 55
svymean(~DEBTFRE3, design = deshhd1215_social, na.rm = TRUE, ci = TRUE)

#able to afford £200?
svymean(~factor(OAEXPNS), design = subset(deshhd1215_social, struggling_housing_g1 == 1), na.rm = TRUE, ci = TRUE)
ftable( svyby(formula = ~factor(OAEXPNS), by = ~grpd_region1, 
              design = subset(deshhd1215_social, struggling_housing_g1 == 1), 
              FUN = svymean, na.rm = TRUE))

#how to afford it
#own income and cut back on essentials
svymean(~factor(OAHOWPY1), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#own income and not cut back
svymean(~factor(OAHOWPY2), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#savings
svymean(~factor(OAHOWPY3), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#credit (card or loan)
svymean(~factor(OAHOWPY4), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#family/friends
svymean(~factor(OAHOWPY5), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)
#other
svymean(~factor(OAHOWPY6), design = subset(deshhd1215_social, struggling_housing_g1 == 1 & OAEXPNS == 1), na.rm = TRUE, ci = TRUE)


###############People who can't afford 80% market rent by GVTREGN
deshhd_1215 <- update(deshhd_1215, aff_rentmed80_g = factor(aff_rentmed80_g))
t_rentm80g_tot <- as.data.frame(ftable(svyby(formula = ~aff_rentmed80_g, by = ~GVTREGN, design = subset(deshhd_1215, social_renter == 1), 
                                             FUN = svytotal, na.rm = TRUE)))
t_rentm80g_tot <- dcast(t_rentm80g_tot, GVTREGN + Var3 ~ Var2)
t_rentm80g_tot$Var3 <- factor(t_rentm80g_tot$Var3, levels = c("aff_rentmed80_g0", "aff_rentmed80_g1"),
                              labels = c("Can't afford", "Can afford"))

t_rentm80g_per <- as.data.frame(ftable(svyby(formula = ~aff_rentmed80_g, by = ~GVTREGN, design = subset(deshhd_1215, social_renter == 1), 
                                             FUN = svymean, na.rm = TRUE)))
t_rentm80g_per <- dcast(t_rentm80g_per, GVTREGN + Var3 ~ Var2)
t_rentm80g_per$Var3 <- factor(t_rentm80g_per$Var3, levels = c("aff_rentmed80_g0", "aff_rentmed80_g1"),
                              labels = c("Can't afford", "Can afford"))





#with HB
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentmed_g, weight.by = hhweight_2)]
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentlq_g, weight.by = hhweight_2)]
# 
# #without HB
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentmed_gnohb, weight.by = hhweight_2)]
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentlq_gnohb, weight.by = hhweight_2)]
# 
# #Cross tabs
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
#                                                                        weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# affdt_1215_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP), sjt.xtab(aff_rentlq_g, GVTREGN,
#                                                                       var.labels=c("Can afford LQ rent", "Region"), 
#                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# #Social renters
# #Cross tabs
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2) & is_HRP ), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
#                                                                        weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2) & is_HRP ), sjt.xtab(aff_rentlq_g, GVTREGN, var.labels=c("Can afford LQ rent", "Region"), 
#                                                                        weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# #All renters
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
#                                                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.xtab(aff_rentlq_g, GVTREGN, var.labels=c("Can afford LQ rent", "Region"), 
#                                                                                                        weight.by = hhweight_2, show.col.prc = TRUE)
#                 ]
# 
# 
# #Household characteristics
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP & GVTREGN == "London"), sjt.xtab(aff_rentmed_g, SELFDEMP, var.labels=c("Can afford med rent", "Region"),
#                                                                                                                             weight.by = hhweight_2, show.row.prc = TRUE)
#                 ]
# 
# affdt_1215_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP & GVTREGN != "London"), sjt.xtab(aff_rentmed_g, SELFDEMP, var.labels=c("Can afford med rent", "Region"),
#                                                                                                                             weight.by = hhweight_2, show.row.prc = TRUE)
#                 ]



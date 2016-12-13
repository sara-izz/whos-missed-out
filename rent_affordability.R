install.packages("devtools")
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

### Source and define calculating functions ####

## Make flatfiles for each year
source("make_flatfile.R")
## Calculate number of bedrooms needed by household
## #see http://england.shelter.org.uk/get_advice/housing_benefit_and_local_housing_allowance/what_is_housing_benefit/local_housing_allowance
source("calculate_bedrooms.R")
## Attach 14/15 private rent for number of bedrooms needed 
source("attach_private_rents.R")
##Logical flags for later analysis #
source("calculate_logical_flags.R")
## Calculate deflated household incomes 
source("calculate_household_incomes.R")
source("calculate_weights.R")
## Tidy up tenure 
source("tidy_tenure.R")
## Tidy up regions 
source("tidy_regions.R")

## Plotting functions
#Points with error bars to compare categories by region
source("plot_survey_grpdregion.R")

# Struggling according to Bramley 2011 definition; more than 25% gross income AND self-reported burden; 
# call calculate.incomes first
flag.struggling <- function(data_table) {
  data_table[ , struggling_housing_g1 := ifelse(housing_cost > (0.25*(hh_grossinc/52)) & (BURDEN == 1 | BURDEN == 2), 1, 0) ]
  
  #try without HB
  data_table[ , struggling_housing_nohb := ifelse(housing_cost > (0.25*(hh_grossinc_nohb/52)) & (BURDEN == 1 | BURDEN == 2), 1, 0) ]
  
  #try with heavy burden only
  data_table[ , struggling_housing_tight := ifelse(housing_cost > (0.25*(hh_grossinc/52)) & (BURDEN == 1), 1, 0) ]
  
  #try simple struggling based on can't afford now
  data_table[ , struggling_housing_g2 := ifelse(housing_cost > (0.25*(hh_grossinc/52)), 1, 0) ]
}


# Flags for afford rent - call calculate.incomes first
flag.afford.rent <- function(data_table){
  # flag if can afford median market rent, using gross household income
  data_table[ , amount_canaff_g := (hh_grossinc*0.35)/12]
  data_table[ nohbai_inc == 0, aff_rentmed_g := ifelse(   amount_canaff_g >= rent_med, 1, 0 )]
  data_table[ nohbai_inc == 0, aff_rentlq_g := ifelse(   amount_canaff_g >= rent_lq, 1, 0 )]
  
  # flag is can afford median market rent, using gross household income without hb
  data_table[ multiple_benu == 0, amount_canaff_gnohb := (hh_grossinc_nohb*0.3)/12]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed_gnohb := ifelse(   amount_canaff_gnohb >= rent_med, 1, 0 )]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq, 1, 0 )]
  
  #amount can afford based on earnings alone (restrict to working sample when using this)
  data_table[ multiple_benu == 0, amount_canaff_gearn := (hh_grossinc_earn*0.3)/12]
  
  #flags for 80% market rent
  data_table[ nohbai_inc == 0, aff_rentmed80_g := ifelse(   amount_canaff_g >= rent_med80, 1, 0 )]
  data_table[ nohbai_inc == 0, aff_rentlq80_g := ifelse(   amount_canaff_g >= rent_lq80, 1, 0 )]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed80_gnohb := ifelse(   amount_canaff_gnohb >= rent_med80, 1, 0 )]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq80_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq80, 1, 0 )]
  
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed80_gearn := ifelse(   amount_canaff_gearn >= rent_med80, 1, 0 )]
  
  #flags for 90% market rent
  data_table[ nohbai_inc == 0, aff_rentmed90_g := ifelse(   amount_canaff_g >= rent_med90, 1, 0 )]
  data_table[ nohbai_inc == 0, aff_rentlq90_g := ifelse(   amount_canaff_g >= rent_lq90, 1, 0 )]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed90_gnohb := ifelse(   amount_canaff_gnohb >= rent_med90, 1, 0 )]
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq90_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq90, 1, 0 )]
  
  data_table[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed90_gearn := ifelse(   amount_canaff_gearn >= rent_med90, 1, 0 )]
}

# prep.dataset <- function(data_table, num_years, hh_survey, dt_year){
#   data_table <- calculate.bedrooms(data_table)
#   data_table <- attach.private.rents(data_table, derived_data_dir)
#   data_table <- calculate.logical.flags(data_table, dt_year)
#   data_table <- calculate.household.incomes(data_table, hh_survey)
#   data_table <- calculate.weights(data_table, num_years)
#   data_table <- tidy.tenure(data_table)
#   
#   #Do after attaching VOA rents
#   data_table <- tidy.regions(data_table, dt_year)
#   
#   #Post-income calc functions
#   if(year != "1112") {data_table <- flag.struggling(data_table)}
#   data_table <- flag.afford.rent(data_table)
# }


#####~~~~~ Start Analysis ~~~~#####

curr_data_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/"
derived_data_dir <- "~/R analysis/whos_missed_out/derived data sets/"
results_dir <- "S:/@Communications, Policy and Campaigns/Research/RESEARCH/More Affordable Homes/Social housing attitudes and needs/Needs/"


affdt_1314 <- make.flatfile(curr_data_dir,"1314")
affdt_1415 <- make.flatfile(curr_data_dir,"1415")
affdt_1213 <- make.flatfile(curr_data_dir,"1213")
affdt_1112 <- make.flatfile(curr_data_dir,"1112")
affdt_1011 <- make.flatfile(curr_data_dir,"1011")
affdt_0910 <- make.flatfile(curr_data_dir,"0910")

## Add year marker to each file
affdt_1314[ , year := 1314]
affdt_1415[ , year := 1415]
affdt_1213[ , year := 1213]
affdt_1112[ , year := 1112]
affdt_1011[ , year := 1011]
affdt_0910[ , year := 0910]

## append datasets together, creating missing value variables for each file where columns are missing (set argument fill=TRUE on rbindlist())
l <- list(affdt_1314, affdt_1415)
affdt_1315 <- rbindlist(l, fill = TRUE)
rm(l)

l <- list(affdt_1213, affdt_1314, affdt_1415)
affdt_1215 <- rbindlist(l, fill = TRUE)
rm(l)

l <- list(affdt_1011, affdt_1112)
affdt_1012 <- rbindlist(l, fill = TRUE)
rm(l)

l <- list(affdt_0910, affdt_1011)
affdt_0911 <- rbindlist(l, fill = TRUE)
rm(l)


#clean up
# rm(affdt_1213)
# rm(affdt_1314)
# rm(affdt_1415)

#Prep 1415 dataset ####
affdt_1415 <- calculate.bedrooms(affdt_1415)
affdt_1415 <- attach.private.rents(affdt_1415, derived_data_dir)
affdt_1415 <- calculate.logical.flags(affdt_1415, "1415")
affdt_1415 <- calculate.household.incomes(affdt_1415, "FRS")
affdt_1415 <- calculate.weights(affdt_1415, 1)
affdt_1415 <- tidy.tenure(affdt_1415)

#Do after attaching VOA rents
affdt_1415 <- tidy.regions(affdt_1415, "1415")

#Post-income calc functions
affdt_1415 <- flag.struggling(affdt_1415)
affdt_1415 <- flag.afford.rent(affdt_1415)

#Prep 1215 dataset ####
# affdt_1215 <- calculate.bedrooms(affdt_1215)
# affdt_1215 <- attach.private.rents(affdt_1215, derived_data_dir)
# affdt_1215 <- calculate.logical.flags(affdt_1215)
# affdt_1215 <- calculate.household.incomes(affdt_1215, "FRS")
# affdt_1215 <- calculate.weights(affdt_1215, 3)
# affdt_1215 <- tidy.tenure(affdt_1215)
# 
# #Do after attaching VOA rents
# affdt_1215 <- tidy.regions(affdt_1215)
# 
# #Post-income calc functions
# affdt_1215 <- flag.struggling(affdt_1215)
# affdt_1215 <- flag.afford.rent(affdt_1215)

#Prep 1315 dataset ####
calculate.bedrooms(affdt_1315)
affdt_1315 <- calculate.bedrooms(affdt_1315)
affdt_1315 <- attach.private.rents(affdt_1315, derived_data_dir)
affdt_1315 <- calculate.logical.flags(affdt_1315, "1315")
affdt_1315 <- calculate.household.incomes(affdt_1315, "FRS")
affdt_1315 <- calculate.weights(affdt_1315, 2)
affdt_1315 <- tidy.tenure(affdt_1315)

#Do after attaching VOA rents
affdt_1315 <- tidy.regions(affdt_1315, "1315")

#Post-income calc functions
affdt_1315 <- flag.struggling(affdt_1315)
affdt_1315 <- flag.afford.rent(affdt_1315)

#Prep 1112 dataset ####
affdt_1112 <- calculate.logical.flags(affdt_1112, "1112")
affdt_1112 <- calculate.household.incomes(affdt_1112, "FRS")
affdt_1112 <- calculate.weights(affdt_1112, 1)
affdt_1112 <- tidy.tenure(affdt_1112)

#Do after attaching VOA rents
affdt_1112 <- tidy.regions(affdt_1112, "1112")

#Prep 1011 dataset ####
affdt_1011 <- calculate.logical.flags(affdt_1011, "1011")
affdt_1011 <- calculate.household.incomes(affdt_1011, "FRS")
affdt_1011 <- calculate.weights(affdt_1011, 1)
affdt_1011 <- tidy.tenure(affdt_1011)

#Do after attaching VOA rents
affdt_1011 <- tidy.regions(affdt_1011, "1011")


#Prep 1012 dataset ####
affdt_1012 <- calculate.logical.flags(affdt_1012, "1012")
affdt_1012 <- calculate.household.incomes(affdt_1012, "FRS")
affdt_1012 <- calculate.weights(affdt_1012, 1)
affdt_1012 <- tidy.tenure(affdt_1012)

#Do after attaching VOA rents
affdt_1012 <- tidy.regions(affdt_1012, "1012")

#Prep 0911 dataset ####
affdt_0911 <- calculate.logical.flags(affdt_0911, "0911")
affdt_0911 <- calculate.household.incomes(affdt_0911, "FRS")
affdt_0911 <- calculate.weights(affdt_0911, 1)
affdt_0911 <- tidy.tenure(affdt_0911)

#Do after attaching VOA rents
affdt_0911 <- tidy.regions(affdt_0911, "0911")


#######Create survey objects and data table limiting  weights work.####### 
#datatable at household level that works with HBAI quantities and weights (i.e. restricted to households with one BU)
# affdt1215_1BUHRP <- affdt_1215[is_HRP & 
#                                  (is.na(hhweight_3) == F & 
#                                     multiple_benu == 0), ]

#datatable at household level that works with FRS quantities and weights
affdt1215_1HRP <- affdt_1215[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1215 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1215_1HRP)
#survey object of private renters only
deshhd1215_prs <- subset(deshhd_1215, prs_renter == 1)
#survey object of social renters only
deshhd1215_social <- subset(deshhd_1215, council_renter == 1 | ha_renter == 1)
#data table of struggling households all tenure distributions
affdt1215_strug <- affdt1215_1HRP [struggling_housing_tight == 1, ]

## Repeat for 1315
affdt1315_1HRP <- affdt_1315[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1315 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1315_1HRP)

#make sure all categorical variables used are factors in main survey design
deshhd_1315 <- update(deshhd_1315, grpd_tenure = factor(grpd_tenure))
deshhd_1315 <- update(deshhd_1315, struggling_housing_tight = factor(struggling_housing_tight))
deshhd_1315 <- update(deshhd_1315, receives_hb = factor(receives_hb))
deshhd_1315 <- update(deshhd_1315, HDAGE = factor(HDAGE))
deshhd_1315 <- update(deshhd_1315, SELFDEMP = factor(SELFDEMP))
deshhd_1315 <- update(deshhd_1315, ECOBU = factor(ECOBU))
deshhd_1315 <- update(deshhd_1315, single_parent = factor(single_parent))
deshhd_1315 <- update(deshhd_1315, has_disabledad = factor(has_disabledad))
deshhd_1315 <- update(deshhd_1315, has_disabledch = factor(has_disabledch))
deshhd_1315 <- update(deshhd_1315, has_carer = factor(has_carer))
deshhd_1315 <- update(deshhd_1315, GIVEHELP = factor(GIVEHELP))
deshhd_1315 <- update(deshhd_1315, ADDMON = factor(ADDMON))
deshhd_1315 <- update(deshhd_1315, DEBTFRE1 = factor(DEBTFRE1))
deshhd_1315 <- update(deshhd_1315, DEBTFRE1 = factor(DEBTFRE2))
deshhd_1315 <- update(deshhd_1315, DEBTFRE1 = factor(DEBTFRE3))

#survey object of private renters only
deshhd1315_prs <- subset(deshhd_1315, prs_renter == 1)
#survey object of social renters only
deshhd1315_social <- subset(deshhd_1315, council_renter == 1 | ha_renter == 1)
#data table of struggling households all tenure distributions
affdt1315_strug <- affdt1315_1HRP [struggling_housing_tight == 1, ]

#Repeat for 1011
affdt1011_1HRP <- affdt_1011[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1011 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1011_1HRP)

#Repeat for 1112
affdt1112_1HRP <- affdt_1112[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1112 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1112_1HRP)

#Repeat for 1012
affdt1012_1HRP <- affdt_1012[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1012 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1012_1HRP)


#Repeat for 1415
affdt1415_1HRP <- affdt_1415[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_1415 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt1415_1HRP)
deshhd1415_prs <- subset(deshhd_1415, prs_renter == 1)

#Repeat for 0911
affdt0911_1HRP <- affdt_0911[is_HRP == 1 & !is.na(grossweight), ]
#Create weighted survey object
deshhd_0911 <- svydesign(ids = ~1, weights = ~ grossweight, data = affdt0911_1HRP)
#survey object of private renters only
deshhd0911_prs <- subset(deshhd_0911, prs_renter == 1)
############### Why is London PRS different to SE PRS? #################


# Is PRS bigger? Prop in each tenure
svytotal(~grpd_tenure, deshhd_1415, na.rm = FALSE)
svytotal(~grpd_tenure, deshhd_1112, na.rm = FALSE)
svytotal(~grpd_tenure, deshhd_1011, na.rm = FALSE)

svymean(~grpd_tenure, deshhd_1415, na.rm = FALSE)
svymean(~grpd_tenure, deshhd_1112, na.rm = FALSE)
svymean(~grpd_tenure, deshhd_1011, na.rm = FALSE)

deshhd_1415 <- update(deshhd_1415, grpd_tenure = factor(grpd_tenure))
plot.survey.grpdregion(deshhd_1415, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2014/15" )
plot.survey.grpdregion(deshhd_1415, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2014/15", 
                       svy_fun = svytotal )



deshhd_1112 <- update(deshhd_1112, grpd_tenure = factor(grpd_tenure))
plot.survey.grpdregion(deshhd_1112, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2011/12" )
plot.survey.grpdregion(deshhd_1112, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2011/12",
                       svy_fun = svytotal)

deshhd_1011 <- update(deshhd_1011, grpd_tenure = factor(grpd_tenure))
plot.survey.grpdregion(deshhd_1011, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2011/12" )
plot.survey.grpdregion(deshhd_1011, "grpd_tenure", var_levels = c("grpd_tenure1", "grpd_tenure2", "grpd_tenure3", "grpd_tenure4"), 
                       var_labels = c("PRS", "social", "outright owner", "mortgaged owner"), p_title = "Tenure by region - 2011/12",
                       svy_fun = svytotal)





affdt1315_1HRP[ ,
                  sjt.xtab(GVTREGN, factor(grpd_tenure, levels = c("1", "2", "3", "4"), 
                                           labels = c("PRS", "social", "outright owner", "mortgaged owner")), 
                           title = "Tenure by region",
                           var.labels=c("Region", "Tenure"), 
                           #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
                           #weight.by = grossweight_3, 
                           show.col.prc = TRUE,
                           show.row.prc = TRUE,
                           use.viewer = FALSE)
                  ]



## Overall rent/incomes for renters in north and London ####

t_rentgross_1315 <- as.data.frame(svyby(~housing_cost_gross, by = ~grpd_region2, design = deshhd1315_prs, 
                                            FUN = svyquantile, 
                                            quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_rentgross_1415 <- as.data.frame(svyby(~housing_cost_gross, by = ~grpd_region2, design = deshhd1415_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_rentgross_0911 <- as.data.frame(svyby(~housing_cost_gross, by = ~grpd_region2, design = deshhd0911_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_rentgross_0910 <- as.data.frame(svyby(~housing_cost_gross, by = ~grpd_region2, design = deshhd0910_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

# rent/income - no HB
t_rentnohb_1315 <- as.data.frame(svyby(~housing_cost_gnohb, by = ~grpd_region2, design = deshhd1315_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_rentnohb_0911 <- as.data.frame(svyby(~housing_cost_gnohb, by = ~grpd_region2, design = deshhd0911_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#rent
t_rent_1315 <- as.data.frame(svyby(~housing_cost, by = ~grpd_region2, design = deshhd1315_prs, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_rent_0911 <- as.data.frame(svyby(~housing_cost, by = ~grpd_region2, design = deshhd0911_prs, 
                                   FUN = svyquantile, 
                                   quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#gross incomes
t_grossinc_1315 <- as.data.frame(svyby(~hh_grossinc, by = ~grpd_region2, design = deshhd1315_prs, 
                                   FUN = svyquantile, 
                                   quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_grossinc_0911 <- as.data.frame(svyby(~hh_grossinc, by = ~grpd_region2, design = deshhd0911_prs, 
                                   FUN = svyquantile, 
                                   quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

#incomes no HB
t_incnohb_1315 <- as.data.frame(svyby(~hh_grossinc_nohb, by = ~grpd_region2, design = deshhd1315_prs, 
                                       FUN = svyquantile, 
                                       quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_incnohb_0911 <- as.data.frame(svyby(~hh_grossinc_nohb, by = ~grpd_region2, design = deshhd0911_prs, 
                                              FUN = svyquantile, 
                                              quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))





# affdt1215_1HRP[  ,
#                   sjt.xtab(GVTREGN, factor(HHSTAT, levels = c("1", "2"), labels = c("Conventional", "Shared")), 
#                            title = "Sharing by region",
#                            var.labels=c("Region", "Shared household?"), 
#                            #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
#                            weight.by = grossweight_3, 
#                            #show.col.prc = TRUE,
#                            show.row.prc = TRUE,
#                            use.viewer = FALSE)
#                   ]
# 
# affdt1215_1HRP[ HHSTAT == 1 ,
#                  sjt.xtab(GVTREGN, factor(multiple_benu, levels = c("0", "1"), labels = c("1 Benefit Unit", "Multiple Benefit Units")), 
#                           title = "More than one benefit unit by region",
#                           var.labels=c("Region", "More than one benefit unit?"), 
#                           #value.labels = c("PRS", "social", "outright owner", "mortgaged owner"),
#                           weight.by = grossweight_3, 
#                           #show.col.prc = TRUE,
#                           show.row.prc = TRUE,
#                           use.viewer = FALSE)
#                  ]

affdt1215_1HRP[ , sjt.xtab( GVTREGN, factor(struggling_housing_tight, levels = c("0", "1"),
                                            labels = c("Not struggling", "Struggling")),
                            title = "Struggling renters by region",
                            weight.by = grossweight_3,
                            show.row.prc = TRUE,
                            show.col.prc = TRUE,
                            use.viewer = FALSE)
                ]

## Difference in struggling renters between north and London ####

#struggling households overall 


#How much of PRS is struggling
deshhd1315_prs <- update(deshhd1315_prs, grpd_tenure = factor(struggling_housing_tight))
plot.survey.grpdregion(deshhd1315_prs, "struggling_housing_tight", var_levels = c("struggling_housing_tight0", "struggling_housing_tight1"), 
                       var_labels = c("Not struggling", "Struggling"), 
                       p_title = "Prop of PRS struggling  - 2013/15", region = "grpd_region2")

deshhd1315_prs <- update(deshhd1315_prs, receives_hb = ifelse(HBENBU>0, 1, 0) )
deshhd1315_prs <- update(deshhd1315_prs, receives_hb = factor(receives_hb) )
deshhd1315_prs_strugg <- subset(deshhd1315_prs, struggling_housing_tight == 1)

## PRS struggling households stats ##
affdt1315_strug_prs <- affdt1315_strug[prs_renter == 1,]

## PRS struggling incomes ####
#gross household income #
ggplot(affdt1315_strug_prs[grpd_region2=="North" | grpd_region2 == "London",], 
       aes(x = hh_grossinc, colour=grpd_region2, weight = grossweight)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_freqpoly(binwidth = 1000) + ggtitle("Gross household income - struggling PRS households") +
  xlim(-100, 50000) #+

#household income no HB #
ggplot(affdt1315_strug_prs[grpd_region2=="North" | grpd_region2 == "London",], 
       aes(x = hh_grossinc_nohb, colour=grpd_region2, fill =grpd_region2, weight = grossweight)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 5000, alpha = .5, position = "identity") + ggtitle("Household income, no HB - struggling PRS households") +
  xlim(-100, 50000) #+

ggplot(affdt1315_strug_prs[grpd_region2=="North" | grpd_region2 == "London",], 
       aes(x = hh_grossinc_nohb, fill =grpd_region2, weight = grossweight)) +
  #geom_freqpoly( aes(group = factor(aff_shared_noincben)))
  geom_histogram(binwidth = 5000) + ggtitle("Household income, no HB - struggling PRS households") +
  scale_fill_manual(values=c("#A7A8AA","#FF0000", "cyan4", "darkorange1")) +
  theme( legend.position = "top", panel.background = element_rect(fill = "white"), 
         panel.grid.major = element_line(colour = "grey85")) +
  guides(fill=guide_legend(title=NULL)) +
  xlab("Gross household income, excluding housing benefit") +
  xlim(-100, 50000) + facet_grid( grpd_region2 ~ .)



#gross incomes
t_grossinc_1315_prsstrugg <- as.data.frame(svyby(~hh_grossinc, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                       FUN = svyquantile, 
                                       quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))


#incomes no HB
t_incnohb_1315_prsstrugg <- as.data.frame(svyby(~hh_grossinc_nohb, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                      FUN = svyquantile, 
                                      quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))


#likelihood of receiving HB

plot.survey.grpdregion(deshhd1315_prs_strugg, "receives_hb", var_levels = c("receives_hb0", "receives_hb1"), 
                       var_labels = c("No HB", "Receives HB"), 
                       p_title = "Prop of struggling PRS households receiving HB", region = "grpd_region2")

#rent
t_rent_1315_prsstrugg <- as.data.frame(svyby(~housing_cost, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                   FUN = svyquantile, 
                                   quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

# rent/income - gross
t_rentgross_1315_prsstrugg <- as.data.frame(svyby(~housing_cost_gross, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

# rent/income - no HB
t_rentnohb_1315_prsstrugg <- as.data.frame(svyby(~housing_cost_gnohb, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                       FUN = svyquantile, 
                                       quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

## PRS struggling characteristics ####

# Age
plot.survey.grpdregion(deshhd1315_prs_strugg, "HDAGE", var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                       var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"), 
                       p_title = "Age of struggling PRS ", region = "grpd_region2")

t_age_prsstrugg <- plot.survey.grpdregion(deshhd1315_prs_strugg, "HDAGE", var_levels = c("HDAGE1", "HDAGE2", "HDAGE3", "HDAGE4", "HDAGE5", "HDAGE6"),
                       var_labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65+"), 
                       p_title = "Age of struggling PRS ", region = "grpd_region2", p_type = "bar")

# Self-reported employment status of head of household
plot.survey.grpdregion(deshhd1315_prs_strugg, "SELFDEMP", var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5",
                                                                         "SELFDEMP6", "SELFDEMP7", "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                       var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                  "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"), 
                       p_title = "Self-reported situation of struggling PRS ", region = "grpd_region2")

# Employment of whole household
# Self-reported employment status
t_ecobu_prsstrugg <- plot.survey.grpdregion(deshhd1315_prs_strugg, "ECOBU", var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
                                                                         "ECOBU8"),
                       var_labels = c("1+ self employed", "Sing/couple all FT", 
                                  "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                  "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                  "Workless, head/spouse unemployed", "Workless, inactive" ), 
                       p_title = "Self-reported situation of struggling PRS ", region = "grpd_region2", p_type = "hbar")

#SELFDEMP of head of households for inactive households
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, ECOBU == 8), "SELFDEMP", var_levels = c("SELFDEMP1", "SELFDEMP2", "SELFDEMP3", "SELFDEMP4", "SELFDEMP5",
                                                                         "SELFDEMP6", "SELFDEMP7", "SELFDEMP8", "SELFDEMP9", "SELFDEMP10"),
                       var_labels = c("Full-time", "Part-time", "FT self-employed", "PT self-employed",
                                      "Unemployed", "Student", "Family home", "Disabled", "Retired", "Other"), 
                       p_title = "Self-reported situation of struggling PRS ", region = "grpd_region2")

#ILO employment of head of household
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, DVIL04A = factor(DVIL04A))
plot.survey.grpdregion(deshhd1315_prs_strugg, "DVIL04A", var_levels = c("DVIL04A1", "DVIL04A2", "DVIL04A3", "DVIL04A4"),
                       var_labels = c("Employed", "Family worker", "Unemployed", "Inactive"), 
                       p_title = "HRP employment (ILO) - struggling PRS ", region = "grpd_region2")



# School age kids
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_kids16 = factor(has_kids16))
plot.survey.grpdregion(deshhd1315_prs_strugg, "has_kids16", var_levels = c("has_kids160", "has_kids161"),
                       var_labels = c("No school age kids", "Has school age kids"), 
                       p_title = "School age kids? - struggling PRS ", region = "grpd_region2")

# Single parents
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_kids16 = factor(single_parent))
plot.survey.grpdregion(deshhd1315_prs_strugg, "single_parent", var_levels = c("single_parent0", "single_parent1"),
                       var_labels = c("Not Single parent head", "Single parent head "), 
                       p_title = "Single parent? - struggling PRS ", region = "grpd_region2")

#% working in each region
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, DVIL04A = factor(DVIL04A, levels = c("1", "2","3","4"),
                                                                        labels = c("Employed", "Unpaid family worker", 
                                                                                   "Unemployed", "Inactive")))
t_singlework_prsstrug <- as.data.frame(svyby( ~DVIL04A, by= ~grpd_region2, design = subset(deshhd1315_prs_strugg, single_parent == 1), 
                                              FUN = svymean, na.rm = TRUE))

plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, single_parent == 1), "ECOBU", var_levels = c("ECOBU1", "ECOBU2", "ECOBU3", "ECOBU4", "ECOBU5", "ECOBU6", "ECOBU7",
                                                                      "ECOBU8"),
                       var_labels = c("1+ self employed", "Sing/couple all FT", 
                                      "Couple, 1 FT, 1 PT",  "Couple, 1 FT, 1 not working", 
                                      "No FT, 1+ PT", "Workless, head/spouse aged 60+", 
                                      "Workless, head/spouse unemployed", "Workless, inactive" ), 
                       p_title = "Households work, single parent struggling PRS ", region = "grpd_region2")

# disabled adult or child
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_disabled = ifelse((has_disabledad ==1 | has_disabledch == 1), 1, 0))
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_disabled = factor(has_disabled))
plot.survey.grpdregion(deshhd1315_prs_strugg, "has_disabled", var_levels = c("has_disabled0", "has_disabled1"),
                       var_labels = c("No disabled", "Has disabled person"), 
                       p_title = "Has disabled adult or child? - struggling PRS ", region = "grpd_region2")

# Someone in household gives care to someone else in household
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_carer = factor(has_carer))
plot.survey.grpdregion(deshhd1315_prs_strugg, "has_carer", var_levels = c("has_carerFALSE", "has_carerTRUE"),
                       var_labels = c("No carer", "Has carer"), 
                       p_title = "Has carer? - struggling PRS ", region = "grpd_region2")

# Someone in household gives care to someone outside household
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, has_carer = factor(GIVEHELP))
plot.survey.grpdregion(deshhd1315_prs_strugg, "GIVEHELP", var_levels = c("GIVEHELP1", "GIVEHELP2"),
                       var_labels = c("Helps outside", "Doesn't"), 
                       p_title = "Help given outside hhld? - struggling PRS ", region = "grpd_region2")

## Finances of struggling PRS households ####

# Savings - struggling households 
t_savings_prsstrugg <- as.data.frame(svyby(~TOTCAPB3, by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                        FUN = svyquantile, 
                                        quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_nosavings_prsstrugg <- as.data.frame(svyby(~(TOTCAPB3==0), by = ~grpd_region2, design = deshhd1315_prs_strugg, 
                                             FUN = svymean, na.rm = TRUE))

# Compare with savings - all PRS households 
t_savings_prs<- as.data.frame(svyby(~TOTCAPB3, by = ~grpd_region2, design = deshhd1315_prs, 
                                           FUN = svyquantile, 
                                           quantiles = c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), ci = TRUE, na.rm = TRUE))

t_nosavings_prs <- as.data.frame(svyby(~(TOTCAPB3==0), by = ~grpd_region2, design = deshhd1315_prs, 
                                             FUN = svymean, na.rm = TRUE))

# Can save £10 a month?
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, ADDMON = factor(ADDMON))
plot.survey.grpdregion(deshhd1315_prs_strugg, "ADDMON", var_levels = c("ADDMON1", "ADDMON2", "ADDMON3", "ADDMON4"),
                       var_labels = c("Do this", "Like to but can't afford", "Don't want to",  "Does not apply" ), 
                       p_title = "Can save £10 a month? - struggling PRS ", region = "grpd_region2")

#Behind on rent
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, DEBTFRE1 = factor(DEBTFRE1))
plot.survey.grpdregion(deshhd1315_prs_strugg, "DEBTFRE1", var_levels = c("DEBTFRE10", "DEBTFRE11", "DEBTFRE12"),
                       var_labels = c("Not behind", "Behind once", "Behind twice or more"), 
                       p_title = "Behind with rent? - struggling PRS ", region = "grpd_region2")

#Behind on utilities
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, DEBTFRE2 = factor(DEBTFRE2))
plot.survey.grpdregion(deshhd1315_prs_strugg, "DEBTFRE2", var_levels = c("DEBTFRE20", "DEBTFRE21", "DEBTFRE22"),
                       var_labels = c("Not behind", "Behind once", "Behind twice or more"), 
                       p_title = "Behind with utilities? - struggling PRS ", region = "grpd_region2")

#Behind on other loan payments
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, DEBTFRE3 = factor(DEBTFRE3))
plot.survey.grpdregion(deshhd1315_prs_strugg, "DEBTFRE3", var_levels = c("DEBTFRE30", "DEBTFRE31", "DEBTFRE32"),
                       var_labels = c("Not behind", "Behind once", "Behind twice or more"), 
                       p_title = "Behind with other loans? - struggling PRS ", region = "grpd_region2")


# Can afford £200 expense?
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAEXPNS = factor(OAEXPNS))
plot.survey.grpdregion(deshhd1315_prs_strugg, "OAEXPNS", var_levels = c("OAEXPNS1", "OAEXPNS2"),
                       var_labels = c("Could meet expense", "Couldn't meet expense"), 
                       p_title = "Behind with other loans? - struggling PRS ", region = "grpd_region2")

#own income and cut back on essentials
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY1 = factor(OAHOWPY1))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY1", 
                       var_levels = c("OAHOWPY11", "OAHOWPY12"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Cut back on essentials - struggling PRS ", region = "grpd_region2")

#own income and not cut back
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY2 = factor(OAHOWPY2))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY2", 
                       var_levels = c("OAHOWPY21", "OAHOWPY22"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Own income and don't cut back - struggling PRS ", region = "grpd_region2")

#savings
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY3 = factor(OAHOWPY3))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY3", 
                       var_levels = c("OAHOWPY31", "OAHOWPY32"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Savings - struggling PRS ", region = "grpd_region2")

#credit (card or loan)
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY4 = factor(OAHOWPY4))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY4", 
                       var_levels = c("OAHOWPY41", "OAHOWPY42"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Credit - struggling PRS ", region = "grpd_region2")

#family/friends
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY5 = factor(OAHOWPY5))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY5", 
                       var_levels = c("OAHOWPY51", "OAHOWPY52"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Friends or family - struggling PRS ", region = "grpd_region2")

#other
deshhd1315_prs_strugg <- update(deshhd1315_prs_strugg, OAHOWPY6 = factor(OAHOWPY6))
plot.survey.grpdregion(my_design = subset(deshhd1315_prs_strugg, OAEXPNS == 1), "OAHOWPY6", 
                       var_levels = c("OAHOWPY61", "OAHOWPY62"),
                       var_labels = c("Yes", "No"), 
                       p_title = "Other - struggling PRS ", region = "grpd_region2")

## Social struggling renters vs PRS

deshhd1315_social <- subset(deshhd_1315, grpd_tenure == 2)

deshhd1315_social <- update(deshhd1315_social, grpd_tenure = factor(struggling_housing_tight))
plot.survey.grpdregion(deshhd1315_social, "struggling_housing_tight", var_levels = c("struggling_housing_tight0", "struggling_housing_tight1"), 
                       var_labels = c("Not struggling", "Struggling"), 
                       p_title = "Prop of Social struggling  - 2013/15", region = "grpd_region2")


################ Autumn Statement analysis #######
# Create surveys for prs and social renters
deshhd1315_prs <- subset(deshhd_1315, prs_renter == 1)
deshhd1315_social <- subset(deshhd_1315, social_renter == 1)

deshhd1315_prs_work <- subset(deshhd_1315, (prs_renter == 1 & DVIL04A == 1))

# Prop who can't afford different rent levels

#based on earned income for households where HRP works
deshhd1315_prs <- update(deshhd1315_prs, aff_rentmed80_gearn = factor(aff_rentmed80_gearn))

t_affmed80earn_prs <- plot.survey.grpdregion(my_design = subset(deshhd1315_prs, DVIL04A == 1), 
                                             "aff_rentmed80_gearn", 
                                             var_levels = c("aff_rentmed80_gearn0", "aff_rentmed80_gearn1"), 
                                             var_labels = c("Can't afford", "Can afford") )

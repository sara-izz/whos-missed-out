#Calculates diferent measures of household income and inflates to 14/15 using CPI, for either HBAI or FRS
#Also calculates housing/income ratios
calculate.household.incomes <- function(data_table, survey){
  data_table[ , not_1stbenu := ifelse(BENUNIT >1 , 1, 0)]
  data_table[ , multiple_benu := ifelse( (sum(not_1stbenu) > 0), 1, 0 ), by = .(SERNUM, year) ]
  data_table[ , nohbai_inc := ifelse( (is.na(ESGINCHH) | is.na(BHCDEF) | is.na(HBENBU)) , 1, 0) ]
  
  if(survey == "HBAI") {
    # incomes based on HBAI variables ####
    ## simple gross household income
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ year == '1415',hh_grossinc := ESGINCHH * BHCDEF * 52]
    #inflate 1314 to 1415 prices
    data_table[ year == '1314',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ year == '1213',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/96.8)]
    data_table[ year == '1112',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/94.3)]
    data_table[ year == '1011',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/90.5)]
    data_table[ year == '0910',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/87.4)]
    data_table[ year == '0809',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/87.0)]
    data_table[ year == '0708',hh_grossinc := ESGINCHH * BHCDEF * 52 * (100/84.5)]
    
    ## gross household income minus HB (following definiton only suitable for households with 1 ben unit)
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ (year == '1415' & multiple_benu == 0), hh_grossinc_nohb := (ESGINCHH - HBENBU) * BHCDEF * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314' & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213' & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/96.8)]
    data_table[ (year == '1112'  & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/94.3)]
    data_table[ (year == '1011'  & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/90.5)]
    data_table[ (year == '0910'  & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/87.4)]
    data_table[ (year == '0809'  & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/87.0)]
    data_table[ (year == '0708'  & multiple_benu == 0), hh_grossinc_nohb:= (ESGINCHH - HBENBU) * BHCDEF * 52 * (100/84.5)]
    
    ## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
    data_table[ (year == '1415'), hh_grossinc_noincben := (HHINC - HHIRBEN) * BHCDEF * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/96.8)]
    data_table[ year == '1112', hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/94.3)]
    data_table[ year == '1011', hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/90.5)]
    data_table[ year == '0910', hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/87.4)]
    data_table[ year == '0809', hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/87.0)]
    data_table[ year == '0708', hh_grossinc_noincben:= (HHINC - HHIRBEN) * BHCDEF * 52 * (100/84.5)]
    
    ## gross employment income for the household
    data_table[ (year == '1415'), hh_grossinc_earn := (ESGJOBHH) * BHCDEF * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), hh_grossinc_earn:= (ESGJOBHH) * BHCDEF * 52 * (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), hh_grossinc_earn:= (ESGJOBHH) * BHCDEF * 52 * (100/96.8)]
    data_table[ year == '1112',hh_grossinc := ESGJOBHH * BHCDEF * 52 * (100/94.3)]
    data_table[ year == '1011',hh_grossinc := ESGJOBHH * BHCDEF * 52 * (100/90.5)]
    data_table[ year == '0910',hh_grossinc := ESGJOBHH * BHCDEF * 52 * (100/87.4)]
    data_table[ year == '0809',hh_grossinc := ESGJOBHH * BHCDEF * 52 * (100/87.0)]
    data_table[ year == '0708',hh_grossinc := ESGJOBHH * BHCDEF * 52 * (100/84.5)]
    
    #Deflate housing costs to 14/15 prices
    data_table[ (year == '1415'), housing_cost := GBHSCOST* BHCDEF]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), housing_cost:= GBHSCOST * BHCDEF* (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), housing_cost:= GBHSCOST * BHCDEF * (100/96.8)]
    data_table[ (year == '1112'), housing_cost:= GBHSCOST * BHCDEF * (100/94.3)]
    data_table[ (year == '1011'), housing_cost:= GBHSCOST * BHCDEF * (100/90.5)]
    data_table[ (year == '0910'), housing_cost:= GBHSCOST * BHCDEF * (100/87.4)]
    data_table[ (year == '0809'), housing_cost:= GBHSCOST * BHCDEF * (100/87.0)]
    data_table[ (year == '0708'), housing_cost:= GBHSCOST * BHCDEF * (100/84.5)]
    
    #Deflate savings to 14/15 prices
    data_table[ (year == '1415'), bu_savings := TOTCAPB3* BHCDEF]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), bu_savings:= TOTCAPB3 * BHCDEF* (100/99)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), bu_savings:= TOTCAPB3 * BHCDEF * (100/96.8)]
    data_table[ (year == '1112'), bu_savings:= TOTCAPB3 * BHCDEF * (100/94.3)]
    data_table[ (year == '1011'), bu_savings:= TOTCAPB3 * BHCDEF * (100/90.5)]
    data_table[ (year == '0910'), bu_savings:= TOTCAPB3 * BHCDEF * (100/87.4)]
    data_table[ (year == '0809'), bu_savings:= TOTCAPB3 * BHCDEF * (100/87.0)]
    data_table[ (year == '0708'), bu_savings:= TOTCAPB3 * BHCDEF * (100/84.5)]
  }
  
  if(survey == "FRS"){
    # Incomes based on FRS variables - so can use GROSS4 and not have to worry about restricting to 1BU ####
    #disadvantage is can't use HBAI in-year deflators; mainly an issue for CPI drop in 14/15
    ## simple gross household income
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ year == '1415',hh_grossinc := HHINC * 52]
    #inflate 1314 to 1415 prices
    data_table[ year == '1314',hh_grossinc := HHINC * 52 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ year == '1213',hh_grossinc := HHINC * 52 * (99.98/96.70)]
    data_table[ year == '1112',hh_grossinc := HHINC * 52 * (99.98/94.23)]
    data_table[ year == '1011',hh_grossinc := HHINC * 52 * (99.98/90.33)]
    data_table[ year == '0910',hh_grossinc := HHINC * 52 * (99.98/87.28)]
    data_table[ year == '0809',hh_grossinc := HHINC * 52 * (99.98/85.35)]
    data_table[ year == '0708',hh_grossinc := HHINC * 52 * (99.98/82.27)]
        
    ## gross household income minus HB (requires merged HBAI and FRS dataset)
    data_table [ , hh_housingben := sum(HBENBU), by = .(SERNUM, year)]
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ (year == '1415' & multiple_benu == 0), hh_grossinc_nohb := (HHINC - HBENBU) * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/96.70)]
    data_table[ (year == '1112' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/94.23)]
    data_table[ (year == '1011' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/90.33)]
    data_table[ (year == '0910' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/87.28)]
    data_table[ (year == '0809' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/85.35)]
    data_table[ (year == '0708' & multiple_benu == 0), hh_grossinc_nohb:= (HHINC - HBENBU) * 52 * (99.98/82.27)]
    
    ## gross household income minus income related benefits (including HB) - most relevant when thinking about ownership products
    data_table[ (year == '1415'), hh_grossinc_noincben := (HHINC - HHIRBEN) * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/96.70)]
    data_table[ (year == '1112'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/94.23)]
    data_table[ (year == '1011'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/90.33)]
    data_table[ (year == '0910'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/87.28)]
    data_table[ (year == '0809'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/85.35)]
    data_table[ (year == '0708'), hh_grossinc_noincben:= (HHINC - HHIRBEN) * 52 * (99.98/82.27)]
    
    ## gross employment income for the household
    data_table[ (year == '1415'), hh_grossinc_earn := (HEARNS) * 52]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/96.70)]
    data_table[ (year == '1112'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/94.23)]
    data_table[ (year == '1011'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/90.33)]
    data_table[ (year == '0910'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/87.28)]
    data_table[ (year == '0809'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/85.35)]
    data_table[ (year == '0708'), hh_grossinc_earn:= (HEARNS) * 52 * (99.98/82.27)]
    
    #Deflate housing costs to 14/15 prices
    data_table[ (year == '1415'), housing_cost := GBHSCOST]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), housing_cost:= GBHSCOST * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), housing_cost:= GBHSCOST * (99.98/96.70)]
    data_table[ (year == '1112'), housing_cost:= GBHSCOST * (99.98/94.23)]
    data_table[ (year == '1011'), housing_cost:= GBHSCOST * (99.98/90.33)]
    data_table[ (year == '0910'), housing_cost:= GBHSCOST * (99.98/87.28)]
    data_table[ (year == '0809'), housing_cost:= GBHSCOST * (99.98/85.35)]
    data_table[ (year == '0708'), housing_cost:= GBHSCOST * (99.98/82.27)]
    
    #Deflate savings to 14/15 prices
    data_table[ (year == '1415'), bu_savings := TOTCAPB3]
    #inflate 1314 to 1415 prices
    data_table[ (year == '1314'), bu_savings:= TOTCAPB3 * (99.98/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ (year == '1213'), bu_savings:= TOTCAPB3 * (99.98/96.70)]
    data_table[ (year == '1112'), bu_savings:= TOTCAPB3 * (99.98/94.23)]
    data_table[ (year == '1011'), bu_savings:= TOTCAPB3 * (99.98/90.33)]
    data_table[ (year == '0910'), bu_savings:= TOTCAPB3 * (99.98/87.28)]
    data_table[ (year == '0809'), bu_savings:= TOTCAPB3 * (99.98/85.35)]
    data_table[ (year == '0708'), bu_savings:= TOTCAPB3 * (99.98/82.27)]
  }
  
  data_table[ , housing_cost_gross := (housing_cost*52)/hh_grossinc ]
  #without hb
  data_table[ , housing_cost_gnohb := (housing_cost*52)/hh_grossinc_nohb ]
  #earned income
  data_table[ , housing_cost_earn := (housing_cost*52)/hh_grossinc_earn ]
}

tidy.tenure <- function(data_table){
  
  data_table[ , prs_renter := ifelse(PTENTYP2 == 3 | PTENTYP2 == 4, 1, 0)]
  data_table[ , council_renter := ifelse(PTENTYP2 == 1, 1, 0)]
  data_table[ , ha_renter := ifelse(PTENTYP2 == 2, 1, 0)]
  data_table[ , social_renter := ifelse(PTENTYP2 == 1 | PTENTYP2 == 2, 1, 0)]
  data_table[ , outright_owner := ifelse(PTENTYP2 == 5 , 1, 0)]
  data_table[ , mortgaged_owner := ifelse(PTENTYP2 == 6, 1, 0)]
  
  #grouped tenure, 1=PRS, 2=social, 3=outright owner, 4=mortgaged owner
  data_table[ , grpd_tenure := ifelse(prs_renter == 1, 1, 0)]
  data_table[ , grpd_tenure := ifelse(council_renter == 1 | ha_renter == 1, 2, grpd_tenure)]
  data_table[ , grpd_tenure := ifelse(outright_owner == 1, 3, grpd_tenure)]
  data_table[ , grpd_tenure := ifelse(mortgaged_owner == 1, 4, grpd_tenure)]
  
  
}
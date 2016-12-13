attach.private.rents <- function(data_table, data_dir){
  
  data_table[ bedrooms_needed == 1, bedrooms_rent := 1]
  data_table[ bedrooms_needed == 2, bedrooms_rent := 2]
  data_table[ bedrooms_needed == 3, bedrooms_rent := 3]
  data_table[ bedrooms_needed >= 4, bedrooms_rent := 4]
  
  voa_rents <- read.csv(paste0(data_dir,"VOA_privaterents_1415.csv"))
  voa_rents <- data.table(voa_rents)
  
  
  rent_keycols = c("GVTREGN", "bedrooms_rent")
  voa_cols = c("GVTREGN", "bedrooms_rent","rent_mean", "rent_lq", "rent_med", "rent_uq")
  
  setkeyv(data_table, rent_keycols)
  setkeyv(voa_rents, rent_keycols)
  
  data_table <- merge(data_table , voa_rents[ , .SD, .SDcols = voa_cols], by = rent_keycols, all.x = TRUE)
  
  #clean up
  rm(rent_keycols)
  rm(voa_cols)
  
  #Add 80% market rent values
  data_table[ , rent_mean80 := rent_mean*0.8]
  data_table[ , rent_lq80 := rent_lq*0.8]
  data_table[ , rent_med80 := rent_med*0.8]
  data_table[ , rent_uq80 := rent_uq*0.8]
  
  #Add 90% market rent values
  data_table[ , rent_mean90 := rent_mean*0.9]
  data_table[ , rent_lq90 := rent_lq*0.9]
  data_table[ , rent_med90 := rent_med*0.9]
  data_table[ , rent_uq90 := rent_uq*0.9]
  
}
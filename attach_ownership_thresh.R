attach.ownership.thresh <- function(data_table, data_dir){
  owner_thresh <- read.csv(paste0(data_dir,"20160815_UdatedOwnershipThresholds.csv"), stringsAsFactors = FALSE)
  owner_thresh <- data.table(owner_thresh)
  
  own_keycols = c("GVTREGN")
  own_cols = c("GVTREGN", "starter_med", "starter_lq", "helptobuy", "shared", "starter_mdeposit",  "starter_lqdeposit", 
               "helptobuy_deposit", "shared_deposit" )
  
  setkeyv(data_table, own_keycols)
  setkeyv(owner_thresh, own_keycols)
  
  data_table <- merge(data_table , owner_thresh[ , .SD, .SDcols = own_cols], by = own_keycols, all.x = TRUE)
  
  #clean up 
  rm(owner_thresh)
  rm(own_cols)
  
  return(data_table)
}
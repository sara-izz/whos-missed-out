#Calculates weights for merged FRS and HBAI dataset 
calculate.weights <- function(data_table, num_years){
  
  data_table[ , hhweight := GS_NEWHH/num_years]
  data_table[ , buweight := GS_NEWBU/num_years]
  data_table[ , grossweight := GROSS4/num_years]
  
}
#deflates variable to target year - needs work but will be worth it
deflate.variable <- function(data_table, variable, target_year, survey){
  
  if(survey == "HBAI") {
    # For HBAI variables ####
    numerator = 1
    if(target_year== "1415") numerator = 100
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ year == '1415', get(variable) := get(variable) * BHCDEF * 52]
    #inflate 1314 to 1415 prices
    data_table[ year == '1314', get(variable) := get(variable) * BHCDEF * 52 * (numerator/99)]
    #inflate 1213 to 1415 prices
    data_table[ year == '1213', get(variable) := get(variable) * BHCDEF * 52 * (numerator/96.8)]
    data_table[ year == '1112', get(variable) := get(variable) * BHCDEF * 52 * (numerator/94.3)]
    # data_table[ year == '1011',hh_grossinc := variable * BHCDEF * 52 * (100/90.5)]
    # data_table[ year == '0910',hh_grossinc := variable * BHCDEF * 52 * (100/87.4)]
    # data_table[ year == '0809',hh_grossinc := variable * BHCDEF * 52 * (100/87.0)]
    # data_table[ year == '0708',hh_grossinc := variable * BHCDEF * 52 * (100/84.5)]
  }
  
  if(survey == "FRS"){
    # Variables based on FRS variables - so can use GROSS4 and not have to worry about restricting to 1BU ####
    #disadvantage is can't use HBAI in-year deflators; mainly an issue for CPI drop in 14/15
    numerator = 1
    if(target_year== "1415") numerator = 99.98
    #deflate SPI'd gross hhold income to average of survey year
    data_table[ year == '1415', get(variable) := variable * 52]
    #inflate 1314 to 1415 prices
    data_table[ year == '1314', get(variable) := variable * 52 * (numerator/98.94)]
    #inflate 1213 to 1415 prices
    data_table[ year == '1213', get(variable) := variable * 52 * (numerator/96.70)]
    data_table[ year == '1112', get(variable) := variable * 52 * (numerator/94.23)]
    # data_table[ year == '1011',hh_grossinc := variable * 52 * (99.98/90.33)]
    # data_table[ year == '0910',hh_grossinc := variable * 52 * (99.98/87.28)]
    # data_table[ year == '0809',hh_grossinc := variable * 52 * (99.98/85.35)]
    # data_table[ year == '0708',hh_grossinc := variable * 52 * (99.98/82.27)]
  }
  
}
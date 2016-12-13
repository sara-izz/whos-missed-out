calculate.logical.flags <- function(data_table, sv_year){

  #Flag for is HRP - can also be used to collapse dataset down to household level for household level variables
  data_table[ , is_HRP := ifelse( PERSON == HRPNUM, 1, 0) ]
  
  ##Has at least one school age kids
  data_table[ ,  has_kids16 := ifelse( (sum(AGEc <= 16) >0) , 1, 0 ), by=.(SERNUM, year)]
  
  ## Lone parent flag
  data_table[ , single_parent := 0]
  data_table[ , single_parent := ifelse((BUKIDS == 5 | BUKIDS == 6 | BUKIDS == 7 | BUKIDS == 8), 1, 0)]
  data_table[ , single_parent := ifelse(DEPCHLDH == 0, 0, single_parent)]
  
  ## single mum flag
  data_table[ , single_mum := 0]
  data_table[ , single_mum := ifelse((single_parent==1 & PERSON == HRPNUM & SEXa==2), 1, 0)]
  
  ## Has at least one disabled adult
  if (sv_year != "1112" & sv_year != "1011" & sv_year != "1012" & sv_year != "0911"){
    data_table[ ,  has_disabledad := ifelse( DISCHHA1 > 0 , 1, 0 ), by=.(SERNUM, year)]
    ## Has at least one disabled child
    data_table[ ,  has_disabledch := ifelse( DISCHHC1 > 0 , 1, 0 ), by=.(SERNUM, year)]
    data_table[ is.na(DEBTFRE1), DEBTFRE1 := 0]
    data_table[ is.na(DEBTFRE2), DEBTFRE2 := 0]
    data_table[ is.na(DEBTFRE3), DEBTFRE3 := 0]
    
  }


  ## Has at least one pensioner
  data_table[ ,  has_pensioner := FALSE]
  data_table[ ,  has_pensioner := ifelse( sum(HDAGE==6) >0 , TRUE, FALSE ), by=.(SERNUM, year)]
  ## Head of household is pensioner
  data_table[ ,  head_pensioner := ifelse( (PERSON==HRPNUM & HDAGE==6) , 1, 0 ), by=.(SERNUM, year)]
  
  #Caring responsibilities
  data_table[ is.na(WHOLOO01), WHOLOO01 := 999]
  data_table[ is.na(WHOLOO02), WHOLOO02 := 999]
  data_table[ is.na(WHOLOO03), WHOLOO03 := 999]
  data_table[ is.na(WHOLOO04), WHOLOO04 := 999]
  data_table[ is.na(WHOLOO05), WHOLOO05 := 999]
  data_table[ is.na(WHOLOO06), WHOLOO06 := 999]
  data_table[ is.na(WHOLOO07), WHOLOO07 := 999]
  data_table[ is.na(WHOLOO08), WHOLOO08 := 999]
  data_table[ is.na(WHOLOO09), WHOLOO09 := 999]
  data_table[ is.na(WHOLOO10), WHOLOO10 := 999]
  data_table[ is.na(WHOLOO11), WHOLOO11 := 999]
  data_table[ is.na(WHOLOO12), WHOLOO12 := 999]
  data_table[ is.na(WHOLOO13), WHOLOO13 := 999]
  data_table[ is.na(WHOLOO14), WHOLOO14 := 999]
  
  data_table[ , has_carer := FALSE]
  data_table[ , has_carer := ifelse (sum(WHOLOO01 == 1 | WHOLOO02 == 1 | WHOLOO03 == 1 | WHOLOO04 == 1 | WHOLOO05 == 1 | WHOLOO06 == 1 | 
                                           WHOLOO07 == 1 | WHOLOO08 == 1 |WHOLOO09 == 1 | WHOLOO10 == 1 | WHOLOO11 == 1 | WHOLOO12 == 1 | 
                                           WHOLOO13 == 1 | WHOLOO14 == 1 ) > 0 , TRUE, FALSE), by=.(SERNUM, year)]
  
  ##Flags to try and establish need to be in area
  #Kids looked after by someone outside household -simpler version
  data_table[ is.na(R01), R01 := 999]
  data_table[ is.na(R02), R02 := 999]
  data_table[ is.na(R03), R03 := 999]
  data_table[ is.na(R04), R04 := 999]
  data_table[ is.na(R05), R05 := 999]
  data_table[ is.na(R06), R06 := 999]
  data_table[ is.na(R07), R07 := 999]
  data_table[ is.na(R08), R08 := 999]
  data_table[ is.na(R09), R09 := 999]
  data_table[ is.na(R10), R10 := 999]
  data_table[ is.na(R11), R11 := 999]
  data_table[ is.na(R12), R12 := 999]
  data_table[ is.na(R13), R13 := 999]
  data_table[ is.na(R14), R14 := 999]
  data_table[ , has_resident_grandparent := ifelse (sum(R01 == 16 | R02 == 16 | R03 == 16 | R04 == 16 | R05 == 16 | R06 == 16 | R07 == 16 |
                                                          R08 == 16 | R09 == 16 | R10 == 16 | R11 == 16 | R12 == 16 | R13 == 16 | 
                                                          R14 == 16) > 0 , 1, 0), by=.(SERNUM, year)]
  
  
  #kid looked after by non-res parent or ex, friends or neighbours or non-resident grandparent
  data_table[ , kid_care_nonres := FALSE]
  data_table[ , kid_care_nonres := ifelse( (CHLOOK == 14 | CHLOOK == 19 | (CHLOOK == 13 & has_resident_grandparent == 0)), TRUE, FALSE), 
              by=.(SERNUM, year)]

}
#### Calculate number of bedrooms needed for each household according to LHA definition found here:
#### http://england.shelter.org.uk/get_advice/housing_benefit_and_local_housing_allowance/what_is_housing_benefit/local_housing_allowance

#for use in affordability_analysis.R with datasets made using make_flatfile.R
require("data.table")

calculate.bedrooms <- function( merged_data) {
  merged_data[ is.na(R01), R01 := 999 ]
  merged_data[ is.na(R02), R02 := 999 ]
  merged_data[ is.na(R03), R03 := 999 ]
  merged_data[ is.na(R04), R04 := 999 ]
  merged_data[ is.na(R05), R05 := 999 ]
  merged_data[ is.na(R06), R06 := 999 ]
  merged_data[ is.na(R07), R07 := 999 ]
  merged_data[ is.na(R08), R08 := 999 ]
  merged_data[ is.na(R09), R09 := 999 ]
  merged_data[ is.na(R10), R10 := 999 ]
  merged_data[ is.na(R11), R11 := 999 ]
  merged_data[ is.na(R12), R12 := 999 ]
  merged_data[ is.na(R13), R13 := 999 ]
  merged_data[ is.na(R14), R14 := 999 ]
  #flag if relationship with anyone in the hhld is spouse (1) or cohabitee (2)
  merged_data[ , ispart_couple := ifelse((R01 == 1 | R02 == 1 | R03 == 1 | R04 == 1 | R05 == 1 | R06 == 1 | R07 == 1 | R08 == 1 |
                                            R09 == 1 | R10 == 1 | R11 == 1 | R12 == 1 | R13 == 1 | R14 == 1 |R01 == 2 | R02 == 2 | 
                                            R03 == 2 | R04 == 2 | R05 == 2 | R06 == 2 | R07 == 2 | R08 == 2 | R09 == 2 | R10 == 2 | 
                                            R11 == 2 | R12 == 2 | R13 == 2 | R14 == 2 ), 1, 0)]
  
  merged_data[ , num_cohabit := 0L]
  merged_data[ , num_cohabit := sum(ispart_couple == '1'), by=.(SERNUM, year)]
  merged_data[ , cohab_rooms :=  num_cohabit/2]  
  
  
  #Number of single over 16
  merged_data[ , single16_rooms := sum(( (IsChild == 'FALSE' | AGEc >=16) & ispart_couple =='0')), by=.(SERNUM, year)]
  
  #Number of pairs of kids under 10 
  #Recode AGEc for adults from NA to 999
  merged_data[ is.na(AGEc), AGEc := 999]
  merged_data[ , num_kids10 := sum(AGEc < 10), by=.(SERNUM, year)]
  merged_data[ , kids10_rooms := floor(num_kids10/2), by=.(SERNUM, year)]
  
  #Flag if left over kid
  merged_data[ , spare_kid10 := FALSE]
  merged_data[ , spare_kid10 := ifelse (num_kids10 %% 2 != 0, TRUE, FALSE)]
  
  #Number of pairs of girls 10-16
  merged_data[ , num_girls1016 := sum( (AGEc >= 10 & AGEc < 16) & SEXc == '2'), by=.(SERNUM, year)]
  merged_data[ , girls1016_rooms := floor(num_girls1016/2), by=.(SERNUM, year)]
  
  #Flag if left over girl 10-16
  merged_data[ , spare_girl1016 := FALSE]
  merged_data[ , spare_girl1016 := ifelse (num_girls1016 %% 2 != 0, TRUE, FALSE)]
  
  #Number of pairs of boys 10-16
  merged_data[ , num_boys1016 := sum( (AGEc >= 10 & AGEc < 16) & SEXc == '1'), by=.(SERNUM, year)]
  merged_data[ , boys1016_rooms := floor(num_boys1016/2), by=.(SERNUM, year)]
  
  #Flag if left over boy 10-16
  merged_data[ , spare_boy1016 := FALSE]
  merged_data[ , spare_boy1016 := ifelse (num_boys1016 %% 2 != 0, TRUE, FALSE)]
  
  #### Extra rooms for leftover kids
  merged_data[ , num_extrarooms := 0]
  
  # Sexes of kids under 10
  merged_data[ ,isgirl_under10 := ifelse((SEXc == '2' & AGEc < 10), 1, 0)]
  merged_data[ ,isboy_under10 := ifelse((SEXc == '1' & AGEc < 10), 1, 0)]
  merged_data[ , num_girlunder10 := sum(isgirl_under10), by =.(SERNUM, year)]
  merged_data[ , num_boyunder10 := sum(isboy_under10), by =.(SERNUM, year)]
  
  ## First try and assign spare 10-16 to spare kids under 10, add on room for them if can't
  #Cases where there is a spare kid under 10, spare girl 10-16 and one of kids under 10 isn't a girl -> 1 extra room for spare girl 10-16
  merged_data[ (spare_kid10 == 'TRUE' & spare_girl1016 == 'TRUE'), 
               num_extrarooms := ifelse( num_girlunder10 == 0, num_extrarooms + 1, num_extrarooms), 
               by=.(SERNUM, year)]
  #Cases where there is a spare kid under 10, spare boy 10-16 and one of kids under 10 isn't a boy -> 1 extra room for spare girl 10-16
  merged_data[ (spare_kid10 == 'TRUE' & spare_boy1016 == 'TRUE'), 
               num_extrarooms := ifelse(num_boyunder10 == 0, num_extrarooms + 1, num_extrarooms), 
               by=.(SERNUM, year)]
  
  
  ## Now add extra rooms for cases where can match spare 10-16 to spare kids under 10
  merged_data[ (spare_kid10 == 'TRUE' & spare_girl1016 == 'TRUE'), 
               num_extrarooms := ifelse( num_girlunder10 > 0, num_extrarooms + 1, num_extrarooms), 
               by=.(SERNUM, year)]
  
  
  merged_data[ (spare_kid10 == 'TRUE' & spare_boy1016 == 'TRUE'), 
               num_extrarooms := ifelse( num_boyunder10 > 0, num_extrarooms + 1, num_extrarooms), 
               by=.(SERNUM, year)]
  
  
  ## Now add extra rooms for cases where can't match spare kid under 10 to spare girl or boy 10-16
  merged_data[ (spare_kid10 == 'TRUE' & (num_girlunder10 == 0)), 
               num_extrarooms := ifelse((spare_boy1016 == FALSE & spare_girl1016 == TRUE), num_extrarooms + 1, num_extrarooms),
               by=.(SERNUM, year)]
  
  
  merged_data[ (spare_kid10 == 'TRUE' & (num_boyunder10 == 0)), 
               num_extrarooms := ifelse((spare_girl1016 == FALSE & spare_boy1016 == TRUE), num_extrarooms + 1, num_extrarooms),
               by=.(SERNUM, year)]
  
  
  merged_data[ ( spare_kid10 == 'TRUE' & spare_girl1016 == 0 & spare_boy1016 == 0 ), 
               num_extrarooms := num_extrarooms + 1,
               by=.(SERNUM, year)]
  
  
  ### Add all rooms together
  merged_data[ , bedrooms_needed := cohab_rooms + single16_rooms + kids10_rooms + girls1016_rooms + boys1016_rooms + num_extrarooms]
}
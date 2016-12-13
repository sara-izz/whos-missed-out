tidy.regions <- function(data_table, sv_year) {
  
  if( sv_year != "1112" & sv_year != "1011" & sv_year != "1012" & sv_year != "0911"){
    data_table <- data_table[ (GVTREGN != 299999999 & GVTREGN != 399999999 & GVTREGN != 499999999),  ]
    
    
    #Create some grouped regions to get decent sample breakdowns
    data_table[ , grpd_region1 := 0.0]
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region1 := ifelse(GVTREGN==112000001 | GVTREGN==112000002  | GVTREGN==112000003, 
                                         1.0, grpd_region1 )]
    #West Mids, South West
    data_table[ , grpd_region1 := ifelse(GVTREGN==112000005 | GVTREGN==112000009 , 
                                         2, grpd_region1 )]
    #East Mids, East
    data_table[ , grpd_region1 := ifelse(GVTREGN==112000004 | GVTREGN==112000006 , 
                                         3, grpd_region1 )]
    #London, South East
    data_table[ , grpd_region1 := ifelse(GVTREGN==112000007 | GVTREGN==112000008 , 
                                         4, grpd_region1 )]
    
    data_table[ , grpd_region1 := factor(grpd_region1, labels = c("North", "West", "East", "London and South East"))]
    
    #North and London only
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region2 := NA]
    data_table[ , grpd_region2 := ifelse(GVTREGN==112000001 | GVTREGN==112000002  | GVTREGN==112000003, 
                                         1, grpd_region2 )]
    #London
    data_table[ , grpd_region2 := ifelse(GVTREGN==112000007, 
                                         2, grpd_region2 )]
    
    data_table[ , grpd_region2 := factor(grpd_region2, labels = c("North", "London"))]
    
    #North and London & South East only
    data_table[ , grpd_region3 := NA]
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region3 := ifelse(GVTREGN==112000001 | GVTREGN==112000002  | GVTREGN==112000003, 
                                         1, grpd_region3 )]
    #London, South East
    data_table[ , grpd_region3 := ifelse(GVTREGN==112000007 | GVTREGN==112000008 , 
                                         4, grpd_region3 )]
    data_table[ , grpd_region3 := factor(grpd_region3, labels = c("North", "London & South East"))]
    
    
    data_table[ , GVTREGN := factor(GVTREGN, levels = c("112000001", "112000002", "112000003", "112000004", "112000005",
                                                        "112000006", "112000007", "112000008", "112000009"),
                                    labels = c("North East", "North West", "Yorks & Humber", "East Midlands", "West Midlands",
                                               "East", "London", "South East", "South West"))
                ]
  }
  else if (sv_year == "1112" | sv_year == "1011" | sv_year == "1012" | sv_year == "0911"){
    data_table <- data_table[ (GVTREGN != 11 & GVTREGN != 12 & GVTREGN != 13),  ]
    
    
    #Create some grouped regions to get decent sample breakdowns
    data_table[ , grpd_region1 := 0.0]
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region1 := ifelse(GVTREGN==1 | GVTREGN==2  | GVTREGN==4, 
                                         1.0, grpd_region1 )]
    #West Mids, South West
    data_table[ , grpd_region1 := ifelse(GVTREGN==6 | GVTREGN==10 , 
                                         2, grpd_region1 )]
    #East Mids, East
    data_table[ , grpd_region1 := ifelse(GVTREGN==5 | GVTREGN==7 , 
                                         3, grpd_region1 )]
    #London, South East
    data_table[ , grpd_region1 := ifelse(GVTREGN==8 | GVTREGN==9 , 
                                         4, grpd_region1 )]
    
    data_table[ , grpd_region1 := factor(grpd_region1, labels = c("North", "West", "East", "London and South East"))]
    
    #North and London only
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region2 := NA]
    data_table[ , grpd_region2 := ifelse(GVTREGN==1 | GVTREGN==2  | GVTREGN==4, 
                                         1, grpd_region2 )]
    #London
    data_table[ , grpd_region2 := ifelse(GVTREGN==8, 
                                         2, grpd_region2 )]
    
    data_table[ , grpd_region2 := factor(grpd_region2, labels = c("North", "London"))]
    
    #North and London & South East only
    data_table[ , grpd_region3 := NA]
    #North East, North West, Yorks & Humber
    data_table[ , grpd_region3 := ifelse(GVTREGN==1 | GVTREGN==2  | GVTREGN==4, 
                                         1, grpd_region3 )]
    #London, South East
    data_table[ , grpd_region3 := ifelse(GVTREGN==8 | GVTREGN==9 , 
                                         4, grpd_region3 )]
    data_table[ , grpd_region3 := factor(grpd_region3, labels = c("North", "London & South East"))]
    
    
    data_table[ , GVTREGN := factor(GVTREGN, levels = c("1", "2", "4", "5", "6", "7", "8", "9", "10"),
                                    labels = c("North East", "North West", "Yorks & Humber", "East Midlands", "West Midlands",
                                               "East", "London", "South East", "South West"))
                ] 
  }
  
  
}
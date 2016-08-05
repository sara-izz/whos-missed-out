#If "haven" not found, do install.packages("haven")
install.packages("sqldf")
install.packages("data.table")
install.packages("dplyr")

require("haven")
require("data.table")
require("dplyr")
#library(sqldf)



######Function definition to create flatfile for each year of data 
make.flatfile <- function(data_dir,year){

  ###Add columns from chdcare to chld

  chldcare <- read_spss(paste0(data_dir,year,"/spss19/chldcare.sav"))
  child <- read_spss(paste0(data_dir,year,"/spss19/child.sav"))
  
  #Convert dataframes to datatables
  CHLDCARE = data.table(chldcare)
  CHILD = data.table(child)
  #Set keys for datatables (i.e. row identifiers)
  keycols = c("SERNUM","BENUNIT","PERSON")
  setkeyv(CHILD, keycols)
  #Add CHLOOK to CHILD datatable
  CHILD[CHLDCARE, CHLOOK := i.CHLOOK]
  #Add flag to CHILD to indicate PERSON is a child
  CHILD$IsChild <- TRUE
  #Check everything's ok
  head(CHILD)
  
  #Keep only needed variables
  cols = c("SERNUM","BENUNIT","PERSON","COHABIT","SEX","AGE","CURQUAL","CDISDIFP","CDISD01","CDISD02","CDISD03",
           "CDISD04","CDISD05","CDISD06","CDISD07","CDISD08","CDISD09","CDISD10","PARENT1","PARENT2","CHLOOK","IsChild")
  CHILD[ , .SD, .SDcols = cols]
  
  ###Add CHILD to adult dataset
  adult <- read_spss(paste0(data_dir,year,"/spss19/adult.sav"))
  ADULT = data.table(adult)
  setkeyv(ADULT, keycols)
  ADULT <- merge(ADULT, CHILD[ , .SD, .SDcols = cols], by = keycols, all = TRUE, suffixes = c("a","c"))
  
  #Turn IsChild NAs in to false
  ADULT %>% mutate_each(funs(replace(., is.na(.), F)), IsChild)
  
  #Add adult variables to list of columns to keep
  remove <- c("COHABIT","SEX","AGE","CURQUAL")
  cols<-cols[! cols %in% remove]
  cols <- c(cols, "COHABITa","SEXa","AGEa","CURQUALa","COHABITc","SEXc","AGEc","CURQUALc","HDAGE","DISBEN1","DISBEN2",
            "DISBEN3","DISBEN4","DISBEN5","DISBEN6","DISDIFP1","DDATREP1","DISD01","DISD02","DISD03","DISD04","DISD05",
            "DISD06","DISD07","DISD08","DISD09","DISD10","DVIL04A","SELFDEMP","NOWANT","RSTRCT","INJLONG","INCDUR",
            "NUMJOB","EMPCONTR","TEMPJOB","HI1QUAL1","HI1QUAL2","HI1QUAL3","HI1QUAL4","HI1QUAL5","HI1QUAL6","HI3QUAL",
            "FTED","ANYED","EDHR")
  
  ###Add benefit unit info to ADULT
  benu <- read_spss(paste0(data_dir,year,"/spss19/benunit.sav"))
  BENU = data.table(benu)
  setkeyv(BENU, c("SERNUM","BENUNIT"))
  #columns to pull from benefit unit
  bencols = c("SERNUM","BENUNIT","BUKIDS","ADDMON","OAEXPNS","OAHOWPY1","OAHOWPY2","OAHOWPY3","OAHOWPY4","OAHOWPY5","OAHOWPY6","DEBTFRE1",
              "DEBTFRE2","TOTSAVBU","TOTCAPB3")
  
  #merge those columns in to ADULT
  ADULT <- merge(ADULT, BENU[ , .SD, .SDcols = bencols], by = c("SERNUM","BENUNIT"), all.x = TRUE, suffixes = c("a","b"))
  
  #add benunit columns to overall columns to keep
  cols <- c(cols, bencols)
  
  ###Add household info into ADULT
  hhold <- read_spss(paste0(data_dir,year,"/spss19/househol.sav"))
  HHOLD = data.table(hhold)
  
  #Columns to take from HHOLD
  hcols = c("SERNUM","HRPNUM", "HHINC", "HBENINC", "HEARNS", "HHLDR01","HHLDR02","HHLDR03","HHLDR04","HHLDR05","HHLDR06","HHLDR07",
            "HHLDR08","HHLDR09","HHLDR10","HHLDR11","HHLDR12","HHLDR13","HHLDR14","HHLDR97","HHSTAT", "DEPCHLDH",
            "DISCHHA1","DISCHHC1","DISWHHA1","DISWHHC1","GBHSCOST", "GIVEHELP", "GVTREGN", "GVTREGNO", "PTENTYP2", "BEDROOM", "BURDEN")
  
  #check
  HHOLD[ , .SD, .SDcols = hcols]
  
  ADULT <- merge(ADULT, HHOLD[ , .SD, .SDcols = hcols], by = "SERNUM", all.x = TRUE, suffixes = c("a","h"))
  
  #add hhold columns to overall columns to keep
  cols <- c(cols, hcols)
  
  ###Add care info to ADULT
  care <- read_spss(paste0(data_dir,year,"/spss19/care.sav"))
  CARE = data.table(care)
  
  cacols <- c("WHOLOO01", "WHOLOO02", "WHOLOO03", "WHOLOO04", "WHOLOO05", "WHOLOO06", "WHOLOO07", "WHOLOO08", "WHOLOO09","WHOLOO10","WHOLOO11",
              "WHOLOO12","WHOLOO13","WHOLOO14","WHOLOO15","WHOLOO16","WHOLOO17","WHOLOO18","WHOLOO19","WHOLOO20")
  keycols <- c("SERNUM", "BENUNIT")
  
  setkeyv(ADULT, keycols)
  setkeyv(CARE, keycols)
  
  ADULT[CARE, ':=' (WHOLOO01=i.WHOLOO01, WHOLOO02=i.WHOLOO02, WHOLOO03=i.WHOLOO03, WHOLOO04=i.WHOLOO04, WHOLOO05=i.WHOLOO05, WHOLOO06=i.WHOLOO06,
                    WHOLOO07=i.WHOLOO07, WHOLOO08=i.WHOLOO08, WHOLOO09=i.WHOLOO09, WHOLOO10=i.WHOLOO10, WHOLOO11=i.WHOLOO11, WHOLOO12=i.WHOLOO12,
                    WHOLOO13=i.WHOLOO13, WHOLOO14=i.WHOLOO14, WHOLOO15=i.WHOLOO15, WHOLOO16=i.WHOLOO16, WHOLOO17=i.WHOLOO17, WHOLOO18=i.WHOLOO18,
                    WHOLOO19=i.WHOLOO19, WHOLOO20=i.WHOLOO20)]
  
  #add care columns to overall columns to keep
  cols <- c(cols, cacols)
  ### New data table that is slimmed down ADULT to variables needed
  
  AFF <- ADULT[, .SD, .SDcols = cols]
  return(AFF)
}

curr_dir <- "S:/@Communications, Policy and Campaigns/Research/STATS & INFO/Statistics/Household Surveys/Family Resources Survey/"

affdt_1314 <- make.flatfile(curr_dir,"2013-14")

#####Sets column names to use in make.flatfile
column.selector <- function(year){
  adult_col <- c()
  child_col <- c()
  benu_col <- c()
  hhold_col <- c()
  if (year=="2013-14"){
    child_col <- c("SERNUM","BENUNIT","PERSON","COHABIT","SEX","AGE","CURQUAL","CDISDIFP","CDISD01","CDISD02","CDISD03",
                   "CDISD04","CDISD05","CDISD06","CDISD07","CDISD08","CDISD09","CDISD10","PARENT1","PARENT2","CHLOOK","IsChild")
    adult_col <- c("HDAGE","DISBEN1","DISBEN2","DISBEN3","DISBEN4","DISBEN5","DISBEN6","DISDIFP1","DDATREP1","DISD01","DISD02","DISD03","DISD04",
                   "DISD05","DISD06","DISD07","DISD08","DISD09","DISD10","DVIL04A","SELFDEMP","NOWANT","RSTRCT","INJLONG","INCDUR", "NUMJOB",
                   "EMPCONTR","TEMPJOB","HI1QUAL1","HI1QUAL2","HI1QUAL3","HI1QUAL4","HI1QUAL5","HI1QUAL6","HI3QUAL","FTED","ANYED","EDHR") 
    #Doesn't include replicated variables in childcols as given suffix in makefile code

    benu_col <- c("SERNUM","BENUNIT","BUKIDS","ADDMON","OAEXPNS","OAHOWPY1","OAHOWPY2","OAHOWPY3","OAHOWPY4","OAHOWPY5","OAHOWPY6","DEBTFRE1",
                 "DEBTFRE2","TOTSAVBU","TOTCAPB3")
    hhold_col <- c("SERNUM","HRPNUM", "HHINC", "HBENINC", "HEARNS", "HHLDR01","HHLDR02","HHLDR03","HHLDR04","HHLDR05","HHLDR06","HHLDR07",
                   "HHLDR08","HHLDR09","HHLDR10","HHLDR11","HHLDR12","HHLDR13","HHLDR14","HHLDR97","HHSTAT", "DEPCHLDH",
                   "DISCHHA1","DISCHHC1","DISWHHA1","DISWHHC1","GBHSCOST", "GIVEHELP", "GVTREGN", "GVTREGNO", "PTENTYP2", "BEDROOM", "BURDEN")

  }
  col_names <- list(child_col,adult_col,benu_col,hhold_col)
  
  return(col_names)
}
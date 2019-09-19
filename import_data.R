library("tidyverse")
library("readxl")
library("RSQLite")

format_ons_pop = function(year, sex){
  xl = read_excel(path=paste0("raw_data/ons_pop/SAPE20DT2-mid-",year,"-lsoa-syoa-estimates-unformatted.xls"),
                  sheet=paste0("Mid-",year," ",sex),
                  skip=4,
                  trim_ws=TRUE)
                  
  colnames(xl) = gsub("\\+" ,"", colnames(xl))
  xl = xl %>% 
    select(-"Area Names", -"All Ages") %>%
    gather("AGE","POP",-1) %>%
    mutate(YEAR=as.integer(year), SEX=str_sub(sex,1,1), AGE=as.integer(AGE), POP=as.integer(POP)) %>%
    select(YEAR,LSOA11CD='Area Codes',SEX,AGE,POP)
  
  return(xl)
}

format_ons_pop_old = function(year, sex){
  if(year<2007){
    if(sex=="Males"){
      excel_path="raw_data/ons_pop/SAPE8DT2a-LSOA-syoa-unformatted-males-mid2002-to-mid2006.xls"
    } else {
      excel_path="raw_data/ons_pop/SAPE8DT3a-LSOA-syoa-unformatted-females-mid2002-to-mid2006.xls"
    }
  } else {
    if(sex=="Males"){
      excel_path="raw_data/ons_pop/SAPE8DT2b-LSOA-syoa-unformatted-males-mid2007-to-mid2010.xls"
    } else {
      excel_path="raw_data/ons_pop/SAPE8DT3b-LSOA-syoa-unformatted-females-mid2007-to-mid2010.xls"
    }
  }
  xl = read_excel(path=excel_path,
                  sheet=paste0("Mid-",year),
                  skip=0,
                  trim_ws=TRUE)
  
  colnames(xl) = gsub("f|m|plus" ,"", colnames(xl))
  xl = xl %>% 
    select(-"LAD11CD",-"LAD11NM",-"all_ages") %>%
    gather("AGE","POP",-1) %>%
    mutate(YEAR=as.integer(year), SEX=str_sub(sex,1,1), AGE=as.integer(AGE), POP=as.integer(POP)) %>%
    select(YEAR,LSOA11CD,SEX,AGE,POP)
  
  return(xl)
}

write_population_data_to_db = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"ons_pop",fail_if_missing=FALSE)
  for (year in 2002:2017) {
    for(sex in c("Males","Females")){
      if(year>2011){
        pop = format_ons_pop(year,sex)
      } else {
        pop = format_ons_pop_old(year,sex)
      }
      dbWriteTable(conn = db, name = "ons_pop", pop, append=TRUE)
    }
  }
  dbDisconnect(db)
}

write_imd_data_to_db = function(database_name="primary_care_data.sqlite3"){
  imd_2015 = read_csv("raw_data/imd/imd_2015.csv")
  imd_dimensions = tibble(NAME=c("OVERALL","INCOME","EMPLOYMENT","EDUCATION","HEALTH","CRIME","HOUSING","ENVIRONMENT","IDACI","IDAOPI"),LABEL=unique(imd_2015$`Indices of Deprivation`) %>% sort())
  imd_2015 = imd_2015 %>% 
    inner_join(imd_dimensions, by=c("Indices of Deprivation"="LABEL")) %>%
    select(LSOA11CD=FeatureCode,DIMENSION=NAME,MEASURE=Measurement,VALUE=Value)
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "imd_dimensions", imd_dimensions, overwrite=TRUE)
  dbWriteTable(conn = db, name = "imd_2015", imd_2015, overwrite=TRUE)
  dbDisconnect(db)
}

write_nhs_geography_to_db = function(database_name="primary_care_data.sqlite3"){
  nhs_geography = read_csv("raw_data/nhs_geography.csv")
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "nhs_geography", nhs_geography, overwrite=TRUE)
  dbDisconnect(db)
}



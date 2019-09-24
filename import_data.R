library("tidyverse")
library("readxl")
library("RSQLite")
library("fingertipsR")
library("scales")

make_age_group  = function(start_age, end_age){
  paste0("AGE_",start_age:end_age, collapse = "+")
}

add_quintiles = function(data_with_deciles){
  data_with_quintiles = data_with_deciles %>% mutate( 
    IMD_QUINTILE = case_when(
      IMD_DECILE == 1 | IMD_DECILE == 2  ~ 1,
      IMD_DECILE == 3 | IMD_DECILE == 4  ~ 2,
      IMD_DECILE == 5 | IMD_DECILE == 6  ~ 3,
      IMD_DECILE == 7 | IMD_DECILE == 8  ~ 4,
      IMD_DECILE == 9 | IMD_DECILE == 10 ~ 5
    ))
  return(data_with_quintiles)
}

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


write_workforce_data_to_db = function(database_name="primary_care_data.sqlite3"){
  
  gp_year = list()
  gp_year[["2004-2008"]] = read_csv("raw_data/workforce/GPs 2004-2013/GPs 2004-2008.csv",guess_max = 100000) %>% 
    select(YEAR=YEAR,PRAC_CODE=PRACTICE_CODE,GP_TYPE,FTE)
  for (year in 2009:2012) {
    gp_year[[as.character(year)]] = read_csv(paste0("raw_data/workforce/GPs 2004-2013/GPs ",year,".csv"),guess_max = 100000) %>%
      select(YEAR=year,PRAC_CODE=prac_code,GP_TYPE=gp_type,FTE=fte)
  }
  gps = map_dfr(gp_year,bind_rows) %>% 
    arrange(YEAR,PRAC_CODE,GP_TYPE,FTE) %>%
    mutate(HC=1) %>%
    drop_na(PRAC_CODE)
  
  gps_2004_2012 = gps %>% 
    group_by(YEAR,PRAC_CODE) %>%
    summarise(TOTAL_GP_HC=sum(HC),TOTAL_GP_FTE=sum(FTE)) %>%
    left_join(
      gps %>% 
      filter(!(GP_TYPE %in% c("GP REG","GP RET"))) %>%
      group_by(YEAR,PRAC_CODE) %>%
      summarise(TOTAL_GP_EXRR_HC=sum(HC),TOTAL_GP_EXRR_FTE=sum(FTE))) %>%
      left_join(
        gps %>% 
          filter(GP_TYPE== "GP REG") %>%
          group_by(YEAR,PRAC_CODE) %>%
          summarise(TOTAL_GP_REG_HC=sum(HC),TOTAL_GP_REG_FTE=sum(FTE))
      ) %>%
     left_join(
      gps %>% 
        filter(GP_TYPE== "GP RET") %>%
        group_by(YEAR,PRAC_CODE) %>%
        summarise(TOTAL_GP_RET_HC=sum(HC),TOTAL_GP_RET_FTE=sum(FTE))
    ) %>%
    mutate_all(~replace(., is.na(.), 0))
    
  
  wf_2013 = read_csv("raw_data/workforce/nhs-staf-2013-gene-prac-data/General Practice 2013 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2013 = wf_2013 %>% mutate(YEAR=2013,
                               TOTAL_GP_EXRRL_HC=TOTAL_GP_EXRR_HC,
                               TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRR_FTE)
  
  wf_2013 = wf_2013 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC,TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC,TOTAL_NURSE_FTE,
                               TOTAL_DPC_HC=DIRECT_PATIENT_CARE_HC,TOTAL_DPC_FTE=DIRECT_PATIENT_CARE_FTE,
                               TOTAL_ADMIN_HC=ADMIN_HC,TOTAL_ADMIN_FTE=ADMIN_FTE)
  
  wf_2014 = read_csv("raw_data/workforce/nhs-staf-2014-gene-prac-data/General Practice 2014 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2014 = wf_2014 %>% mutate(YEAR=2014,
                               TOTAL_GP_EXRRL_HC=TOTAL_GP_EXRR_HC,
                               TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRR_FTE)
  
  wf_2014 = wf_2014 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_0to4:FEMALE_85plus,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC,TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC,TOTAL_NURSE_FTE,
                               TOTAL_DPC_HC=DIRECT_PATIENT_CARE_HC,TOTAL_DPC_FTE=DIRECT_PATIENT_CARE_FTE,
                               TOTAL_ADMIN_HC=ADMIN_HC,TOTAL_ADMIN_FTE=ADMIN_FTE)
  
  colnames(wf_2014) = colnames(wf_2013)
  
  wf_2015 = read_csv("raw_data/workforce/nhs-staf-2015-gene-prac-data/General Practice September 2015 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2015 = wf_2015 %>% 
    mutate(YEAR=2015,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC = TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE = TOTAL_GP_FTE-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2015 = wf_2015 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE,
                               TOTAL_DPC_HC,TOTAL_DPC_FTE,
                               TOTAL_ADMIN_HC,TOTAL_ADMIN_FTE)
  
  wf_2016 = read_csv("raw_data/workforce/nhs-staf-sept-2016-gene-prac-data/General Practice September 2016 Practice Level.csv",
                      na=c("","NA","EST","NS",".","ND"))
  # switched to March file due to excessive missing data in September file
  # wf_2016 = read_csv("raw_data/workforce/nhs-staf-march-2016-gene-prac-data/General Practice March 2016 Practice Level.csv",
  #                    na=c("","NA","EST","NS",".","ND"))
  wf_2016 = wf_2016 %>% 
    mutate(YEAR=2016,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_FTE-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2016 = wf_2016 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE,
                               TOTAL_DPC_HC,TOTAL_DPC_FTE,
                               TOTAL_ADMIN_HC,TOTAL_ADMIN_FTE)
  
  wf_2017 = read_csv("raw_data/workforce/general_and_personal_medical_services__england_-_september_2017/General Practice September 2017 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2017 = wf_2017 %>% 
    mutate(YEAR=2017,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_FTE-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2017 = wf_2017 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE,
                               TOTAL_DPC_HC,TOTAL_DPC_FTE,
                               TOTAL_ADMIN_HC,TOTAL_ADMIN_FTE)
  
  wf_2018 = read_csv("raw_data/workforce/GP Prov Sept 2018 Practice Level CSV/General Practice Provisional Sept 2018 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2018 = wf_2018 %>% 
    mutate(YEAR=2018,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST1_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST1_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_FTE-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2018 = wf_2018 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE,
                               TOTAL_DPC_HC,TOTAL_DPC_FTE,
                               TOTAL_ADMIN_HC,TOTAL_ADMIN_FTE)
  
  wf = bind_rows(gps_2004_2012,wf_2013,wf_2014,wf_2015,wf_2016,wf_2017,wf_2018)
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn=db, name="gp_workforce", wf, overwrite=TRUE)
  dbDisconnect(db)
}

write_practice_imd_data_to_db = function(database_name="primary_care_data.sqlite3"){
  practice_imd = deprivation_decile(AreaTypeID=7,Year=2015)
  practice_imd = practice_imd %>% select(PRAC_CODE=AreaCode, IMD_SCORE=IMDscore, IMD_DECILE=decile)
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn=db, name="gp_imd_2015", practice_imd, overwrite=TRUE)
  dbDisconnect(db)
}

write_ons_pop_by_imd = function(database_name="primary_care_data.sqlite3"){
  decile_pop = read_excel("raw_data/ons_pop/populationbyageimdengland20012017.xls",
                          sheet = "Table 1",
                          skip = 12)
  decile_pop = decile_pop %>% select(YEAR=Year,SEX=Sex,AGE=Age,IMD_DECILE=`IMD decile`,POP=Count)
  
  quintile_pop = read_excel("raw_data/ons_pop/populationbyageimdengland20012017.xls",
                          sheet = "Table 2",
                          skip = 12)
  quintile_pop = quintile_pop %>% select(YEAR=Year,SEX=Sex,AGE=Age,IMD_QUINTILE=`IMD quintile`,POP=Count)
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn=db, name="ons_pop_decile", decile_pop, overwrite=TRUE)
  
  dbWriteTable(conn=db, name="ons_pop_quintile", quintile_pop, overwrite=TRUE)
  
  dbDisconnect(db)
}

write_carr_hill_population_data_to_db = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"carr_hill_pop",fail_if_missing=FALSE)
  
  imd_health = tbl(db, "imd_2015") %>% 
    filter(DIMENSION=="HEALTH" & MEASURE=="Score") %>%
    select(LSOA11CD,IMD_HEALTH=VALUE) %>%
    collect()
  
  for (year in 2002:2017) {
    if(year>2011){
      male_pop = format_ons_pop(year,"Males")
      female_pop = format_ons_pop(year,"Females")
    } else {
      male_pop = format_ons_pop_old(year,"Males")
      female_pop = format_ons_pop_old(year,"Females")
    }
    
    pop = bind_rows(male_pop,female_pop)
    
    pop = pop %>% 
      mutate(SEX = ifelse(SEX=="M","MALE","FEMALE")) %>%
      spread(AGE,POP,sep="_") %>%
      mutate_(AGE_0_4 = make_age_group(0,4),
              AGE_5_14 = make_age_group(5,14),
              AGE_15_44 = make_age_group(15,44),
              AGE_45_64 = make_age_group(45,64),
              AGE_65_74 = make_age_group(65,74),
              AGE_75_84 = make_age_group(75,84),
              AGE_85PLUS = make_age_group(85,90)) %>%
      select(YEAR,SEX,LSOA11CD, AGE_0_4,AGE_5_14,AGE_15_44,AGE_45_64,AGE_65_74,AGE_75_84,AGE_85PLUS) %>%
      gather(AGE_GROUP,POP,AGE_0_4,AGE_5_14,AGE_15_44,AGE_45_64,AGE_65_74,AGE_75_84,AGE_85PLUS) %>%
      unite(POP_GROUP,SEX,AGE_GROUP) %>%
      spread(POP_GROUP,POP) %>%
      inner_join(imd_health) %>%
      mutate(TOTAL_POP = MALE_AGE_0_4 + 
               MALE_AGE_5_14 + 
               MALE_AGE_15_44 + 
               MALE_AGE_45_64 +
               MALE_AGE_65_74 +
               MALE_AGE_75_84 +
               MALE_AGE_85PLUS +
               FEMALE_AGE_0_4 +
               FEMALE_AGE_5_14 +
               FEMALE_AGE_15_44 +
               FEMALE_AGE_45_64 +
               FEMALE_AGE_65_74 +
               FEMALE_AGE_75_84 +
               FEMALE_AGE_85PLUS,
             ADJUSTED_POP = (2.354*MALE_AGE_0_4 + 
                               1*MALE_AGE_5_14 + 
                               0.913*MALE_AGE_15_44 + 
                               1.373*MALE_AGE_45_64 +
                               2.531*MALE_AGE_65_74 +
                               3.254*MALE_AGE_75_84 +
                               3.193*MALE_AGE_85PLUS +
                               2.241*FEMALE_AGE_0_4 +
                               1.030*FEMALE_AGE_5_14 +
                               1.885*FEMALE_AGE_15_44 +
                               2.115*FEMALE_AGE_45_64 +
                               2.820*FEMALE_AGE_65_74 +
                               3.301*FEMALE_AGE_75_84 +
                               3.090*FEMALE_AGE_85PLUS) * 1.054^IMD_HEALTH) 
    
    adj_pop = pop %>% 
      group_by(YEAR) %>%
      summarise(TOTAL = sum(TOTAL_POP), ADJ=sum(ADJUSTED_POP)) %>%
      mutate(AF=TOTAL/ADJ) %>%
      select(YEAR,AF) %>%
      inner_join(pop) %>%
      mutate(NORMALISED_ADJ_POP = ADJUSTED_POP*AF) %>%
      select(YEAR,LSOA11CD,NEED_ADJ_POP=NORMALISED_ADJ_POP,TOTAL_POP)
    
    dbWriteTable(conn = db, name = "carr_hill_pop", adj_pop, append=TRUE)
  }
  
  carr_hill_pop = tbl(db, "carr_hill_pop") %>% collect()
  
  imd = tbl(db, "imd_2015") %>% 
    filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
    select(LSOA11CD,IMD_DECILE=VALUE) %>%
    collect()
  
  decile_pop_adjusted = inner_join(carr_hill_pop,imd) %>%
    group_by(YEAR, IMD_DECILE) %>%
    summarise(NEED_ADJ_POP=sum(NEED_ADJ_POP), TOTAL_POP=sum(TOTAL_POP))
  
  dbWriteTable(conn=db, name="adj_pop_decile", decile_pop_adjusted, overwrite=TRUE)
  
  dbDisconnect(db)
}

process_attribution_dataset = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"ads_imd_props",fail_if_missing=FALSE)
  dbRemoveTable(db,"ads_lsoa_props",fail_if_missing=FALSE)
  
  imd = tbl(db, "imd_2015") %>% 
    filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
    select(LSOA11CD,IMD_DECILE=VALUE) %>%
    collect()
  
  
  for (year in 2004:2018) {
  
      if (year %in% 2004:2012) {
        ads = read_csv(paste0("raw_data/gp_pop_lsoa/ls",year,".csv")) 
        
        ads_formatted = ads %>%
          mutate(POP=rowSums(.[3:ncol(ads)])) %>%
          select(PRAC_CODE=1,LSOA11CD=2,POP)
      }  
      
      if(year==2013){
        ads = bind_rows(read_csv(paste0("raw_data/gp_pop_lsoa/ls2013 agesex under 24.csv")),
                        read_csv(paste0("raw_data/gp_pop_lsoa/ls2013 agesex over 23.csv"))) 
        ads_formatted = ads %>% 
          select(PRAC_CODE=prac,LSOA11CD=lsoa11,POP=pop) %>%
          group_by(PRAC_CODE, LSOA11CD) %>%
          summarise(POP=sum(POP))
      }
          
      if(year %in% 2014:2018) {
        ads = read_csv(paste0("raw_data/gp_pop_lsoa/gp-reg-patients-lsoa-oct-",year,".csv"),guess_max=20000)
        if(year<2017){
        ads_formatted = inner_join(ads %>%  
          filter(`NAME OF FORMER VARIABLE`=="LSOA_CODE") %>%
          select(-2, -`NAME OF FORMER VARIABLE`) %>%
        gather(LSOA,LSOA_CODE, -PRACTICE_CODE),
        ads %>%  
          filter(`NAME OF FORMER VARIABLE`=="All Patients") %>%
          select(-2, -`NAME OF FORMER VARIABLE`) %>%
          gather(LSOA,POP, -PRACTICE_CODE)) %>% 
          select(PRAC_CODE=PRACTICE_CODE, LSOA11CD=LSOA_CODE, POP) %>%
          drop_na(LSOA11CD) %>%
          mutate(POP=as.integer(POP))
        } else {
          ads_formatted = ads%>%
            select(PRAC_CODE=PRACTICE_CODE,LSOA11CD=LSOA_CODE,POP=`Number of Patients`)
        }
      }
    
    ads_lsoa_props = inner_join(ads_formatted %>% group_by(PRAC_CODE) %>% summarise(TOTAL_POP=sum(POP)), ads_formatted) %>%
      mutate(LSOA_PROP=POP/TOTAL_POP, YEAR=year) %>%
      select(YEAR,PRAC_CODE,LSOA11CD,LSOA_PROP)
    
    ads_decile = inner_join(ads_formatted,imd) %>%
      group_by(PRAC_CODE, IMD_DECILE) %>%
      summarise(POP=sum(POP))
      
    ads_imd_props = inner_join(ads_decile,ads_decile %>%
      group_by(PRAC_CODE) %>%
      summarise(TOTAL_POP=sum(POP))) %>%
      ungroup() %>%
      mutate(DECILE_PROP=POP/TOTAL_POP, YEAR=year) %>%
      select(YEAR,PRAC_CODE,IMD_DECILE,DECILE_PROP)
    
    dbWriteTable(conn=db, name="ads_imd_props", ads_imd_props, append=TRUE)
    dbWriteTable(conn=db, name="ads_lsoa_props", ads_lsoa_props, append=TRUE)
    
  }
  dbDisconnect(db)
}


calculate_gp_per_100k_trend_by_imd_practice_level = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_data = tbl(db, "gp_workforce") %>%
    left_join(tbl(db, "gp_imd_2015") %>% select(PRAC_CODE,IMD_SCORE,IMD_DECILE)) %>%
    collect() %>%
    drop_na(TOTAL_GP_FTE,TOTAL_GP_EXRR_FTE,TOTAL_PATIENTS,IMD_DECILE) %>%
    mutate(NEED_ADJUSTED_POP = 2.354*MALE_PATIENTS_0TO4 + 
             1*MALE_PATIENTS_5TO14 + 
             0.913*MALE_PATIENTS_15TO44 + 
             1.373*MALE_PATIENTS_45TO64 +
             2.531*MALE_PATIENTS_65TO74 +
             3.254*MALE_PATIENTS_75TO84 +
             3.193*MALE_PATIENTS_85PLUS +
             2.241*FEMALE_PATIENTS_0TO4 +
             1.030*FEMALE_PATIENTS_5TO14 +
             1.885*FEMALE_PATIENTS_15TO44 +
             2.115*FEMALE_PATIENTS_45TO64 +
             2.820*FEMALE_PATIENTS_65TO74 +
             3.301*FEMALE_PATIENTS_75TO84 +
             3.090*FEMALE_PATIENTS_85PLUS) 
  dbDisconnect(db)
  
  gp_data = add_quintiles(gp_data)
  
  graph_data = gp_data %>% inner_join(gp_data %>%
                                        group_by(YEAR) %>%
                                        summarise(TOTAL_POP=sum(TOTAL_PATIENTS),NEED_ADJUSTED_POP=sum(NEED_ADJUSTED_POP)) %>%
                                        mutate(NORMALISATION_FACTOR=TOTAL_POP/NEED_ADJUSTED_POP) %>%
                                        select(YEAR,NORMALISATION_FACTOR)) %>%
    mutate(NEED_ADJUSTED_POP=NEED_ADJUSTED_POP*NORMALISATION_FACTOR) %>%
    ungroup() %>%
    group_by(YEAR,IMD_QUINTILE) %>%
    summarise(GP_FTE=sum(TOTAL_GP_FTE),GP_EXRR_FTE=sum(TOTAL_GP_EXRR_FTE),ADJ_POP=sum(NEED_ADJUSTED_POP),POP=sum(TOTAL_PATIENTS)) %>%
    mutate(GPS_PER_100K=round(100000*GP_FTE/POP,1),GPS_PER_100K_ADJ=round(100000*GP_FTE/ADJ_POP,1),
           GPS_EXRR_PER_100K=round(100000*GP_EXRR_FTE/POP,1),GPS_EXRR_PER_100K_ADJ=round(100000*GP_EXRR_FTE/ADJ_POP,1))
  
  
  graph_data = gather(graph_data,VAR,VAL,GP_FTE,GP_EXRR_FTE,POP,ADJ_POP,GPS_PER_100K,GPS_PER_100K_ADJ,GPS_EXRR_PER_100K,GPS_EXRR_PER_100K_ADJ) %>%
    mutate(IMD_QUINTILE=as.factor(IMD_QUINTILE),
           VAR=factor(VAR,c("POP",
                            "ADJ_POP",
                            "GP_FTE",
                            "GP_EXRR_FTE",
                            "GPS_PER_100K",
                            "GPS_PER_100K_ADJ",
                            "GPS_EXRR_PER_100K",
                            "GPS_EXRR_PER_100K_ADJ"),
                      c("Population",
                        "Adjusted Population (age weights)",
                        "Total GPs (FTE)",
                        "Total GPs (FTE excl. RR)",
                        "GPs (FTE) per 100k population unadjusted",
                        "GPs (FTE) per 100k population need adjusted (age weights)",
                        "GPs (FTE excl. RR) per 100k population unadjusted",
                        "GPs (FTE excl. RR) per 100k population need adjusted (age weights)")))
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  gp_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(.~VAR, scales="free", ncol=2, labeller=labeller(VAR = label_wrap_gen(40))) +
    scale_y_continuous(labels = comma) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a")) +
    labs(title = "Trends in GP supply in England by neighbourhood deprivation quintile",
         subtitle = "Based on practice level population and IMD (2015) deciles for years 2012 - 2018",
         caption = "Note: IMD aggregated to practice level and patient registered populations rather than attributed LSOA populations used")
  
  ggsave("figures/gp_trends_practice.png", gp_plot, width=30, height=35, units="cm", dpi="print")
  
}

calculate_gp_per_100k_trend_by_imd_lsoa_level = function(database_name="primary_care_data.sqlite3", imd_aggregated=TRUE, end_year=2014){
  db = dbConnect(SQLite(), dbname=database_name)
  gps = tbl(db, "gp_workforce") %>% 
    select(YEAR,PRAC_CODE,TOTAL_GP_EXRR_FTE, TOTAL_GP_FTE) %>%
    collect()
  
  if(imd_aggregated){
    ads_imd = tbl(db, "ads_imd_props") %>% collect()
    
    ads_quintile = add_quintiles(ads_imd) %>%
      group_by(YEAR,PRAC_CODE,IMD_QUINTILE) %>%
      summarise(QUINTILE_PROP=sum(DECILE_PROP))
    
    gps_imd_quintile = inner_join(gps,ads_quintile) %>%
      mutate(TOTAL_GP_EXRR_FTE = QUINTILE_PROP*TOTAL_GP_EXRR_FTE,
             TOTAL_GP_FTE = QUINTILE_PROP*TOTAL_GP_FTE) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(TOTAL_GP_EXRR_FTE = sum(TOTAL_GP_EXRR_FTE,na.rm=TRUE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE,na.rm=TRUE))
    
    adj_pop = tbl(db, "adj_pop_decile") %>% collect()
    adj_pop_quintile = add_quintiles(adj_pop) %>% 
      group_by(YEAR, IMD_QUINTILE) %>%
      summarise(NEED_ADJ_POP=sum(NEED_ADJ_POP), TOTAL_POP=sum(TOTAL_POP))

    
    gps_quintile = inner_join(gps_imd_quintile,adj_pop_quintile)
  
  } else {
    ads_lsoa = tbl(db, "ads_lsoa_props") %>% collect()
    
    adj_pop_lsoa = tbl(db, "carr_hill_pop") %>% collect()
    
    imd = tbl(db, "imd_2015") %>% 
      filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
      select(LSOA11CD,IMD_DECILE=VALUE) %>%
      collect() %>% add_quintiles()
    
    gps_lsoa = inner_join(gps,ads_lsoa) %>%
      mutate(TOTAL_GP_EXRR_FTE = LSOA_PROP*TOTAL_GP_EXRR_FTE,
             TOTAL_GP_FTE = LSOA_PROP*TOTAL_GP_FTE) %>%
      group_by(YEAR,LSOA11CD) %>%
      summarise(TOTAL_GP_EXRR_FTE = sum(TOTAL_GP_EXRR_FTE,na.rm=FALSE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE,na.rm=FALSE)) %>%
      inner_join(adj_pop_lsoa) %>%
      inner_join(imd)
    
    gps_quintile = gps_lsoa %>% 
      drop_na(TOTAL_GP_EXRR_FTE,TOTAL_GP_FTE,NEED_ADJ_POP,TOTAL_POP,IMD_QUINTILE) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(TOTAL_GP_EXRR_FTE = sum(TOTAL_GP_EXRR_FTE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE),
                NEED_ADJ_POP = sum(NEED_ADJ_POP),
                TOTAL_POP = sum(TOTAL_POP))
  }
  
  
  graph_data = gps_quintile %>%
    filter(YEAR<=end_year) %>%
    mutate(GPS_PER_100K=round(100000*TOTAL_GP_FTE/TOTAL_POP,1),
           GPS_PER_100K_ADJ=round(100000*TOTAL_GP_FTE/NEED_ADJ_POP,1),
           GPS_EXRR_PER_100K=round(100000*TOTAL_GP_EXRR_FTE/TOTAL_POP,1),
           GPS_EXRR_PER_100K_ADJ=round(100000*TOTAL_GP_EXRR_FTE/NEED_ADJ_POP,1)) %>%
    mutate(IMD_QUINTILE=as.factor(IMD_QUINTILE)) %>%
    gather(VAR,VAL,
           TOTAL_GP_FTE,
           TOTAL_GP_EXRR_FTE,
           TOTAL_POP,
           NEED_ADJ_POP,
           GPS_PER_100K,
           GPS_PER_100K_ADJ,
           GPS_EXRR_PER_100K,
           GPS_EXRR_PER_100K_ADJ) %>%
    mutate(VAR=factor(VAR,c("TOTAL_POP",
                            "NEED_ADJ_POP",
                            "TOTAL_GP_FTE",
                            "TOTAL_GP_EXRR_FTE",
                            "GPS_PER_100K",
                            "GPS_PER_100K_ADJ",
                            "GPS_EXRR_PER_100K",
                            "GPS_EXRR_PER_100K_ADJ"),
                      c("Population",
                        "Need adjusted Population",
                        "Total GPs (FTE)",
                        "Total GPs (FTE excl. RR)",
                        "GPs (FTE) per 100k population unadjusted",
                        "GPs (FTE) per 100k population need adjusted",
                        "GPs (FTE excl. RR) per 100k population unadjusted",
                        "GPs (FTE excl. RR) per 100k population need adjusted")))
  
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  gp_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    geom_vline(xintercept=c(2012.5), linetype="dashed", colour="lightgrey") +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(VAR~.,ncol=2,scales="free",labeller = labeller(VAR = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a")) +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years 2004 - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data before dashed line from DH using 2001 LSOA neighbourhoods after the dashed line are from NHS Digital based on LSOA 2011 neighbourhoods")
  
  ggsave("figures/gp_trends.png", gp_plot, width=30, height=35, units="cm", dpi="print")
  
  dbDisconnect(db)
}
  
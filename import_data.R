library("tidyverse")
library("readxl")
library("RSQLite")
library("fingertipsR")
library("scales")
library("purrr")
library("modelr")
library("ggpubr")
library("rgdal")
library("rmapshaper")

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

age_range_to_integer = function(age_range){
  age = c(30,32,37,42,47,52,57,62,67,70,NA)
  names(age) = c("UNDER30","30TO34","35TO39","40TO44","45TO49","50TO54","55TO59","60TO64","65TO69","70PLUS","UNKNOWN")
  
  return(unname(age[age_range]))
}

write_lsoa_01_11_mapping = function(database_name="primary_care_data.sqlite3"){
  mapping = read_csv("raw_data/Lower_Layer_Super_Output_Area_2001_to_Lower_Layer_Super_Output_Area_2011_to_Local_Authority_District_2011_Lookup_in_England_and_Wales.csv")
  weighted_mapping = mapping %>% left_join(
    mapping %>% filter(CHGIND=="S") %>%
    group_by(LSOA01CD) %>%
    summarise(WEIGHT=1/n())) %>%
    mutate(WEIGHT=if_else(CHGIND %in% c("M","U","X"),1,WEIGHT))
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "lsoa_2001_2011_mapping", weighted_mapping, overwrite=TRUE)
  dbDisconnect(db)
}

format_ons_pop = function(year, sex){
  if(year<2018){
    num="20"
    ext="xls"
  } else {
    num="21"
    ext="xlsx"
  }
  xl = read_excel(path=paste0("raw_data/ons_pop/SAPE",num,"DT2-mid-",year,"-lsoa-syoa-estimates-unformatted.",ext),
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
  for (year in 2002:2018) {
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


write_imd_2019_data_to_db = function(database_name="primary_care_data.sqlite3"){
  imd_2019 = read_csv("raw_data/imd/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_2.csv") %>% select(c(1,5,17,6,7))
  names(imd_2019) = c("LSOA11CD","IMD_SCORE","IMD_HEALTH_SCORE","IMD_RANK","IMD_DECILE")
  imd_2019 = imd_2019 %>% add_quintiles()
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "imd_2019", imd_2019, overwrite=TRUE)
  dbDisconnect(db)
}



write_workforce_varlist_to_db = function(database_name="primary_care_data.sqlite3"){
  varlist = read_csv("raw_data/workforce/GPs 2015-2018/varlist.csv")
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "workforce_variables", varlist, overwrite=TRUE)
  dbDisconnect(db)
}

get_workforce_varlist = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_vars =tbl(db,"workforce_variables") %>% filter(ATTRIBUTE==TRUE) %>% select(VARNAMES) %>% collect()
  dbDisconnect(db)
  return(workforce_vars$VARNAMES)
}
write_workforce_data_to_db_new = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_vars=get_workforce_varlist()
  
  gp_year = list()
  for (year in 2015:2018) {
    wf = read_csv(paste0("raw_data/workforce/GPs 2015-2019/General Practice September ",year," Practice Level.csv"),
                  guess_max = 100000,
                  na=c("","NA","EST","NS",".","ND"))
    wf = wf %>% mutate(YEAR=year)
    
    gp_year[[as.character(year)]] = wf %>%
      select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT, gp_vars)
  }
  
  workforce = map_dfr(gp_year,bind_rows) %>% 
    arrange(YEAR,PRAC_CODE)


  dbWriteTable(conn=db, name="gp_workforce_newdata", workforce, overwrite=TRUE)
  dbDisconnect(db)

}


linear_impute = function(df, varname, default) {
  f = as.formula(paste0(varname,"~ YEAR"))
  if(all(is.na(df[,varname]))){
    df[,varname] = default
  }
  model = lm(f, data = df)
  return(model)
}

map_linear_impute = function(nested_df, variable, default){
  df = nested_df %>%
    mutate(model = map(data, linear_impute, varname=variable, default)) %>%
    mutate(pred  = map2(data, model, add_predictions)) %>%
    unnest(pred)
  
  df[is.na(df[variable]),variable] = df[is.na(df[variable]),"pred"]
  df[df[variable]<0,variable] = 0
  if (grepl("_HC",variable)) {
    df[variable] = round(df[variable])
  }
  return(df %>% select(-pred))
}

impute_missing_gp_data_lm = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_workforce = tbl(db, "gp_workforce_newdata") %>% filter(YEAR>2014) %>% collect() 
  
  gp_workforce_imputed = gp_workforce
  gp_vars =  get_workforce_varlist()
  
  for (variable in gp_vars) {
    print(variable)
    gp_workforce_nested = gp_workforce_imputed %>% 
      group_by(PRAC_CODE) %>%
      nest()
    gp_workforce_imputed = map_linear_impute(gp_workforce_nested, variable, median(gp_workforce[[variable]], na.rm=TRUE))
  }
  
  dbWriteTable(conn=db, name="gp_workforce_newdata_imputed", gp_workforce_imputed, overwrite=TRUE)
  dbDisconnect(db)
  
}

add_new_variables_to_imputation = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_workforce_prev_imputed = tbl(db, "gp_workforce_newdata_imputed") %>% filter(YEAR>2014) %>% select(-contains("_PATIENTS")) %>% collect() 
  already_imputed = names(gp_workforce_prev_imputed)
  gp_workforce = tbl(db, "gp_workforce_newdata") %>% filter(YEAR>2014) %>% select(-already_imputed,"YEAR","PRAC_CODE") %>% collect() 
  
  gp_workforce_imputed = gp_workforce
  gp_vars = get_workforce_varlist()
  gp_vars = setdiff(gp_vars, already_imputed)
  
  for (variable in gp_vars) {
    print(variable)
    gp_workforce_nested = gp_workforce_imputed %>% 
      group_by(PRAC_CODE) %>%
      nest()
    gp_workforce_imputed = map_linear_impute(gp_workforce_nested, variable, median(gp_workforce[[variable]], na.rm=TRUE))
  }
  
  gp_workforce_updated_imputed = gp_workforce_prev_imputed %>% inner_join(gp_workforce_imputed)
  dbWriteTable(conn=db, name="gp_workforce_newdata_imputed", gp_workforce_updated_imputed, overwrite=TRUE)
  
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
    ungroup() %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(TOTAL_GP_EXRRL_HC=TOTAL_GP_EXRR_HC,
        TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRR_FTE)
    
  
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

write_carr_hill_population_data_to_db = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"carr_hill_pop",fail_if_missing=FALSE)

  imd_health = tbl(db, "imd_2019") %>% 
    select(LSOA11CD,IMD_HEALTH=IMD_HEALTH_SCORE) %>%
    collect()
  
  for (year in 2002:2018) {
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
  
  imd = tbl(db, "imd_2019") %>% 
    select(LSOA11CD,IMD_DECILE) %>%
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
  dbRemoveTable(db,"ads_prac_props",fail_if_missing=FALSE)
  
  imd = tbl(db, "imd_2019") %>% 
    select(LSOA11CD,IMD_DECILE) %>%
    collect()
  
  lsoa_mapping = tbl(db,"lsoa_2001_2011_mapping") %>%
    select(LSOA01CD,LSOA11CD,WEIGHT) %>%
    collect()
  
  for (year in 2004:2018) {
  
      if (year %in% 2004:2012) {
        ads = read_csv(paste0("raw_data/gp_pop_lsoa/ls",year,".csv")) 
        
        ads_formatted_2001 = ads %>%
          mutate(POP=rowSums(.[3:ncol(ads)])) %>%
          select(PRAC_CODE=1,LSOA01CD=2,POP)
        ads_formatted = left_join(ads_formatted_2001, lsoa_mapping) %>%
          group_by(PRAC_CODE,LSOA11CD) %>%
          summarise(POP=sum(WEIGHT*POP)) %>%
          drop_na(LSOA11CD)
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
    
    ads_prac_props = inner_join(ads_formatted %>% group_by(LSOA11CD) %>% summarise(TOTAL_POP=sum(POP)), ads_formatted) %>%
      mutate(PRAC_PROP=POP/TOTAL_POP, YEAR=year) %>%
      select(YEAR,PRAC_CODE,LSOA11CD,PRAC_PROP) 
    
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
    dbWriteTable(conn=db, name="ads_prac_props", ads_prac_props, append=TRUE)
    
  }
  dbDisconnect(db)
}

calculate_gp_practice_imd_2019 = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  ads = tbl(db, "ads_prac_props") %>% collect()
  imd = tbl(db, "imd_2019") %>% select(LSOA11CD,IMD_SCORE) %>% collect()
  pop = tbl(db, "carr_hill_pop") %>% filter(YEAR==2015) %>% select(LSOA11CD,TOTAL_POP) %>% collect()
  
  prac_imd = ads %>% inner_join(imd) %>% inner_join(pop) %>%
    group_by(YEAR,PRAC_CODE) %>%
    summarise(IMD_SCORE=sum(PRAC_PROP*TOTAL_POP*IMD_SCORE), TOTAL_POP=sum(PRAC_PROP*TOTAL_POP)) %>%
    mutate(IMD_SCORE=IMD_SCORE/TOTAL_POP) %>%
    ungroup() %>%
    select(YEAR,PRAC_CODE,IMD_SCORE,TOTAL_POP)
    
  prac_imd = prac_imd %>% 
    group_by(YEAR) %>%
    arrange(-IMD_SCORE) %>%
    mutate(CUM_POP = cumsum(TOTAL_POP), PROP = CUM_POP/max(CUM_POP),
           IMD_DECILE=cut(PROP,10,labels=FALSE),
           IMD_QUINTILE = cut(PROP,5,labels=FALSE)) %>%
  select(YEAR,PRAC_CODE,IMD_SCORE,IMD_DECILE,IMD_QUINTILE)
  
  dbWriteTable(conn=db, name="gp_imd_2019", prac_imd, overwrite=TRUE)
  
  dbDisconnect(db)
}

calculate_ccg_imd_2019 = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  ccg_imd = tbl(db,"ccg_imd_2019") %>% collect()
  ccg_pop = tbl(db,"ccg_pop") %>% filter(YEAR==2015) %>% select(CCG19CD,TOTAL_POP) %>% collect()
  ccg_imd_quintiles = ccg_imd %>% inner_join(ccg_pop) %>% arrange(-average_score) %>% 
    mutate(CUM_POP = cumsum(TOTAL_POP), PROP = CUM_POP/max(CUM_POP),
           IMD_DECILE=cut(PROP,10,labels=FALSE),
           IMD_QUINTILE = cut(PROP,5,labels=FALSE)) %>%
    select(-TOTAL_POP,-CUM_POP,-PROP)
  
  dbWriteTable(conn=db, name="ccg_imd_2019", ccg_imd_quintiles, overwrite=TRUE)
  
  dbDisconnect(db)
  
}

calculate_gp_practice_populations = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  ads = tbl(db, "ads_prac_props") %>% collect()
  pop = tbl(db, "carr_hill_pop") %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect()
  
  prac_pop = ads %>% inner_join(pop) %>%
    group_by(YEAR,PRAC_CODE) %>%
    summarise(NEED_ADJ_POP=sum(PRAC_PROP*NEED_ADJ_POP), TOTAL_POP=sum(PRAC_PROP*TOTAL_POP)) %>%
    ungroup() 
  
  dbWriteTable(conn=db, name="gp_population", prac_pop, overwrite=TRUE)
  
  dbDisconnect(db)
}

attribute_workforce_to_imd_lsoa_level = function(database_name="primary_care_data.sqlite3", imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_vars = get_workforce_varlist()
  
  if(imputed){
    gp_table = "gp_workforce_newdata_imputed"
  } else {
    gp_table = "gp_workforce_newdata"
  }
  gps = tbl(db, gp_table) %>% 
    select(YEAR,PRAC_CODE,workforce_vars) %>%
    collect()
  
    ads_lsoa = tbl(db, "ads_lsoa_props") %>% filter(YEAR>2014) %>% collect()
    
    adj_pop_lsoa = tbl(db, "carr_hill_pop") %>% filter(YEAR>2014) %>% collect()
    
    imd = tbl(db, "imd_2019") %>% 
      collect()
    
    # drops LSOAs in Wales and NO2011 which seems to be some NHS digital null code
    gps_lsoa = inner_join(gps,ads_lsoa) %>%
      mutate_at(vars(one_of(workforce_vars)), ~(.*LSOA_PROP)) %>%
      group_by(YEAR,LSOA11CD) %>%
      summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) %>%
      inner_join(adj_pop_lsoa) %>%
      inner_join(imd)
    
    if(imputed){
      lsoa_table = "gp_workforce_newdata_lsoa_imputed"
    } else {
      lsoa_table = "gp_workforce_newdata_lsoa"
    }
    dbWriteTable(conn=db, name=lsoa_table, gps_lsoa, overwrite=TRUE)
    
    gp_imd = tbl(db, "gp_imd_2019") %>% select(YEAR,PRAC_CODE,IMD_QUINTILE) %>% collect()
    
    # some practices do not have LSOAs associated with them in ADS data so: 
    # (a) for those that have practice IMD use that 
    # (b) for those that do not have practice IMD split GPs equally across quintiles
    missing_lsoa_a = left_join(crossing(tibble(YEAR=2015:2018), tibble(IMD_QUINTILE=1:5)),
      left_join(gps,ads_lsoa) %>% 
      filter(is.na(LSOA11CD)) %>%
      left_join(gp_imd) %>%
      filter(!is.na(IMD_QUINTILE)) %>%
      select(one_of(workforce_vars),YEAR, IMD_QUINTILE) %>%
      group_by(YEAR, IMD_QUINTILE) %>%
      summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) %>%
      ungroup()) %>%
      replace(is.na(.), 0) %>%
      arrange(YEAR,IMD_QUINTILE)
    
    missing_lsoa_b = inner_join(crossing(tibble(YEAR=2015:2018), tibble(IMD_QUINTILE=1:5)),
                                left_join(gps,ads_lsoa) %>% 
                                  filter(is.na(LSOA11CD)) %>%
                                  left_join(gp_imd) %>%
                                  filter(is.na(IMD_QUINTILE)) %>%
                                  select(one_of(workforce_vars),YEAR) %>%
                                  group_by(YEAR) %>%
                                  summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) %>%
                                  mutate_at(vars(one_of(workforce_vars)), ~(./5))) %>%
      ungroup() %>%
      arrange(YEAR,IMD_QUINTILE)
    
    gps_quintile = gps_lsoa %>% 
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise_at(vars(one_of(c(workforce_vars,"NEED_ADJ_POP","TOTAL_POP"))), sum, na.rm=TRUE) %>%
      ungroup() %>%
      arrange(YEAR,IMD_QUINTILE)
    
    missing_quintiles = missing_lsoa_a
    missing_quintiles[,workforce_vars] = missing_lsoa_a[,workforce_vars] + missing_lsoa_b[,workforce_vars]
    
    dbWriteTable(conn=db, name="gp_workforce_missing_imd_quintiles", missing_quintiles, overwrite=TRUE)
    
    gps_quintile[,workforce_vars] = gps_quintile[,workforce_vars] + missing_quintiles[,workforce_vars]
    
    if(imputed){
      quintiles_table = "gp_workforce_imd_quintiles_imputed"
    } else {
      quintiles_table = "gp_workforce_imd_quintiles"
    }
    dbWriteTable(conn=db, name=quintiles_table, gps_quintile, overwrite=TRUE)
    dbDisconnect(db)
}


ccg_workforce = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_vars = get_workforce_varlist()
    
  ccg_lsoa = tbl(db, "ccg_lsoa_lad_mapping") %>% 
    select(LSOA11CD,CCG19CD) %>%
    collect()
  
  workforce_lsoa = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>%
    collect()
  
  workforce_ccg = ccg_lsoa %>%
    inner_join(workforce_lsoa) %>% 
    group_by(YEAR,CCG19CD) %>%
    summarise_at(vars(one_of(c(workforce_vars,"NEED_ADJ_POP","TOTAL_POP"))), sum, na.rm=TRUE) %>%
    ungroup() %>% 
    arrange(YEAR,CCG19CD)
  
  dbWriteTable(conn = db, name = "ccg_workforce_imputed", workforce_ccg, overwrite=TRUE)
  
  dbDisconnect(db)
}


calculate_pcn_imd_2019 = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  imd = tbl(db, "gp_imd_2019") %>% select(YEAR,PRAC_CODE,IMD_SCORE) %>% collect()
  pop = tbl(db, "gp_population") %>% select(YEAR,PRAC_CODE,TOTAL_POP) %>% collect()
  pcn_prac_mapping = tbl(db, "pcn_gp_practice_mapping") %>% select(PRAC_CODE,PCN_CODE) %>% collect()
  
  pcn_imd = imd %>% inner_join(pop) %>% inner_join(pcn_prac_mapping) %>%
    group_by(YEAR,PCN_CODE) %>%
    summarise(IMD_SCORE=sum(TOTAL_POP*IMD_SCORE), TOTAL_POP=sum(TOTAL_POP)) %>%
    mutate(IMD_SCORE=IMD_SCORE/TOTAL_POP) %>%
    ungroup() %>%
    select(YEAR,PCN_CODE,IMD_SCORE)
  
  pcn_imd = pcn_imd %>% 
    group_by(YEAR) %>%
    mutate(IMD_RANK = row_number(-1*IMD_SCORE),
           IMD_DECILE = ntile(IMD_RANK,10)) %>% 
    add_quintiles()
  
  dbWriteTable(conn=db, name="pcn_imd_2019", pcn_imd, overwrite=TRUE)
  
  dbDisconnect(db)
}

pcn_workforce = function(database_name="primary_care_data.sqlite3"){
  
  db = dbConnect(SQLite(), dbname=database_name)
  
  pcn_prac_mapping = tbl(db, "pcn_gp_practice_mapping") %>% select("PRAC_CODE","PCN_CODE","PCN_NAME") %>% collect()
  workforce_prac = tbl(db, "gp_workforce_newdata_imputed") %>% collect()
  prac_pop = tbl(db,"gp_population") %>% collect()
  workforce_vars = get_workforce_varlist()
  
  workforce_pcn = pcn_prac_mapping %>%
    inner_join(workforce_prac) %>% 
    inner_join(prac_pop) %>%
    group_by(YEAR,PCN_CODE) %>%
    summarise_at(vars(one_of(c(workforce_vars,"NEED_ADJ_POP","TOTAL_POP"))), sum, na.rm=TRUE) %>%
    ungroup() %>% 
    arrange(YEAR,PCN_CODE)
  
  dbWriteTable(conn = db, name = "pcn_workforce_imputed", workforce_pcn, overwrite=TRUE)
  
  dbDisconnect(db)
}


###############################################
## code to generate geojson maps
###############################################

make_england_ccg_geojson = function(database_name="primary_care_data.sqlite3"){
  for(year in 2015:2018){
    ccg_map = readOGR("raw_data/geography/shape_files/Clinical_Commissioning_Groups_April_2019_Ultra_Generalised_Clipped_Boundaries_England/", 
                      "Clinical_Commissioning_Groups_April_2019_Ultra_Generalised_Clipped_Boundaries_England",
                      verbose=FALSE, stringsAsFactors=FALSE)
    ccg_map = spTransform(ccg_map, CRS("+proj=longlat +ellps=WGS84"))
    db = dbConnect(SQLite(), dbname=database_name)
    ccg_imd = tbl(db,"ccg_imd_2019") %>% select(CCG19CD,IMD_SCORE=average_score) %>% collect()
    ccg_workforce = tbl(db,"ccg_workforce_imputed") %>% filter(YEAR==year) %>% select(CCG19CD,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE, TOTAL_NURSES_FTE,TOTAL_DPC_FTE,TOTAL_ADMIN_FTE,TOTAL_POP,NEED_ADJ_POP) %>% collect()
    dbDisconnect(db)
    
    ccg_data = ccg_imd %>% inner_join(ccg_workforce) %>% 
      mutate(GP_HC_100k=round(100000*TOTAL_GP_EXRRL_HC/TOTAL_POP,2),
             GP_FTE_100k = round(100000*TOTAL_GP_EXRRL_FTE/TOTAL_POP,2),
             NURSE_FTE_100k = round(100000*TOTAL_NURSES_FTE/TOTAL_POP,2),
             DPC_FTE_100k = round(100000*TOTAL_DPC_FTE/TOTAL_POP,2),
             ADMIN_FTE_100k = round(100000*TOTAL_ADMIN_FTE/TOTAL_POP,2)) %>%
      select(ccg19cd=CCG19CD,IMD_SCORE,TOTAL_POP,NEED_ADJ_POP,GP_HC_100k,GP_FTE_100k,NURSE_FTE_100k,DPC_FTE_100k,ADMIN_FTE_100k)
    
    ccg_map@data = ccg_map@data %>% left_join(ccg_data)
    writeOGR(ccg_map, dsn=paste0("maps/ccg_",year,"_map.geojson"), layer="OGRGeoJSON", driver="GeoJSON", check_exists=FALSE)
  }
}

make_england_lsoa_geojson = function(database_name="primary_care_data.sqlite3"){
  lsoa_map = readOGR("raw_data/geography/shape_files/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales/", 
                     "Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales",
                     verbose=FALSE, stringsAsFactors=FALSE)
  lsoa_map = spTransform(lsoa_map, CRS("+proj=longlat +ellps=WGS84"))
  db = dbConnect(SQLite(), dbname=database_name)
  lsoa_workforce = tbl(db,"gp_workforce_newdata_lsoa_imputed") %>% select(YEAR, LSOA11CD, TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE, TOTAL_NURSES_FTE,TOTAL_DPC_FTE,TOTAL_ADMIN_FTE,TOTAL_POP,NEED_ADJ_POP,IMD_SCORE,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  for (year in 2015:2018) {
      lsoa_year_map = lsoa_map
      lsoa_data = lsoa_workforce %>% filter(YEAR==year) %>%
        mutate(GP_HC_100k=round(100000*TOTAL_GP_EXRRL_HC/TOTAL_POP,2),
               GP_FTE_100k = round(100000*TOTAL_GP_EXRRL_FTE/TOTAL_POP,2),
               NURSE_FTE_100k = round(100000*TOTAL_NURSES_FTE/TOTAL_POP,2),
               DPC_FTE_100k = round(100000*TOTAL_DPC_FTE/TOTAL_POP,2),
               ADMIN_FTE_100k = round(100000*TOTAL_ADMIN_FTE/TOTAL_POP,2)) %>%
        select(lsoa11cd=LSOA11CD,IMD_SCORE,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,GP_HC_100k,GP_FTE_100k,NURSE_FTE_100k,DPC_FTE_100k,ADMIN_FTE_100k)
      lsoa_year_map@data = lsoa_year_map@data %>% left_join(lsoa_data)
      
      lsoa_year_map = ms_simplify(lsoa_year_map)
      writeOGR(lsoa_year_map, dsn=paste0("maps/ccg_lsoa_all_",year,"_map.geojson"), layer="OGRGeoJSON", driver="GeoJSON", check_exists=FALSE)
      
    }
  
}

make_ccg_geojson = function(database_name="primary_care_data.sqlite3"){
  lsoa_map = readOGR("raw_data/geography/shape_files/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales/", 
                     "Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales",
                     verbose=FALSE, stringsAsFactors=FALSE)
  db = dbConnect(SQLite(), dbname=database_name)
  ccg_lsoa = tbl(db, "ccg_lsoa_lad_mapping") %>% select(CCG19CD,LSOA11CD) %>% collect()
  lsoa_workforce = tbl(db,"gp_workforce_newdata_lsoa_imputed") %>% select(YEAR, LSOA11CD, TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE, TOTAL_NURSES_FTE,TOTAL_DPC_FTE,TOTAL_ADMIN_FTE,TOTAL_POP,NEED_ADJ_POP,IMD_SCORE,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  for (year in 2015:2018) {
    
    ccgs = ccg_lsoa %>% distinct(CCG19CD)
    for (ccg in ccgs$CCG19CD) {
      lsoas = ccg_lsoa %>% filter(CCG19CD==ccg) %>% select(LSOA11CD)
      ccg_lsoa_map = subset(lsoa_map, lsoa11cd %in% lsoas$LSOA11CD)
      ccg_lsoa_map = spTransform(ccg_lsoa_map, CRS("+proj=longlat +ellps=WGS84"))
      ccg_data = lsoa_workforce %>% filter(YEAR==year) %>%
        mutate(GP_HC_100k=round(100000*TOTAL_GP_EXRRL_HC/TOTAL_POP,2),
               GP_FTE_100k = round(100000*TOTAL_GP_EXRRL_FTE/TOTAL_POP,2),
               NURSE_FTE_100k = round(100000*TOTAL_NURSES_FTE/TOTAL_POP,2),
               DPC_FTE_100k = round(100000*TOTAL_DPC_FTE/TOTAL_POP,2),
               ADMIN_FTE_100k = round(100000*TOTAL_ADMIN_FTE/TOTAL_POP,2)) %>%
        select(lsoa11cd=LSOA11CD,IMD_SCORE,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,GP_HC_100k,GP_FTE_100k,NURSE_FTE_100k,DPC_FTE_100k,ADMIN_FTE_100k)
      ccg_lsoa_map@data = ccg_lsoa_map@data %>% left_join(ccg_data)
      writeOGR(ccg_lsoa_map, dsn=paste0("maps/ccg_lsoa_",ccg,"_",year,"_map.geojson"), layer="OGRGeoJSON", driver="GeoJSON", check_exists=FALSE)
      
    }
    
  }
  
}
  

import_ethnicity_data = function(database_name="primary_care_data.sqlite3"){
  bame = read_csv("raw_data/census pop/ethnicity_lsoa.csv")
  bame = bame %>% mutate(BAME_Q=ntile(BAME_PERCENTAGE,5))
  
  religion = read_csv("raw_data/census pop/religion_lsoa.csv")
  religion = religion %>% filter(grepl("^E",LSOA11CD))
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn = db, name = "bame_lsoa", bame, overwrite=TRUE)
  dbWriteTable(conn = db, name = "religion_lsoa", religion, overwrite=TRUE)
  dbDisconnect(db)
  
}


#################################################
#### payments to GP practices data
#################################################

import_payments_data = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"gp_practice_payments",fail_if_missing=FALSE)
  
  payments = read_csv(paste0("raw_data/payments/nhspaymentsgp-15-16-csv.csv"))
  colnames(payments) = gsub("_\xa3","",colnames(payments))
  
  payments = payments %>% mutate(YEAR=2015) %>% select(YEAR,
                                                       PRAC_CODE=PracticeCode,
                                                       PATIENTS=NumberOfRegisteredPatientsLastKnownFigure,
                                                       WEIGHTED_PATIENTS=NumberOfWeightedPatientsLastKnownFigure,
                                                       TOTAL_PAYMENTS=TotalNHSPaymentsToGeneralPractice,
                                                       NET_PAYMENTS=TotalNHSPaymentsToGeneralPracticeMinusDeductions)
  dbWriteTable(conn=db, name="gp_practice_payments", payments, append=TRUE)
  
  payments = read_csv(paste0("raw_data/payments/nhspaymentsgp-16-17-csv.csv"),skip=1,col_names=FALSE)
  
  payments = payments %>% mutate(YEAR=2016) %>% select(YEAR,
                                                       PRAC_CODE=5,
                                                       PATIENTS=17,
                                                       WEIGHTED_PATIENTS=18,
                                                       TOTAL_PAYMENTS=52,
                                                       NET_PAYMENTS=54)
  dbWriteTable(conn=db, name="gp_practice_payments", payments, append=TRUE)
  
  payments = read_csv(paste0("raw_data/payments/nhspaymentsgp-17-18-csv.csv"))
  colnames(payments) = gsub("_\xa3","",colnames(payments))
  
  payments = payments %>% mutate(YEAR=2017) %>% select(YEAR,
                                                       PRAC_CODE=`Practice Code`,
                                                       PATIENTS=`Number of Registered Patients (Last Known Figure)`,
                                                       WEIGHTED_PATIENTS=`Number of Weighted Patients (Last Known Figure)`,
                                                       TOTAL_PAYMENTS=`Total NHS Payments to General Practice`,
                                                       NET_PAYMENTS=`Total NHS Payments to General Practice Minus Deductions`)
  dbWriteTable(conn=db, name="gp_practice_payments", payments, append=TRUE)
  
  payments = read_csv(paste0("raw_data/payments/nhspaymentsgp-18-19-csv.csv"))
  colnames(payments) = gsub("_\xa3","",colnames(payments))
  
  payments = payments %>% mutate(YEAR=2018) %>% select(YEAR,
                                                       PRAC_CODE=`Practice Code`,
                                                       PATIENTS=`Average Number of Registered Patients`,
                                                       WEIGHTED_PATIENTS=`Average Number of Weighted Patients`,
                                                       TOTAL_PAYMENTS=`Total NHS Payments to General Practice`,
                                                       NET_PAYMENTS=`Total NHS Payments to General Practice Minus Deductions`)
  dbWriteTable(conn=db, name="gp_practice_payments", payments, append=TRUE)
  
  
  dbDisconnect(db)
}


attribute_payments_data_to_lsoa = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  payments = tbl(db, "gp_practice_payments") %>% collect()
  ads_lsoa = tbl(db, "ads_lsoa_props") %>% filter(YEAR>2014) %>% collect()
  payments_lsoa = inner_join(payments,ads_lsoa) %>%
    mutate_at(vars(one_of(c("TOTAL_PAYMENTS","NET_PAYMENTS","PATIENTS","WEIGHTED_PATIENTS"))), ~(.*LSOA_PROP)) %>%
    group_by(YEAR,LSOA11CD) %>%
    summarise_at(vars(one_of(c("TOTAL_PAYMENTS","NET_PAYMENTS","PATIENTS","WEIGHTED_PATIENTS"))), sum, na.rm=TRUE)
  
  dbWriteTable(conn=db, name="gp_practice_payments_lsoa", payments_lsoa, overwrite=TRUE)
  dbDisconnect(db)
}

#################################################
#### GP patient satisfaction survey data
#################################################

import_gpps = function(database_name="primary_care_data.sqlite3"){
  gpps = read_csv("raw_data/gpps/GPPS_1516_1819.csv")
  gpps = gpps %>% filter(GPPS_GOOD>0)
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(db,"gpps",gpps, overwrite=TRUE)
  dbDisconnect(db)
}


attribute_gpps_data_to_lsoa = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gpps = tbl(db, "gpps") %>% collect()
  ads_lsoa = tbl(db, "ads_prac_props") %>% filter(YEAR>2014) %>% collect()
  # ggps data does not cover every practice in ads so we need to renormalise data aggregated at LSOAs
  missing = inner_join(gpps,ads_lsoa) %>% 
    group_by(YEAR,LSOA11CD) %>% 
    summarise(check=sum(PRAC_PROP)) %>% 
    mutate(missing_data_inflation_factor = 1/check)
  gpps_lsoa = inner_join(gpps,ads_lsoa) %>%
    inner_join(missing) %>%
    mutate_at(vars(one_of(c("GPPS_GOOD"))), ~(.*PRAC_PROP*missing_data_inflation_factor)) %>%
    group_by(YEAR,LSOA11CD) %>%
    summarise_at(vars(one_of(c("GPPS_GOOD"))), sum, na.rm=TRUE)
  
  dbWriteTable(conn=db, name="gpps_lsoa", gpps_lsoa, overwrite=TRUE)
  dbDisconnect(db)
}

######################################################
#### care quality commission ratings for GP practices
######################################################

import_cqc = function(database_name="primary_care_data.sqlite3"){
  cqc = read_csv("raw_data/cqc/CQC_1516_1819.csv")
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(db,"cqc",cqc, overwrite=TRUE)
  dbDisconnect(db)
}

#################################################
#### quality and outcomes framework data
#################################################

import_qof_data = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  dbRemoveTable(db,"gp_practice_qof",fail_if_missing=FALSE)
  
  for (y in 15:18) {
    qof = read_csv(paste0("raw_data/qof/ACHIEVEMENT_",y,y+1,".csv"))
    
    year = 2000+y
    points = qof %>% 
      filter(MEASURE=="ACHIEVED_POINTS") %>% 
      group_by(PRACTICE_CODE) %>% 
      summarise(TOTAL_POINTS=sum(VALUE)) %>%
      mutate(YEAR=year) %>%
      select(YEAR,PRAC_CODE=PRACTICE_CODE,TOTAL_POINTS)
    
    dbWriteTable(db,"gp_practice_qof",points,append=TRUE)
  }
  dbDisconnect(db)
}

attribute_qof_data_to_lsoa = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  qof = tbl(db, "gp_practice_qof") %>% collect()
  ads_lsoa = tbl(db, "ads_prac_props") %>% filter(YEAR>2014) %>% collect()
  # qof data does not cover every practice in ads so we need to renormalise data aggregated at LSOAs
  missing = inner_join(qof,ads_lsoa) %>% 
    group_by(YEAR,LSOA11CD) %>% 
    summarise(check=sum(PRAC_PROP)) %>% 
    mutate(missing_data_inflation_factor = 1/check)
  qof_lsoa = inner_join(qof,ads_lsoa) %>%
    inner_join(missing) %>%
    mutate_at(vars(one_of(c("TOTAL_POINTS"))), ~(.*PRAC_PROP*missing_data_inflation_factor)) %>%
    group_by(YEAR,LSOA11CD) %>%
    summarise_at(vars(one_of(c("TOTAL_POINTS"))), sum, na.rm=TRUE)
  
  dbWriteTable(conn=db, name="gp_practice_qof_lsoa", qof_lsoa, overwrite=TRUE)
  dbDisconnect(db)
}

#################################################
#### GP practice appointment data
#################################################
write_appointment_data = function(database_name="primary_care_data.sqlite3"){
  
  db = dbConnect(SQLite(), dbname=database_name)
  ons_codes = tbl(db, "CCG_ONS_CODE_MAPPING") %>% distinct(CCG19CD,CCG19CDH) %>% collect()
  coverage = read_csv("raw_data/appointments/Appointments_GP_Daily/APPOINTMENTS_GP_COVERAGE.csv") 
  coverage = coverage  %>%
    select(CCG19CDH=COMMISSIONER_ORGANISATION_CODE,
           DATE=Appointment_Month,
           INCLUDED=`Included Practices`,
           OPEN=`Open Practices`,
           PATIENTS_INCLUDED=`Patients registered at included practices`,
           PATIENT_OPEN=`Patients registered at open practices`) %>% 
    mutate(DATE=dmy(DATE),MONTH=month(DATE),YEAR=year(DATE)) %>%
    inner_join(ons_codes) %>%
    select(CCG19CD,YEAR,MONTH,OPEN,INCLUDED,PATIENT_OPEN,PATIENTS_INCLUDED)
  dbWriteTable(conn=db, name="appointment_data_coverage", coverage, overwrite=TRUE)
  
  dbRemoveTable(db,"appointment_data",fail_if_missing=FALSE)
  
  for (year in 18:19) {
    for (month in month(1:12,label = TRUE)) {
      filename = paste0("raw_data/appointments/Appointments_GP_Daily/CCG_CSV_",month,"_",year,".csv")
      if (file.exists(filename)){
        appts = read_csv(filename)
        appts = appts %>% select(CCG19CD=CCG_ONS_CODE, 
                                 DATE=Appointment_Date, 
                                 STATUS=APPT_STATUS,
                                 HCP=HCP_TYPE,
                                 MODE=APPT_MODE,
                                 WAIT=TIME_BETWEEN_BOOK_AND_APPT,
                                 COUNT=COUNT_OF_APPOINTMENTS) %>%
          mutate(DATE=dmy(DATE),
                 DAY=as.character(wday(DATE,label=TRUE)),
                 WEEK=week(DATE),
                 MONTH=month(DATE),
                 YEAR=year(DATE),
                 DATE=as.character(DATE))
        dbWriteTable(conn=db, name="appointment_data", appts, append=TRUE)
      }
    }
  }
  
  dbDisconnect(db)
}

#################################################
#### import political boundaries
#################################################
import_political_geography = function(database_name="primary_care_data.sqlite3"){
  lsoa_ward = read_csv("raw_data/geography/political/Lower_Layer_Super_Output_Area_2011_to_Ward_2018_Lookup_in_England_and_Wales_v3.csv")
  ward_cons = read_csv("raw_data/geography/political/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_December_2018_Lookup_in_the_United_Kingdom.csv")
  political_geography = inner_join(lsoa_ward %>% select(LSOA11CD,WD18CD),ward_cons) %>% select(-FID)
  
  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn=db, name="political_geography", political_geography, overwrite=TRUE)
  dbDisconnect(db)
}

#################################################
#### code to create online version of database
#################################################
  make_online_database = function(offline_database_name="primary_care_data.sqlite3",online_database_name="primary_care_data_online.sqlite3"){
    db = dbConnect(SQLite(), dbname=offline_database_name)
    ccg_imd_2019 = tbl(db, "ccg_imd_2019") %>% collect()
    ccg_lsoa_lad_mapping = tbl(db, "ccg_lsoa_lad_mapping") %>% collect()
    ccg_ons_code_mapping = tbl(db, "ccg_ons_code_mapping") %>% collect()
    ccg_practice_mapping = tbl(db, "ccg_practice_mapping") %>% collect()
    ccg_workforce_imputed = tbl(db, "ccg_workforce_imputed") %>% collect()
    gp_imd_2019 = tbl(db, "gp_imd_2019") %>% collect()
    gp_population = tbl(db, "gp_population") %>% collect()
    gp_workforce_imd_quintiles_imputed = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect()
    gp_workforce_newdata_imputed = tbl(db, "gp_workforce_newdata_imputed") %>% collect()
    gp_workforce_newdata_lsoa_imputed = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>% collect()
    workforce_variables = tbl(db,"workforce_variables") %>% collect()
    pcn_imd_2019 = tbl(db,"pcn_imd_2019") %>% collect()
    pcn_workforce_imputed = tbl(db,"pcn_workforce_imputed") %>% collect()
    pcn_gp_practice_mapping = tbl(db,"pcn_gp_practice_mapping") %>% collect()
    dbDisconnect(db)
    
    db2 = dbConnect(SQLite(), dbname=online_database_name)
    dbWriteTable(conn = db2, name = "ccg_imd_2019", ccg_imd_2019, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "ccg_lsoa_lad_mapping", ccg_lsoa_lad_mapping, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "ccg_ons_code_mapping", ccg_ons_code_mapping, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "ccg_practice_mapping", ccg_practice_mapping, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "ccg_workforce_imputed", ccg_workforce_imputed, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "gp_imd_2019", gp_imd_2019, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "gp_population", gp_population, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "gp_workforce_imd_quintiles_imputed", gp_workforce_imd_quintiles_imputed, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "gp_workforce_newdata_imputed", gp_workforce_newdata_imputed, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "gp_workforce_newdata_lsoa_imputed", gp_workforce_newdata_lsoa_imputed, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "workforce_variables", workforce_variables, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "pcn_imd_2019", pcn_imd_2019, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "pcn_workforce_imputed", pcn_workforce_imputed, overwrite=TRUE)
    dbWriteTable(conn = db2, name = "pcn_gp_practice_mapping", pcn_gp_practice_mapping, overwrite=TRUE)
    dbDisconnect(db2)
  }


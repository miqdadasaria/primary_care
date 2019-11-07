library("tidyverse")
library("readxl")
library("RSQLite")
library("fingertipsR")
library("scales")
library("purrr")
library("modelr")
library("ggpubr")

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
  gp_vars=c("TOTAL_GP_HC","TOTAL_GP_EXL_HC","TOTAL_GP_EXRL_HC","TOTAL_GP_EXRRL_HC","TOTAL_GP_SEN_PTNR_HC","TOTAL_GP_PTNR_PROV_HC","TOTAL_GP_SAL_BY_PRAC_HC","TOTAL_GP_SAL_BY_OTH_HC","TOTAL_GP_REG_ST3_4_HC","TOTAL_GP_REG_F1_2_HC","TOTAL_GP_RET_HC","TOTAL_GP_LOCUM_VAC_HC","TOTAL_GP_LOCUM_ABS_HC","TOTAL_GP_LOCUM_OTH_HC",
            "MALE_GP_EXRRL_HC","MALE_GP_REG_ST3_4_HC","MALE_GP_RET_HC","FEMALE_GP_EXRRL_HC","FEMALE_GP_REG_ST3_4_HC","FEMALE_GP_RET_HC",
            "TOTAL_GP_FTE","TOTAL_GP_EXL_FTE","TOTAL_GP_EXRL_FTE","TOTAL_GP_EXRRL_FTE","TOTAL_GP_SEN_PTNR_FTE","TOTAL_GP_PTNR_PROV_FTE","TOTAL_GP_SAL_BY_PRAC_FTE","TOTAL_GP_SAL_BY_OTH_FTE","TOTAL_GP_REG_ST3_4_FTE","TOTAL_GP_REG_F1_2_FTE","TOTAL_GP_RET_FTE","TOTAL_GP_LOCUM_VAC_FTE","TOTAL_GP_LOCUM_ABS_FTE","TOTAL_GP_LOCUM_OTH_FTE",
            "MALE_GP_EXRRL_FTE","MALE_GP_REG_ST3_4_FTE","MALE_GP_RET_FTE","FEMALE_GP_EXRRL_FTE","FEMALE_GP_REG_ST3_4_FTE","FEMALE_GP_RET_FTE",
            "MALE_GP_EXRRL_HC_UNDER30","MALE_GP_EXRRL_HC_30TO34","MALE_GP_EXRRL_HC_35TO39","MALE_GP_EXRRL_HC_40TO44","MALE_GP_EXRRL_HC_45TO49","MALE_GP_EXRRL_HC_50TO54","MALE_GP_EXRRL_HC_55TO59","MALE_GP_EXRRL_HC_60TO64","MALE_GP_EXRRL_HC_65TO69","MALE_GP_EXRRL_HC_70PLUS","MALE_GP_EXRRL_HC_UNKNOWN_AGE",
            "FEMALE_GP_EXRRL_HC_UNDER30","FEMALE_GP_EXRRL_HC_30TO34","FEMALE_GP_EXRRL_HC_35TO39","FEMALE_GP_EXRRL_HC_40TO44","FEMALE_GP_EXRRL_HC_45TO49","FEMALE_GP_EXRRL_HC_50TO54","FEMALE_GP_EXRRL_HC_55TO59","FEMALE_GP_EXRRL_HC_60TO64","FEMALE_GP_EXRRL_HC_65TO69","FEMALE_GP_EXRRL_HC_70PLUS","FEMALE_GP_EXRRL_HC_UNKNOWN_AGE",
            "TOTAL_NURSES_HC","TOTAL_N_PRAC_NURSE_HC","TOTAL_N_ADV_NURSE_PRAC_HC","TOTAL_N_NURSE_SPEC_HC","TOTAL_N_EXT_ROLE_NURSE_HC","TOTAL_N_TRAINEE_NURSE_HC","TOTAL_N_DISTRICT_NURSE_HC","TOTAL_N_NURSE_DISP_HC",
            "TOTAL_NURSES_FTE","TOTAL_N_PRAC_NURSE_FTE","TOTAL_N_ADV_NURSE_PRAC_FTE","TOTAL_N_NURSE_SPEC_FTE","TOTAL_N_EXT_ROLE_NURSE_FTE","TOTAL_N_TRAINEE_NURSE_FTE","TOTAL_N_DISTRICT_NURSE_FTE","TOTAL_N_NURSE_DISP_FTE",
            "TOTAL_DPC_HC","TOTAL_DPC_DISPENSER_HC","TOTAL_DPC_HCA_HC","TOTAL_DPC_PHLEB_HC","TOTAL_DPC_PHARMA_HC","TOTAL_DPC_PHYSIO_HC","TOTAL_DPC_PODIA_HC","TOTAL_DPC_PHYSICIAN_ASSOC_HC","TOTAL_DPC_OTH_HC",
            "TOTAL_DPC_FTE","TOTAL_DPC_DISPENSER_FTE","TOTAL_DPC_HCA_FTE","TOTAL_DPC_PHLEB_FTE","TOTAL_DPC_PHARMA_FTE","TOTAL_DPC_PHYSIO_FTE","TOTAL_DPC_PODIA_FTE","TOTAL_DPC_PHYSICIAN_ASSOC_FTE","TOTAL_DPC_OTH_FTE",
            "TOTAL_ADMIN_HC","TOTAL_ADMIN_MANAGER_HC","TOTAL_ADMIN_MED_SECRETARY_HC","TOTAL_ADMIN_RECEPT_HC","TOTAL_ADMIN_TELEPH_HC","TOTAL_ADMIN_ESTATES_ANC_HC","TOTAL_ADMIN_OTH_HC",
            "TOTAL_ADMIN_FTE","TOTAL_ADMIN_MANAGER_FTE","TOTAL_ADMIN_MED_SECRETARY_FTE","TOTAL_ADMIN_RECEPT_FTE","TOTAL_ADMIN_TELEPH_FTE","TOTAL_ADMIN_ESTATES_ANC_FTE","TOTAL_ADMIN_OTH_FTE")
  
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
write_workforce_data_to_db_new = function(database_name="primary_care_data.sqlite3"){
  gp_vars=c("TOTAL_GP_HC","TOTAL_GP_EXL_HC","TOTAL_GP_EXRL_HC","TOTAL_GP_EXRRL_HC","TOTAL_GP_SEN_PTNR_HC","TOTAL_GP_PTNR_PROV_HC","TOTAL_GP_SAL_BY_PRAC_HC","TOTAL_GP_SAL_BY_OTH_HC","TOTAL_GP_REG_ST3_4_HC","TOTAL_GP_REG_F1_2_HC","TOTAL_GP_RET_HC","TOTAL_GP_LOCUM_VAC_HC","TOTAL_GP_LOCUM_ABS_HC","TOTAL_GP_LOCUM_OTH_HC",
          "MALE_GP_EXRRL_HC","MALE_GP_REG_ST3_4_HC","MALE_GP_RET_HC","FEMALE_GP_EXRRL_HC","FEMALE_GP_REG_ST3_4_HC","FEMALE_GP_RET_HC",
          "TOTAL_GP_FTE","TOTAL_GP_EXL_FTE","TOTAL_GP_EXRL_FTE","TOTAL_GP_EXRRL_FTE","TOTAL_GP_SEN_PTNR_FTE","TOTAL_GP_PTNR_PROV_FTE","TOTAL_GP_SAL_BY_PRAC_FTE","TOTAL_GP_SAL_BY_OTH_FTE","TOTAL_GP_REG_ST3_4_FTE","TOTAL_GP_REG_F1_2_FTE","TOTAL_GP_RET_FTE","TOTAL_GP_LOCUM_VAC_FTE","TOTAL_GP_LOCUM_ABS_FTE","TOTAL_GP_LOCUM_OTH_FTE",
          "MALE_GP_EXRRL_FTE","MALE_GP_REG_ST3_4_FTE","MALE_GP_RET_FTE","FEMALE_GP_EXRRL_FTE","FEMALE_GP_REG_ST3_4_FTE","FEMALE_GP_RET_FTE",
          "MALE_GP_EXRRL_HC_UNDER30","MALE_GP_EXRRL_HC_30TO34","MALE_GP_EXRRL_HC_35TO39","MALE_GP_EXRRL_HC_40TO44","MALE_GP_EXRRL_HC_45TO49","MALE_GP_EXRRL_HC_50TO54","MALE_GP_EXRRL_HC_55TO59","MALE_GP_EXRRL_HC_60TO64","MALE_GP_EXRRL_HC_65TO69","MALE_GP_EXRRL_HC_70PLUS","MALE_GP_EXRRL_HC_UNKNOWN_AGE",
          "FEMALE_GP_EXRRL_HC_UNDER30","FEMALE_GP_EXRRL_HC_30TO34","FEMALE_GP_EXRRL_HC_35TO39","FEMALE_GP_EXRRL_HC_40TO44","FEMALE_GP_EXRRL_HC_45TO49","FEMALE_GP_EXRRL_HC_50TO54","FEMALE_GP_EXRRL_HC_55TO59","FEMALE_GP_EXRRL_HC_60TO64","FEMALE_GP_EXRRL_HC_65TO69","FEMALE_GP_EXRRL_HC_70PLUS","FEMALE_GP_EXRRL_HC_UNKNOWN_AGE",
          "TOTAL_NURSES_HC","TOTAL_N_PRAC_NURSE_HC","TOTAL_N_ADV_NURSE_PRAC_HC","TOTAL_N_NURSE_SPEC_HC","TOTAL_N_EXT_ROLE_NURSE_HC","TOTAL_N_TRAINEE_NURSE_HC","TOTAL_N_DISTRICT_NURSE_HC","TOTAL_N_NURSE_DISP_HC",
          "TOTAL_NURSES_FTE","TOTAL_N_PRAC_NURSE_FTE","TOTAL_N_ADV_NURSE_PRAC_FTE","TOTAL_N_NURSE_SPEC_FTE","TOTAL_N_EXT_ROLE_NURSE_FTE","TOTAL_N_TRAINEE_NURSE_FTE","TOTAL_N_DISTRICT_NURSE_FTE","TOTAL_N_NURSE_DISP_FTE",
          "TOTAL_DPC_HC","TOTAL_DPC_DISPENSER_HC","TOTAL_DPC_HCA_HC","TOTAL_DPC_PHLEB_HC","TOTAL_DPC_PHARMA_HC","TOTAL_DPC_PHYSIO_HC","TOTAL_DPC_PODIA_HC","TOTAL_DPC_PHYSICIAN_ASSOC_HC","TOTAL_DPC_OTH_HC",
          "TOTAL_DPC_FTE","TOTAL_DPC_DISPENSER_FTE","TOTAL_DPC_HCA_FTE","TOTAL_DPC_PHLEB_FTE","TOTAL_DPC_PHARMA_FTE","TOTAL_DPC_PHYSIO_FTE","TOTAL_DPC_PODIA_FTE","TOTAL_DPC_PHYSICIAN_ASSOC_FTE","TOTAL_DPC_OTH_FTE",
          "TOTAL_ADMIN_HC","TOTAL_ADMIN_MANAGER_HC","TOTAL_ADMIN_MED_SECRETARY_HC","TOTAL_ADMIN_RECEPT_HC","TOTAL_ADMIN_TELEPH_HC","TOTAL_ADMIN_ESTATES_ANC_HC","TOTAL_ADMIN_OTH_HC",
          "TOTAL_ADMIN_FTE","TOTAL_ADMIN_MANAGER_FTE","TOTAL_ADMIN_MED_SECRETARY_FTE","TOTAL_ADMIN_RECEPT_FTE","TOTAL_ADMIN_TELEPH_FTE","TOTAL_ADMIN_ESTATES_ANC_FTE","TOTAL_ADMIN_OTH_FTE")

  gp_year = list()
  for (year in 2015:2018) {
    wf = read_csv(paste0("raw_data/workforce/GPs 2015-2018/General Practice September ",year," Practice Level.csv"),
                  guess_max = 100000,
                  na=c("","NA","EST","NS",".","ND"))
    wf = wf %>% mutate(YEAR=year)
    
    gp_year[[as.character(year)]] = wf %>%
      select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS, gp_vars)
  }
  
  workforce = map_dfr(gp_year,bind_rows) %>% 
    arrange(YEAR,PRAC_CODE)

  db = dbConnect(SQLite(), dbname=database_name)
  dbWriteTable(conn=db, name="gp_workforce_newdata", workforce, overwrite=TRUE)
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


calculate_missing_gp_data_per_var = function(gps, target_data, varname){
  results = gps %>% 
    group_by(YEAR) %>% 
    summarise(TOTAL=sum(get(varname),na.rm=TRUE)) %>%
    inner_join(target_data %>% select(YEAR,paste0(varname,"_TARGET"))) %>%
    mutate(DIFF = get(paste0(varname,"_TARGET"))-TOTAL,
           INFLATE_NON_MISSING = get(paste0(varname,"_TARGET"))/TOTAL) %>%
    inner_join(gps %>% group_by(YEAR) %>% summarise(MISSING_N=sum(is.na(get(varname)) | get(varname)==0), NON_MISSING_N=n()-MISSING_N)) %>%
    mutate(IMPUTE_MISSING = DIFF/MISSING_N) %>%
    select(YEAR,INFLATE_NON_MISSING,IMPUTE_MISSING)
  
  return(results)
}

impute_missing_gp_data = function(gps, mode){
  target_data = tribble(
    ~YEAR, ~TOTAL_GP_HC_TARGET, ~TOTAL_GP_FTE_TARGET, ~TOTAL_GP_EXRRL_HC_TARGET, ~TOTAL_GP_EXRRL_FTE_TARGET,
    2015, 39988, 33675, 34873, 28631,
    2016, 40464, 34323, 34686, 28592,
    2017, 39871, 33437, 34293, 27928,
    2018, 40196, 33327, 34213, 27447
  )
  gps_imp = gps
  for(varname in c("TOTAL_GP_FTE","TOTAL_GP_EXRRL_FTE")){
    impute = calculate_missing_gp_data_per_var(gps, target_data, varname)
    if(mode==1){
      print("mode: 1")
      gps_imp = gps_imp %>% left_join(impute) %>%
        mutate(!!varname := if_else(is.na(get(varname)) | get(varname)==0,IMPUTE_MISSING,get(varname))) %>%
        select(-INFLATE_NON_MISSING,-IMPUTE_MISSING)
    } else if(mode==2){
      print("mode: 2")
      gps_imp = gps_imp %>% left_join(impute) %>%
        mutate(!!varname := if_else(!is.na(INFLATE_NON_MISSING), get(varname) * INFLATE_NON_MISSING, get(varname))) %>% 
        select(-INFLATE_NON_MISSING,-IMPUTE_MISSING)
    } else {
      print("mode: 0")
      gps_imp = gps_imp
    }
  }
  return(gps_imp)
}

calculate_gp_per_100k_trend_by_imd_lsoa_level = function(database_name="primary_care_data.sqlite3", imd_aggregated=TRUE, start_year=2004, end_year=2014, imputation_mode=0){
  db = dbConnect(SQLite(), dbname=database_name)
  gps = tbl(db, "gp_workforce") %>% 
    select(YEAR,PRAC_CODE,TOTAL_GP_EXRRL_FTE, TOTAL_GP_FTE) %>%
    collect()
  
  gps = impute_missing_gp_data(gps,imputation_mode)
  
  if(imd_aggregated){
    ads_imd = tbl(db, "ads_imd_props") %>% collect()
    
    ads_quintile = add_quintiles(ads_imd) %>%
      group_by(YEAR,PRAC_CODE,IMD_QUINTILE) %>%
      summarise(QUINTILE_PROP=sum(DECILE_PROP))
    
    # if no ads records split GPs equally across quintiles
    missing_ads = left_join(gps,ads_quintile) %>% 
      filter(is.na(IMD_QUINTILE)) %>% 
      select(-IMD_QUINTILE,-QUINTILE_PROP) %>%
      group_by(YEAR) %>%
      summarise(MISSING_GP_FTE=sum(TOTAL_GP_FTE,na.rm=TRUE)/5,
                MISSING_GP_EXRRL_FTE=sum(TOTAL_GP_EXRRL_FTE,na.rm=TRUE)/5)
      
    gps_imd_quintile = inner_join(gps,ads_quintile) %>%
      mutate(TOTAL_GP_EXRRL_FTE = QUINTILE_PROP*TOTAL_GP_EXRRL_FTE,
             TOTAL_GP_FTE = QUINTILE_PROP*TOTAL_GP_FTE) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(TOTAL_GP_EXRRL_FTE = sum(TOTAL_GP_EXRRL_FTE,na.rm=TRUE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE,na.rm=TRUE)) %>%
      left_join(missing_ads) %>%
      mutate(TOTAL_GP_FTE=TOTAL_GP_FTE+MISSING_GP_FTE,
             TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRRL_FTE+MISSING_GP_EXRRL_FTE) %>%
      select(-starts_with("MISSING"))
    
    adj_pop = tbl(db, "adj_pop_decile") %>% collect()
    adj_pop_quintile = add_quintiles(adj_pop) %>% 
      group_by(YEAR, IMD_QUINTILE) %>%
      summarise(NEED_ADJ_POP=sum(NEED_ADJ_POP), TOTAL_POP=sum(TOTAL_POP)) 
    
    gps_quintile = inner_join(gps_imd_quintile,adj_pop_quintile) %>%
      ungroup()
  
  } else {
    ads_lsoa = tbl(db, "ads_lsoa_props") %>% collect()
    
    adj_pop_lsoa = tbl(db, "carr_hill_pop") %>% collect()
    
    imd = tbl(db, "imd_2015") %>% 
      filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
      select(LSOA11CD,IMD_DECILE=VALUE) %>%
      collect() %>% add_quintiles()
    
    gps_lsoa = inner_join(gps,ads_lsoa) %>%
      mutate(TOTAL_GP_EXRRL_FTE = LSOA_PROP*TOTAL_GP_EXRRL_FTE,
             TOTAL_GP_FTE = LSOA_PROP*TOTAL_GP_FTE) %>%
      group_by(YEAR,LSOA11CD) %>%
      summarise(TOTAL_GP_EXRRL_FTE = sum(TOTAL_GP_EXRRL_FTE,na.rm=FALSE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE,na.rm=FALSE)) %>%
      inner_join(adj_pop_lsoa) %>%
      inner_join(imd)
   
    # if no ads records split GPs equally across quintiles
    missing_lsoa = left_join(gps,ads_lsoa) %>% 
      filter(is.na(LSOA11CD)) %>% 
      select(-starts_with("LSOA")) %>%
      group_by(YEAR) %>%
      summarise(MISSING_GP_FTE=sum(TOTAL_GP_FTE,na.rm=TRUE)/5,
                MISSING_GP_EXRRL_FTE=sum(TOTAL_GP_EXRRL_FTE,na.rm=TRUE)/5)
    
    gps_quintile = gps_lsoa %>% 
      drop_na(TOTAL_GP_EXRRL_FTE,TOTAL_GP_FTE,NEED_ADJ_POP,TOTAL_POP,IMD_QUINTILE) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(TOTAL_GP_EXRRL_FTE = sum(TOTAL_GP_EXRRL_FTE),
                TOTAL_GP_FTE = sum(TOTAL_GP_FTE),
                NEED_ADJ_POP = sum(NEED_ADJ_POP),
                TOTAL_POP = sum(TOTAL_POP)) %>%
      ungroup() %>%
      inner_join(missing_lsoa) %>%
      mutate(TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRRL_FTE+MISSING_GP_EXRRL_FTE,
             TOTAL_GP_FTE=TOTAL_GP_FTE+MISSING_GP_FTE) %>% 
      select(-starts_with("MISSING"))
  }
  
  graph_data = gps_quintile %>%
    filter(YEAR %in% start_year:end_year) %>%
    mutate(GPS_PER_100K=round(100000*TOTAL_GP_FTE/TOTAL_POP,1),
           GPS_PER_100K_ADJ=round(100000*TOTAL_GP_FTE/NEED_ADJ_POP,1),
           GPS_EXRRL_PER_100K=round(100000*TOTAL_GP_EXRRL_FTE/TOTAL_POP,1),
           GPS_EXRRL_PER_100K_ADJ=round(100000*TOTAL_GP_EXRRL_FTE/NEED_ADJ_POP,1),
           YEAR=as.integer(YEAR)) %>%
    mutate(IMD_QUINTILE=as.factor(IMD_QUINTILE)) %>%
    gather(VAR,VAL,
           TOTAL_GP_FTE,
           TOTAL_GP_EXRRL_FTE,
           TOTAL_POP,
           NEED_ADJ_POP,
           GPS_PER_100K,
           GPS_PER_100K_ADJ,
           GPS_EXRRL_PER_100K,
           GPS_EXRRL_PER_100K_ADJ) %>%
    mutate(VAR=factor(VAR,c("TOTAL_POP",
                            "NEED_ADJ_POP",
                            "TOTAL_GP_FTE",
                            "TOTAL_GP_EXRRL_FTE",
                            "GPS_PER_100K",
                            "GPS_PER_100K_ADJ",
                            "GPS_EXRRL_PER_100K",
                            "GPS_EXRRL_PER_100K_ADJ"),
                      c("Population",
                        "Need adjusted Population",
                        "Total GPs (FTE)",
                        "Total GPs (FTE excl. RRL)",
                        "GPs (FTE) per 100k population unadjusted",
                        "GPs (FTE) per 100k population need adjusted",
                        "GPs (FTE excl. RRL) per 100k population unadjusted",
                        "GPs (FTE excl. RRL) per 100k population need adjusted")))
  
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  gp_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data before dashed line from DH using 2001 LSOA neighbourhoods mapped to 2011 LSOAs after the dashed line are from NHS Digital based on LSOA 2011 neighbourhoods")
  if (2013 %in% start_year:end_year) {
    gp_plot = gp_plot + geom_vline(xintercept=c(2012.5), linetype="dashed", colour="lightgrey")
  }
  
    if (imd_aggregated) {
     aggregation_unit = "IMD_QUINTILE"
    } else {
     aggregation_unit = "LSOA"
    }
  
  ggsave(paste0("figures/gp_trends_",aggregation_unit,"_",start_year,"_",end_year,"_",imputation_mode,".png"), gp_plot, width=35, height=35, units="cm", dpi="print")
  
  dbDisconnect(db)
}


attribute_workforce_to_imd_lsoa_level = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_vars=c("TOTAL_GP_HC","TOTAL_GP_EXL_HC","TOTAL_GP_EXRL_HC","TOTAL_GP_EXRRL_HC","TOTAL_GP_SEN_PTNR_HC","TOTAL_GP_PTNR_PROV_HC","TOTAL_GP_SAL_BY_PRAC_HC","TOTAL_GP_SAL_BY_OTH_HC","TOTAL_GP_REG_ST3_4_HC","TOTAL_GP_REG_F1_2_HC","TOTAL_GP_RET_HC","TOTAL_GP_LOCUM_VAC_HC","TOTAL_GP_LOCUM_ABS_HC","TOTAL_GP_LOCUM_OTH_HC",
            "MALE_GP_EXRRL_HC","MALE_GP_REG_ST3_4_HC","MALE_GP_RET_HC","FEMALE_GP_EXRRL_HC","FEMALE_GP_REG_ST3_4_HC","FEMALE_GP_RET_HC",
            "TOTAL_GP_FTE","TOTAL_GP_EXL_FTE","TOTAL_GP_EXRL_FTE","TOTAL_GP_EXRRL_FTE","TOTAL_GP_SEN_PTNR_FTE","TOTAL_GP_PTNR_PROV_FTE","TOTAL_GP_SAL_BY_PRAC_FTE","TOTAL_GP_SAL_BY_OTH_FTE","TOTAL_GP_REG_ST3_4_FTE","TOTAL_GP_REG_F1_2_FTE","TOTAL_GP_RET_FTE","TOTAL_GP_LOCUM_VAC_FTE","TOTAL_GP_LOCUM_ABS_FTE","TOTAL_GP_LOCUM_OTH_FTE",
            "MALE_GP_EXRRL_FTE","MALE_GP_REG_ST3_4_FTE","MALE_GP_RET_FTE","FEMALE_GP_EXRRL_FTE","FEMALE_GP_REG_ST3_4_FTE","FEMALE_GP_RET_FTE",
            "MALE_GP_EXRRL_HC_UNDER30","MALE_GP_EXRRL_HC_30TO34","MALE_GP_EXRRL_HC_35TO39","MALE_GP_EXRRL_HC_40TO44","MALE_GP_EXRRL_HC_45TO49","MALE_GP_EXRRL_HC_50TO54","MALE_GP_EXRRL_HC_55TO59","MALE_GP_EXRRL_HC_60TO64","MALE_GP_EXRRL_HC_65TO69","MALE_GP_EXRRL_HC_70PLUS","MALE_GP_EXRRL_HC_UNKNOWN_AGE",
            "FEMALE_GP_EXRRL_HC_UNDER30","FEMALE_GP_EXRRL_HC_30TO34","FEMALE_GP_EXRRL_HC_35TO39","FEMALE_GP_EXRRL_HC_40TO44","FEMALE_GP_EXRRL_HC_45TO49","FEMALE_GP_EXRRL_HC_50TO54","FEMALE_GP_EXRRL_HC_55TO59","FEMALE_GP_EXRRL_HC_60TO64","FEMALE_GP_EXRRL_HC_65TO69","FEMALE_GP_EXRRL_HC_70PLUS","FEMALE_GP_EXRRL_HC_UNKNOWN_AGE",
            "TOTAL_NURSES_HC","TOTAL_N_PRAC_NURSE_HC","TOTAL_N_ADV_NURSE_PRAC_HC","TOTAL_N_NURSE_SPEC_HC","TOTAL_N_EXT_ROLE_NURSE_HC","TOTAL_N_TRAINEE_NURSE_HC","TOTAL_N_DISTRICT_NURSE_HC","TOTAL_N_NURSE_DISP_HC",
            "TOTAL_NURSES_FTE","TOTAL_N_PRAC_NURSE_FTE","TOTAL_N_ADV_NURSE_PRAC_FTE","TOTAL_N_NURSE_SPEC_FTE","TOTAL_N_EXT_ROLE_NURSE_FTE","TOTAL_N_TRAINEE_NURSE_FTE","TOTAL_N_DISTRICT_NURSE_FTE","TOTAL_N_NURSE_DISP_FTE",
            "TOTAL_DPC_HC","TOTAL_DPC_DISPENSER_HC","TOTAL_DPC_HCA_HC","TOTAL_DPC_PHLEB_HC","TOTAL_DPC_PHARMA_HC","TOTAL_DPC_PHYSIO_HC","TOTAL_DPC_PODIA_HC","TOTAL_DPC_PHYSICIAN_ASSOC_HC","TOTAL_DPC_OTH_HC",
            "TOTAL_DPC_FTE","TOTAL_DPC_DISPENSER_FTE","TOTAL_DPC_HCA_FTE","TOTAL_DPC_PHLEB_FTE","TOTAL_DPC_PHARMA_FTE","TOTAL_DPC_PHYSIO_FTE","TOTAL_DPC_PODIA_FTE","TOTAL_DPC_PHYSICIAN_ASSOC_FTE","TOTAL_DPC_OTH_FTE",
            "TOTAL_ADMIN_HC","TOTAL_ADMIN_MANAGER_HC","TOTAL_ADMIN_MED_SECRETARY_HC","TOTAL_ADMIN_RECEPT_HC","TOTAL_ADMIN_TELEPH_HC","TOTAL_ADMIN_ESTATES_ANC_HC","TOTAL_ADMIN_OTH_HC",
            "TOTAL_ADMIN_FTE","TOTAL_ADMIN_MANAGER_FTE","TOTAL_ADMIN_MED_SECRETARY_FTE","TOTAL_ADMIN_RECEPT_FTE","TOTAL_ADMIN_TELEPH_FTE","TOTAL_ADMIN_ESTATES_ANC_FTE","TOTAL_ADMIN_OTH_FTE")
  
  gps = tbl(db, "gp_workforce_newdata_imputed") %>% 
    select(YEAR,PRAC_CODE,workforce_vars) %>%
    collect()
  
    ads_lsoa = tbl(db, "ads_lsoa_props") %>% filter(YEAR>2014) %>% collect()
    
    adj_pop_lsoa = tbl(db, "carr_hill_pop") %>% filter(YEAR>2014) %>% collect()
    
    imd = tbl(db, "imd_2015") %>% 
      filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
      select(LSOA11CD,IMD_DECILE=VALUE) %>%
      collect() %>% add_quintiles()
    
    # drops LSOAs in Wales and NO2011 which seems to be some NHS digital null code
    gps_lsoa = inner_join(gps,ads_lsoa) %>%
      mutate_at(vars(one_of(workforce_vars)), ~(.*LSOA_PROP)) %>%
      group_by(YEAR,LSOA11CD) %>%
      summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) %>%
      inner_join(adj_pop_lsoa) %>%
      inner_join(imd)
    
    dbWriteTable(conn=db, name="gp_workforce_newdata_imputed_lsoa", gps_lsoa, overwrite=TRUE)
    
    gp_imd = tbl(db, "gp_imd_2015") %>% collect() %>% add_quintiles() %>% select(PRAC_CODE,IMD_QUINTILE)
    
    # some practices do not have LSOAs associated with them in ADS data so: 
    # (a) for those that have practice IMD use that 
    # (b) for those that do not have practive IMD split GPs equally across quintiles
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
    
    dbWriteTable(conn=db, name="gp_workforce_imd_quintiles", gps_quintile, overwrite=TRUE)
    dbDisconnect(db)
}

graph_population_trends = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  population = tbl(db, "gp_workforce_imd_quintiles") %>% select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
  dbDisconnect(db)
  
  graph_data = population %>% gather(POP_TYPE,POP,contains("POP")) %>%
    mutate(POP_TYPE = factor(POP_TYPE,c("TOTAL_POP","NEED_ADJ_POP"),c("Total Population","Need Adjusted Population")),
           IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  pop_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=POP, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    facet_wrap(POP_TYPE~.,scales="fixed",labeller = labeller(POP_TYPE = label_wrap_gen(40))) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/pop_trends.png"), pop_plot, width=25, height=10, units="cm", dpi="print")
  
}

graph_gp_trends = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
  dbDisconnect(db)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,
           TOTAL_GP_HC,
           TOTAL_GPEXRRL_HC=TOTAL_GP_EXRRL_HC,TOTAL_GPREGST34_HC=TOTAL_GP_REG_ST3_4_HC,
           TOTAL_GPRET_HC = TOTAL_GP_RET_HC,
           TOTAL_GP_LOCUM_ABS_HC,TOTAL_GP_LOCUM_VAC_HC,TOTAL_GP_LOCUM_OTH_HC,
           TOTAL_GP_FTE,
           TOTAL_GPEXRRL_FTE=TOTAL_GP_EXRRL_FTE,TOTAL_GPREGST34_FTE=TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GPRET_FTE = TOTAL_GP_RET_FTE,
           TOTAL_GP_LOCUM_ABS_FTE,TOTAL_GP_LOCUM_VAC_FTE,TOTAL_GP_LOCUM_OTH_FTE) %>% 
    mutate(TOTAL_GPLOCUM_HC=TOTAL_GP_LOCUM_ABS_HC+TOTAL_GP_LOCUM_VAC_HC+TOTAL_GP_LOCUM_OTH_HC,
              TOTAL_GPLOCUM_FTE=TOTAL_GP_LOCUM_ABS_FTE+TOTAL_GP_LOCUM_VAC_FTE+TOTAL_GP_LOCUM_OTH_FTE) %>%
    select(-contains("GP_LOCUM")) %>%
    gather(VARIABLE,VALUE,TOTAL_GP_HC:TOTAL_GPLOCUM_FTE) %>%
    separate(VARIABLE,c("x","VARIABLE","TYPE")) %>%
    select(-x) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GP","GPEXRRL","GPREGST34","GPRET","GPLOCUM"),
                             c("All GPs","GPs excluding registrars, retainers and locums","GP registrars (ST3-4)","GP retainers","GP locums")))
   
    
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  gp_plot_raw = ggplot(graph_data) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
    ggsave(paste0("figures/gp_trends_raw.png"), gp_plot_raw, width=25, height=35, units="cm", dpi="print")
  
    gp_plot_pop = ggplot(graph_data) + 
      aes(x=YEAR, y=(100000*VALUE/TOTAL_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
      geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
      geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
      xlab("Year") +
      ylab("") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
      scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
      scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
      scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
      scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
      facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            text=element_text(family = "Roboto", colour = "#3e3f3a"),
            legend.position = "bottom") +
      labs(title = "Trends in GP supply per 100,000 population by neighbourhood deprivation",
           subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
           caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
    
    ggsave(paste0("figures/gp_trends_pop.png"), gp_plot_pop, width=25, height=35, units="cm", dpi="print")
    
    gp_plot_pop_adj = ggplot(graph_data) + 
      aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
      geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
      geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
      xlab("Year") +
      ylab("") +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
      scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
      scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
      scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
      scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
      facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
      theme_bw() + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            plot.margin = unit(c(1, 1, 1, 1), "lines"),
            text=element_text(family = "Roboto", colour = "#3e3f3a"),
            legend.position = "bottom") +
      labs(title = "Trends in GP supply per 100,000 need adjusted population by neighbourhood deprivation",
           subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
           caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
    
    ggsave(paste0("figures/gp_trends_pop_adj.png"), gp_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

graph_gp_age_sex = function(database_name="primary_care_data.sqlite3", year=2018){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() %>% filter(YEAR==year) %>% select(IMD_QUINTILE,NEED_ADJ_POP,TOTAL_POP,contains("_HC_"))
  dbDisconnect(db)
  
  graph_data = gps_quintile %>% gather(VARNAME,VALUE,MALE_GP_EXRRL_HC_UNDER30:FEMALE_GP_EXRRL_HC_UNKNOWN_AGE) %>%
    separate(VARNAME,c("SEX","A","B","C","AGE")) %>%
    select(IMD_QUINTILE, NEED_ADJ_POP,TOTAL_POP,SEX,AGE,GP_EXRRL_HC=VALUE) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           AGE = factor(AGE,
                        c("UNDER30","30TO34","35TO39","40TO44","45TO49","50TO54","55TO59","60TO64","65TO69","70PLUS","UNKNOWN"),
                        c("<30","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+","Unknown")),
           SEX = factor(SEX,c("FEMALE","MALE"),c("Female","Male")))
  
  gp_plot = ggplot(graph_data, aes(x=AGE, y=GP_EXRRL_HC)) +
    geom_bar(stat="identity") +
    facet_grid(SEX ~ IMD_QUINTILE) +
    theme_bw() + 
    ylab("GPs excluding registrars, retainers and locums (headcount)") +
    xlab("Age of GPs") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Age/Sex distribution of GP supply by neighbourhood deprivation quintile",
         subtitle = paste0("Data for England in years ",year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_",year,".png"), gp_plot, width=35, height=25, units="cm", dpi="print")
  
  gp_plot_pop = ggplot(graph_data, aes(x=AGE, y=(100000*GP_EXRRL_HC/TOTAL_POP))) +
    geom_bar(stat="identity") +
    facet_grid(SEX ~ IMD_QUINTILE) +
    theme_bw() + 
    ylab("GPs excluding registrars, retainers and locums (headcount per 100,000 population)") +
    xlab("Age of GPs") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Age/Sex distribution of GP supply per 100,000 population by neighbourhood deprivation quintile",
         subtitle = paste0("Data for England in years ",year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_pop_",year,".png"), gp_plot_pop, width=35, height=25, units="cm", dpi="print")
  
  gp_plot_pop_adj = ggplot(graph_data, aes(x=AGE, y=(100000*GP_EXRRL_HC/NEED_ADJ_POP))) +
    geom_bar(stat="identity") +
    facet_grid(SEX ~ IMD_QUINTILE) +
    theme_bw() + 
    ylab("GPs excluding registrars, retainers and locums (headcount per 100,000 need adjusted population)") +
    xlab("Age of GPs") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Age/Sex distribution of GP supply per 100,000 need adjusted population by neighbourhood deprivation quintile",
         subtitle = paste0("Data for England in years ",year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_pop_adj_",year,".png"), gp_plot_pop_adj, width=35, height=25, units="cm", dpi="print")
}

graph_gp_trends_by_sex = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
  dbDisconnect(db)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,
           contains("MALE")) %>% 
    select(-contains("_HC_")) %>%
    gather(VARIABLE,VALUE,MALE_GP_EXRRL_HC:FEMALE_GP_RET_FTE) %>%
    mutate(VARIABLE = gsub("REG_ST3_4","REGST34",VARIABLE),
            VARIABLE = gsub("_GP_","_",VARIABLE)) %>%
    separate(VARIABLE,c("SEX","VARIABLE","TYPE")) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           SEX = factor(SEX,c("FEMALE","MALE"),c("Female","Male")),
           VARIABLE = factor(VARIABLE,
                             c("EXRRL","REGST34","RET"),
                             c("GPs excluding registrars, retainers and locums","GP registrars (ST3-4)","GP retainers")))
  
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  gp_plot_sex_hc_raw = ggplot(graph_data %>% filter(TYPE=="Headcount")) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP headcount") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_raw.png"), gp_plot_sex_hc_raw, width=25, height=30, units="cm", dpi="print")
  

  gp_plot_sex_fte_raw = ggplot(graph_data %>% filter(TYPE=="Full time equivalent")) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP full time equivalent") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_raw.png"), gp_plot_sex_fte_raw, width=25, height=30, units="cm", dpi="print")

  
  gp_plot_sex_hc_pop = ggplot(graph_data %>% filter(TYPE=="Headcount")) + 
    aes(x=YEAR, y=(100000*VALUE/TOTAL_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP headcount per 100,000 population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_pop.png"), gp_plot_sex_hc_pop, width=25, height=30, units="cm", dpi="print")
  
  
  gp_plot_sex_fte_pop = ggplot(graph_data %>% filter(TYPE=="Full time equivalent")) + 
    aes(x=YEAR, y=(100000*VALUE/TOTAL_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP full time equivalent per 100,000 population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_pop.png"), gp_plot_sex_fte_pop, width=25, height=30, units="cm", dpi="print")
  
  
  gp_plot_sex_hc_pop_adj = ggplot(graph_data %>% filter(TYPE=="Headcount")) + 
    aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP headcount per 100,000 need adjusted population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_pop_adj.png"), gp_plot_sex_hc_pop_adj, width=25, height=30, units="cm", dpi="print")
  
  
  gp_plot_sex_fte_pop_adj = ggplot(graph_data %>% filter(TYPE=="Full time equivalent")) + 
    aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("GP full time equivalent per 100,000 need adjusted population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~SEX,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_pop_adj.png"), gp_plot_sex_fte_pop_adj, width=25, height=30, units="cm", dpi="print")
  
}

graph_all_staff_trends = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
  dbDisconnect(db)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,
           TOTAL_GPEXRRL_HC=TOTAL_GP_EXRRL_HC, TOTAL_GPEXRRL_FTE=TOTAL_GP_EXRRL_FTE,
           TOTAL_NURSES_HC, TOTAL_NURSES_FTE,
           TOTAL_DPC_HC, TOTAL_DPC_FTE,
           TOTAL_ADMIN_HC, TOTAL_ADMIN_FTE
           ) %>%
    gather(VARIABLE,VALUE,TOTAL_GPEXRRL_HC:TOTAL_ADMIN_FTE) %>%
    separate(VARIABLE,c("x","VARIABLE","TYPE")) %>%
    select(-x) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GPEXRRL","NURSES","DPC","ADMIN"),
                             c("GPs excluding registrars, retainers and locums","Nurses","Direct patient care staff","Admin staff")))
  
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  all_staff_plot_raw = ggplot(graph_data) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in general practice workforce supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_raw.png"), all_staff_plot_raw, width=25, height=35, units="cm", dpi="print")
  
  all_staff_plot_pop = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/TOTAL_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in general practice workforce supply per 100,000 population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_pop.png"), all_staff_plot_pop, width=25, height=35, units="cm", dpi="print")
  
  all_staff_plot_pop_adj = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in general practice workforce supply per 100,000 need adjusted population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_pop_adj.png"), all_staff_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

graph_gp_locum_trends = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
  dbDisconnect(db)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,
           GPLOCUMABS_HC=TOTAL_GP_LOCUM_ABS_HC,
           GPLOCUMVAC_HC = TOTAL_GP_LOCUM_VAC_HC,
           GPLOCUMOTH_HC = TOTAL_GP_LOCUM_OTH_HC,
           GPLOCUMABS_FTE = TOTAL_GP_LOCUM_ABS_FTE,
           GPLOCUMVAC_FTE = TOTAL_GP_LOCUM_VAC_FTE,
           GPLOCUMOTH_FTE = TOTAL_GP_LOCUM_OTH_FTE) %>% 
    gather(VARIABLE,VALUE,GPLOCUMABS_HC:GPLOCUMOTH_FTE) %>%
    separate(VARIABLE,c("VARIABLE","TYPE")) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GPLOCUMABS","GPLOCUMVAC","GPLOCUMOTH"),
                             c( "Locums covering vacancies","Locums covering sickness/maternity/paternity","Locums other")))
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  gp_plot_raw = ggplot(graph_data) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP locum supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_raw.png"), gp_plot_raw, width=25, height=35, units="cm", dpi="print")
  
  gp_plot_pop = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/TOTAL_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP locum supply per 100,000 population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_pop.png"), gp_plot_pop, width=25, height=35, units="cm", dpi="print")
  
  gp_plot_pop_adj = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "Trends in GP locum supply per 100,000 need adjusted population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2015 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_pop_adj.png"), gp_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

opening_and_closing_practices = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_practices = tbl(db, "gp_workforce_newdata_imputed") %>% select(YEAR,PRAC_CODE,PRAC_NAME,TOTAL_GP_HC) %>% collect() 
  gp_imd = tbl(db, "gp_imd_2015") %>% collect() %>% add_quintiles() %>% select(PRAC_CODE,IMD_QUINTILE)
  dbDisconnect(db)
  
  gp_practices_2015 = gp_practices %>% filter(YEAR==2015)
  gp_practices_2016 = gp_practices %>% filter(YEAR==2016)
  gp_practices_2017 = gp_practices %>% filter(YEAR==2017)
  gp_practices_2018 = gp_practices %>% filter(YEAR==2018)
  
  closed_2016 = setdiff(gp_practices_2015$PRAC_CODE,gp_practices_2016$PRAC_CODE)
  opened_2016 = setdiff(gp_practices_2016$PRAC_CODE,gp_practices_2015$PRAC_CODE)
  closed_2017 = setdiff(gp_practices_2016$PRAC_CODE,gp_practices_2017$PRAC_CODE)
  opened_2017 = setdiff(gp_practices_2017$PRAC_CODE,gp_practices_2016$PRAC_CODE)
  closed_2018 = setdiff(gp_practices_2017$PRAC_CODE,gp_practices_2018$PRAC_CODE)
  opened_2018 = setdiff(gp_practices_2018$PRAC_CODE,gp_practices_2017$PRAC_CODE)
  
  open_close = bind_rows(
  gp_practices_2015 %>% filter(PRAC_CODE %in% closed_2016) %>% mutate(OPEN_CLOSE="closed", YEAR=2016),
  gp_practices_2016 %>% filter(PRAC_CODE %in% opened_2016) %>% mutate(OPEN_CLOSE="opened"),
  gp_practices_2016 %>% filter(PRAC_CODE %in% closed_2017) %>% mutate(OPEN_CLOSE="closed", YEAR=2017),
  gp_practices_2017 %>% filter(PRAC_CODE %in% opened_2017) %>% mutate(OPEN_CLOSE="opened"),
  gp_practices_2017 %>% filter(PRAC_CODE %in% closed_2018) %>% mutate(OPEN_CLOSE="closed", YEAR=2018),
  gp_practices_2018 %>% filter(PRAC_CODE %in% opened_2018) %>% mutate(OPEN_CLOSE="opened")) %>% left_join(gp_imd)
  
  open_closed_quintiles = open_close %>% 
    group_by(YEAR,OPEN_CLOSE,IMD_QUINTILE) %>% 
    summarise(NUM_PRACTICES=n(),GP_HC=sum(TOTAL_GP_HC)) %>%
    arrange(YEAR,OPEN_CLOSE,IMD_QUINTILE)

  return(open_closed_quintiles)
}


library("tidyverse")
library("readxl")
library("RSQLite")
library("fingertipsR")
library("scales")

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
  wf_2013 = read_csv("raw_data/workforce/nhs-staf-2013-gene-prac-data/General Practice 2013 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2013 = wf_2013 %>% mutate(YEAR=2013,
                               TOTAL_GP_EXRRL_HC=TOTAL_GP_EXRR_HC,
                               TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRR_FTE)
  
  wf_2013 = wf_2013 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC,TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC,TOTAL_NURSE_FTE)
  
  wf_2014 = read_csv("raw_data/workforce/nhs-staf-2014-gene-prac-data/General Practice 2014 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2014 = wf_2014 %>% mutate(YEAR=2014,
                               TOTAL_GP_EXRRL_HC=TOTAL_GP_EXRR_HC,
                               TOTAL_GP_EXRRL_FTE=TOTAL_GP_EXRR_FTE)
  
  wf_2014 = wf_2014 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_0to4:FEMALE_85plus,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC,TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC,TOTAL_NURSE_FTE)
  
  colnames(wf_2014) = colnames(wf_2013)
  
  wf_2015 = read_csv("raw_data/workforce/nhs-staf-2015-gene-prac-data/General Practice September 2015 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2015 = wf_2015 %>% 
    mutate(YEAR=2015,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC = TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE = TOTAL_GP_HC-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2015 = wf_2015 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE)
  
  wf_2016 = read_csv("raw_data/workforce/nhs-staf-sept-2016-gene-prac-data/General Practice September 2016 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2016 = wf_2016 %>% 
    mutate(YEAR=2016,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_HC-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2016 = wf_2016 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE)
  
  wf_2017 = read_csv("raw_data/workforce/general_and_personal_medical_services__england_-_september_2017/General Practice September 2017 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2017 = wf_2017 %>% 
    mutate(YEAR=2017,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST3_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST3_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_HC-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2017 = wf_2017 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE)
  
  wf_2018 = read_csv("raw_data/workforce/GP Prov Sept 2018 Practice Level CSV/General Practice Provisional Sept 2018 Practice Level.csv",
                     na=c("","NA","EST","NS",".","ND"))
  
  wf_2018 = wf_2018 %>% 
    mutate(YEAR=2018,
           TOTAL_GP_REG_HC=TOTAL_GP_REG_F1_2_HC+TOTAL_GP_REG_ST1_4_HC, 
           TOTAL_GP_REG_FTE=TOTAL_GP_REG_F1_2_FTE+TOTAL_GP_REG_ST1_4_FTE,
           TOTAL_GP_EXRR_HC=TOTAL_GP_HC-TOTAL_GP_REG_HC-TOTAL_GP_RET_HC,
           TOTAL_GP_EXRR_FTE=TOTAL_GP_HC-TOTAL_GP_REG_FTE-TOTAL_GP_RET_FTE)
  
  wf_2018 = wf_2018 %>% select(YEAR,PRAC_CODE,PRAC_NAME,CONTRACT,TOTAL_PATIENTS,
                               MALE_PATIENTS_0TO4:FEMALE_PATIENTS_85PLUS,
                               TOTAL_GP_HC,TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRR_HC,TOTAL_GP_REG_HC,TOTAL_GP_RET_HC,
                               TOTAL_GP_FTE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRR_FTE,TOTAL_GP_REG_FTE,TOTAL_GP_RET_FTE,
                               TOTAL_NURSE_HC=TOTAL_NURSES_HC,TOTAL_NURSE_FTE=TOTAL_NURSES_FTE)
  
  wf = bind_rows(wf_2013,wf_2014,wf_2015,wf_2016,wf_2017,wf_2018)
  

  
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

calculate_gp_per_100k_trend_by_imd = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_data = tbl(db, "gp_workforce") %>%
    left_join(tbl(db, "gp_imd_2015") %>% select(PRAC_CODE,IMD_SCORE,IMD_DECILE)) %>%
    collect() %>%
    drop_na(TOTAL_GP_EXRR_FTE,TOTAL_PATIENTS,IMD_DECILE,IMD_SCORE) %>%
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
                         3.090*FEMALE_PATIENTS_85PLUS +
                         IMD_SCORE^1.054) 
  
   graph_data = gp_data %>% inner_join(gp_data %>%
    group_by(YEAR) %>%
    summarise(TOTAL_POP=sum(TOTAL_PATIENTS),NEED_ADJUSTED_POP=sum(NEED_ADJUSTED_POP)) %>%
    mutate(NORMALISATION_FACTOR=TOTAL_POP/NEED_ADJUSTED_POP) %>%
    select(YEAR,NORMALISATION_FACTOR)) %>%
     mutate(NEED_ADJUSTED_POP=NEED_ADJUSTED_POP*NORMALISATION_FACTOR) %>%
     ungroup() %>%
    group_by(YEAR,IMD_DECILE) %>%
    summarise(GP_FTE=sum(TOTAL_GP_EXRR_FTE),ADJ_POP=sum(NEED_ADJUSTED_POP),POP=sum(TOTAL_PATIENTS)) %>%
    mutate(GPS_PER_100K=round(100000*GP_FTE/POP,1),GPS_PER_100K_ADJ=round(100000*GP_FTE/ADJ_POP,1))
  dbDisconnect(db)
  
  graph_data = gather(graph_data,GP_var,GP_val,GPS_PER_100K,GPS_PER_100K_ADJ) %>%
    mutate(IMD_DECILE=as.factor(IMD_DECILE),
           GP_var=factor(GP_var,c("GPS_PER_100K","GPS_PER_100K_ADJ"),c("GPs per 100k population unadjusted","GPs per 100k population need adjusted")))
  
  imd_labels = c("D1 (most deprived)","D2","D3","D4","D5","D6","D7","D8","D9","D10 (least deprived)")
  main_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=GP_val, group=IMD_DECILE, colour=IMD_DECILE) + 
    geom_line(aes(linetype=IMD_DECILE, size=IMD_DECILE)) + 
    geom_point(aes(shape=IMD_DECILE, colour=IMD_DECILE)) +
    xlab("Year") +
    ylab("GPs per 100,000 population") +
    scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightblue","lightblue","lightblue","lightblue","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Group", values=c(19,21,21,24,24,24,24,0,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,2,2,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Group", values=c(1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(.~GP_var) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=rel(1.7)))
  
}

calculate_ons_pop_by_decile = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  
  imd = tbl(db, "imd_2015")
  
  imd = imd %>% 
    filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
    select(LSOA11CD,IMD_DECILE=VALUE)
  
  ons_pop = tbl(db, "ons_pop")
  
  decile_pop = inner_join(imd, ons_pop) %>%
    group_by(YEAR,IMD_DECILE,SEX,AGE) %>%
    summarise(POP=sum(POP)) %>% 
    collect()
  
  dbWriteTable(conn=db, name="ons_pop_decile_manual", decile_pop, overwrite=TRUE)
  
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
  
make_age_group  = function(start_age, end_age){
  paste0("AGE_",start_age:end_age, collapse = "+")
}
  
write_adjusted_pop_by_imd = function(database_name="primary_care_data.sqlite3"){
    db = dbConnect(SQLite(), dbname=database_name)
    decile_pop = tbl(db, "ons_pop_decile") %>% collect()
  
    decile_pop = decile_pop %>% 
    mutate(AGE = ifelse(AGE=="90+",90,AGE),
           SEX = ifelse(SEX==1,"MALE","FEMALE")) %>%
    spread(AGE,POP,sep="_") %>%
    mutate_(AGE_0_4 = make_age_group(0,4),
            AGE_5_14 = make_age_group(5,14),
            AGE_15_44 = make_age_group(15,44),
            AGE_45_64 = make_age_group(45,64),
            AGE_65_74 = make_age_group(65,74),
            AGE_75_84 = make_age_group(75,84),
            AGE_85PLUS = make_age_group(85,90)) %>%
    select(YEAR,SEX,IMD_DECILE, AGE_0_4,AGE_5_14,AGE_15_44,AGE_45_64,AGE_65_74,AGE_75_84,AGE_85PLUS) %>%
    gather(AGE_GROUP,POP,AGE_0_4,AGE_5_14,AGE_15_44,AGE_45_64,AGE_65_74,AGE_75_84,AGE_85PLUS) %>%
    unite(POP_GROUP,SEX,AGE_GROUP) %>%
    spread(POP_GROUP,POP) %>%
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
             FEMALE_AGE_85PLUS) 
  
  imd = tbl(db, "imd_2015") %>% 
    filter((DIMENSION=="OVERALL" & MEASURE=="Decile") | (DIMENSION=="HEALTH" & MEASURE=="Score")) %>%
    select(LSOA11CD,DIMENSION,VALUE) %>%
    collect()
  imd = imd %>% 
    spread(DIMENSION,VALUE) %>%
    group_by(OVERALL) %>%
    summarise(IMD_HEALTH =mean(HEALTH)) %>%
    ungroup() %>%
    select(IMD_DECILE=OVERALL, IMD_HEALTH)
  
  # health deprivation adjustment needs to be done at LSOA levels 
  # otherwise get negative decile populations
  decile_pop_adjusted =inner_join(decile_pop,imd) %>%
    mutate(ADJUSTED_POP = 2.354*MALE_AGE_0_4 + 
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
    3.090*FEMALE_AGE_85PLUS) 
  
    decile_pop_adjusted2=decile_pop_adjusted %>% 
    group_by(YEAR) %>%
    summarise(TOTAL = sum(TOTAL_POP), ADJ=sum(ADJUSTED_POP)) %>%
    mutate(AF=TOTAL/ADJ) %>%
    ungroup() %>%
    select(YEAR,AF) %>%
    inner_join(decile_pop_adjusted) %>%
    mutate(NORMALISED_ADJ_POP = ADJUSTED_POP*AF) %>%
      select(YEAR,IMD_DECILE,NEED_ADJ_POP=NORMALISED_ADJ_POP)
    
    dbWriteTable(conn=db, name="adj_pop_decile", decile_pop_adjusted2, overwrite=TRUE)
    
    dbDisconnect(db)
}

process_attribution_dataset = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  imd = tbl(db, "imd_2015") %>% 
    filter(DIMENSION=="OVERALL" & MEASURE=="Decile") %>%
    select(LSOA11CD,IMD_DECILE=VALUE) %>%
    collect()
  
  for (year in 2014:2018) {
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
    
    ads_decile = inner_join(ads_formatted,imd) %>%
      group_by(PRAC_CODE, IMD_DECILE) %>%
      summarise(POP=sum(POP))
      
    ads_props = inner_join(ads_decile,ads_decile %>%
      group_by(PRAC_CODE) %>%
      summarise(TOTAL_POP=sum(POP))) %>%
      ungroup() %>%
      mutate(DECILE_PROP=POP/TOTAL_POP, YEAR=year) %>%
      select(YEAR,PRAC_CODE,IMD_DECILE,DECILE_PROP)
    
    dbWriteTable(conn=db, name="ads_imd_props", ads_props, append=TRUE)
  }
  dbDisconnect(db)
}

allocate_gps_to_imd_deciles = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps = tbl(db, "gp_workforce") %>% 
    select(YEAR,PRAC_CODE,TOTAL_GP_EXRR_FTE) %>%
    collect()
  
  ads = tbl(db, "ads_imd_props") %>% collect()
  
  results = inner_join(gps,ads) %>%
    mutate(TOTAL_GP_FTE = DECILE_PROP*TOTAL_GP_EXRR_FTE) %>%
    group_by(YEAR,IMD_DECILE) %>%
    summarise(TOTAL_GP_FTE = sum(TOTAL_GP_FTE,na.rm=TRUE))
  
  results= results %>% 
    spread(IMD_DECILE,TOTAL_GP_FTE,sep="_") %>%
    mutate(`1`=IMD_DECILE_1+IMD_DECILE_2,
           `2`=IMD_DECILE_3+IMD_DECILE_4,
           `3`=IMD_DECILE_5+IMD_DECILE_6,
           `4`=IMD_DECILE_7+IMD_DECILE_8,
           `5`=IMD_DECILE_9+IMD_DECILE_10) %>%
    select(YEAR,`1`:`5`) %>%
    gather(IMD_QUINTILE,TOTAL_GP_FTE,-YEAR) %>%
    mutate(IMD_QUINTILE=as.numeric(IMD_QUINTILE))
  
  adj_pop = tbl(db, "adj_pop_decile") %>% collect()
  adj_pop = adj_pop %>% 
    spread(IMD_DECILE,NEED_ADJ_POP,sep="_") %>%
    mutate(`1`=IMD_DECILE_1+IMD_DECILE_2,
           `2`=IMD_DECILE_3+IMD_DECILE_4,
           `3`=IMD_DECILE_5+IMD_DECILE_6,
           `4`=IMD_DECILE_7+IMD_DECILE_8,
           `5`=IMD_DECILE_9+IMD_DECILE_10) %>%
    select(YEAR,`1`:`5`) %>%
    gather(IMD_QUINTILE,NEED_ADJ_POP,-YEAR)%>%
    mutate(IMD_QUINTILE=as.numeric(IMD_QUINTILE))
  
  pop = tbl(db, "ons_pop_quintile") %>% collect()
  pop = pop %>% group_by(YEAR, IMD_QUINTILE) %>%
    summarise(POP=sum(POP))
  
  previous_data = read_csv("raw_data/previous_data.csv")
  
  graph_data = inner_join(results,pop) %>%
    mutate(GP_PER_100k=100000*TOTAL_GP_FTE/POP) %>%
    bind_rows(previous_data) %>%
    mutate(IMD_QUINTILE=as.factor(IMD_QUINTILE)) %>%
    gather(VAR,VAL,TOTAL_GP_FTE:GP_PER_100k) %>%
    mutate(VAR=factor(VAR,c("POP","TOTAL_GP_FTE","GP_PER_100k"),c("Total population","Total number of GPs excluding registrars and retainers (FTE)","GPs per 100,000 population")))
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  gp_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    geom_vline(xintercept=c(2013.5), linetype="dashed", colour="lightgrey") +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(VAR~.,ncol=3,scales="free",labeller = labeller(VAR = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a")) +
    labs(title = "Trends in GP supply in England by neighbourhood deprivation quintile",
         subtitle = "Unadjusted numbers for 2004 - 2017 based on IMD 2010 & IMD 2015",
         caption = "Note: Data before dashed line based on previous publication using IMD 2010 quintiles after the dashed line are the newly calculated results based on IMD 2015 quintiles")
  
  ggsave("figures/gp_trends.png", gp_plot, width=40, height=10, units="cm", dpi="print")
  
  dbDisconnect(db)
}
  
library("tidyverse")
library("RSQLite")
library("readxl")

data_quality_summary = function(database_name="primary_care_data.sqlite3"){
  
db = dbConnect(SQLite(), dbname=database_name)
gp_data = tbl(db,"gp_workforce_newdata") %>% collect()
gp_data_imputed = tbl(db,"gp_workforce_newdata_imputed") %>% collect()
gp_imd = tbl(db,"gp_imd_2019") %>% filter(YEAR>2014) %>% collect()
ads_lsoa = tbl(db,"ads_lsoa_props") %>% filter(YEAR>2014) %>% collect()
ccg_prac = tbl(db,"ccg_practice_mapping") %>% collect()
gp_data_imd = tbl(db,"gp_workforce_imd_quintiles") %>% filter(YEAR>2014) %>% select(-contains("_POP")) %>% arrange(YEAR,IMD_QUINTILE) %>% collect()
gp_data_imd_imputed = tbl(db,"gp_workforce_imd_quintiles_imputed") %>% filter(YEAR>2014) %>% select(-contains("_POP")) %>% arrange(YEAR,IMD_QUINTILE) %>% collect()
gp_data_imd_missing = tbl(db,"gp_workforce_missing_imd_quintiles") %>% filter(YEAR>2014)  %>% arrange(YEAR,IMD_QUINTILE) %>% collect()

dbDisconnect(db)


workforce_practice_count = gp_data %>% group_by(YEAR) %>% summarise(workforce_practice_count=n())

imd_practice_count = gp_imd %>% group_by(YEAR) %>% summarise(imd_practice_count=n())

ccg_practice_count = ccg_prac %>% summarise(ccg_practice_count=n()) %>% crossing(YEAR=2015:2018)

ads_practice_count = ads_lsoa %>% filter(grepl("^E01",LSOA11CD)) %>% group_by(YEAR) %>% distinct(PRAC_CODE) %>% summarise(ads_practice_count=n())

workforce_practice_count %>% inner_join(ccg_practice_count) %>% inner_join(ads_practice_count) %>% inner_join(imd_practice_count) 

workforce_practice_list = gp_data %>% select(YEAR,PRAC_CODE,WORKFORCE_PRAC_NAME=PRAC_NAME) %>% mutate(WORKFORCE="WORKFORCE")

ads_practice_list = ads_lsoa %>% filter(grepl("^E01",LSOA11CD)) %>% distinct(YEAR,PRAC_CODE) %>% mutate(ADS="ADS")

ccg_practice_list = ccg_prac %>% select(PRAC_CODE=PRACTICE_CODE, CCG_PRAC_NAME=PRACTICE_NAME) %>% crossing(YEAR=2015:2018) %>% mutate(CCG="CCG")

missing_practices = workforce_practice_list %>% full_join(ccg_practice_list) %>% full_join(ads_practice_list) %>% 
  filter(is.na(WORKFORCE) | is.na(CCG) | is.na(ADS)) %>% select(YEAR,PRAC_CODE,WORKFORCE_PRAC_NAME,CCG_PRAC_NAME,WORKFORCE,CCG,ADS) %>% arrange(YEAR,PRAC_CODE) 

write_csv(missing_practices,"data_quality/missing_practices.csv")

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

unimputed_practice_data = gp_data %>% group_by(YEAR) %>%
  summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) 

imputed_practice_data = gp_data_imputed %>% group_by(YEAR) %>%
  summarise_at(vars(one_of(workforce_vars)), sum, na.rm=TRUE) 

diff_practice_data = imputed_practice_data - unimputed_practice_data
diff_practice_data$YEAR = 2015:2018

n_missing_practice_data = gp_data %>% group_by(YEAR) %>%
  select(workforce_vars) %>%
  summarise_all(list(~ sum(is.na(.))))


diff_gp_data_imd = gp_data_imd_imputed - gp_data_imd
diff_gp_data_imd$YEAR = gp_data_imd$YEAR
diff_gp_data_imd$IMD_QUINTILE = gp_data_imd$IMD_QUINTILE

gp_data %>% group_by(YEAR) %>% summarise(sum(TOTAL_GP_EXRRL_FTE, na.rm = TRUE))
gp_data %>% group_by(YEAR) %>% filter(is.na(TOTAL_GP_EXRRL_FTE)) %>% summarise(n())
gp_data_imputed %>% group_by(YEAR) %>% summarise(sum(TOTAL_GP_EXRRL_FTE))

ads_lsoa %>% filter(grepl("^E01",LSOA11CD)) %>% group_by(YEAR) %>%distinct(LSOA11CD) %>% summarise(lsoa_count=n())

ind_2018 = read_csv("raw_data/workforce/GPs 2015-2018/General Practice September 2018 Individual Level.csv")
ind_2018 %>% distinct(STAFF_GROUP)
ind_2018 %>% filter(STAFF_GROUP=="GP") %>% distinct(JOB_ROLE)
ind_gp_exrrl_2018 = ind_2018 %>% filter(STAFF_GROUP=="GP") %>% 
  filter(!(JOB_ROLE %in% c("Retainer","Registrar ST1/4","Locum - Covering Vacancy","Locum - other","Locum - Covering Sickness/Maternity/Paternity")))
ind_gp_exrrl_2018 %>% summarise(ind_gp_exrrl_2018_fte=sum(FTE),ind_gp_exrrl_2018_hc=n()) %>% mutate(YEAR=2018)

prac_2018 = read_csv("raw_data/workforce/GPs 2015-2018/General Practice September 2016 Practice Level.csv")
x = prac_2018 %>% filter(TOTAL_GP_EXRRL_FTE != "ND") %>% distinct(TOTAL_GP_EXRRL_FTE)
prac_2018 %>% filter(TOTAL_GP_EXRRL_FTE != "ND") %>% summarise(prac_gp_exrrl_2018_fte=sum(as.double(TOTAL_GP_EXRRL_FTE)),prac_gp_exrrl_2018_hc=sum(as.double(TOTAL_GP_EXRRL_HC)))

}

get_practice_summary = function(year){
  prac = read_csv(paste0("raw_data/workforce/GPs 2015-2018/General Practice September ",year," Practice Level.csv"), guess_max = 100000)
  prac_summary = prac %>% filter(!(TOTAL_GP_EXRRL_FTE %in% c("ND","NS","EST"))) %>% 
    summarise(prac_gp_exrrl_fte=sum(as.double(TOTAL_GP_EXRRL_FTE)),prac_gp_exrrl_hc=sum(as.double(TOTAL_GP_EXRRL_HC))) %>%
    mutate(YEAR=year)
  return(prac_summary)
}

get_ind_summary = function(year){
  ind = read_csv(paste0("raw_data/workforce/GPs 2015-2018/General Practice September ",year," Individual Level.csv"), guess_max = 100000)
  ind_summary = ind %>% filter(STAFF_GROUP=="GP") %>% 
    filter(!(JOB_ROLE %in% c("Retainer","Registrar ST1/4","Locum - Covering Vacancy","Locum - other","Locum - Covering Sickness/Maternity/Paternity"))) %>% 
    summarise(ind_gp_exrrl_fte=sum(FTE),ind_gp_exrrl_hc=n()) %>% mutate(YEAR=year)
  return(ind_summary)
}

get_imputed_summary = function(year, database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_data_imputed = tbl(db,"gp_workforce_newdata_imputed") %>% filter(YEAR==year) %>% select(YEAR, TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRRL_HC) %>% collect()
  imputed_summary = gp_data_imputed %>% group_by(YEAR) %>% summarise(imputed_gp_exrrl_fte=sum(TOTAL_GP_EXRRL_FTE),imputed_gp_exrrl_hc=sum(TOTAL_GP_EXRRL_HC))
  dbDisconnect(db)
  return(imputed_summary)
}

get_non_imputed_summary = function(year, database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_data_imputed = tbl(db,"gp_workforce_newdata") %>% filter(YEAR==year) %>% select(YEAR, TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRRL_HC) %>% collect()
  imputed_summary = gp_data_imputed %>% group_by(YEAR) %>% summarise(imputed_gp_exrrl_fte=sum(TOTAL_GP_EXRRL_FTE,na.rm=TRUE),imputed_gp_exrrl_hc=sum(TOTAL_GP_EXRRL_HC,na.rm=TRUE))
  dbDisconnect(db)
  return(imputed_summary)
}

get_published_summary = function(year){
  gp_data_published = tribble(
    ~YEAR, ~published_gp_exrrl_fte, ~published_gp_exrrl_hc,
    2018,   27396, 34118,
    2017,   27836, 34267,
    2016,   28458, 34836,
    2015,   29271, 35586
  )
  return(gp_data_published %>% filter(YEAR==year))
}

get_full_summary = function(year){
  full_summary = get_practice_summary(year) %>% 
    inner_join(get_ind_summary(year)) %>% 
    inner_join(get_imputed_summary(year)) %>%
    inner_join(get_published_summary(year)) %>%
    select(YEAR,
           published_gp_exrrl_fte,prac_gp_exrrl_fte,ind_gp_exrrl_fte,imputed_gp_exrrl_fte,
           published_gp_exrrl_hc,prac_gp_exrrl_hc,ind_gp_exrrl_hc,imputed_gp_exrrl_hc)
  return(full_summary)
}

summ_2018 = get_full_summary(2018)
summ_2017 = get_full_summary(2017)
summ_2016 = get_full_summary(2016)
summ_2015 = get_full_summary(2015)

full_summary = bind_rows(summ_2018,summ_2017,summ_2016,summ_2015)

db = dbConnect(SQLite(), dbname=database_name)
gp_data_imputed = tbl(db,"gp_workforce_newdata_imputed") %>% collect()
ads_lsoa = tbl(db,"ads_lsoa_props") %>% filter(YEAR>2014) %>% collect()
ccg_prac = tbl(db,"ccg_practice_mapping") %>% collect()
dbDisconnect(db)

workforce_count = gp_data_imputed %>% group_by(YEAR) %>% summarise(practice_count_workforce = n())
ads_count = ads_lsoa %>% group_by(YEAR) %>% distinct(PRAC_CODE) %>% summarise(practice_count_ads = n())
ccg_count = ccg_prac %>% nrow()
workforce_ads_count = gp_data_imputed %>% inner_join(ads_lsoa) %>% group_by(YEAR) %>% distinct(PRAC_CODE) %>% summarise(practice_count_wf_ads = n())
workforce_ads_ccg_count = gp_data_imputed %>% inner_join(ccg_prac %>% rename(PRAC_CODE=PRACTICE_CODE)) %>% inner_join(ads_lsoa) %>% group_by(YEAR) %>% distinct(PRAC_CODE) %>% summarise(practice_count_wf_ads_ccg = n())

gp_prac_counts = workforce_count %>% inner_join(ads_count) %>% inner_join(workforce_ads_count) %>% inner_join(workforce_ads_ccg_count)

write_csv(gp_prac_counts,"data_quality/gp_prac_counts.csv")
dbDisconnect(db)
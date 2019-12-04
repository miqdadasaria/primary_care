library(tidyverse)
library(ggplot2)
library(plotly)
library(RSQLite)
library(scales)


get_ccg_list = function(database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  ccgs = tbl(db,"ccg_ons_code_mapping") %>% select(CCG19CD,CCG19NM) %>% distinct(CCG19CD,CCG19NM) %>% collect()
  ccg_list = as.list(ccgs$CCG19CD)
  names(ccg_list)=ccgs$CCG19NM
  return(ccg_list)
}

get_workforce_varlist = function(database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_vars =tbl(db,"workforce_variables") %>% filter(ATTRIBUTE==TRUE) %>% select(VARNAMES) %>% collect()
  dbDisconnect(db)
  return(workforce_vars$VARNAMES)
}

make_data_table = function(x_var, y_var, adj_method, year, geography, trim_outliers, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  
  if(geography == "lsoa"){
    graph_data = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>% filter(YEAR==year) %>% collect()
    names = tbl(db, "ccg_lsoa_lad_mapping") %>% select(LSOA11CD,LSOA11NM) %>% collect()
    graph_data = graph_data %>% inner_join(names) %>% rename(ONS_CODE=LSOA11CD, NAME=LSOA11NM)
  } else if(geography == "ccg"){
    data = tbl(db,"ccg_workforce_imputed") %>% filter(YEAR==year) %>% collect()
    imd = tbl(db, "ccg_imd_2019") %>% select(CCG19CD,IMD_SCORE=average_score,IMD_RANK=rank_average_score) %>% collect()
    names = tbl(db,"ccg_ons_code_mapping") %>% select(CCG19CD,CCG19NM) %>% distinct(CCG19CD,CCG19NM) %>% collect()
    graph_data = data %>% inner_join(imd) %>% inner_join(names)
    graph_data = graph_data %>% rename(ONS_CODE=CCG19CD, NAME=CCG19NM)
  } else if(geography == "pcn"){
    data = tbl(db,"pcn_workforce_imputed") %>% filter(YEAR==year) %>% collect()
    imd = tbl(db, "pcn_imd_2019") %>% filter(YEAR==year) %>% select(PCN_CODE,IMD_SCORE,IMD_RANK) %>% collect()
    names = tbl(db,"pcn_gp_practice_mapping") %>% select(PCN_CODE,PCN_NAME) %>% distinct(PCN_CODE,PCN_NAME) %>% collect()
    graph_data = data %>% inner_join(imd) %>% inner_join(names)
    graph_data = graph_data %>% rename(ONS_CODE=PCN_CODE, NAME=PCN_NAME)
  } else if(geography == "gp_practice"){
    # TODO: investigate why too many practices in workforce with 0 patient populations
    data = tbl(db,"gp_workforce_newdata_imputed") %>% filter(YEAR==year) %>% collect()
    pop = tbl(db, "gp_population") %>% filter(YEAR==year) %>% collect()
    imd = tbl(db, "gp_imd_2019") %>% filter(YEAR==year) %>% collect()
    graph_data = data %>% inner_join(pop) %>% inner_join(imd)
    graph_data = graph_data %>% rename(ONS_CODE=PRAC_CODE, NAME=PRAC_NAME)
    #graph_data = left_join(data,inner_join(pop,imd))
  } else {
    warning(paste0("geography must be one of lsoa, ccg or gp_practice you specified: ",geography))
    return()
  }
  
  dbDisconnect(db)
  
  graph_data = graph_data %>% mutate(Name = gsub("Pcn","PCN",gsub("Ccg","CCG",gsub("Nhs","NHS",str_to_title(NAME)))),
                                     'ONS code' = ONS_CODE,
                                     'IMD (2019) score' = round(IMD_SCORE,0),
                                     'IMD (2019) rank' = IMD_RANK,
                                     'Total population' = round(TOTAL_POP,0),
                                     'Need adjusted population' = round(NEED_ADJ_POP,0),
                                     'x' = round(get(x_var),2),
                                     'y' = get(y_var),2) %>%
    select(Name,'ONS code','IMD (2019) score','IMD (2019) rank','Total population','Need adjusted population','x','y')
  
  if(adj_method == "pop"){
    graph_data = graph_data %>% mutate(y = 100000*y/`Total population`)
  } else if(adj_method == "adj_pop"){
    graph_data = graph_data %>% mutate(y = 100000*y/`Need adjusted population`)
  } 
  
  if(trim_outliers){
    y_bar = graph_data %>% summarise(y_bar=mean(y)) %>% as.numeric()
    sigma = graph_data %>% summarise(sigma=sd(y)) %>% as.numeric()
    graph_data = graph_data %>% filter(y > (y_bar-3*sigma) & y < (y_bar+3*sigma))
  }
  graph_data = graph_data %>% mutate(y=round(y,2))
  
  return(graph_data)
}

make_var_label = function(varname){
  label = gsub("_HC","_headcount",varname)
  label = gsub("_FTE","_fulltime equivalent",label)
  label = gsub("_EXL","_excluding locums",label)
  label = gsub("_EXRL","_excluding registrars and locums",label)
  label = gsub("_EXRRL","_excluding registrars, retainers and locums",label)
  label = gsub("_POP","_population",label)
  label = gsub("IMD_","index of multiple deprivation (2019)_",label)
  label = gsub("_ADJ_","_adjusted_",label) 
  label = gsub("_DPC_","_direct patient care_",label) 
  label = gsub("_COQ_","_country of qualification_",label) 
  label = gsub("_"," ",label)
  label = lapply(strwrap(as.character(str_to_lower(label)), width=40, simplify=FALSE), paste, collapse="\n")
  return(label)
}

workforce_scatter_plot = function(x_var, y_var, adj_method, year, geography, trim_outliers){
    graph_data = make_data_table(x_var, y_var, adj_method, year, geography, trim_outliers)
    
    y_lab = make_var_label(y_var)
    x_lab = make_var_label(x_var)
      
    if(adj_method=="pop"){
      y_lab = paste0(y_lab," per 100,000 population")
    } else if(adj_method=="adj_pop"){
      y_lab = paste0(y_lab," per 100,000 need adjusted population")
    }
    
    scatter_plot = ggplot(graph_data, aes(x=x, y=y)) +
      geom_point(alpha=0.6, aes(colour=`IMD (2019) score`, size=`Total population`))  +
      xlab(x_lab) +
      ylab(y_lab) +
      stat_smooth(method='lm', colour="darkred", size=0.5, linetype=2, alpha=0.5, se=FALSE) +
      theme_minimal() +
      scale_color_distiller("IMD (2019) score", palette="YlGn", direction=1) +
      scale_size_continuous("Total population") +
      labs(title = "Trends in primary care supply by neighbourhood deprivation",
           subtitle = paste0("Data for England in ",year," based on IMD 2019 quintiles"),
           caption = "Note: Low IMD score means more affluent - high IMD score means more deprived; Low IMD rank means more deprived - high IMD rank means more affluent")
    
    
    return(scatter_plot)

}

imd_plot = function(y_var, adj_method, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() 
  dbDisconnect(db)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,TOTAL_POP,NEED_ADJ_POP,y_var) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
  y_lab = make_var_label(y_var)
  
  if(adj_method=="pop"){
    graph_data = graph_data %>%
      mutate(y = 100000*get(y_var)/TOTAL_POP)
    y_lab = paste0(y_lab," per 100,000 population")
  } else if(adj_method=="adj_pop"){
    graph_data = graph_data %>%
      mutate(y = 100000*get(y_var)/NEED_ADJ_POP) 
    y_lab = paste0(y_lab," per 100,000 need adjusted population")
  } else {
    graph_data = graph_data %>%
      mutate(y = get(y_var))
  }
  
  imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
  start_year=2015
  end_year=2018
  gp_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=y, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
    xlab("Year") +
    ylab(y_lab) +
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
    labs(title = "Trends in primary care supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  return(gp_plot)
}

gp_age_sex_plot = function(year, adj_method, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() %>% filter(YEAR==year) %>% select(IMD_QUINTILE,NEED_ADJ_POP,TOTAL_POP,contains("_HC_"))
  dbDisconnect(db)
  
  graph_data = gps_quintile %>% gather(VARNAME,VALUE,MALE_GP_EXRRL_HC_UNDER30:FEMALE_GP_EXRRL_HC_UNKNOWN_AGE) %>%
    separate(VARNAME,c("SEX","A","B","C","AGE")) %>%
    select(IMD_QUINTILE, NEED_ADJ_POP,TOTAL_POP,SEX,AGE,GP_EXRRL_HC=VALUE) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           AGE = factor(AGE,
                        c("UNDER30","30TO34","35TO39","40TO44","45TO49","50TO54","55TO59","60TO64","65TO69","70PLUS","UNKNOWN"),
                        c("<30","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+","Unknown")),
           SEX = factor(SEX,c("FEMALE","MALE"),c("Female","Male")))
  
  
  y_lab = "GPs excluding registrars, retainers and locums (headcount)"
  
  if(adj_method=="pop"){
    graph_data = graph_data %>%
      mutate(y = 100000*GP_EXRRL_HC/TOTAL_POP)
    y_lab = paste0(y_lab," per 100,000 population")
  } else if(adj_method=="adj_pop"){
    graph_data = graph_data %>%
      mutate(y = 100000*GP_EXRRL_HC/NEED_ADJ_POP) 
    y_lab = paste0(y_lab," per 100,000 need adjusted population")
  } else {
    graph_data = graph_data %>%
      mutate(y = GP_EXRRL_HC)
  }
  y_lab = lapply(strwrap(as.character(str_to_lower(y_lab)), width=40, simplify=FALSE), paste, collapse="\n")
  
  gp_plot = ggplot(graph_data, aes(x=AGE, y=y)) +
    geom_bar(stat="identity") +
    facet_grid(SEX ~ IMD_QUINTILE) +
    theme_bw() + 
    ylab(y_lab) +
    xlab("Age of GPs") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Age/Sex distribution of GP supply by neighbourhood deprivation quintile",
         subtitle = paste0("Data for England in years ",year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  return(gp_plot)
 }

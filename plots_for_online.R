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
  
make_data_table = function(x_var, y_var, adj_method, year, geography, trim_outliers, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  
  if(geography == "lsoa"){
    graph_data = tbl(db, "gp_workforce_newdata_imputed_lsoa") %>% filter(YEAR==year) %>% collect()
    names = tbl(db, "ccg_lsoa_lad_mapping") %>% select(LSOA11CD,LSOA11NM) %>% collect()
    graph_data = graph_data %>% inner_join(names) %>% rename(ONS_CODE=LSOA11CD, NAME=LSOA11NM)
  } else if(geography == "ccg"){
    data = tbl(db,"ccg_workforce") %>% filter(YEAR==year) %>% collect()
    pop = tbl(db, "ccg_pop") %>% filter(YEAR==year) %>% collect()
    imd = tbl(db, "ccg_imd_2019") %>% select(CCG19CD,IMD_SCORE=average_score,IMD_RANK=rank_average_score) %>% collect()
    names = tbl(db,"ccg_ons_code_mapping") %>% select(CCG19CD,CCG19NM) %>% distinct(CCG19CD,CCG19NM) %>% collect()
    graph_data = data %>% inner_join(pop) %>% inner_join(imd) %>% inner_join(names)
    graph_data = graph_data %>% rename(ONS_CODE=CCG19CD, NAME=CCG19NM)
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
  
  graph_data = graph_data %>% mutate(Name = NAME,
                                     'ONS code' = ONS_CODE,
                                     'IMD (2019) score' = IMD_SCORE,
                                     'IMD (2019) rank' = IMD_RANK,
                                     'Total population' = TOTAL_POP,
                                     'Need adjusted population' = NEED_ADJ_POP,
                                     'x' = get(x_var),
                                     'y' = get(y_var)) %>%
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
  
  return(graph_data)
}

make_var_label = function(y_var){
  y_lab = gsub("_"," ",y_var)
  
  y_lab = gsub("HC","headcount",y_lab)
  y_lab = gsub("FTE","fulltime equivalent",y_lab)
  y_lab = gsub("EXL","excluding locums",y_lab)
  y_lab = gsub("EXRL","excluding registrars and locums",y_lab)
  y_lab = gsub("EXRRL","excluding registrars, retainers and locums",y_lab)
  y_lab = lapply(strwrap(as.character(str_to_lower(y_lab)), width=40, simplify=FALSE), paste, collapse="\n")
  return(y_lab)
}

workforce_scatter_plot = function(x_var, y_var, adj_method, year, geography, trim_outliers){
    graph_data = make_data_table(x_var, y_var, adj_method, year, geography, trim_outliers)
    
    y_lab = make_var_label(y_var)
    
    if(adj_method=="pop"){
      y_lab = paste0(y_lab," per 100,000 population")
    } else if(adj_method=="adj_pop"){
      y_lab = paste0(y_lab," per 100,000 need adjusted population")
    }
    
    scatter_plot = ggplot(graph_data, aes(x=x, y=y)) +
      geom_point(alpha=0.6, aes(colour=`IMD (2019) score`, size=`Total population`))  +
      xlab(str_to_lower(gsub("_"," ",x_var))) +
      ylab(y_lab) +
      stat_smooth(method='lm', colour="darkred", size=0.5, linetype=2, alpha=0.5, se=FALSE) +
      theme_minimal() +
      scale_color_distiller("IMD (2019) score", palette="YlGn", direction=1) +
      scale_size_continuous("Total population")
    
    return(scatter_plot)

}

imd_plot = function(y_var, adj_method, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
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
    labs(title = "Trends in GP supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  return(gp_plot)
}
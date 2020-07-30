library("tidyverse")
library("readxl")
library("RSQLite")
library("fingertipsR")
library("scales")
library("purrr")
library("modelr")
library("ggpubr")

graph_population_trends = function(database_name="primary_care_data.sqlite3", level="lsoa"){
  db = dbConnect(SQLite(), dbname=database_name)
  if(level=="lsoa"){
    population = tbl(db,"carr_hill_pop") %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% filter(YEAR>2014) %>% collect() 
    list = tbl(db,"gp_practice_payments_lsoa") %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
  } else {
    population = tbl(db,"gp_population") %>% select(YEAR,PRAC_CODE,TOTAL_POP,NEED_ADJ_POP) %>% filter(YEAR>2014) %>% collect() 
    list = tbl(db,"gp_practice_payments") %>% select(YEAR,PRAC_CODE,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    imd = tbl(db,"gp_imd_2019") %>% select(YEAR,PRAC_CODE,IMD_QUINTILE) %>% collect()
  }

  dbDisconnect(db)
  
  list_quintiles = inner_join(list,imd) %>%
    group_by(YEAR,IMD_QUINTILE) %>%
    summarise(LIST_POP=sum(PATIENTS),
              ADJ_LIST_POP=sum(WEIGHTED_PATIENTS))
  
  ons_quintiles = inner_join(population,imd) %>%
    group_by(YEAR,IMD_QUINTILE) %>%
    summarise(TOTAL_POP=sum(TOTAL_POP),
              NEED_ADJ_POP=sum(NEED_ADJ_POP))
    
  graph_data = ons_quintiles %>% 
    inner_join(list_quintiles) %>%
    gather(POP_TYPE,POP,contains("POP")) %>%
    mutate(POP_TYPE = factor(POP_TYPE,c("TOTAL_POP","NEED_ADJ_POP","LIST_POP","ADJ_LIST_POP"),
                             c("Total population (ONS)","Need adjusted population (ONS + Carr Hill 2007)","Practice list size (NHS Digital)","Weighted practice list size (NHS Digital)")),
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
    facet_wrap(POP_TYPE~.,scales="fixed",labeller = labeller(POP_TYPE = label_wrap_gen(60))) +
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/pop_trends_",level,".png"), pop_plot, width=25, height=30, units="cm", dpi="print")
  
}

graph_gp_trends = function(database_name="primary_care_data.sqlite3", imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  if(imputed){
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() 
    suffix = "_imputed"
  } else {
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
    suffix = "_not_imputed"
  }
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_raw",suffix,".png"), gp_plot_raw, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_pop",suffix,".png"), gp_plot_pop, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_pop_adj",suffix,".png"), gp_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

graph_gp_age_sex = function(database_name="primary_care_data.sqlite3", year=2018, imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  if(imputed){
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() %>% filter(YEAR==year) %>% select(IMD_QUINTILE,NEED_ADJ_POP,TOTAL_POP,contains("_HC_"))
    suffix = "_imputed"
  } else {
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() %>% filter(YEAR==year) %>% select(IMD_QUINTILE,NEED_ADJ_POP,TOTAL_POP,contains("_HC_"))
    suffix = "_not_imputed"
  }
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
         subtitle = paste0("Data for England in years ",year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_",year,suffix,".png"), gp_plot, width=35, height=25, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_pop_",year,suffix,".png"), gp_plot_pop, width=35, height=25, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  
  ggsave(paste0("figures/gp_age_sex_pop_adj_",year,suffix,".png"), gp_plot_pop_adj, width=35, height=25, units="cm", dpi="print")
}

graph_gp_trends_by_sex = function(database_name="primary_care_data.sqlite3", imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  if(imputed){
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() 
    suffix = "_imputed"
  } else {
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
    suffix = "_not_imputed"
  }
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_raw",suffix,".png"), gp_plot_sex_hc_raw, width=25, height=30, units="cm", dpi="print")
  
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_raw",suffix,".png"), gp_plot_sex_fte_raw, width=25, height=30, units="cm", dpi="print")
  
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_pop",suffix,".png"), gp_plot_sex_hc_pop, width=25, height=30, units="cm", dpi="print")
  
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_pop",suffix,".png"), gp_plot_sex_fte_pop, width=25, height=30, units="cm", dpi="print")
  
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_hc_pop_adj",suffix,".png"), gp_plot_sex_hc_pop_adj, width=25, height=30, units="cm", dpi="print")
  
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_trends_sex_fte_pop_adj",suffix,".png"), gp_plot_sex_fte_pop_adj, width=25, height=30, units="cm", dpi="print")
  
}

graph_all_staff_trends = function(database_name="primary_care_data.sqlite3", imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  if(imputed){
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() 
    suffix = "_imputed"
  } else {
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
    suffix = "_not_imputed"
  }
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_raw",suffix,".png"), all_staff_plot_raw, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_pop",suffix,".png"), all_staff_plot_pop, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_pop_adj",suffix,".png"), all_staff_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

graph_gp_locum_trends = function(database_name="primary_care_data.sqlite3", imputed=TRUE){
  db = dbConnect(SQLite(), dbname=database_name)
  if(imputed){
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles_imputed") %>% collect() 
    suffix = "_imputed"
  } else {
    gps_quintile = tbl(db, "gp_workforce_imd_quintiles") %>% collect() 
    suffix = "_not_imputed"
  }
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_raw",suffix,".png"), gp_plot_raw, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_pop",suffix,".png"), gp_plot_pop, width=25, height=35, units="cm", dpi="print")
  
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
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/gp_locum_trends_pop_adj",suffix,".png"), gp_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

opening_and_closing_practices = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gp_practices = tbl(db, "gp_workforce_newdata_imputed") %>% select(YEAR,PRAC_CODE,PRAC_NAME,TOTAL_GP_HC) %>% collect() 
  gp_imd = tbl(db, "gp_imd_2019") %>% select(PRAC_CODE,IMD_QUINTILE) %>% collect()
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



eth_perc = function(x){
  return (round((x/32844)*100,0))
}

ethnicity_plot = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  bame_q = tbl(db,"bame_lsoa") %>% select(LSOA11CD,BAME_Q) %>% collect()
  imd_q = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  eth_table = imd_q %>% 
    inner_join(bame_q) %>% 
    group_by(IMD_QUINTILE, BAME_Q) %>% 
    summarise(total=n()) %>% 
    spread(BAME_Q,total) %>% mutate(row_total=`1`+`2`+`3`+`4`+`5`) %>% 
    ungroup() %>% 
    select(IMD_QUINTILE,"BAME_Q1 (most BAME)" =`5`,"BAME_Q2" =`4`,"BAME_Q3" =`3`,"BAME_Q4" =`2`,"BAME_Q5 (least BAME)" =`1`,"Row Total"=row_total)
  eth_table = rbind(eth_table,c("col_total",eth_table[,-1] %>% colSums()))
  eth_table = eth_table %>% 
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,c(1:5,"col_total"),c("IMD_Q1 (most deprived)","IMD_Q2","IMD_Q3","IMD_Q4","IMD_Q5 (least deprived)","Column Total"))) 
  
  eth_table_perc = eth_table %>% 
    mutate_if(is.character,as.integer) %>%
    mutate_if(is.integer,eth_perc)
  
  write_csv(eth_table_perc,"figures/ethnicity_dist.csv")
}

graph_all_staff_trends_ethnicity = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps_lsoa = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP,
                                                                     TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE,
                                                                     TOTAL_NURSES_HC, TOTAL_NURSES_FTE,
                                                                     TOTAL_DPC_HC, TOTAL_DPC_FTE,
                                                                     TOTAL_ADMIN_HC, TOTAL_ADMIN_FTE) %>% collect() 
  bame_q = tbl(db,"bame_lsoa") %>% select(LSOA11CD,BAME_Q) %>% collect()
  imd_q = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  graph_data = gps_lsoa %>%
    inner_join(bame_q) %>% 
    inner_join(imd_q) %>%
    filter(IMD_QUINTILE==1) %>%
    select(YEAR,BAME_Q,TOTAL_POP,NEED_ADJ_POP,
           TOTAL_GPEXRRL_HC=TOTAL_GP_EXRRL_HC, TOTAL_GPEXRRL_FTE=TOTAL_GP_EXRRL_FTE,
           TOTAL_NURSES_HC, TOTAL_NURSES_FTE,
           TOTAL_DPC_HC, TOTAL_DPC_FTE,
           TOTAL_ADMIN_HC, TOTAL_ADMIN_FTE
    ) %>%
    group_by(YEAR,BAME_Q) %>%
    summarise(TOTAL_POP=sum(TOTAL_POP),NEED_ADJ_POP=sum(NEED_ADJ_POP),
              TOTAL_GPEXRRL_HC=sum(TOTAL_GPEXRRL_HC), TOTAL_GPEXRRL_FTE=sum(TOTAL_GPEXRRL_FTE),
              TOTAL_NURSES_HC=sum(TOTAL_NURSES_HC), TOTAL_NURSES_FTE=sum(TOTAL_NURSES_FTE),
              TOTAL_DPC_HC=sum(TOTAL_DPC_HC), TOTAL_DPC_FTE=sum(TOTAL_DPC_FTE),
              TOTAL_ADMIN_HC=sum(TOTAL_ADMIN_HC), TOTAL_ADMIN_FTE=sum(TOTAL_ADMIN_FTE)) %>%
    ungroup() %>%
    gather(VARIABLE,VALUE,TOTAL_GPEXRRL_HC:TOTAL_ADMIN_FTE) %>%
    separate(VARIABLE,c("x","VARIABLE","TYPE")) %>%
    select(-x) %>%
    mutate(BAME_QUINTILE = factor(BAME_Q,5:1, c("Q1 (most BAME)","Q2","Q3","Q4","Q5 (least BAME)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GPEXRRL","NURSES","DPC","ADMIN"),
                             c("GPs excluding registrars, retainers and locums","Nurses","Direct patient care staff","Admin staff")))
  
  
  bame_labels = c("Q1 (most BAME)","Q2","Q3","Q4","Q5 (least BAME)")
  start_year=2015
  end_year=2018
 
  all_staff_plot_pop_adj = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/NEED_ADJ_POP), group=BAME_QUINTILE, colour=BAME_QUINTILE) + 
    geom_line(aes(linetype=BAME_QUINTILE, size=BAME_QUINTILE)) + 
    geom_point(aes(shape=BAME_QUINTILE, colour=BAME_QUINTILE)) +
    xlab("Year") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="BAME Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=bame_labels) +
    scale_shape_manual(name="BAME Quintile Group", values=c(19,21,24,0,15), labels=bame_labels) +
    scale_linetype_manual(name="BAME Quintile Group", values=c(1,2,2,2,1), labels=bame_labels) +
    scale_size_manual(name="BAME Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=bame_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          text=element_text(family = "Roboto", colour = "#3e3f3a"),
          legend.position = "bottom") +
    labs(title = "General practice workforce supply by ethnicity per 100,000 need adjusted pop in most deprived fifth of LSOAs",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("figures/all_staff_trends_imd1_bame_pop_adj.png"), all_staff_plot_pop_adj, width=25, height=35, units="cm", dpi="print")
}

plot_workforce_gp_age = function(database_name="primary_care_data.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps = tbl(db,"gp_workforce_newdata_imputed") %>% filter(YEAR==2018) %>% select(PRAC_CODE, contains("GP_EXRRL_HC")) %>% collect()
  imd = tbl(db,"gp_imd_2019") %>% filter(YEAR==2018) %>% select(PRAC_CODE,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  
  graph_data = gps %>%
    inner_join(imd) %>% 
    gather(VARNAME,VALUE,MALE_GP_EXRRL_HC_UNDER30:TOTAL_GP_EXRRL_HC_UNKNOWN_AGE) %>%
    separate(VARNAME,c("SEX","A","B","C","AGE")) %>%
    select(PRAC_CODE, TOTAL_GP_EXRRL_HC, IMD_QUINTILE,SEX,AGE,GP_EXRRL_HC=VALUE) %>%
    mutate(AGE = age_range_to_integer(AGE),
           SEX = factor(SEX,c("FEMALE","MALE", "TOTAL"),c("Female","Male", "Total")))
  
  graph_data_total = graph_data %>% filter(SEX=="Total") %>% group_by(PRAC_CODE, IMD_QUINTILE, TOTAL_GP_EXRRL_HC) %>%
    summarise(AGE=weighted.mean(AGE,GP_EXRRL_HC,na.rm=TRUE))
  
  ggplot(data=graph_data_total, mapping=aes(x=TOTAL_GP_EXRRL_HC,y=AGE)) +
    geom_point() + 
    geom_smooth(method="lm")
}
library("tidyverse")
library("RSQLite")
library("lubridate")
library("scales")
library("qpdf")
library("tmap")
library("tmaptools")
library("sf")

imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")

##############################################################################################################
# lsoa level attributed data (level="lsoa) or practice level actual data (level="practice")
# lsoa level ONS population (population="ons) or practice list size (population="list")
##############################################################################################################
plot_payments_data = function(database_name="primary_care_data.sqlite3", level="practice", population="list", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  if(level=="lsoa"){
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE)  %>% collect()
    payments = tbl(db, "gp_practice_payments_lsoa") %>% collect()
    pop = tbl(db,"carr_hill_pop") %>% filter(YEAR>2014) %>% collect()
  } else {
    imd = tbl(db,"gp_imd_2019") %>% select(PRAC_CODE,YEAR,IMD_QUINTILE) %>% filter(YEAR>2014) %>% collect()
    payments = tbl(db, "gp_practice_payments") %>% collect()
    pop = tbl(db,"gp_population") %>% filter(YEAR>2014) %>% collect()
  }
  dbDisconnect(db)
  
  if(population=="ons"){
    raw = "TOTAL_POP"
    adj = "NEED_ADJ_POP"
  } else {
    raw = "PATIENTS" 
    adj = "WEIGHTED_PATIENTS"
  }
  
  graph_data = inner_join(imd, payments) %>% 
    inner_join(pop) %>%
    group_by(YEAR,IMD_QUINTILE) %>% 
    summarise(PATIENTS=sum(get(raw)),
              WEIGHTED_PATIENTS=sum(get(adj)),
              TOTAL_PAYMENTS=sum(TOTAL_PAYMENTS),
              NET_PAYMENTS=sum(NET_PAYMENTS)) %>%
    mutate(UNWEIGHTED_PAYMENT=TOTAL_PAYMENTS/PATIENTS,
           UNWEIGHTED_NETPAYMENT=NET_PAYMENTS/PATIENTS,
           WEIGHTED_PAYMENT=TOTAL_PAYMENTS/WEIGHTED_PATIENTS,
           WEIGHTED_NETPAYMENT=NET_PAYMENTS/WEIGHTED_PATIENTS) %>%
    select(YEAR,IMD_QUINTILE,ends_with("PAYMENT")) %>%
    gather(PAYTYPE,VALUE,-c(YEAR,IMD_QUINTILE)) %>%
    separate(PAYTYPE,c("W","N")) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           W = factor(W, c("UNWEIGHTED","WEIGHTED"), c("Raw","Need Adjusted")),
           N = factor(N,c("PAYMENT","NETPAYMENT"),c("Total Payment","Net Payment")))
  
  start_year=2015
  end_year=2018
  pay_plot = ggplot(graph_data %>% filter(N == "Net Payment", W == "Need Adjusted")) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Net payments per registered weighted patient (\u00a3)") +
    #facet_grid(.~W,scales="fixed") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw(base_size = 6) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in practice payments per patient by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year,"/",(start_year+1)-2000," - ",end_year,"/",(end_year+1)-2000," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital, ONS and MHCLG quintiles aggregated from LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/payment_trends_",level,"_",population,".",output_type), pay_plot, width=10, height=10, units="cm", dpi="print")
}

plot_payments_data_london = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE)  %>% collect()
    payments = tbl(db, "gp_practice_payments_lsoa") %>% collect()
    london_lsoa = tbl(db,"london_lsoas") %>% collect()
  dbDisconnect(db)
               
    raw = "PATIENTS" 
    adj = "WEIGHTED_PATIENTS"
  
  graph_data = inner_join(imd, payments) %>% 
    inner_join(london_lsoa) %>%
    group_by(YEAR,IMD_QUINTILE,LONDON) %>% 
    summarise(PATIENTS=sum(get(raw)),
              WEIGHTED_PATIENTS=sum(get(adj)),
              TOTAL_PAYMENTS=sum(TOTAL_PAYMENTS),
              NET_PAYMENTS=sum(NET_PAYMENTS)) %>%
    ungroup() %>%
    mutate(UNWEIGHTED_PAYMENT=TOTAL_PAYMENTS/PATIENTS,
           UNWEIGHTED_NETPAYMENT=NET_PAYMENTS/PATIENTS,
           WEIGHTED_PAYMENT=TOTAL_PAYMENTS/WEIGHTED_PATIENTS,
           WEIGHTED_NETPAYMENT=NET_PAYMENTS/WEIGHTED_PATIENTS) %>%
    select(YEAR,IMD_QUINTILE, LONDON,ends_with("PAYMENT")) %>%
    gather(PAYTYPE,VALUE,-c(YEAR,IMD_QUINTILE,LONDON)) %>%
    separate(PAYTYPE,c("W","N")) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           W = factor(W, c("UNWEIGHTED","WEIGHTED"), c("Raw","Need Adjusted")),
           N = factor(N,c("PAYMENT","NETPAYMENT"),c("Total Payment","Net Payment")))
  
  start_year=2015
  end_year=2018
  pay_plot = ggplot(subset(graph_data,LONDON==1)) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Payments per registered patient (\u00a3)") +
    facet_grid(W~N,scales="fixed") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in practice payments per patient by neighbourhood deprivation in London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (payments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/payment_trends_lsoa_list_london.",output_type), pay_plot, width=20, height=20, units="cm", dpi="print")
  
  pay_plot = ggplot(subset(graph_data,LONDON==0)) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Payments per registered patient (\u00a3)") +
    facet_grid(W~N,scales="fixed") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in practice payments per patient by neighbourhood deprivation outside London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (payments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/payment_trends_lsoa_list_non_london.",output_type), pay_plot, width=20, height=20, units="cm", dpi="print")
}

plot_appointments_data = function(database_name="primary_care_data.sqlite3", population="reported", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  appt = tbl(db,"appointment_data") %>% collect()
  denom = tbl(db,"appointment_data_coverage") %>% select(YEAR,MONTH,CCG19CD,PATIENTS_INCLUDED,PATIENT_OPEN) %>% collect()
  imd = tbl(db,"ccg_imd_2019") %>% select(CCG19CD,IMD_QUINTILE) %>% collect()
  ons_pop = tbl(db,"ccg_pop") %>% filter(YEAR==2018) %>% select(CCG19CD,NEED_ADJ_POP) %>% collect()
  list_pop = tbl(db, "gp_practice_payments_lsoa") %>% filter(YEAR==2018) %>% select(LSOA11CD,WEIGHTED_PATIENTS) %>% collect()
  ccg_lsoa = tbl(db, "ccg_lsoa_lad_mapping") %>% select(CCG19CD,LSOA11CD) %>% collect()
  dbDisconnect(db)
  
  pop = list_pop %>% 
    inner_join(ccg_lsoa) %>% 
    group_by(CCG19CD) %>%
    summarise(WEIGHTED_PATIENTS=sum(WEIGHTED_PATIENTS)) %>%
    ungroup() %>%
    inner_join(ons_pop) %>% 
    inner_join(denom) %>% 
    mutate(population_adjustment_factor=PATIENTS_INCLUDED/PATIENT_OPEN,
           WEIGHTED_PATIENTS=WEIGHTED_PATIENTS*population_adjustment_factor,
           NEED_ADJ_POP=NEED_ADJ_POP*population_adjustment_factor,
           DATE=ymd(paste0(YEAR,case_when(MONTH >=1 & MONTH <=3 ~ "03",
                                          MONTH >=4 & MONTH <=6 ~ "06",
                                          MONTH >=7 & MONTH <=9 ~ "09",
                                          MONTH >=10 & MONTH <=12 ~ "12"),"15"))) %>%
    group_by(CCG19CD,DATE) %>%
    summarise(WEIGHTED_PATIENTS=sum(WEIGHTED_PATIENTS),
              NEED_ADJ_POP=sum(NEED_ADJ_POP),
              PATIENTS_INCLUDED=sum(PATIENTS_INCLUDED))
  
  imd = imd %>% mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5,imd_labels))
  
  if(population=="ons"){
    pop = pop %>%
      mutate(PATIENTS=NEED_ADJ_POP)
  } else if(population=="list"){
    pop = pop %>%
      mutate(PATIENTS=WEIGHTED_PATIENTS)
  } else if(population=="reported"){
    pop = pop %>%
      mutate(PATIENTS=PATIENTS_INCLUDED)
  }
  
  # group into quaters
  appt = appt %>%
    mutate(DATE=ymd(paste0(YEAR,case_when(MONTH >=1 & MONTH <=3 ~ "03",
                                          MONTH >=4 & MONTH <=6 ~ "06",
                                          MONTH >=7 & MONTH <=9 ~ "09",
                                          MONTH >=10 & MONTH <=12 ~ "12"),"15"))) %>%
    select(-YEAR,-MONTH,-WEEK,-DAY)
  
  graph_data_0 = appt %>% 
    group_by(DATE,CCG19CD) %>% 
    summarise(APPTS=sum(COUNT)) %>% 
    ungroup() %>%
    inner_join(pop) %>% 
    inner_join(imd) %>% 
    group_by(DATE,IMD_QUINTILE) %>%
    summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
    mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2)) %>%
    ungroup() %>%
    select(DATE,IMD_QUINTILE,APPTS,PATIENTS,APPTS_PER_100_PATS) %>%
    gather("VAR","VAL",c(APPTS,PATIENTS,APPTS_PER_100_PATS)) %>%
    mutate(VAR = factor(VAR,levels=c("APPTS","PATIENTS","APPTS_PER_100_PATS"),labels=c("Appointments","Patients","Appointments per 100 patients")))
  
  graph_data_1 = appt %>% 
    mutate(HCP = if_else(HCP=="Unknown","Other Practice staff",HCP)) %>%
    group_by(DATE,CCG19CD,HCP) %>% 
    summarise(APPTS=sum(COUNT)) %>% 
    ungroup() %>%
    inner_join(pop) %>% 
    inner_join(imd) %>% 
    group_by(DATE,HCP,IMD_QUINTILE) %>%
    summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
    mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2)) %>%
    ungroup()
  
  graph_data_2 = appt %>% 
    filter(!(MODE %in% c("Unknown","Video Conference/Online","Home Visit"))) %>%
    group_by(DATE,CCG19CD,MODE) %>% 
    summarise(APPTS=sum(COUNT)) %>% 
    ungroup() %>%
    inner_join(pop) %>% 
    inner_join(imd) %>% 
    group_by(DATE,MODE,IMD_QUINTILE) %>%
    summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
    mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2)) %>%
    ungroup() %>%
    mutate(MODE = factor(MODE,c("Face-to-Face","Telephone"),c("Face-to-Face","Telephone")))
  
  graph_data_3 = appt %>% 
    filter(STATUS=="DNA" & DATE == ymd("20191215")) %>%
    inner_join(imd) %>%
    group_by(DATE,IMD_QUINTILE) %>% 
    summarise(DNA=sum(COUNT)) %>% 
    ungroup() %>%
  inner_join(graph_data_0 %>% filter(VAR=="Appointments")) %>%
    mutate(DNA=DNA/VAL) %>%
    select(DATE,IMD_QUINTILE,DNA)
  
  
  raw_plot = ggplot(graph_data_0) + 
    aes(x=DATE, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Date") +
    ylab("") +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y")) + 
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(VAR~.,scales="free_y", nrow=3) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x=element_text(angle=90),
          legend.position = "top") +
    labs(title = "Trends in primary care appointments by neighbourhood deprivation",
         subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/appointments_raw_",population,".",output_type), raw_plot, width=25, height=35, units="cm", dpi="print")
  
  
  hcp_plot = ggplot(graph_data_1) + 
    aes(x=DATE, y=APPTS_PER_100_PATS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Date") +
    ylab("Appointements per 100 patients per month") +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y")) + 
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(.~HCP) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x=element_text(angle=90),
          
          legend.position = "top") +
    labs(title = "Trends in primary care appointments by neighbourhood deprivation",
         subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (appointments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  ggsave(paste0("briefing/appointments_hcp_",population,".",output_type), hcp_plot, width=25, height=15, units="cm", dpi="print")
  
  mode_plot = ggplot(graph_data_2) + 
    aes(x=DATE, y=APPTS_PER_100_PATS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Date") +
    ylab("Appointements per 100 patients per month") +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y")) + 
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_wrap(.~MODE, scales = "free") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          axis.text.x=element_text(angle=90),
          legend.position = "top") +
    labs(title = "Trends in primary care appointments by neighbourhood deprivation",
         subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/appointments_mode_",population,".",output_type), mode_plot, width=35, height=15, units="cm", dpi="print")
  
  dna_plot = ggplot(graph_data_3) + 
    aes(x=IMD_QUINTILE, y=DNA) + 
    geom_col() +
    xlab("Deprivation") +
    ylab("Did not attend (% of all appointments)") +
    scale_y_continuous(labels = percent) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in primary care appointments by neighbourhood deprivation",
         subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/appointments_status_",population,".",output_type), dna_plot, width=20, height=15, units="cm", dpi="print")
}

plot_workforce_mix = function(database_name="primary_care_data.sqlite3", level="lsoa", population="ons", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  if(level=="lsoa"){
    gps = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>% 
      filter(YEAR>2014) %>% 
      select(YEAR,
             LSOA11CD,
             TOTAL_GP_EXRRL_HC,
             TOTAL_GP_EXRRL_FTE,
             TOTAL_NURSES_HC, 
             TOTAL_NURSES_FTE,
             TOTAL_DPC_HC,
             TOTAL_DPC_FTE,
             TOTAL_ADMIN_HC,
             TOTAL_ADMIN_FTE) %>% 
      collect()
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
    ons_pop = tbl(db,"carr_hill_pop") %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    list_pop = tbl(db,"gp_practice_payments_lsoa") %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
   } else {
    gps = tbl(db, "gp_workforce_newdata_imputed") %>% 
      filter(YEAR>2014) %>% 
      select(YEAR,
             PRAC_CODE,
             TOTAL_GP_EXRRL_HC,
             TOTAL_GP_EXRRL_FTE,
             TOTAL_NURSES_HC, 
             TOTAL_NURSES_FTE,
             TOTAL_DPC_HC,
             TOTAL_DPC_FTE,
             TOTAL_ADMIN_HC,
             TOTAL_ADMIN_FTE) %>% 
      collect() 
    imd = tbl(db,"gp_imd_2019") %>% select(YEAR,PRAC_CODE,IMD_QUINTILE) %>% filter(YEAR>2014) %>% collect()
    ons_pop = tbl(db, "gp_population") %>% select(YEAR,PRAC_CODE,TOTAL_POP,NEED_ADJ_POP) %>% collect()
    list_pop = tbl(db,"gp_practice_payments") %>% select(YEAR,PRAC_CODE,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
  }
  dbDisconnect(db)
  
  if(population=="ons"){
    pop_var = "NEED_ADJ_POP"
  } else {
    pop_var = "WEIGHTED_PATIENTS"
  }
  
  gps_quintile = gps %>% 
    inner_join(imd) %>%
    inner_join(ons_pop) %>%
    inner_join(list_pop) %>%
    group_by(YEAR,IMD_QUINTILE) %>%
    summarise_at(vars(one_of(c("TOTAL_GP_EXRRL_HC",
                               "TOTAL_GP_EXRRL_FTE",
                               "TOTAL_NURSES_HC", 
                               "TOTAL_NURSES_FTE",
                               "TOTAL_DPC_HC",
                               "TOTAL_DPC_FTE",
                               "TOTAL_ADMIN_HC",
                               "TOTAL_ADMIN_FTE",
                               pop_var))), sum, na.rm=TRUE)
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,
           pop_var,
           TOTAL_GPEXRRL_HC=TOTAL_GP_EXRRL_HC, 
           TOTAL_GPEXRRL_FTE=TOTAL_GP_EXRRL_FTE,
           TOTAL_NURSES_HC, 
           TOTAL_NURSES_FTE,
           TOTAL_DPC_HC,
           TOTAL_DPC_FTE,
           TOTAL_ADMIN_HC,
           TOTAL_ADMIN_FTE
    ) %>%
    gather(VARIABLE,VALUE,TOTAL_GPEXRRL_HC:TOTAL_ADMIN_FTE) %>%
    separate(VARIABLE,c("x","VARIABLE","TYPE")) %>%
    select(-x) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GPEXRRL","NURSES","DPC","ADMIN"),
                             c("GPs excluding registrars, retainers and locums","Nurses","Direct patient care staff","Administrative staff")))
  start_year=2015
  end_year=2018
  all_staff_plot_raw = ggplot(graph_data) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw(base_size = 15) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year,"/",(start_year+1)-2000," - ",end_year,"/",(end_year+1)-2000," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital, ONS and MHCLG quintiles aggregated from LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_raw_",level,".",output_type), all_staff_plot_raw, width=25, height=35, units="cm", dpi="print")
  
  all_staff_plot_pop = ggplot(graph_data) + 
    aes(x=YEAR, y=(100000*VALUE/get(pop_var)), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff per 100,000 population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply per 100,000 population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year,"/",(start_year+1)-2000," - ",end_year,"/",(end_year+1)-2000," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital, ONS and MHCLG quintiles aggregated from LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_",level,"_",population,".",output_type), all_staff_plot_pop, width=25, height=35, units="cm", dpi="print")
  
  fte_staff_plot_pop = ggplot(
    subset(graph_data, TYPE=="Full time equivalent" & 
             VARIABLE %in% c("GPs excluding registrars, retainers and locums","Nurses","Direct patient care staff"))) + 
    aes(x=YEAR, y=(100000*VALUE/get(pop_var)), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff (FTE) per 100,000 weighted population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(.~VARIABLE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw(base_size = 8) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply per 100,000 population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year,"/",(start_year+1)-2000," - ",end_year,"/",(end_year+1)-2000," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital, ONS and MHCLG quintiles aggregated from LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_fte_",level,"_",population,".",output_type), fte_staff_plot_pop, width=20, height=10, units="cm", dpi="print")
  
}

plot_workforce_mix_london = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
    gps = tbl(db, "gp_workforce_newdata_lsoa_imputed") %>% 
      filter(YEAR>2014) %>% 
      select(YEAR,
             LSOA11CD,
             TOTAL_GP_EXRRL_HC,
             TOTAL_GP_EXRRL_FTE,
             TOTAL_NURSES_HC, 
             TOTAL_NURSES_FTE) %>% 
      collect()
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
    ons_pop = tbl(db,"carr_hill_pop") %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    london_lsoa = tbl(db,"london_lsoas") %>% collect()
  dbDisconnect(db)

    pop_var = "NEED_ADJ_POP"

  
  gps_quintile = gps %>% 
    inner_join(imd) %>%
    inner_join(ons_pop) %>%
    inner_join(london_lsoa) %>%
    group_by(YEAR,IMD_QUINTILE,LONDON) %>%
    summarise_at(vars(one_of(c("TOTAL_GP_EXRRL_HC",
                               "TOTAL_GP_EXRRL_FTE",
                               "TOTAL_NURSES_HC", 
                               "TOTAL_NURSES_FTE",
                               pop_var))), sum, na.rm=TRUE) %>%
    ungroup()
  
  graph_data = gps_quintile %>%
    select(YEAR,IMD_QUINTILE,LONDON,
           pop_var,
           TOTAL_GPEXRRL_HC=TOTAL_GP_EXRRL_HC, 
           TOTAL_GPEXRRL_FTE=TOTAL_GP_EXRRL_FTE,
           TOTAL_NURSES_HC, 
           TOTAL_NURSES_FTE
    ) %>%
    gather(VARIABLE,VALUE,TOTAL_GPEXRRL_HC:TOTAL_NURSES_FTE) %>%
    separate(VARIABLE,c("x","VARIABLE","TYPE")) %>%
    select(-x) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           TYPE = factor(TYPE, c("HC","FTE"),c("Headcount","Full time equivalent")),
           VARIABLE = factor(VARIABLE,
                             c("GPEXRRL","NURSES"),
                             c("GPs excluding registrars, retainers and locums","Nurses")))
  start_year=2015
  end_year=2018
  all_staff_plot_raw = ggplot(subset(graph_data,LONDON==1)) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply by neighbourhood deprivation in London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_raw_lsoa_london.",output_type), all_staff_plot_raw, width=25, height=35, units="cm", dpi="print")
  
  all_staff_plot_raw = ggplot(subset(graph_data,LONDON==0)) + 
    aes(x=YEAR, y=VALUE, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply by neighbourhood deprivation outside London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_raw_lsoa_non_london.",output_type), all_staff_plot_raw, width=25, height=35, units="cm", dpi="print")
  
  
  all_staff_plot_pop = ggplot(subset(graph_data,LONDON==1)) + 
    aes(x=YEAR, y=(100000*VALUE/get(pop_var)), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff per 100,000 population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply per 100,000 population by neighbourhood deprivation in London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_lsoa_ons_london.",output_type), all_staff_plot_pop, width=25, height=35, units="cm", dpi="print")
  
  all_staff_plot_pop = ggplot(subset(graph_data,LONDON==0)) + 
    aes(x=YEAR, y=(100000*VALUE/get(pop_var)), group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Numbers of staff per 100,000 population") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    facet_grid(VARIABLE~TYPE,scales="free",labeller = labeller(VARIABLE = label_wrap_gen(40))) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in general practice workforce supply per 100,000 population by neighbourhood deprivation outside London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/workforce_lsoa_ons_non_london.",output_type), all_staff_plot_pop, width=25, height=35, units="cm", dpi="print")
  
}

plot_gpps = function(database_name="primary_care_data.sqlite3", level="lsoa", population="ons", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  
  if (level=="lsoa") {
    ons_pop = tbl(db,"carr_hill_pop") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    list_pop = tbl(db,"gp_practice_payments_lsoa") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    imd = tbl(db, "imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
    gpps = tbl(db, "gpps_lsoa") %>% collect()
  } else {
    ons_pop = tbl(db, "gp_population") %>% select(YEAR,PRAC_CODE,TOTAL_POP,NEED_ADJ_POP) %>% collect()
    list_pop = tbl(db,"gp_practice_payments") %>% filter(YEAR>2014) %>% select(YEAR,PRAC_CODE,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    imd = tbl(db, "gp_imd_2019") %>% select(YEAR,PRAC_CODE,IMD_QUINTILE) %>% filter(YEAR>2014) %>% collect()
    gpps = tbl(db, "gpps") %>% collect()
  }
  dbDisconnect(db)
  
  if(population=="ons"){
    graph_data = gpps %>% 
      inner_join(ons_pop) %>% 
      inner_join(imd) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(GPPS=weighted.mean(GPPS_GOOD,NEED_ADJ_POP)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  } else {
    graph_data = gpps %>% 
      inner_join(list_pop) %>% 
      inner_join(imd) %>%
      group_by(YEAR,IMD_QUINTILE) %>%
      summarise(GPPS=weighted.mean(GPPS_GOOD,WEIGHTED_PATIENTS)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  }
  
  start_year=2015
  end_year=2018
  gpps_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=GPPS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Percentage of patients rating practice very good or fairly good") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in GPPS score by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/gpps_trends_",level,"_",population,".",output_type), gpps_plot, width=20, height=20, units="cm", dpi="print")
}

plot_gpps_london = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)

    ons_pop = tbl(db,"carr_hill_pop") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    list_pop = tbl(db,"gp_practice_payments_lsoa") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    imd = tbl(db, "imd_2019") %>% select(LSOA11CD,IMD_QUINTILE) %>% collect()
    gpps = tbl(db, "gpps_lsoa") %>% collect()
    london_lsoa = tbl(db,"london_lsoas") %>% collect()
  dbDisconnect(db)
  
  graph_data = gpps %>% 
      inner_join(ons_pop) %>% 
      inner_join(imd) %>%
      inner_join(london_lsoa) %>%
      group_by(YEAR,IMD_QUINTILE,LONDON) %>%
      summarise(GPPS=weighted.mean(GPPS_GOOD,NEED_ADJ_POP)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
 
  
  start_year=2015
  end_year=2018
  gpps_plot = ggplot(subset(graph_data,LONDON==1)) + 
    aes(x=YEAR, y=GPPS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Percentage of patients rating practice very good or fairly good") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in GPPS score by neighbourhood deprivation in London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/gpps_trends_lsoa_ons_london.",output_type), gpps_plot, width=20, height=20, units="cm", dpi="print")
  
  gpps_plot = ggplot(subset(graph_data,LONDON==0)) + 
    aes(x=YEAR, y=GPPS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Percentage of patients rating practice very good or fairly good") +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in GPPS score by neighbourhood deprivation outside London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/gpps_trends_lsoa_ons_non_london.",output_type), gpps_plot, width=20, height=20, units="cm", dpi="print")
}

plot_cqc = function(database_name="primary_care_data.sqlite3", output_type="png"){
  
  db = dbConnect(SQLite(), dbname=database_name)
  cqc = tbl(db,"cqc") %>% collect()
  imd = tbl(db,"gp_imd_2019") %>% filter(YEAR==2018) %>% select(PRAC_CODE,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  graph_data = cqc %>% 
    inner_join(imd) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")),
           CQC_OVERALL = factor(CQC_OVERALL,c("Inadequate","Requires improvement","Good","Outstanding"),c("Inadequate","Requires improvement","Good","Outstanding")))
  
  graph_data = graph_data %>% 
    group_by(IMD_QUINTILE) %>% 
    summarise(N=n()) %>% 
    ungroup() %>% 
    inner_join(graph_data) %>% 
    group_by(IMD_QUINTILE,CQC_OVERALL,N) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    mutate(prop = n/N)
  
  cqc_plot = ggplot(graph_data, aes(x=IMD_QUINTILE,y=prop)) +
    geom_col() +
    xlab("Deprivation") +
    ylab("Percentage of practices in deprivation group awarded CQC rating") +
    scale_y_continuous(labels = percent) +
    facet_wrap(~CQC_OVERALL, scales="free")  +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in CQC rating by neighbourhood deprivation",
         subtitle = paste0("Latest rating data for England from ",2020," deprivation based on IMD 2019 quintiles"),
         caption = "Note: Data are from the Care Quality Commission (CQC) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/cqc_practice",".",output_type), cqc_plot, width=25, height=20, units="cm", dpi="print")
  
  cqc_plot_2 = ggplot(subset(graph_data,CQC_OVERALL %in% c("Inadequate","Requires improvement","Outstanding")), aes(x=IMD_QUINTILE,y=prop)) +
    geom_col() +
    xlab("Deprivation") +
    ylab("Percentage of practices in deprivation group awarded CQC rating") +
    scale_y_continuous(labels = percent) +
    facet_wrap(~CQC_OVERALL, nrow=1) +
    theme_bw(base_size = 8) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in CQC rating by neighbourhood deprivation",
         subtitle = paste0("Latest rating data for England from ",2020," deprivation based on IMD 2019 quintiles"),
         caption = "Note: Data are from the Care Quality Commission (CQC) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/cqc_practice_2",".",output_type), cqc_plot_2, width=30, height=10, units="cm", dpi="print")
  
}

plot_qof_data = function(database_name="primary_care_data.sqlite3", level="lsoa", population="ons", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  if(level=="lsoa"){
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE)  %>% collect()
    points = tbl(db, "gp_practice_qof_lsoa") %>% collect()
    ons_pop = tbl(db,"carr_hill_pop") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    list_pop = tbl(db,"gp_practice_payments_lsoa") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
  } else {
    imd = tbl(db,"gp_imd_2019") %>% select(PRAC_CODE,YEAR,IMD_QUINTILE) %>% filter(YEAR>2014) %>% collect()
    points = tbl(db, "gp_practice_qof") %>% collect()
    ons_pop = tbl(db, "gp_population") %>% select(YEAR,PRAC_CODE,TOTAL_POP,NEED_ADJ_POP) %>% collect()
    list_pop = tbl(db,"gp_practice_payments") %>% filter(YEAR>2014) %>% select(YEAR,PRAC_CODE,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
  }
  dbDisconnect(db)
  
  if(population=="ons"){
    graph_data = inner_join(imd, points) %>% 
      inner_join(ons_pop) %>%
      group_by(YEAR,IMD_QUINTILE) %>% 
      summarise(POINTS=weighted.mean(TOTAL_POINTS,NEED_ADJ_POP)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  } else {
    graph_data = inner_join(imd, points) %>% 
      inner_join(list_pop) %>%
      group_by(YEAR,IMD_QUINTILE) %>% 
      summarise(POINTS=weighted.mean(TOTAL_POINTS,WEIGHTED_PATIENTS)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  }
  
  start_year=2015
  end_year=2018
  qof_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=POINTS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Total QOF Points") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in QOF points by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/qof_trends_",level,"_",population,".",output_type), qof_plot, width=20, height=20, units="cm", dpi="print")
}

plot_qof_data_london = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
    imd = tbl(db,"imd_2019") %>% select(LSOA11CD,IMD_QUINTILE)  %>% collect()
    points = tbl(db, "gp_practice_qof_lsoa") %>% collect()
    ons_pop = tbl(db,"carr_hill_pop") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,TOTAL_POP,NEED_ADJ_POP) %>% collect() 
    list_pop = tbl(db,"gp_practice_payments_lsoa") %>% filter(YEAR>2014) %>% select(YEAR,LSOA11CD,PATIENTS,WEIGHTED_PATIENTS) %>% collect()
    london_lsoa = tbl(db,"london_lsoas") %>% collect()
  dbDisconnect(db)

    graph_data = inner_join(imd, points) %>% 
      inner_join(ons_pop) %>%
      inner_join(london_lsoa) %>%
      group_by(YEAR,IMD_QUINTILE,LONDON) %>% 
      summarise(POINTS=weighted.mean(TOTAL_POINTS,NEED_ADJ_POP)) %>%
      ungroup() %>%
      mutate(IMD_QUINTILE = factor(IMD_QUINTILE, 1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
  
  start_year=2015
  end_year=2018
  qof_plot = ggplot(subset(graph_data,LONDON==1)) + 
    aes(x=YEAR, y=POINTS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Total QOF Points") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in QOF points by neighbourhood deprivation in London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/qof_trends_lsoa_ons_london.",output_type), qof_plot, width=20, height=20, units="cm", dpi="print")
  
  qof_plot = ggplot(subset(graph_data,LONDON==0)) + 
    aes(x=YEAR, y=POINTS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("Total QOF Points") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          
          legend.position = "top") +
    labs(title = "Trends in QOF points by neighbourhood deprivation outside London",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (QOF) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/qof_trends_lsoa_ons_non_london.",output_type), qof_plot, width=20, height=20, units="cm", dpi="print")
}

plot_gp_prac_size = function(database_name="primary_care_data.sqlite3", year=2018, measure="FTE", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  gps = tbl(db, "gp_workforce_newdata_imputed")  %>% filter(YEAR==year) %>% select(PRAC_CODE,TOTAL_GP_EXRRL_FTE,TOTAL_GP_EXRRL_HC) %>% collect()
  imd = tbl(db,"gp_imd_2019") %>% filter(YEAR==year) %>% select(PRAC_CODE, IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  graph_data = gps %>% inner_join(imd) %>%
    mutate(IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
  pracsize_fte_plot = ggplot(filter(graph_data,get(paste0("TOTAL_GP_EXRRL_",measure))<=20), aes(get(paste0("TOTAL_GP_EXRRL_",measure)), stat(density), colour=IMD_QUINTILE, linetype=IMD_QUINTILE)) +
    geom_freqpoly(binwidth =1, alpha=1) +
    ylab("Proportion of practices in deprivation group") +
    xlab(paste0("GP practice size - GPs excluding registrars, retainers and locums (",measure,")")) +
    scale_y_continuous(labels = percent) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "GP practice size by neighbourhood deprivation quintile",
         subtitle = paste0("Data for England in year ",year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/practice_size_",year,"_",measure,".",output_type), pracsize_fte_plot, width=25, height=25, units="cm", dpi="print")
  
  single_handers = graph_data %>% 
    filter(TOTAL_GP_EXRRL_HC==1) %>%
    group_by(IMD_QUINTILE) %>%
    summarise(sh = n())
  
  sh_plot = ggplot(single_handers, aes(x=IMD_QUINTILE,y=sh)) +
    geom_col() +
    xlab("Deprivation") +
    ylab("Number of practices staffed by a single GP") +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Practices run by a single GP by neighbourhood deprivation",
         subtitle = paste0("Data for England in year ",year,"/",(year+1)-2000," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital, ONS and MHCLG quintiles aggregated from LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/practice_size_1_",year,"_",measure,".",output_type), sh_plot, width=18, height=18, units="cm", dpi="print")
  
}

plot_population_trends = function(database_name="primary_care_data.sqlite3", level="lsoa", output_type="png"){
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
  
  # graph_data = ons_quintiles %>% 
  #   inner_join(list_quintiles) %>%
  #   gather(POP_TYPE,POP,contains("POP")) %>%
  #   mutate(POP_TYPE = factor(POP_TYPE,c("TOTAL_POP","NEED_ADJ_POP","LIST_POP","ADJ_LIST_POP"),
  #                            c("Total population (ONS)","Need adjusted population (ONS + Carr Hill 2007)","Practice list size (NHS Digital)","Weighted practice list size (NHS Digital)")),
  #          IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
   graph_data = ons_quintiles %>% 
     gather(POP_TYPE,POP,contains("POP")) %>%
     mutate(POP_TYPE = factor(POP_TYPE,c("TOTAL_POP","NEED_ADJ_POP"),
                              c("Total population (ONS)","Need adjusted population (ONS + Carr Hill 2007)")),
           IMD_QUINTILE = factor(IMD_QUINTILE,1:5, c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")))
  
  start_year=2015
  end_year=2018
  pop_plot = ggplot(graph_data) + 
    aes(x=YEAR, y=POP, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
    geom_line(aes(linetype=IMD_QUINTILE)) + 
    geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE), size=3) +
    xlab("Year") +
    ylab("") +
    facet_wrap(POP_TYPE~.,scales="fixed",labeller = labeller(POP_TYPE = label_wrap_gen(60))) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks=start_year:end_year, labels=paste(str_sub(start_year:end_year,3), str_sub((start_year+1):(end_year+1),3),sep="/")) +
    scale_colour_manual(name="IMD Quintile Group", values=c("red","lightblue","lightblue","lightblue","blue"), labels=imd_labels) +
    scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
    scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
    scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          legend.position = "top") +
    labs(title = "Trends in population by neighbourhood deprivation",
         subtitle = paste0("Data for England in years ",start_year," - ",end_year," based on IMD 2019 quintiles"),
         caption = "Note: Data are from NHS Digital (workforce), ONS (population and LSOA) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
  
  ggsave(paste0("briefing/pop_trends_",level,".",output_type), pop_plot, width=25, height=15, units="cm", dpi="print")
  
}


generate_pngs_for_blog = function(){
  database_name="primary_care_data.sqlite3"
  level="lsoa"
  population="ons"
  year=2018
  measure="FTE"
  output_type = "png"
  
  # generate relevant pdf graphs from each indicator
  plot_payments_data(database_name, level, population = "list", output_type) 
  plot_workforce_mix(database_name, level, population, output_type)
  plot_qof_data(database_name, level, population, output_type)
  plot_gpps(database_name, level, population, output_type)
  plot_cqc()
  plot_appointments_data()
}

generate_pdf_slide_deck = function(database_name="primary_care_data.sqlite3",level="lsoa",population="ons",year=2018,measure="FTE"){
  output_type = "pdf"
  
  # generate relevant pdf graphs from each indicator
  plot_payments_data(database_name, level, population = "list", output_type) 
  plot_payments_data_london(database_name, output_type)
  plot_workforce_mix(database_name, level, population, output_type)
  plot_workforce_mix_london(database_name, output_type)
  plot_qof_data(database_name, level, population, output_type)
  plot_qof_data_london(database_name, output_type)
  plot_gpps(database_name, level, population, output_type)
  plot_gpps_london(database_name, output_type)

    
  # combine into one pdf file
  pdf_combine(input=c(paste0("briefing/payment_trends_",level,"_list.",output_type),
                      paste0("briefing/payment_trends_",level,"_list_london.",output_type),
                      paste0("briefing/payment_trends_",level,"_list_non_london.",output_type),
                      paste0("briefing/workforce_raw_",level,".",output_type),
                      paste0("briefing/workforce_raw_",level,"_london.",output_type),
                      paste0("briefing/workforce_raw_",level,"_non_london.",output_type),
                      paste0("briefing/workforce_",level,"_",population,".",output_type),
                      paste0("briefing/workforce_",level,"_",population,"_london.",output_type),
                      paste0("briefing/workforce_",level,"_",population,"_non_london.",output_type),
                      paste0("briefing/qof_trends_",level,"_",population,".",output_type),
                      paste0("briefing/qof_trends_",level,"_",population,"_london.",output_type),
                      paste0("briefing/qof_trends_",level,"_",population,"_non_london.",output_type),
                      paste0("briefing/gpps_trends_",level,"_",population,".",output_type),
                      paste0("briefing/gpps_trends_",level,"_",population,"_london.",output_type),
                      paste0("briefing/gpps_trends_",level,"_",population,"_non_london.",output_type)
                      ),
              output = paste0("briefing/primary_care_breifing_plots_combined_",level,"_",population,".",output_type))
}

political_map = function(database_name="primary_care_data.sqlite3"){
  const_map = st_read("raw_data/geography/political/Westminster_Parliamentary_Constituencies_December_2018_GB_BFE_V2/",
                      stringsAsFactors = FALSE)
  const_map = subset(const_map, grepl("^E",pcon18cd))
  const_map = simplify_shape(const_map, fact=0.05)
  
  db = dbConnect(SQLite(), dbname=database_name)
  lsoa_workforce = tbl(db,"gp_workforce_newdata_lsoa_imputed") %>% filter(YEAR==2018) %>% select(LSOA11CD, TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE, TOTAL_NURSES_FTE,TOTAL_DPC_FTE,TOTAL_ADMIN_FTE,TOTAL_POP,NEED_ADJ_POP) %>% collect()
  political_geography = tbl(db,"political_geography") %>% select(LSOA11CD,pcon18cd=PCON18CD) %>% collect()
  dbDisconnect(db)
  
  const_workforce = lsoa_workforce %>% 
    inner_join(political_geography) %>%
    group_by(pcon18cd) %>%
    summarise_at(vars(one_of(c("TOTAL_GP_EXRRL_HC","TOTAL_GP_EXRRL_FTE","TOTAL_NURSES_FTE","TOTAL_DPC_FTE","TOTAL_ADMIN_FTE","TOTAL_POP","NEED_ADJ_POP"))), sum, na.rm=TRUE) %>%
    ungroup() 
  
    const_data = const_workforce %>%
      mutate(GP_HC_100k=round(100000*TOTAL_GP_EXRRL_HC/NEED_ADJ_POP,2),
             GP_FTE_100k = round(100000*TOTAL_GP_EXRRL_FTE/NEED_ADJ_POP,2),
             NURSE_FTE_100k = round(100000*TOTAL_NURSES_FTE/NEED_ADJ_POP,2),
             DPC_FTE_100k = round(100000*TOTAL_DPC_FTE/NEED_ADJ_POP,2),
             ADMIN_FTE_100k = round(100000*TOTAL_ADMIN_FTE/NEED_ADJ_POP,2)) %>%
      select(pcon18cd,TOTAL_POP,NEED_ADJ_POP,GP_HC_100k,GP_FTE_100k,NURSE_FTE_100k,DPC_FTE_100k,ADMIN_FTE_100k)
    const_map = const_map %>% left_join(const_data)
    
    tmap = tm_shape(const_map) +
      tm_polygons("GP_FTE_100k",
                  palette="seq",
                  border.col = "white", 
                  border.alpha = 0.5,
                  title="GPs EXRRL per 100,000 need adjusted population") +
      tm_scale_bar()
    
    tmap_save(tmap, filename="briefing/gp_per_100k.pdf")
  
}

ccg_map = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  ccg_workforce = tbl(db,"ccg_workforce_imputed") %>% filter(YEAR==2018) %>% select(CCG19CD, TOTAL_GP_EXRRL_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, NEED_ADJ_POP) %>% collect()
  appt = tbl(db,"appointment_data") %>% filter(YEAR==2019) %>% collect()
  denom = tbl(db,"appointment_data_coverage") %>% filter(YEAR==2019) %>% select(YEAR,MONTH,CCG19CD,PATIENTS=PATIENTS_INCLUDED,PATIENT_OPEN) %>% collect()
  imd = tbl(db,"ccg_imd_2019") %>% select(CCG19CD,IMD_QUINTILE) %>% collect()
  dbDisconnect(db)
  
  appointments = appt %>% 
    filter(YEAR==2019) %>%
    group_by(CCG19CD) %>% 
    summarise(APPTS = sum(COUNT)) %>%
    ungroup() %>%
    inner_join(denom %>% filter(YEAR==2019) %>% group_by(CCG19CD) %>% summarise(PATIENTS=mean(PATIENTS),PATIENT_OPEN=mean(PATIENT_OPEN)) %>% ungroup()) %>%
    inner_join(ccg_workforce %>% select(CCG19CD,NEED_ADJ_POP)) %>%
    mutate(NEED_ADJ_POP = NEED_ADJ_POP*PATIENTS/PATIENT_OPEN,
      APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
      APPTS_PER_100_POP=round(100*APPTS/NEED_ADJ_POP,2)) %>%
    select(CCG19CD,APPTS,APPTS_PER_100_PATS,APPTS_PER_100_POP)

  gp_appointments = appt %>% 
    filter(HCP=="GP") %>%
    filter(YEAR==2019) %>%
    group_by(CCG19CD) %>% 
    summarise(APPTS = sum(COUNT)) %>%
    ungroup() %>%
    inner_join(denom %>% filter(YEAR==2019) %>% group_by(CCG19CD) %>% summarise(PATIENTS=mean(PATIENTS),PATIENT_OPEN=mean(PATIENT_OPEN)) %>% ungroup()) %>%
    inner_join(ccg_workforce %>% select(CCG19CD,NEED_ADJ_POP)) %>%
    mutate(NEED_ADJ_POP = NEED_ADJ_POP*PATIENTS/PATIENT_OPEN,
           GP_APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
           GP_APPTS_PER_100_POP=round(100*APPTS/NEED_ADJ_POP,2)) %>%
    select(CCG19CD,GP_APPTS=APPTS,GP_APPTS_PER_100_PATS,GP_APPTS_PER_100_POP)

  non_gp_appointments = appt %>% 
    filter(HCP!="GP") %>%
    filter(YEAR==2019) %>%
    group_by(CCG19CD) %>% 
    summarise(APPTS = sum(COUNT)) %>%
    ungroup() %>%
    inner_join(denom %>% filter(YEAR==2019) %>% group_by(CCG19CD) %>% summarise(PATIENTS=mean(PATIENTS),PATIENT_OPEN=mean(PATIENT_OPEN)) %>% ungroup()) %>%
    inner_join(ccg_workforce %>% select(CCG19CD,NEED_ADJ_POP)) %>%
    mutate(NEED_ADJ_POP = NEED_ADJ_POP*PATIENTS/PATIENT_OPEN,
           NON_GP_APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
           NON_GP_APPTS_PER_100_POP=round(100*APPTS/NEED_ADJ_POP,2)) %>%
    select(CCG19CD,NON_GP_APPTS=APPTS,NON_GP_APPTS_PER_100_PATS,NON_GP_APPTS_PER_100_POP)
  
    
  ccg_data = appointments %>% 
    inner_join(gp_appointments) %>%
    inner_join(non_gp_appointments) %>%
    inner_join(ccg_workforce) %>%
    inner_join(imd) %>%
    mutate(GP_FTE_100k = round(100000*TOTAL_GP_EXRRL_FTE/NEED_ADJ_POP,2),
           TOTAL_NON_GP_FTE = TOTAL_NURSES_FTE+TOTAL_DPC_FTE,
           NURSES_FTE_100k = round(100000*TOTAL_NURSES_FTE/NEED_ADJ_POP,2),
           DPC_FTE_100k = round(100000*TOTAL_DPC_FTE/NEED_ADJ_POP,2),
           NON_GP_FTE_100k = round(100000*TOTAL_NON_GP_FTE/NEED_ADJ_POP,2),
           APPTS_PER_GP = APPTS/TOTAL_GP_EXRRL_FTE, 
           GP_APPTS_PER_GP = GP_APPTS/TOTAL_GP_EXRRL_FTE,
           NON_GP_APPTS_PER_NON_GP = NON_GP_APPTS/(TOTAL_NURSES_FTE+TOTAL_DPC_FTE)) %>%
    select(ccg19cd=CCG19CD,
           APPTS_PER_GP, GP_APPTS_PER_GP, NON_GP_APPTS_PER_NON_GP,
           APPTS_PER_100_PATS,GP_APPTS_PER_100_PATS,NON_GP_APPTS_PER_100_PATS,APPTS_PER_100_POP,GP_APPTS_PER_100_POP,NON_GP_APPTS_PER_100_POP,
           TOTAL_GP_EXRRL_FTE,GP_FTE_100k,
           TOTAL_NURSES_FTE,NURSES_FTE_100k,
           TOTAL_DPC_FTE,DPC_FTE_100k,
           TOTAL_NON_GP_FTE,NON_GP_FTE_100k,
           IMD_QUINTILE)
  
  ccg_map = st_read("raw_data/geography/shape_files/Clinical_Commissioning_Groups_April_2019_Ultra_Generalised_Clipped_Boundaries_England/",
                    stringsAsFactors = FALSE)
  ccg_map = simplify_shape(ccg_map, fact=0.05)
  ccg_map = ccg_map %>% left_join(ccg_data)
  
  ccg_imd_map = tm_shape(ccg_map) +
    tm_polygons("IMD_QUINTILE",
                breaks = 0:5,
                labels = imd_labels,
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Index of Multiple Deprivation 2019") +
    tm_scale_bar()
  
  tmap_save(ccg_imd_map, filename=paste0("briefing/ccg_imd.",output_type))
  
  ccg_gp_map = tm_shape(ccg_map) +
    tm_polygons("GP_FTE_100k",
                breaks=seq(min(ccg_map$GP_FTE_100k),max(ccg_map$GP_FTE_100k),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="GPs EXRRL per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(ccg_gp_map, filename=paste0("briefing/ccg_gp_per_100k.",output_type))
  
  
  ccg_appt_pop_map = tm_shape(ccg_map) +
    tm_polygons("APPTS_PER_100_POP",
                breaks=seq(min(ccg_map$APPTS_PER_100_POP),max(ccg_map$APPTS_PER_100_POP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments per year per 100 need adjusted population (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_appt_pop_map, filename=paste0("briefing/ccg_appt_100_pop.",output_type))
  
  ccg_gp_appt_pop_map = tm_shape(ccg_map) +
    tm_polygons("GP_APPTS_PER_100_POP",
                breaks=seq(min(ccg_map$GP_APPTS_PER_100_POP),max(ccg_map$GP_APPTS_PER_100_POP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments with GP per year per 100 need adjusted population (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_gp_appt_pop_map, filename=paste0("briefing/ccg_gp_appt_100_pop.",output_type))
  
  ccg_non_gp_appt_pop_map = tm_shape(ccg_map) +
    tm_polygons("NON_GP_APPTS_PER_100_POP",
                breaks=seq(min(ccg_map$NON_GP_APPTS_PER_100_POP),max(ccg_map$NON_GP_APPTS_PER_100_POP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments with Nurse or DPC staff per year per 100 need adjusted population (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_non_gp_appt_pop_map, filename=paste0("briefing/ccg_non_gp_appt_100_pop.",output_type))
  
  ccg_appt_pop_gp_map = tm_shape(ccg_map) +
    tm_polygons("GP_APPTS_PER_100_POP",
                breaks=seq(min(ccg_map$GP_APPTS_PER_100_POP),max(ccg_map$GP_APPTS_PER_100_POP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments with a GP per year per 100 need adjusted population (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_appt_pop_gp_map, filename=paste0("briefing/ccg_appt_gp_100_pop.",output_type)) 
  
  ccg_appt_per_gp_map = tm_shape(ccg_map) +
    tm_polygons("GP_APPTS_PER_GP",
                breaks=seq(min(ccg_map$GP_APPTS_PER_GP),max(ccg_map$GP_APPTS_PER_GP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments with a GP per GP (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_appt_per_gp_map, filename=paste0("briefing/ccg_appt_per_gp.",output_type)) 
  
  ccg_appt_per_non_gp_map = tm_shape(ccg_map) +
    tm_polygons("NON_GP_APPTS_PER_NON_GP",
                breaks=seq(min(ccg_map$NON_GP_APPTS_PER_NON_GP),max(ccg_map$NON_GP_APPTS_PER_NON_GP),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Appontments with a Nurse or DPC staff per Nurse or DPC staff (2019)") +
    tm_scale_bar()
  
  tmap_save(ccg_appt_per_non_gp_map, filename=paste0("briefing/ccg_appt_per_non_gp.",output_type)) 
  
  ccg_nurse_map = tm_shape(ccg_map) +
    tm_polygons("NURSES_FTE_100k",
                breaks=seq(min(ccg_map$NURSES_FTE_100k),max(ccg_map$NURSES_FTE_100k),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Nurses FTE per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(ccg_nurse_map, filename=paste0("briefing/ccg_nurse_per_100k.",output_type))
  
  ccg_dpc_map = tm_shape(ccg_map) +
    tm_polygons("DPC_FTE_100k",
                breaks=seq(min(ccg_map$DPC_FTE_100k),max(ccg_map$DPC_FTE_100k),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="DPC staff FTE per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(ccg_dpc_map, filename=paste0("briefing/ccg_dpc_per_100k.",output_type))
  
  ccg_non_gp_map = tm_shape(ccg_map) +
    tm_polygons("NON_GP_FTE_100k",
                breaks=seq(min(ccg_map$NON_GP_FTE_100k),max(ccg_map$NON_GP_FTE_100k),length.out=6),
                legend.format=list(digits=0),
                palette="seq",
                border.col = "white", 
                border.alpha = 0.5,
                title="Nurses and DPC staff FTE per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(ccg_non_gp_map, filename=paste0("briefing/ccg_non_gp_per_100k.",output_type))
  
  
  if (output_type=="pdf") {
    pdf_combine(input=c(paste0("briefing/ccg_imd.",output_type),
                        paste0("briefing/ccg_appt_100_pop.",output_type),
                        paste0("briefing/ccg_gp_appt_100_pop.",output_type),
                        paste0("briefing/ccg_gp_per_100k.",output_type),
                        paste0("briefing/ccg_appt_per_gp.",output_type),
                        paste0("briefing/ccg_non_gp_appt_100_pop.",output_type),
                        paste0("briefing/ccg_nurse_per_100k.",output_type),
                        paste0("briefing/ccg_dpc_per_100k.",output_type),
                        paste0("briefing/ccg_non_gp_per_100k.",output_type),
                        paste0("briefing/ccg_appt_per_non_gp.",output_type)
    ),
    output = "briefing/appointments_maps.pdf")
  }
}

lsoa_map = function(database_name="primary_care_data.sqlite3", output_type="png"){
  db = dbConnect(SQLite(), dbname=database_name)
  workforce_lsoa = tbl(db,"gp_workforce_newdata_lsoa_imputed") %>% filter(YEAR==2018) %>% select(LSOA11CD, TOTAL_GP_EXRRL_FTE, NEED_ADJ_POP, IMD_QUINTILE) %>% collect()
  qof_lsoa = tbl(db,"gp_practice_qof_lsoa") %>% filter(YEAR==2018) %>% select(LSOA11CD,TOTAL_POINTS) %>% collect()
  gpps_lsoa = tbl(db,"gpps_lsoa") %>% filter(YEAR==2018) %>% select(LSOA11CD,GPPS_GOOD) %>% collect()
  london_lsoa = tbl(db,"london_lsoas") %>% collect()
  dbDisconnect(db)
  
  lsoa_data = workforce_lsoa %>% left_join(qof_lsoa) %>% left_join(gpps_lsoa) %>% left_join(london_lsoa) %>%
    mutate(GP_FTE_100k=100000*TOTAL_GP_EXRRL_FTE/NEED_ADJ_POP) %>%
    select(lsoa11cd = LSOA11CD, LONDON, URBAN, GP_FTE_100k, IMD_QUINTILE, QOF_POINTS=TOTAL_POINTS, GPPS=GPPS_GOOD)
  
  
  lsoa_map = st_read("raw_data/geography/shape_files/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales/",
                    stringsAsFactors = FALSE)
  lsoa_map = subset(lsoa_map, grepl("^E",lsoa11cd))
  lsoa_map = simplify_shape(lsoa_map, fact=0.05)
  lsoa_map = lsoa_map %>% left_join(lsoa_data)
  
  london_lsoa_map = subset(lsoa_map, LONDON==TRUE)
    
  lsoa_gp_map = tm_shape(lsoa_map) +
    tm_polygons("GP_FTE_100k",
                breaks = c(0,20,40,60,80,200),
                labels = c("0-20","20-40","40-60","60-80","80+"),
                legend.format=list(digits=0),
                palette="seq",
                lwd = 0,
                title="GPs EXRRL per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_gp_map, filename=paste0("briefing/lsoa_gp_per_100k.",output_type))
  
  lsoa_gp_map_london = tm_shape(london_lsoa_map) +
    tm_polygons("GP_FTE_100k",
                breaks = c(0,20,40,60,80,200),
                labels = c("0-20","20-40","40-60","60-80","80+"),
                legend.format=list(digits=0),
                legend.position=c("bottom","left"),
                palette="seq",
                lwd = 0,
                title="GPs EXRRL per 100,000 need adjusted population (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_gp_map_london, filename=paste0("briefing/lsoa_gp_per_100k_london.",output_type))
  
  lsoa_imd_map = tm_shape(lsoa_map) +
    tm_polygons("IMD_QUINTILE",
                breaks = 0:5,
                labels = imd_labels,
                legend.format=list(digits=0),
                palette="seq",
                lwd = 0,
                title="Index of Multiple Deprivation 2019") +
    tm_scale_bar()
  
  tmap_save(lsoa_imd_map, filename=paste0("briefing/lsoa_imd_score.",output_type))
  
  lsoa_imd_map_london = tm_shape(london_lsoa_map) +
    tm_polygons("IMD_QUINTILE",
                breaks = 0:5,
                labels = imd_labels,
                legend.format=list(digits=0),
                legend.position=c("bottom","left"),
                palette="seq",
                lwd = 0,
                title="Index of Multiple Deprivation 2019") +
    tm_scale_bar()
  
  tmap_save(lsoa_imd_map_london, filename=paste0("briefing/lsoa_imd_score_london.",output_type))
    
  lsoa_qof_map = tm_shape(lsoa_map) +
    tm_polygons("QOF_POINTS",
                breaks = c(300,500,515,530,545,560),
                labels = c("300-500","500-515","515-530","530-545","545-560"),
                legend.format=list(digits=0),
                palette="seq",
                lwd = 0,
                title="Total quality and outcomes framework points (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_qof_map, filename=paste0("briefing/lsoa_qof_map.",output_type))

  lsoa_qof_map_london = tm_shape(london_lsoa_map) +
    tm_polygons("QOF_POINTS",
                breaks = c(300,500,515,530,545,560),
                labels = c("300-500","500-515","515-530","530-545","545-560"),
                legend.format=list(digits=0),
                legend.position=c("bottom","left"),
                palette="seq",
                lwd = 0,
                title="Total quality and outcomes framework points (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_qof_map_london, filename=paste0("briefing/lsoa_qof_map_london.",output_type))
    
  lsoa_gpps_map = tm_shape(lsoa_map) +
    tm_polygons("GPPS",
                breaks = c(0.4,0.6,0.7,0.8,0.9,1),
                labels = c("40%-60%","60%-70%","70%-80%","80%-90%","90%-100%"),
                palette="seq",
                lwd = 0,
                title="Percentage rating GP practice good overall (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_gpps_map, filename=paste0("briefing/lsoa_gpps_map.",output_type))
  
  lsoa_gpps_map_london = tm_shape(london_lsoa_map) +
    tm_polygons("GPPS",
                breaks = c(0.4,0.6,0.7,0.8,0.9,1),
                labels = c("40%-60%","60%-70%","70%-80%","80%-90%","90%-100%"),
                legend.position=c("bottom","left"),
                palette="seq",
                lwd = 0,
                title="Percentage rating GP practice good overall (2018)") +
    tm_scale_bar()
  
  tmap_save(lsoa_gpps_map_london, filename=paste0("briefing/lsoa_gpps_map_london.",output_type))
  
  
  if (output_type=="pdf") {
    pdf_combine(input=c("briefing/lsoa_gp_per_100k.pdf",
                        "briefing/lsoa_gp_per_100k_london.pdf",
                        "briefing/lsoa_imd_score.pdf",
                        "briefing/lsoa_imd_score_london.pdf",
                        "briefing/lsoa_qof_map.pdf",
                        "briefing/lsoa_qof_map_london.pdf",
                        "briefing/lsoa_gpps_map.pdf",
                        "briefing/lsoa_gpps_map_london.pdf"
    ),
    output = "briefing/primary_care_breifing_maps.pdf")
    
  }
  
}

workforce_sql = "select IMD_QUINTILE, 
round(100000*sum(TOTAL_GP_EXRRL_FTE)/sum(need_adj_pop),2) gp,
round(100000*sum(TOTAL_NURSES_FTE)/sum(need_adj_pop),2) nurse,
round(100000*sum(TOTAL_DPC_FTE)/sum(need_adj_pop),2) dpc, 
round(100000*sum(TOTAL_DPC_HCA_FTE)/sum(need_adj_pop),2) hca, 
round(100000*sum(TOTAL_DPC_PHARMA_FTE)/sum(need_adj_pop),2) pharma, 
round(100000*sum(TOTAL_DPC_PHYSIO_FTE)/sum(need_adj_pop),2) physio,
round(100000*sum(TOTAL_DPC_PHLEB_FTE)/sum(need_adj_pop),2) phleb,
round(100000*sum(TOTAL_DPC_PHYSICIAN_ASSOC_FTE)/sum(need_adj_pop),2)  phys_assoc,
round(100000*sum(TOTAL_DPC_DISPENSER_FTE)/sum(need_adj_pop),2)  dispenser,
round(100000*sum(TOTAL_DPC_OTH_FTE)/sum(need_adj_pop),2)  dpc_other
from gp_workforce_newdata_lsoa_imputed where year==2018 
GROUP BY IMD_QUINTILE"


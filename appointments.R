library("tidyverse")
library("RSQLite")
library("lubridate")
library("scales")
library("qpdf")

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

database_name="primary_care_data.sqlite3"
db = dbConnect(SQLite(), dbname=database_name)
appt = tbl(db,"appointment_data") %>% collect()
denom = tbl(db,"appointment_data_coverage") %>% select(YEAR,MONTH,CCG19CD,PATIENTS=PATIENTS_INCLUDED) %>% collect()
ccg_imd = tbl(db,"ccg_imd_2019") %>% select(CCG19CD,IMD_SCORE=average_score) %>% collect()
ccg_pop = tbl(db,"ccg_pop") %>% filter(YEAR==2015) %>% select(CCG19CD,TOTAL_POP) %>% collect()
imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")
ccg_imd_quintiles = ccg_imd %>% inner_join(ccg_pop) %>% arrange(-IMD_SCORE) %>% 
  mutate(CUM_POP = cumsum(TOTAL_POP), PROP = CUM_POP/max(CUM_POP), 
         IMD_QUINTILE = cut(PROP,5,labels=imd_labels))

graph_data_0 = appt %>% 
  group_by(YEAR,MONTH,CCG19CD) %>% 
  summarise(APPTS=sum(COUNT)) %>% 
  ungroup() %>%
  inner_join(denom) %>% 
  inner_join(ccg_imd_quintiles) %>% 
  group_by(YEAR,MONTH,IMD_QUINTILE) %>%
  summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
  mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
         DATE=ymd(paste0(YEAR,if_else(MONTH<10,"0",""),MONTH,"15"))) %>%
  ungroup() %>%
  select(DATE,IMD_QUINTILE,APPTS,PATIENTS,APPTS_PER_100_PATS) %>%
  gather("VAR","VAL",c(APPTS,PATIENTS,APPTS_PER_100_PATS)) %>%
  mutate(VAR = factor(VAR,levels=c("APPTS","PATIENTS","APPTS_PER_100_PATS"),labels=c("Appointments","Patients","Appointments per 100 patients")))

graph_data_1 = appt %>% 
  mutate(HCP = if_else(HCP=="Unknown","Other Practice staff",HCP)) %>%
  group_by(YEAR,MONTH,CCG19CD,HCP) %>% 
  summarise(APPTS=sum(COUNT)) %>% 
  ungroup() %>%
  inner_join(denom) %>% 
  inner_join(ccg_imd_quintiles) %>% 
  group_by(YEAR,MONTH,HCP,IMD_QUINTILE) %>%
  summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
  mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
         DATE=ymd(paste0(YEAR,if_else(MONTH<10,"0",""),MONTH,"15")))

graph_data_2 = appt %>% 
  group_by(YEAR,MONTH,CCG19CD,MODE) %>% 
  summarise(APPTS=sum(COUNT)) %>% 
  ungroup() %>%
  inner_join(denom) %>% 
  inner_join(ccg_imd_quintiles) %>% 
  group_by(YEAR,MONTH,MODE,IMD_QUINTILE) %>%
  summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
  mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
         DATE=ymd(paste0(YEAR,if_else(MONTH<10,"0",""),MONTH,"15")))

graph_data_3 = appt %>% 
  mutate(STATUS = if_else(STATUS=="Appt Status Not Provided","Unknown",STATUS)) %>%
  filter(STATUS!="Unknown") %>%
  group_by(YEAR,MONTH,CCG19CD,STATUS) %>% 
  summarise(APPTS=sum(COUNT)) %>% 
  ungroup() %>%
  inner_join(denom) %>% 
  inner_join(ccg_imd_quintiles) %>% 
  group_by(YEAR,MONTH,STATUS,IMD_QUINTILE) %>%
  summarise(APPTS = sum(APPTS), PATIENTS = sum(PATIENTS)) %>%
  mutate(APPTS_PER_100_PATS=round(100*APPTS/PATIENTS,2),
         DATE=ymd(paste0(YEAR,if_else(MONTH<10,"0",""),MONTH,"15")))

raw_plot = ggplot(graph_data_0) + 
  aes(x=DATE, y=VAL, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
  geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
  geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
  xlab("Date") +
  ylab("") +
  scale_y_continuous(labels = comma) +
  scale_x_date(breaks="month", labels=date_format("%b-%y")) + 
  scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
  scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
  scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
  scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
  facet_wrap(VAR~.,scales="free_y", nrow=3) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x=element_text(angle=90),
        legend.position = "bottom") +
  labs(title = "Trends in primary care appointments by neighbourhood deprivation",
       subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
       caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")

ggsave(paste0("figures/appointments_raw.pdf"), raw_plot, width=25, height=35, units="cm", dpi="print")


hcp_plot = ggplot(graph_data_1) + 
  aes(x=DATE, y=APPTS_PER_100_PATS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
  geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
  geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
  xlab("Date") +
  ylab("Appointements per 100 patients") +
  scale_y_continuous(labels = comma) +
  scale_x_date(breaks="month", labels=date_format("%b-%y")) + 
  scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
  scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
  scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
  scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
  facet_wrap(.~HCP) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x=element_text(angle=90),
        legend.position = "bottom") +
  labs(title = "Trends in primary care appointments by neighbourhood deprivation",
       subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
       caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")
ggsave(paste0("figures/appointments_hcp.pdf"), hcp_plot, width=25, height=15, units="cm", dpi="print")



mode_plot = ggplot(graph_data_2) + 
  aes(x=DATE, y=APPTS_PER_100_PATS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
  geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
  geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
  xlab("Date") +
  ylab("Appointements per 100 patients") +
  scale_y_continuous(labels = comma) +
  scale_x_date(breaks="month", labels=date_format("%b-%y")) + 
  scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
  scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
  scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
  scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
  facet_wrap(.~MODE, scales = "free") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x=element_text(angle=90),
        legend.position = "bottom") +
  labs(title = "Trends in primary care appointments by neighbourhood deprivation",
       subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
       caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")

ggsave(paste0("figures/appointments_mode.pdf"), mode_plot, width=35, height=25, units="cm", dpi="print")


status_plot = ggplot(graph_data_3) + 
  aes(x=DATE, y=APPTS_PER_100_PATS, group=IMD_QUINTILE, colour=IMD_QUINTILE) + 
  geom_line(aes(linetype=IMD_QUINTILE, size=IMD_QUINTILE)) + 
  geom_point(aes(shape=IMD_QUINTILE, colour=IMD_QUINTILE)) +
  xlab("Date") +
  ylab("Appointements per 100 patients") +
  scale_y_continuous(labels = comma) +
  scale_x_date(breaks="month", labels=date_format("%b-%y")) + 
  scale_colour_manual(name="IMD Quintile Group", values=c("black","lightblue","lightblue","lightblue","darkgrey"), labels=imd_labels) +
  scale_shape_manual(name="IMD Quintile Group", values=c(19,21,24,0,15), labels=imd_labels) +
  scale_linetype_manual(name="IMD Quintile Group", values=c(1,2,2,2,1), labels=imd_labels) +
  scale_size_manual(name="IMD Quintile Group", values=c(1,0.5,0.5,0.5,1), labels=imd_labels) +
  facet_wrap(.~STATUS, scales = "free") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.text.x=element_text(angle=90),
        legend.position = "bottom") +
  labs(title = "Trends in primary care appointments by neighbourhood deprivation",
       subtitle = paste0("Data for English CCGs in years ",2018," - ",2019," based on IMD 2019 quintiles"),
       caption = "Note: Data are from NHS Digital (appoiontments) and DWP (index of multiple deprivation) based on LSOA 2011 neighbourhoods")

ggsave(paste0("figures/appointments_status.pdf"), status_plot, width=25, height=15, units="cm", dpi="print")

pdf_combine(input=c("figures/appointments_raw.pdf",
                    "figures/appointments_hcp.pdf",
                    "figures/appointments_mode.pdf",
                    "figures/appointments_status.pdf"),
output = "figures/appointments_combined.pdf")


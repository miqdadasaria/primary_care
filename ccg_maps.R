library(rgdal)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(RSQLite)

read_geojson = function(year, selected_ccg){
  if(selected_ccg=="ccg"){
    ccg_map = readOGR(dsn=paste0("maps/ccg_",year,"_map.geojson"), verbose=FALSE, stringsAsFactors=FALSE)
  } else {
    ccg_map = readOGR(dsn=paste0("maps/ccg_lsoa_",selected_ccg,"_",year,"_map.geojson"), verbose=FALSE, stringsAsFactors=FALSE)
  }
  return(ccg_map)
}

make_popup_messages = function(map, selected_ccg){
  if(selected_ccg=="ccg"){
    popup_messages = paste0("<b>Name: </b>",map$ccg19nm,"<br>",
                          "<b>IMD 2019 Score: </b>",map$IMD_SCORE,"<br>",
                          "<b>Total Population: </b>",map$TOTAL_POP,"<br>",
                          "<b>Need Adjusted Population: </b>",round(map$NEED_ADJ_POP,0),"<br>",
                          "<b>GPs headcount (EXRRL) per 100k population: </b>",map$GP_HC_100k,"<br>",
                          "<b>GPs FTE (EXRRL) per 100k population: </b>",map$GP_FTE_100k,"<br>",
                          "<b>Nurses FTE per 100k population: </b>",map$NURSE_FTE_100k,"<br>",
                          "<b>Direct patient care FTE per 100k population: </b>",map$DPC_FTE_100k,"<br>",
                          "<b>Admin FTE per 100k population: </b>",map$ADMIN_FTE_100k,"<br>")
  } else {
    popup_messages = paste0("<b>Name: </b>",map$lsoa11nm,"<br>",
                            "<b>IMD 2019 Score: </b>",map$IMD_SCORE,"<br>",
                            "<b>IMD 2019 Quintile: </b>",map$IMD_QUINTILE,"<br>",
                            "<b>Total Population: </b>",map$TOTAL_POP,"<br>",
                            "<b>Need Adjusted Population: </b>",round(map$NEED_ADJ_POP,0),"<br>",
                            "<b>GPs headcount (EXRRL) per 100k population: </b>",map$GP_HC_100k,"<br>",
                            "<b>GPs FTE (EXRRL) per 100k population: </b>",map$GP_FTE_100k,"<br>",
                            "<b>Nurses FTE per 100k population: </b>",map$NURSE_FTE_100k,"<br>",
                            "<b>Direct patient care FTE per 100k population: </b>",map$DPC_FTE_100k,"<br>",
                            "<b>Admin FTE per 100k population: </b>",map$ADMIN_FTE_100k,"<br>")
  }
  return(popup_messages)  
}

make_gp_markers = function(year, database_name="primary_care_data_online.sqlite3"){
  db = dbConnect(SQLite(), dbname=database_name)
  geo = tbl(db,"ccg_practice_mapping") %>% select(PRAC_CODE=PRACTICE_CODE,LONG,LAT,CCG19CD) %>% collect()
  ccg_name = tbl(db,"ccg_ons_code_mapping") %>% select(CCG19CD,CCG19NM) %>% distinct(CCG19CD,CCG19NM) %>% collect()
  pcn_name = tbl(db,"pcn_gp_practice_mapping") %>% select(PRAC_CODE,PCN_NAME) %>% collect()
  data = tbl(db,"gp_workforce_newdata_imputed") %>% filter(YEAR==year) %>% collect()
  pop = tbl(db, "gp_population") %>% filter(YEAR==year) %>% collect()
  imd = tbl(db, "gp_imd_2019") %>% filter(YEAR==year) %>% collect()
  gp_practice_data = data %>% inner_join(pop) %>% inner_join(imd) %>% inner_join(geo) %>% inner_join(ccg_name) %>% left_join(pcn_name)  
  dbDisconnect(db)
  gp_practices = gp_practice_data %>%
    select(PRAC_NAME, PRAC_CODE, CCG19NM,PCN_NAME, IMD_SCORE, IMD_QUINTILE, TOTAL_POP, NEED_ADJ_POP,
           TOTAL_GP_EXRRL_HC, TOTAL_GP_EXRRL_FTE,
           TOTAL_NURSES_FTE,TOTAL_DPC_FTE,TOTAL_ADMIN_FTE,LONG,LAT) %>%
    mutate(description = paste0("<b>Name: </b>",str_to_title(PRAC_NAME),"<br>",
                                "<b>CCG Name: </b>",gsub("Ccg","CCG",gsub("Nhs","NHS",str_to_title(CCG19NM))),"<br>",
                                "<b>PCN Name: </b>",gsub("Pcn","PCN",str_to_title(ifelse(is.na(PCN_NAME),"",PCN_NAME))),"<br>",
                                "<b>IMD 2019 Score: </b>",round(IMD_SCORE,2),"<br>",
                                "<b>IMD 2019 Quintile: </b>",IMD_QUINTILE,"<br>",
                                "<b>Total Population: </b>",round(TOTAL_POP,0),"<br>",
                                "<b>Need Adjusted Population: </b>",round(NEED_ADJ_POP,0),"<br>",
                                "<b>GPs headcount (EXRRL): </b>",TOTAL_GP_EXRRL_HC,"<br>",
                                "<b>GPs FTE (EXRRL) per 100k population: </b>",round(100000*TOTAL_GP_EXRRL_FTE/TOTAL_POP,2),"<br>",
                                "<b>Nurses FTE per 100k population: </b>",round(100000*TOTAL_NURSES_FTE/TOTAL_POP,2),"<br>",
                                "<b>Direct patient care FTE per 100k population: </b>",round(100000*TOTAL_DPC_FTE/TOTAL_POP,2),"<br>",
                                "<b>Admin FTE per 100k population: </b>",round(100000*TOTAL_ADMIN_FTE/TOTAL_POP,2),"<br>")) %>%
    select(PRAC_NAME,LONG,LAT,description)
  
  return(gp_practices)
}

make_choropleth_map = function(year, selected_ccg, database_name="primary_care_data_online.sqlite3"){
  ccg_map = read_geojson(year, selected_ccg)
    
  popup_message = make_popup_messages(ccg_map, selected_ccg)
  
  gp_practice_markers = make_gp_markers(year, database_name)
  imd_pal = colorBin("Oranges", ccg_map$IMD_SCORE, n=5, pretty = FALSE)
  gp_pal = colorBin("Blues", ccg_map$GP_FTE_100k, n=5, pretty = FALSE)
  nurse_pal = colorBin("Reds", ccg_map$NURSE_FTE_100k, n=5, pretty = FALSE)
  dpc_pal = colorBin("Greens", ccg_map$DPC_FTE_100k, n=5, pretty = FALSE)
  admin_pal = colorBin("Purples", ccg_map$ADMIN_FTE_100k, n=5, pretty = FALSE)

  
  choropleth_map = leaflet(ccg_map) %>% 
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = imd_pal(ccg_map$IMD_SCORE), 
                color="black",
                group="IMD") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = gp_pal(ccg_map$GP_FTE_100k), 
                color="black",
                group="GPs") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = nurse_pal(ccg_map$NURSE_FTE_100k), 
                color="black",
                group="Nurses") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = dpc_pal(ccg_map$DPC_FTE_100k), 
                color="black",
                group="DPC Staff") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 1, 
                fillOpacity = 0.7, 
                weight = 1, 
                popup = popup_message, 
                fillColor = admin_pal(ccg_map$ADMIN_FTE_100k), 
                color="black",
                group="Admin Staff") %>%
    addLayersControl(
      baseGroups=c("IMD", "GPs", "Nurses", "DPC Staff", "Admin Staff"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    setView(lng=mean(ccg_map@bbox["x",]), lat=mean(ccg_map@bbox["y",]), zoom=if_else(selected_ccg=="ccg",6,11)) %>% 
      addMarkers(lng=~LONG, lat=~LAT, 
                 popup=~as.character(description), 
                 label=~str_to_title(PRAC_NAME),
                 data=gp_practice_markers, 
                 clusterOptions=markerClusterOptions(),
                 labelOptions = labelOptions(noHide = T,
                                             direction = 'auto'))
  return(choropleth_map)
}


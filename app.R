# Primary care workforce shiny app
# 
# Author: Miqdad Asaria
# Date: 28/11/2019
###############################################################################

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
source("ccg_maps.R")
source("plots_for_online.R")

ccg_list = get_ccg_list()

server = shinyServer(function(input, output) {
    
    output$primary_care_map = renderLeaflet({
        withProgress(message = 'Generating interactive primary care map...',
                     make_choropleth_map(input$year, input$selected_ccg)
        )
    })
    
    output$scatter = renderPlot({
        withProgress(message = 'Generating scatter plot...',
                     workforce_scatter_plot(input$x_var, input$y_var, input$adj_method, input$year, input$geography, input$trim_outliers)
                     
        )
    })
    
    output$imd_plot = renderPlot({
        withProgress(message = 'Generating imd plot...',
                     imd_plot(input$y_var, input$adj_method)
        )
    })
    
    output$age_sex_plot = renderPlot({
        withProgress(message = 'Generating imd plot...',
                     gp_age_sex_plot(input$year, input$adj_method)
        )
    })
    
    output$raw_data = renderDataTable({
        withProgress(message = 'Loading raw data table',{
            table = make_data_table(input$x_var, input$y_var, input$adj_method, input$year, input$geography, input$trim_outliers)
            datatable(table,
                      style = 'bootstrap',
                      rownames = FALSE,
                      options = list(pageLength = 15, autoWidth = TRUE, dom='ftrpi'))
        })
    })
})


ui = shinyUI(fluidPage(theme = "sandstone.css",
                       
                       titlePanel("Primary Care Workforce Explorer"),
                       
                       sidebarPanel(
                           selectInput("year", "Local Authority:",
                                       2015:2018,
                                       selected = 2018),
                           
                           selectInput("selected_ccg", "CCG for map:",
                                       c("All England"="",
                                            ccg_list),
                                       selected = ""
                           ),
                           
                           selectInput("geography", "Geography for scatter plot:",
                                       list("Clinical Commisioning Group"="ccg",
                                            "GP Practice"="gp_practice",
                                            "Lower Layer Super Output Area"="lsoa"),
                                       selected = "ccg"
                           ),

                           
                           selectInput("x_var", "X variable for scatter plot:",
                                       list("IMD_SCORE","IMD_RANK","TOTAL_POP"),
                                       selected = "IMD_SCORE"
                           ),
                           
                           selectInput("y_var", "Y variable for plots:",
                                       as.list(sort(c("TOTAL_GP_HC","TOTAL_GP_EXL_HC","TOTAL_GP_EXRL_HC","TOTAL_GP_EXRRL_HC","TOTAL_GP_SEN_PTNR_HC","TOTAL_GP_PTNR_PROV_HC","TOTAL_GP_SAL_BY_PRAC_HC","TOTAL_GP_SAL_BY_OTH_HC","TOTAL_GP_REG_ST3_4_HC","TOTAL_GP_REG_F1_2_HC","TOTAL_GP_RET_HC","TOTAL_GP_LOCUM_VAC_HC","TOTAL_GP_LOCUM_ABS_HC","TOTAL_GP_LOCUM_OTH_HC",
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
                                         "TOTAL_ADMIN_FTE","TOTAL_ADMIN_MANAGER_FTE","TOTAL_ADMIN_MED_SECRETARY_FTE","TOTAL_ADMIN_RECEPT_FTE","TOTAL_ADMIN_TELEPH_FTE","TOTAL_ADMIN_ESTATES_ANC_FTE","TOTAL_ADMIN_OTH_FTE","TOTAL_POP","NEED_ADJ_POP"))),
                                       selected = "TOTAL_GP_EXRRL_FTE"
                           ),
                           
                           selectInput("adj_method", "Adjust y values by:",
                                       list("No adjustment"="raw",
                                            "per 100,000 population"="pop",
                                            "per 100,000 need adjusted population"="adj_pop"),
                                       selected = "pop"
                           ),
                           
                           radioButtons("trim_outliers", "Trim outliers on scatter plots:",
                                        list(TRUE,FALSE),
                                        selected=TRUE)
                           
                       ),
                       
                       mainPanel(
                           tabsetPanel(id="tabset",
                                       tabPanel("Map", leafletOutput("primary_care_map")),
                                       tabPanel("Scatter Plots", plotOutput("scatter")),
                                       tabPanel("IMD Quintile Trends", plotOutput("imd_plot")),
                                       tabPanel("GP Age Sex IMD Plots", plotOutput("age_sex_plot")),
                                       tabPanel("Raw Data", dataTableOutput("raw_data"))
                           )
                       )
))

shinyApp(ui = ui, server = server)

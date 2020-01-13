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

workforce_vars = get_workforce_varlist()


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
                                       c("All England (CCG)"="ccg",
                                         "All England (LSOA)"="all",
                                            ccg_list),
                                       selected = ""
                           ),
                           
                           selectInput("geography", "Geography for scatter plot:",
                                       list("Clinical Commisioning Group"="ccg",
                                            "Primary Care Network"="pcn",
                                            "GP Practice"="gp_practice",
                                            "Lower Layer Super Output Area"="lsoa"),
                                       selected = "ccg"
                           ),

                           
                           selectInput("x_var", "X variable for scatter plot:",
                                       list("IMD_SCORE","IMD_RANK","TOTAL_POP","TOTAL_GP_EXRRL_FTE"),
                                       selected = "IMD_SCORE"
                           ),
                           
                           selectInput("y_var", "Y variable for plots:",
                                       as.list(sort(workforce_vars)),
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
                                        selected=TRUE),
                           tags$div(
                               HTML("<small><small>
         <p>This site was produced by <a href='http://www.lse.ac.uk/lse-health/people/miqdad-asaria'>Miqdad Asaria</a> 
         as part of a fellowship funded by the <a href='https://www.health.org.uk'>Health Foundation</a>
         based at the <a href='https://www.lse.ac.uk/'>London School of Economics</a>. 
         <p>Source code can be found <a href='https://github.com/miqdadasaria/primary_care'>here</a>.
         </small></small>")
                           )
                           
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

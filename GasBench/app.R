## Load required libraries-----------------------------------------------------
library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
## set up UI-------------------------------------------------------------------
ui <- navbarPage(
  theme = shinythemes::shinytheme("yeti"),
  title = 'GASBENCH II',
  ## tab for data input--------------------------------------------------------
  tabPanel(title = 'DATA INPUT',
           sidebarLayout(
             sidebarPanel(
               fileInput(inputId = 'raw.data', 
                         label = 'SELECT A FILE'),
               checkboxGroupInput(inputId = 'boxes', 
                                  label =  'SELECT PEAKS TO EXCLUDE')),
             mainPanel(dataTableOutput(outputId = 'peak.table')))),
  
  tabPanel(title = 'SELECT STANDARDS',
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(
                 inputId = 'standards',
                 label = 'SELECT STANDARDS',
                 choices = sort(c('NBS 18', 'CM', 'USGS 44', 'NBS 19')
                 )
               )
             ),
             mainPanel(dataTableOutput('stds')))),
  
  tabPanel(title = 'LINEARITY'),
  tabPanel(title = 'STANDARD CORRECTION'),
  tabPanel(title = 'DRIFT CORRECTIONS'),
  tabPanel(title = 'RAW DATA',
           mainPanel(dataTableOutput('raw.table'))),
  tabPanel(title = 'DEBUG',
           mainPanel(verbatimTextOutput('bugs')))
)


## set up Server---------------------------------------------------------------
server <- function(input, output, session){
  ## check that data is loaded-------------------------------------------------
  filedata <- reactive({
    infile <- input$raw.data
    if (is.null(infile))
      # if nothing uploaded use NULL to prevent observeEvent from triggering
      return(NULL)
    tbl <- read_csv(infile$datapath)
    return(tbl)
  })
  
  ## update checkboxes---------------------------------------------------------
  observeEvent(filedata(), {
    updateCheckboxGroupInput(session,
                             inputId = 'boxes',
                             choices = unique(filedata()$`Peak Nr.`),
                             selected = c(1:4))
  })
  # observeEvent(filedata(), {
  #   updateCheckboxGroupInput(session,
  #                            inputId = 'standards',
  #                            choices = unique(filedata()$`Identifier 2`),
  #                            selected = c(1:4))
  # })
  
  ## subset data based on checkboxes-------------------------------------------
  sample.peaks <- reactive({
    peaks <- filedata()
    if(is.null(peaks)) {return(NULL)}
    
    peaks <- filedata() %>% 
      select( 
        `Peak Nr.`,
        `Identifier 1`,
        `Identifier 2`,
        `Ampl. 44`,
        `d 13C/12C`,
        `d 18O/16O`) %>%
      filter(!(dat$`Peak Nr.` %in% input$boxes))
    return(peaks)
  })
  
  stand <- reactive({
    peaks <- filedata()
    if(is.null(peaks)) {return(NULL)}
    peaks <- filedata() %>% 
      select( 
        `Peak Nr.`,
        `Identifier 1`,
        `Identifier 2`,
        `Ampl. 44`,
        `d 13C/12C`,
        `d 18O/16O`) %>%
      filter((dat$`Identifier 2` %in% input$standards) & !(dat$`Peak Nr.` %in% input$boxes))
    return(peaks)
  })
  
  ## render outputs------------------------------------------------------------
  output$peak.table <- renderDataTable(sample.peaks())
  output$raw.table  <- renderDataTable(filedata())
  output$stds <- renderDataTable(stand())
  output$bugs <- renderText(input$boxes)
}

shinyApp(ui, server)

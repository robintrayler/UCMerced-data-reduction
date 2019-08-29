## Load required libraries-----------------------------------------------------
library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)

## set up ui ------------------------------------------------------------------
ui <- navbarPage(
  theme = shinythemes::shinytheme('sandstone'),
  # shinythemes::themeSelector(),
  title = 'GASBENCH II DATA REDUCTION v.0.1',
  
  ## tab for file input
  tabPanel(title = 'LOAD DATA',
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h2('1) load a file'),
               p('Please select .csv file exported from Isodat 3.0 
                            using the "Carbonates" export template.'),
               fileInput(inputId = 'raw_data', label = ''),
               h2('2) select peaks to exclude'),
               p('Select the reference peaks to exclude. The default Carbonates 
                 GasBench method begins with 3 reference gas peaks followed by 7 sample
                 peaks. The reference gas peaks MUST be excluded. By default this program also
                 excludes the first sample peak to limit carryover effects.'),
               checkboxGroupInput(inputId = 'selected_peaks', label = '')
             ),
             mainPanel(dataTableOutput(outputId = 'peak_filtered_table')))),
  
  ## tab for standard selection
  tabPanel(title = 'SELECT STANDARDS',
           sidebarLayout(
             sidebarPanel(width = 4,
                          h2('3) select standards'),
                          p('please select standards to use for data correction'),
                          checkboxGroupInput(inputId = 'selected_standards', label = '')),
             mainPanel(h2('standards plot'),
               plotlyOutput('standards_plot'),
               h2('certified standard values'),
                       dataTableOutput(outputId = 'standard_values'))
           )),
  
  ## tab for linearity corrections
  tabPanel(title = 'LINEARITY'),
  
  ## tab for drift corrections
  tabPanel(title = 'DRIFT CORRECTION',
           sidebarLayout(
             sidebarPanel(h2('4) select standard for drift correction'),
                          p('some text here about drift corrections'),
                          selectInput(inputId = 'drift_standard', 
                                      label = '', choices = NA),
                          h3('perform drift correction?'),
                          radioButtons(inputId = 'drift_correction',
                                       label = '',
                                       choices = c('yes','no'),
                                       selected = 'no')),
             mainPanel(plotlyOutput(outputId = 'drift_correction_plot'))
           )),
  
  ## tab for standard corrections
  tabPanel(title = 'STANDARD CORRECTIONS'),
  
  ## tab for final report
  tabPanel(title = 'FINAL REPORT',
           mainPanel(
             dataTableOutput(outputId = 'final_report_table')
           )),
  
  ## tab for raw datas
  tabPanel(title = 'RAW DATA',
           mainPanel(
             dataTableOutput(outputId = 'raw_data_table')
           )),
  
  ## tab for debug stuff
  tabPanel(title = 'DEBUG',
           mainPanel(verbatimTextOutput(outputId = 'debug'))),
  
  ## tab for info
  tabPanel(title = 'INFORMATION',
           p('This software was developed by Robin B. Trayler')))

## set up server --------------------------------------------------------------
server <- function(input, output, session){
  ## hard coded table of standard values 
  standards_table <- 
    tibble(name = c('NBS 18', 'NBS 19', 'USGS 44', 'CM'), 
           d13C_true = c(-5.014, 1.95, -42.1, 2.10),
           d18O_true = c(-23.2, -2.20, NA, -2.01))
  
  ## check if data is loaded
  filedata <- reactive({
    infile <- input$raw_data
    if (is.null(infile)) {return(NULL)}
    # if nothing uploaded use NULL to prevent errors
    tbl <- read_csv(infile$datapath)
    return(tbl)
  })
  
  ## update select peak checkboxes --------------------------------------------
  observeEvent(filedata(), {
    updateCheckboxGroupInput(session,
                             inputId = 'selected_peaks',
                             choices = unique(filedata()$`Peak Nr.`),
                             selected = c(1:4))
  })
  
  ## check for extra peaks ----------------------------------------------------
  observeEvent(filedata(), {
    if (length(unique(filedata()$`Peak Nr.`)) > 10) {
      showNotification('Warning! Some samples have more than 10 peaks.
                       10 peaks are expected by default. Please check
                       raw data for excess peaks.',
                       type = 'warning',
                       duration = 15)
    }
  })
  
  ## filter out selected peaks ------------------------------------------------
  peak_filtered_data <- reactive({
    if (is.null(filedata())) {return(NULL)}
    data <- filedata() %>%
      select(
        `Peak Nr.`,
        `Identifier 1`,
        `Identifier 2`,
        `Ampl. 44`,
        `d 13C/12C`,
        `d 18O/16O`)  %>%
      filter(!(`Peak Nr.` %in% input$selected_peaks))
    return(data)
  })
  
  ## summarize the data -------------------------------------------------------
  data_summary <- reactive({
    if (is.null(peak_filtered_data())) {return(NULL)}
    data <- peak_filtered_data() %>% 
      group_by(`Identifier 1`) %>%
      summarise(d13C_measured = mean(`d 13C/12C`),
                d13C_sd = sd(`d 13C/12C`),
                d18O_measured = mean(`d 18O/16O`),
                d18O_sd = sd(`d 18O/16O`),
                name = unique(`Identifier 2`)) %>% 
      mutate_if(is.numeric, round, 2)
    return(data)
  })
  
  ## update standards checkbox ------------------------------------------------
  observeEvent(filedata(), {
    updateCheckboxGroupInput(session,
                             inputId = 'selected_standards',
                             choices = unique(standards_table[['name']]),
                             selected = c(1:4))
  })
  
  ## filter standards table ---------------------------------------------------
  use_standards <- reactive({
    data <- standards_table %>% filter(name %in% input$selected_standards)
    return(data)
  })
  
  ## join true and measured values --------------------------------------------
  stds_only <- reactive({
    data_summary() %>% filter(name %in% input$selected_standards)
  })
  
  stds_correction_table <- reactive({
    inner_join(use_standards(), stds_only(), by = 'name')
  }) 
  
  ## update drift correction options ------------------------------------------
  observeEvent(use_standards(), {
    updateSelectInput(session, inputId = 'drift_standard',
                      choices = input$selected_standards)
  })
  
  ## calculate coefficients for drift correction ------------------------------
  drift_coef <- reactive({
    if (is.null(use_standards()) | input$drift_correction == 'no') {
      return(NULL)
    }
    if (input$drift_correction == 'yes') {
      data <- stds_correction_table() %>% filter(name == input$drift_standard)
      drift_fit <- lm(data$d13C_measured ~ data$`Identifier 1`)
      return(coef(drift_fit))
    }
  })
  
  ## render outputs -----------------------------------------------------------
  ## table with selected peaks removed
  output$peak_filtered_table <- renderDataTable(peak_filtered_data())
  
  ## selected standards table 
  output$standards_plot <- renderPlotly({
    stds_correction_table() %>% 
      plot_ly(x = ~d13C_measured, 
              y = ~d13C_true, 
              type = 'scatter',
              mode = 'markers',
              marker = list(
                size = 10,
                color = 'gold'
              ),
              name = 'd13C') %>% 
      add_markers(x = ~d18O_measured, ~ d18O_true,
                  marker = list(size = 10, 
                                color = 'steelblue'),
                  name = 'd18O') %>% 
      layout(xaxis = list(title = 'measured value'),
             yaxis = list(title = 'true value'))
  })
  output$standard_values <- renderDataTable(use_standards())
  
  ## raw data table
  output$raw_data_table <- renderDataTable(filedata())
  
  ## final report table
  output$final_report_table <- renderDataTable(data_summary())
  
  ## debug table 
  output$debug <- renderText(drift_coef())
}

shinyApp(ui, server)
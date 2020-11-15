library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(sf)
import::from('data.table', '%between%')

## load data
radpko_bases_m <- readRDS('radpko_bases_m.rds')
radpko_bases_y <- readRDS('radpko_bases_y.rds')
radpko_adm2_m <- readRDS('radpko_adm2_m.rds')
radpko_adm2_y <- readRDS('radpko_adm2_y.rds')

ssa <- readRDS('ssa.Rds')


## define lists for checkboxes
missions <- as.list(unique(radpko_bases_y$mission))
names(missions) <- unique(radpko_bases_y$mission)
contribs <- list()
contribs$countries <- radpko_bases_y %>%
  select(albania:zambia) %>% 
  select(!matches('_')) %>%
  names()
contribs$regions <- c('afr', 'asian', 'west')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      ## input: spatial unit
      radioButtons(inputId = 'unit',
                   label = 'Geographic unit:',
                   choices = list('Base',
                                  'ADM2'),
                   selected = 'Base',
                   inline = T),
      
      ## input: timescale
      radioButtons(inputId = 'timescale',
                   label = 'Temporal unit:',
                   choices = list('Month' = T,
                                  'Year' = F),
                   selected = T,
                   inline = T),
      
      ## input: year
      sliderInput(inputId = 'year',
                  label = 'Year:',
                  min = min(radpko_bases_y$year),
                  max = max(radpko_bases_y$year),
                  value = range(radpko_bases_y$year),
                  step = 1,
                  sep = ''),
      
      ## input: checkboxes for mission
      pickerInput(inputId = 'mission',
                  label = 'Mission:',
                  choices = missions,
                  selected = missions,
                  multiple = T,
                  options = list(`actions-box` = T,
                                 `selected-text-format` = 'count > 3')),
      
      ##
      checkboxInput(inputId = 'cc',
                    label = 'List contributing countries?',
                    value = T),
      
      ##
      checkboxInput(inputId = 'country_vars',
                    label = 'Include country summaries?',
                    value = T),
      
      ## input: contributors
      checkboxGroupInput(inputId = 'contributors',
                         label = 'Troop contributors:',
                         choices = list('Region' = 'regions',
                                        'Country' = 'countries'),
                         selected = list('Region' = 'regions',
                                         'Country' = 'countries')),
      
      ## input: personnel type
      checkboxGroupInput(inputId = 'personnel',
                         label = 'Personnel type:',
                         choices = list('Peacekeepers' = '_pko',
                                        'Troops' = '_untrp',
                                        'Police' = '_unpol',
                                        'Observers' = '_unmob'),
                         selected = c('_pko', '_untrp', '_unpol', '_unmob')),
      
      pickerInput(inputId = 'format',
                  label = 'Download format: ',
                  choices = list('.csv', '.dta'),
                  selected = '.csv'),
      
      ## download button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = 'coverage_plot'),
      textOutput('vars')
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  ## select yearly or monthly data
  timescale <- reactive({
    
    if (input$unit == 'Base') {
      
      if (input$timescale) {
        
        radpko_bases_m
        
      } else {
        
        radpko_bases_y
        
      }
      
    } else if (input$unit == 'ADM2') {
      
      if (input$timescale) {
        
        radpko_adm2_m
        
      } else {
        
        radpko_adm2_y
        
      }
      
    }
    
  })
  
  ## filter by years
  filter.year <- reactive({
    
    timescale() %>% filter(year %between% input$year)
    
  })
  
  ## filter by mission
  filter.mission <- reactive({
    
    filter.year() %>% st_drop_geometry() %>% filter(mission %in% input$mission)
    
  })
  filter.mission.sf <- reactive({
    
    filter.year() %>% filter(mission %in% input$mission) %>% 
      group_by(id) %>% 
      slice(1)
    
  })
  
  ## 
  filter.contributors <- reactive({
    
    if (!is.null(input$contributors)) {
      
      contributors_re <- str_c('^', unname(unlist(contribs[input$contributors])),
                               collapse = '|')
      
      filter.mission() %>% select(mission:f_unmob,
                                  matches('^cc|_cc$'),
                                  matches(contributors_re))
      
    } else {
      
      filter.mission() %>% select(mission:f_unmob,
                                  matches('^cc|_cc$'))
      
    }
    
  })
  
  ## filter by personnel type
  filter.personnel <- reactive({
    
    ## limit to selected personnel
    if (!is.null(input$personnel)) {
      
      ## include country variables
      if (input$country_vars) {
        
        filter.contributors() %>% select(mission:f_unmob,
                                         matches('^cc|_cc$'),
                                         any_of(contribs$countries),
                                         matches(str_c(str_c('.*',
                                                             input$personnel, '$'),
                                                       collapse = '|')))
        
      } else { # exclude country variables
        
        filter.contributors() %>% select(mission:f_unmob,
                                         matches('^cc|_cc$'),
                                         matches(str_c(str_c('.*',
                                                             input$personnel, '$'),
                                                       collapse = '|')))
        
      }
      
    } else if (input$country_vars) {
      
      filter.contributors() %>% select(mission:f_unmob,
                                       matches('^cc|_cc$'),
                                       any_of(contribs$countries))
      
    } else {
      
      filter.contributors() %>% select(mission:f_unmob,
                                       matches('^cc|_cc$'))
      
    }
    
  })
  
  ## include contributing countries?
  filter.cc <- reactive({
    
    if (input$cc) {
      
      filter.personnel()
      
    } else {
      
      filter.personnel() %>% 
        select(-matches('^cc|_cc$'))
      
    }
    
  })
  
  data.out <- reactive({
    
    filter.cc() %>% select(any_of(names(radpko_bases_m)))
    
  })
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    if (input$timescale) {
      
      filter.mission() %>% 
        group_by(date, mission) %>%
        summarize(pko_deployed = sum(pko_deployed)) %>% 
        ggplot(aes(x = date, y = pko_deployed, color = mission)) +
        geom_line() +
        labs(x = '', y = 'Total peacekepers deployed') +
        scale_color_discrete(name = 'Mission') +
        theme_bw() +
        theme(panel.grid = element_blank())
      
    } else {
      
      filter.mission() %>% 
        group_by(year, mission) %>%
        summarize(pko_deployed = sum(pko_deployed)) %>% 
        ggplot(aes(x = year, y = pko_deployed, color = mission)) +
        geom_line() +
        labs(x = '', y = 'Total peacekepers deployed') +
        scale_color_discrete(name = 'Mission') +
        theme_bw() +
        theme(panel.grid = element_blank())
      
    }
    
    
  })
  
  output$coverage_plot <- renderPlot({
    
    # plot(ssa$geom)
    # plot(filter.mission.sf()$geometry, add = T, pch = 16, col = '#5b92e5')
    
    ssa %>% 
      st_filter(filter.mission.sf()) %>% 
      ggplot() +
      geom_sf() +
      geom_sf(data = filter.mission.sf(), inherit.aes = F,
              color = '#5b92e5', fill = '#5b92e5') +
      theme_bw() +
      theme(panel.grid = element_blank())
    
  })
  
  output$vars <- renderText(str_c(format(nrow(data.out()),
                                         big.mark = ',', scientific = F),
                                  ' observations; ',
                                  ncol(data.out()),
                                  ' variables'))
  
  ## create download handler with appropriate file extension and content function
  output$downloadData <- downloadHandler(
          filename = function() {
            str_c('radpko', input$format)
            },
          content = function(file) {
            if (input$format == '.csv') {
              write_csv(data.out(), file)
            } else if (input$format == '.dta') {
              haven::write_dta(data.out() %>%
                                 rename_with(~ str_replace_all(.x, '\\.', '_')),
                               file)
            }
          }
        )
  
}

shinyApp(ui = ui, server = server, options = list('display.mode' = 'showcase'))

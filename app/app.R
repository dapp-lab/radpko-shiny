library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(sf)
library(leaflet)
import::from('data.table', '%between%')

## load data
radpko_bases_m <- readRDS('radpko_bases_m.rds')
radpko_bases_y <- readRDS('radpko_bases_y.rds')
radpko_adm0_m <- readRDS('radpko_adm0_m.rds')
radpko_adm0_y <- readRDS('radpko_adm0_y.rds')
radpko_adm2_m <- readRDS('radpko_adm2_m.rds')
radpko_adm2_y <- readRDS('radpko_adm2_y.rds')
radpko_grid_m <- readRDS('radpko_grid_m.rds')
radpko_grid_y <- readRDS('radpko_grid_y.rds')

## reset CRS to deal with older PROJ on shinyapps.io
st_crs(radpko_bases_m) <- 4326
st_crs(radpko_bases_y) <- 4326
st_crs(radpko_adm0_m) <- 4326
st_crs(radpko_adm0_y) <- 4326
st_crs(radpko_adm2_m) <- 4326
st_crs(radpko_adm2_y) <- 4326
st_crs(radpko_grid_m) <- 4326
st_crs(radpko_grid_y) <- 4326

## define lists for checkboxes
missions <- as.list(unique(radpko_bases_y$mission))
names(missions) <- unique(radpko_bases_y$mission)
contribs <- radpko_bases_y %>%
  st_drop_geometry() %>% 
  select(albania:zambia) %>% 
  select(!matches('_')) %>%
  names()

contribs <- setNames(as.list(c('afr', 'asian', 'west', contribs)),
                     str_to_title(c('African', 'Asian', 'Western', contribs)))


## create bounding box for empty leaflet
bbox_bases <- unname(st_bbox(radpko_bases_y))
bbox_adm0 <- unname(st_bbox(radpko_adm0_y))
bbox_adm2 <- unname(st_bbox(radpko_adm2_y))
bbox_grid <- unname(st_bbox(radpko_grid_y))

## define UI
ui <- fluidPage(
  ## input
  fluidRow(
    column(5,
           ## input: spatial unit
           radioButtons(inputId = 'unit',
                        label = 'Geographic unit:',
                        choices = list('Base',
                                       'PRIO-GRID',
                                       'ADM2',
                                       'Country'),
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
                       sep = '')
    ),
    column(3,
           ## input: dropdown for mission
           pickerInput(inputId = 'mission',
                       label = 'Mission:',
                       choices = missions,
                       selected = missions,
                       multiple = T,
                       options = list(`actions-box` = T,
                                      `selected-text-format` = 'count > 3')),
           ## input: contributors
           pickerInput(inputId = 'contributors',
                       label = 'Troop contributors:',
                       choices = contribs,
                       selected = contribs,
                       multiple = T,
                       options = list(`actions-box` = T,
                                      `selected-text-format` = 'count > 3')),
           ## input: personnel type
           pickerInput(inputId = 'personnel',
                       label = 'Personnel type:',
                       choices = list('Total peacekeepers' = 'pko',
                                      'Troops' = 'untrp',
                                      'Police' = 'unpol',
                                      'Observers' = 'unmob'),
                       selected = list('Total peacekeepers' = 'pko',
                                       'Troops' = 'untrp',
                                       'Police' = 'unpol',
                                       'Observers' = 'unmob'),
                       multiple = T,
                       options = list(`actions-box` = T,
                                      `selected-text-format` = 'count > 3'))
    ),
    column(4,
           ##input: download type
           pickerInput(inputId = 'format',
                       label = 'Download format: ',
                       choices = list('.csv', '.dta'),
                       selected = '.csv'),
           
           ## download button
           downloadButton('downloadData', 'Download'))
  ),
  tabsetPanel(
    id = 'tabs',
    selected = 'map',
    tabPanel('Map', value = 'map',
             leafletOutput(outputId = 'leaflet'),
             textOutput('vars')
    ),
    tabPanel('Histogram', value = 'hist',
             plotOutput(outputId = 'histogram_plot')),
    tabPanel('Timeseries', value = 'ts',
             plotOutput(outputId = 'timeseries_plot')),
    tabPanel('Code', value = 'code',
             'test text')
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  ## select temporal and geographic unit
  timescale <- reactive({
    
    if (input$tabs == 'map') {
      
      if (input$unit == 'Base') {
        
        if (input$timescale) {
          
          radpko_bases_m
          
        } else {
          
          radpko_bases_y
          
        }
        
      } else if (input$unit == 'PRIO-GRID') {
        
        if (input$timescale) {
          
          radpko_grid_m
          
        } else {
          
          radpko_grid_y
          
        }
        
      } else if (input$unit == 'ADM2') {
        
        if (input$timescale) {
          
          radpko_adm2_m
          
        } else {
          
          radpko_adm2_y
          
        }
        
      } else if (input$unit == 'Country') {
        
        if (input$timescale) {
          
          radpko_adm0_m
          
        } else {
          
          radpko_adm0_y
          
        }
        
      }
      
    } else if (input$tabs == 'hist' | input$tabs == 'ts') {
      
      if (input$timescale) {
        
        radpko_bases_m
        
      } else {
        
        radpko_bases_y
        
      }
      
    }
    
  })
  
  ## filter by years
  filter.year <- reactive({
    
    timescale() %>% filter(year %between% input$year)
    
  })
  
  ## filter by mission
  filter.mission <- reactive({
    
    if (is.null(input$mission)) {
      
      filter.year() %>% slice(0)
      
    } else {
      
      filter.year() %>%
        st_drop_geometry() %>%
        filter(str_detect(mission,
                          str_c(input$mission, collapse = '|')))
      
    }
    
  })
  filter.mission.sf <- reactive({
    
    filter.year() %>%
      filter(str_detect(mission, str_c(input$mission, collapse = '|'))) %>% 
      group_by(id) %>% 
      slice(1)
    
  })
  
  ## filter by country and region contributors
  filter.contributors <- reactive({
    
    if (is.null(input$contributors)) {
      
      filter.mission()
      
    } else {
      
      contributors_re <- str_c('^', input$contributors,
                               collapse = '|')
      
      ## get maximum value in each contributor column by row
      contributors_filter <- filter.mission() %>%
        select(matches(contributors_re), -matches('^cc|_cc$')) %>% 
        apply(1, max, na.rm = T)
      
      filter.mission()[contributors_filter > 0, ] %>%
        select(id:f_unmob,
               matches('^cc|_cc$'),
               matches(contributors_re))
      
    }
    
  })
  filter.contributors.sf <- reactive({
    
    if (is.null(input$contributors)) {
      
      filter.mission.sf()
      
    } else {
      
      contributors_re <- str_c('^', input$contributors,
                               collapse = '|')
      
      ## get maximum value in each contributor column by row
      contributors_sf_filter <- filter.mission.sf() %>%
        select(matches(contributors_re), -matches('^cc|_cc$')) %>% 
        st_drop_geometry() %>% 
        apply(1, max, na.rm = T)
      
      filter.mission.sf()[contributors_sf_filter > 0, ]
      
    }
    
  })
  
  ## filter by personnel type
  filter.personnel <- reactive({
    
    ## limit to selected personnel
    if (is.null(input$personnel)) {
      
      filter.contributors() %>% select(id:f_unmob,
                                       matches('^cc|_cc$'),
                                       any_of(unlist(contribs, use.names = F)))

    } else {
      
      personnel_filter <- filter.contributors() %>%
        select(matches(str_c(str_c('.*', input$personnel, '$'),
                             collapse = '|'))) %>% 
        apply(1, max, na.rm = T)
      
      filter.contributors()[personnel_filter > 0, ] %>%
        select(id:f_unmob,
               matches('^cc|_cc$'),
               any_of(unlist(contribs, use.names = F)),
               matches(str_c(str_c('.*',
                                   input$personnel, '$'),
                             collapse = '|')))
      
    }
    
  })
  filter.personnel.sf <- reactive({
    
    ## limit to selected personnel
    if (is.null(input$personnel)) {
      
      filter.contributors.sf() %>% select(id:f_unmob,
                                       matches('^cc|_cc$'),
                                       any_of(unlist(contribs, use.names = F)))
      
    } else {
      
      personnel_filter <- filter.contributors.sf() %>%
        select(matches(str_c(str_c('.*', input$personnel, '$'),
                             collapse = '|'))) %>% 
        st_drop_geometry() %>% 
        apply(1, max, na.rm = T)
      
      filter.contributors.sf()[personnel_filter > 0, ]
      
    }
    
  })
  
  ## re-order columns to initial order
  data.out <- reactive({
    
    filter.personnel() %>% select(any_of(names(radpko_bases_m)))
    
  })
  
  ## output leaflet
  output$leaflet <- renderLeaflet({
    
    if (input$unit == 'Base') {
      
      if (is.null(input$mission)) {
        
        leaflet(radpko_bases_y) %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          fitBounds(bbox_bases[1], bbox_bases[2], bbox_bases[3], bbox_bases[4])
        
      } else {
        
        filter.personnel.sf() %>% 
          leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addCircleMarkers(radius = 2.5, stroke = F, label = ~str_to_title(id),
                           fill = T, fillOpacity = 1, fillColor = '#5b92e5')
        
      }
      
    } else if (input$unit == 'PRIO-GRID') {
      
      if (is.null(input$mission)) {
        
        leaflet(radpko_grid_y) %>% 
          addProviderTiles(providers$CartoDB.Positron)  %>% 
          fitBounds(bbox_grid[1], bbox_grid[2], bbox_grid[3], bbox_grid[4])
        
      } else {
        
        filter.personnel.sf() %>% 
          leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(stroke = F, fill = T, label = ~id,
                      fillOpacity = 1, fillColor = '#5b92e5',
                      smoothFactor = .75)
      }
      
    } else if (input$unit == 'ADM2') {
      
      if (is.null(input$mission)) {
        
        leaflet(radpko_adm2_y) %>% 
          addProviderTiles(providers$CartoDB.Positron)  %>% 
          fitBounds(bbox_adm2[1], bbox_adm2[2], bbox_adm2[3], bbox_adm2[4])
        
      } else {
        
        filter.personnel.sf() %>% 
          leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(stroke = F, fill = T, label = ~id,
                      fillOpacity = 1, fillColor = '#5b92e5',
                      smoothFactor = .25)
      }
      
    } else if (input$unit == 'Country') {
      
      if (is.null(input$mission)) {
        
        leaflet(radpko_adm0_y) %>% 
          addProviderTiles(providers$CartoDB.Positron)  %>% 
          fitBounds(bbox_adm0[1], bbox_adm0[2], bbox_adm0[3], bbox_adm0[4])
        
      } else {
        
        filter.personnel.sf() %>% 
          leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(stroke = F, fill = T, label = ~id,
                      fillOpacity = 1, fillColor = '#5b92e5',
                      smoothFactor = .25)
      }
      
    } 
    
  })
  
  ## output time series plot
  output$timeseries_plot <- renderPlot(
    filter.personnel() %>%
      select(any_of(names(radpko_bases_m))) %>% # return to original order
      group_by(mission, date) %>% 
      summarize(across(c(pko_deployed, mltipko_deployed, untrp:west_unmob), mean),
                .groups = 'drop') %>% 
      ggplot(aes(x = date, y = pko_deployed, color = mission)) +
      labs(x = 'Date', y = 'Count') +
      geom_line() +
      scale_color_manual(name = 'Mission',
                         breaks = unname(unlist(missions)),
                         values = colorRampPalette(colorblind_pal()(8))(length(missions))) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank())
  )
  
  ## output histogram plot
  output$histogram_plot <- renderPlot(
    filter.personnel() %>%
      select(any_of(names(radpko_bases_m))) %>% # return to original order
      group_by(mission) %>% 
      summarize(across(any_of(ifelse(input$personnel == 'pko', 'pko_deployed', input$personnel)), mean),
                .groups = 'drop_last') %>%
      pivot_longer(-mission) %>% 
      mutate(name = factor(name, levels = c('pko_deployed', 'untrp', 'unpol', 'unmob'))) %>% 
      ggplot(aes(x = name, y = value, color = name, fill = name)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ mission, scales = 'free_y') +
      labs(x = str_c('Average', ifelse(input$timescale, 'monthly', 'yearly'),
                     'personnel deployment', sep = ' '), y = 'Count') +
      scale_color_manual(breaks = c('pko_deployed', 'untrp', 'unpol', 'unmob'),
                         labels = c('Total peacekeepers', 'Troops', 'Police', 'Observers'),
                         values = colorblind_pal()(4),
                         name = '') +
      scale_fill_manual(breaks = c('pko_deployed', 'untrp', 'unpol', 'unmob'),
                        labels = c('Total peacekeepers', 'Troops', 'Police', 'Observers'),
                        values = colorblind_pal()(4),
                        name = '') +
      theme_bw() +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
            legend.position = 'bottom', panel.grid = element_blank(),
            panel.border = element_blank()) +
      guides(colour = guide_legend(nrow = 1))
  )
  
  
  ## output data dimensionality
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

shinyApp(ui = ui, server = server, options = list('display.mode' = 'normal'))

source('./code/main.R')
library(shiny)
library(leaflet)
library(raster)

#----------load data--------------
luter <- as.data.table(readRDS('./data/input/gridded/luterbacher/luterbacher.rds'))
luter <- as.data.frame(luter)  # change format of year to numeic
luter[,3] <- sapply(luter[, 3], as.numeric)
luter <- as.data.table(luter)

#-------------time series markers grid--------------
one_layer <- luter[year == 1500 & season == 'wi']
grid_id <- one_layer [, c('lon', 'lat', 'cell_id')]

#--------------user interface------------------
ui<- fluidPage(titlePanel('Seasonal temperature data - Luterbacher'),
               fluidRow( column(width = 5, offset = 0.5,
                                numericInput('chosen_year', 'Enter year (valid input: 1500 - 1900):',
                                             value = 1500,  min = 1500, max = 1900, step = 1),
                                radioButtons('seas', 'Season', c('winter' = 'wi', 'spring' = 'sp', 'summer' = 'su', 'autumn' = 'au'), inline = TRUE), 
                                actionButton('btn', 'Select')
                                
               )),
               splitLayout(
                 leafletOutput('map1', width = '100%', height = "550px"),
                 plotOutput('plot')
               ))

#----------------------server------------------------

server <- shinyServer(function(input, output) {
  
  subset_data <- eventReactive(input$btn, {                 # map with Run button  
    data_year <- luter[year == input$chosen_year]          # input year data table filter
    data_seas <- data_year[season == input$seas]             # input season data table filter
    data_cut <- data_seas[, c('lon', 'lat',  'temp')] 
    raster_data <- rasterFromXYZ(data_cut, crs = '+proj=longlat +datum=WGS84')  # convert to raster (only X Y Z input)
    return(raster_data)
  })
  
  output$map1 <- renderLeaflet({
    col_pal <- colorNumeric(colorRamp(c("#3300CC", "#FF0000")),
                            values(subset_data()$temp), na.color = "transparent")  #colour palet
    leaflet()%>% addTiles()%>%    
      addRasterImage(subset_data(), colors = col_pal, opacity = 0.8,  group = 'Raster')%>%
      addAwesomeMarkers(grid_id$lon, grid_id$lat,  group = 'Clickable time series',
                        options = markerOptions(opacity = 0), layerId = grid_id$cell_id)%>% 
      addLegend(pal = col_pal, values = values(subset_data()$temp), title = "Temperature")%>%
      addLayersControl(baseGroups = 'Raster', overlayGroups = 'Clickable time series', options = layersControlOptions(collapsed = FALSE)) # turn off marker layer to make quicker leaflet reaction  
  })
  
  store_react <- reactiveValues(clickedMarker = NULL) # reactive values
  observeEvent(input$map1_marker_click, {
    store_react$clickedMarker <- input$map1_marker_click
    
    output$plot <- renderPlot({
      ggplot(luter[luter$cell_id == store_react$clickedMarker$id], aes(year, temp)) +  # plot precip by season in every year
        geom_line() + 
        geom_vline(xintercept = unique(luter[year == input$chosen_year]$year), col = 'red') +
        facet_wrap(~season) +
        theme_bw() + 
        ggtitle(paste('Seasonal temperature in lon:',store_react$clickedMarker$lon,
                      ' lat:',store_react$clickedMarker$lat ))+ # print clicked tile as header
        ylab('Temp. (°C)') + 
        xlab('Year')
    })})
})


#------------run shiny----------------
shinyApp(ui, server)

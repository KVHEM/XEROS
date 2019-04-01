library(RDS)
library(shiny)
library(data.table)
library(leaflet)
library(raster)


#----------load data--------------
pauling <- as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))
#pauling <- readRDS("../../Projects/2018XEROS/data/input/gridded/pauling/pauling.rds")   #secondary path 

pauling[, precip_z := scale(precip), .(season, lat, long)]

#--------------user interface------------------
ui<- fluidPage(titlePanel('Seasonal precipitation data - Pauling'),
  sidebarPanel(
  numericInput('chosen_year', 'Enter year (valid input: 1500 - 2000):',
                 value = 1500,  min = 1500, max = 2000, step = 1),
   radioButtons('seas', 'Season', c('winter'='wi', 'spring'='sp', 'summer'='su', 'autumn'='au')), 
   actionButton('btn', 'Select')
    
  ),
  mainPanel(column(12,
    leafletOutput('map1', width = '100%',height="550px")
  )))


#----------------------server------------------------

server <- function(input, output, session) {
  subset_data <- eventReactive(input$btn, {                 # map with Run button  
    data_year <- pauling[year == input$chosen_year]          # input year data table filter
    data_seas <- data_year[season == input$seas]             # input season data table filter
    data_cut <- data_seas[, c('long', 'lat', 'precip_z')]
    raster_data <- rasterFromXYZ(data_cut, crs = '+proj=longlat +datum=WGS84')  # convert to raster (only X Y Z input)
    return(raster_data)
   })

  output$map1 <- renderLeaflet({
    col_pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")),
                            values(subset_data()$precip_z), na.color = "transparent")  #colour palet
    leaflet() %>% addTiles() %>%    
    addRasterImage(subset_data(), colors = col_pal, opacity = 0.8) %>%
    addLegend(pal = col_pal, values = values(subset_data()$precip_z), title = "Precip. (z-score)")
  })
}

#------------run shiny----------------
shinyApp(ui, server)





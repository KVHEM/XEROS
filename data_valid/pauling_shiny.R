library(RDS)
library(shiny)
library(data.table)
library(leaflet)
library(raster)


#----------load data--------------
pauling <-as.data.table(readRDS('../../data/input/gridded/pauling/pauling.rds'))

#--------------user interface------------------
ui<- fluidPage(fluidRow(
  sidebarPanel(
  numericInput('chosen_year', 'Enter the year:\n (valid input: 1500 - 2000)',
                 value = 1500,  min = 1500, max = 2000, step = 1),
   radioButtons('seas', 'Season', c('winter'='wi', 'spring'='sp', 'summer'='su', 'autumn'='au')), 
   actionButton('btn', 'Run')
    
  ),
  mainPanel(
    leafletOutput('map1')
  )))


#----------------------server------------------------

server <- function(input, output, session) {
  
  
    subset_data <- eventReactive(c(input$seas, input$chosen_year),{
    data_year <- pauling[year==input$chosen_year] 
    data_seas <- data_year[season==input$seas] 
    data_cut <- data_seas[,c( 'lat','long', 'precip')]
    raster_data <- rasterFromXYZ(data_cut,crs='+proj=longlat +datum=WGS84')
    return(raster_data)
   })
   

 
  
  output$map1 <- renderLeaflet({
    col_pal <- colorNumeric(rev(c("#0C2C84", "#41B6C4", "#FFFFCC")),
                            values(subset_data()$precip), na.color = "transparent")
    leaflet() %>% addTiles() %>% addProviderTiles(providers$Esri.WorldImagery)%>%
    addRasterImage(subset_data(), colors = col_pal,opacity = 0.8) %>%
      addLegend(pal = col_pal, values = values(subset_data()$precip),
              title = "Precipitation")
  })
  
  
}


#------------run shiny----------------
shinyApp(ui, server)





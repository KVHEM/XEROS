library(leaflet)
library(shiny) 
library(ggplot2)

#---------------load data---------------------
#pages2k_ts <- readRDS("../../Projects/2018XEROS/data/input/point/pages2k_eu.rds")   #secondary path 
#pages2k_meta <- readRDS("../../Projects/2018XEROS/data/input/point/pages2k_eu_meta.rds") #secondary path
pages2k_ts <- readRDS("../../data/input/point/pages2k_eu.rds")
pages2k_meta <- readRDS("../../data/input/point/pages2k_eu_meta.rds")

# -----------prepare data for printing---------------
meta_print <- pages2k_meta[,c('name', 'id', 'region', 'long', 'lat', 'archive', 'min_year', 'max_year')] 
colnames(meta_print)<-c('Name', 'id', 'Region', 'Long', 'Lat', 'Archive', 'Year min.', 'Year max.')
#colour palette for legend and markers
pal <- colorFactor(palette = c('red', 'blue', 'orange','green'),domain = meta_print$Archive) 

# --------------user interface-------------
ui <- fluidPage(fluidRow(
 leafletOutput('map'),
  tableOutput('info'),
  plotOutput('plot')
))

#------------------shiny function------------------
server <- shinyServer(function(input, output) {
  store_react <- reactiveValues(clickedMarker=NULL) # reactive values
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
    addCircleMarkers(lng =meta_print$Long, lat = meta_print$Lat, 
                   layerId = meta_print$id, color = pal(meta_print$Archive)) %>%
    addLegend('topleft', pal = pal, values = meta_print$Archive)
  })
  # store reactive velues
  observeEvent(input$map_marker_click,{
    print('observed map_marker_click')
    store_react$clickedMarker <- input$map_marker_click
    print(store_react$clickedMarker)
    
  # print metadata of chosen marker
  output$info <- renderTable({
      return(subset(meta_print,id == store_react$clickedMarker$id))
  })
  
  # plot temp
  output$plot <- renderPlot({
    plot(ggplot(pages2k_ts[pages2k_ts$id %in% store_react$clickedMarker$id,], aes(time, temp)) +
           geom_line() +
           theme_bw())
  })})
})

#-------run shiny---------------------
shinyApp(ui, server)


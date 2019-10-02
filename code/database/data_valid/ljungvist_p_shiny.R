library(leaflet)
library(shiny) 
library(ggplot2)

#---------------load data---------------------
#ljungvist_ts <- readRDS("../../Projects/2018XEROS/data/input/point/ljungvist_eu.rds")   #secondary path 
#ljungvist_meta <- readRDS("../../Projects/2018XEROS/data/input/point/ljungvist_eu_meta.rds") #secondary path
ljungvist_ts <- readRDS("../../data/input/point/ljungvist_p.rds")
ljungvist_meta <- readRDS("../../data/input/point/ljungvist_p_meta.rds")

ids_eu <- ljungvist_meta[long > -20 & long < 40 & lat > 35, id]
ljungvist_ts <- ljungvist_ts[id %in% ids_eu]
ljungvist_meta <- ljungvist_meta[id %in% ids_eu]

# -----------prepare data for printing---------------
meta_print <- ljungvist_meta[,c('name', 'id', 'long', 'lat', 'proxy')] 
colnames(meta_print) <- c('Name', 'id', 'Long', 'Lat', 'Proxy')
#colour palette for legend and markers
pal <- colorFactor(palette = c('red', 'blue', 'orange','green'), domain = meta_print$Proxy) 

# --------------user interface-------------
ui <- fluidPage(fluidRow(
 leafletOutput('map'),
  tableOutput('info'),
  plotOutput('plot')
))

#------------------shiny function------------------
server <- shinyServer(function(input, output) {
  store_react <- reactiveValues(clickedMarker = NULL) # reactive values
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
    addCircleMarkers(lng =meta_print$Long, lat = meta_print$Lat, 
                   layerId = meta_print$id, color = pal(meta_print$Proxy)) %>%
    addLegend('topleft', pal = pal, values = meta_print$Proxy)
  })
  # store reactive velues
  observeEvent(input$map_marker_click, {
    print('observed map_marker_click')
    store_react$clickedMarker <- input$map_marker_click
    print(store_react$clickedMarker)
    
  # print metadata of chosen marker
  output$info <- renderTable({
      return(subset(meta_print, id == store_react$clickedMarker$id))
  })
  
  # plot precip
  output$plot <- renderPlot({
    plot(ggplot(ljungvist_ts[ljungvist_ts$id %in% store_react$clickedMarker$id, ], aes(time, value)) +
           geom_line() +
           theme_bw())
  })})
})

#-------run shiny---------------------
shinyApp(ui, server)


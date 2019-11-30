source('../../main.R')
library(leaflet)
library(shiny) 
library(plyr)

#---------------load and prepare data---------------------
dta <- readRDS(file = "../../data/output/database/ghcn_pauling.rds")
pauling <- data.table(readRDS('../../data/input/gridded/pauling/pauling.rds')) 

dtb <- pauling[cell_id %in% unique(dta$cell_id)]
meta_print <- unique(dta[, c('cell_id', 'lon', 'lat', 'period', 'season', 'n_val')] )
colnames(meta_print) <- c('id', 'Lon', 'Lat', 'Period', 'Season', 'N')
dtb[, mov_var := zoo::rollapplyr(precip, 1:.N, mean), by = .(cell_id, season)]

ecdf_plot <- ddply(dta, .(cell_id, season, period, dataset), summarize,
                   precip = unique(precip),
                   ecdf = ecdf(precip)(unique(precip)))

# --------------user interface-------------
ui <- fluidPage(fluidRow(
  leafletOutput('map'),
  tableOutput('info'),
  plotOutput('plot_dist'),
  plotOutput('plot_ts'),
  plotOutput('plot_all')
))

#------------------shiny function------------------
server <- shinyServer(function(input, output) {
  store_react <- reactiveValues(clickedMarker = NULL) # reactive values
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addCircleMarkers(lng = meta_print$Lon, 
                       lat = meta_print$Lat, 
                       layerId = meta_print$id, 
                       color = 'lightslategrey' 
                       ) 
  })
  # store reactive velues
  observeEvent(input$map_marker_click, {
    print('observed map_marker_click')
    store_react$clickedMarker <- input$map_marker_click
    print(store_react$clickedMarker)
    
    # print metadata of chosen marker
    output$info <- renderTable({
      return(subset(meta_print[, c('id', 'Period', 'Season', 'N')], id == store_react$clickedMarker$id))
    })
    # plot ecdf 
    output$plot_dist <- renderPlot({
      plot(ggplot(ecdf_plot[ecdf_plot$cell_id %in% store_react$clickedMarker$id, ], 
                  aes(x = precip, y = ecdf, color = dataset, linetype = period)) + 
             geom_line(lwd = 1) +
             scale_colour_manual(values = c("seagreen", "orange3")) +
             facet_wrap(~season) +
             theme_bw())
    })
    # plot timeseries/reconstruction comparison
    output$plot_ts <- renderPlot({
      plot(ggplot(dta[dta$cell_id %in% store_react$clickedMarker$id, ], 
                    aes(x = year, y = precip, color = dataset, linetype = period)) + 
               geom_line() +
               scale_colour_manual(values = c("seagreen", "orange3")) +
               facet_wrap(~season) +
               theme_bw())
    })
    #plot whole reconstruction
    output$plot_all <- renderPlot({
      plot(ggplot(dtb[dtb$cell_id %in% store_react$clickedMarker$id, ], 
                  aes(x = year, y = precip)) + 
             geom_line() +
             facet_wrap(~season) +
             theme_bw())
    }) 
  })
})

#-------run shiny---------------------
shinyApp(ui, server)


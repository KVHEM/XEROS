source('../../main.R')
source('../../graphics.R')
library(shiny) 
library(plotrix)
library(leaflet)

dta <- readRDS('../../data/output/database/norm_ghcn_paul_owda.rds')

meta_print <- unique(dta[, c('cell_id', 'lon', 'lat', 'period', 'season', 'n_val')])
colnames(meta_print) <- c('id', 'Lon', 'Lat', 'Period', 'Season', 'N')

# computing ecdf for both dta and owda
ecdf_plot <- ddply(dta, .(cell_id, season, period, dataset), summarize,
                   precip_scale = unique(precip_scale),
                   ecdf = ecdf(precip_scale)(unique(precip_scale)))

# adding station name to metadata
load("../../data/input/point/ghcn/ghcn_meta_seas.rdata")
ghcn_meta_seas[, id := NULL]
setnames(ghcn_meta_seas, old = 'cell_id', new = 'id')
meta_print <- unique(dta[, c('cell_id', 'lon', 'lat', 'period', 'season', 'n_val')])
colnames(meta_print) <- c('id', 'Lon', 'Lat', 'Period', 'Season', 'N')

meta_print <- merge(meta_print, ghcn_meta_seas, by = 'id')

to_plot_td <- dta[season == 'su']


# --------------user interface-------------
ui <- fluidPage(fluidRow(
  leafletOutput('map')),
  fluidRow(splitLayout(
    tableOutput('info'),
    plotOutput('plot_taylor_paul'),
    plotOutput('plot_taylor_owda'))),
  fluidRow(
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
      return(subset(meta_print[, c('id', 'station_name', 'Period', 'Season', 'N')], id == store_react$clickedMarker$id))
    })
    
    # plot taylor diagram
    output$plot_taylor_paul<- renderPlot({
      
      same_length_1 <- merge(to_plot_td[to_plot_td$cell_id %in% store_react$clickedMarker$id & 
                                        dataset == 'Pauling'], 
                           to_plot_td[to_plot_td$cell_id %in% store_react$clickedMarker$id & 
                                        dataset == 'GHCN'], by = 'year')
      
      same_length_2 <- merge(to_plot_td[to_plot_td$cell_id %in% store_react$clickedMarker$id & 
                                          dataset == 'OWDA'], 
                             to_plot_td[to_plot_td$cell_id %in% store_react$clickedMarker$id & 
                                          dataset == 'GHCN'], by = 'year')
      
      taylor.diagram(same_length_1$precip_scale.x, same_length_1$precip_scale.y, sd.arcs = TRUE, ref.sd = TRUE)
      
      taylor.diagram(same_length_2$precip_scale.x, same_length_2$precip_scale.y, add = TRUE, col = "blue")
    })
    
    # plot ecdf 
    output$plot_dist <- renderPlot({
      plot(ggplot(ecdf_plot[ecdf_plot$cell_id %in% store_react$clickedMarker$id, ], 
                  aes(x = precip_scale, y = ecdf, color = dataset, linetype = period)) + 
             geom_line(lwd = 1) +
             scale_colour_manual(values = c("seagreen", "orange3", 'red')) +
             facet_wrap(~season) +
             theme_bw())
    })
    # plot timeseries/reconstruction comparison
    output$plot_ts <- renderPlot({
      plot(ggplot(dta[dta$cell_id %in% store_react$clickedMarker$id, ], 
                  aes(x = year, y = precip_scale, color = dataset, linetype = period)) + 
             geom_line() +
             scale_colour_manual(values = c("seagreen", "orange3", 'red')) +
             facet_wrap(~season) +
             theme_bw())
    })
  })
})

#-------run shiny---------------------
shinyApp(ui, server)

precip <- dta[, .(dataset, cell_id, year, precip)]
precip[, precip := sum(precip), .(cell_id, dataset, year)]
precip <- unique(precip)
precip[, precip := scale(precip), .(dataset, cell_id)]
precip_taylor <- dcast(precip, ... ~ dataset, value.var = "precip")
taylor.diagram(precip_taylor$GHCN, precip_taylor$Pauling, sd.arcs = TRUE, ref.sd = TRUE)
taylor.diagram(precip_taylor[year < 1900, GHCN], precip_taylor[year < 1900, Pauling], add = TRUE, col = 'dark red')
taylor.diagram(precip_taylor[year > 1900, GHCN], precip_taylor[year > 1900, Pauling], add = TRUE, col = 'tomato3')
taylor.diagram(precip_taylor$GHCN, precip_taylor$OWDA, add = TRUE, col = 'blue')
taylor.diagram(precip_taylor[year < 1900, GHCN], precip_taylor[year < 1900, OWDA], add = TRUE, col = 'dark blue')
taylor.diagram(precip_taylor[year > 1900, GHCN], precip_taylor[year > 1900, OWDA], add = TRUE, col = 'steelblue')

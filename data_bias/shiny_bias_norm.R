library(leaflet)
library(shiny) 
library(ggplot2)
library(plyr)
library(data.table)
library(RDS)


#---------------load and prepare data---------------------
dta <- readRDS(file = "../../data/input/ghcn_pauling.rds")
pauling <- data.table(readRDS('../../data/input/gridded/pauling/pauling.rds')) 

dta <- readRDS(file = "../../Projects/2018XEROS/data/input/ghcn_pauling.rds") #alternative path
pauling <- data.table(readRDS('../../Projects/2018XEROS/data/input/gridded/pauling/pauling.rds')) #alternative path


dtb <- pauling[cell_id %in% unique(dta$cell_id)]
meta_print <- unique(dta[, c('cell_id', 'long', 'lat', 'period', 'season', 'n_val')] )
colnames(meta_print) <- c('id', 'Long', 'Lat', 'Period', 'Season', 'N')
dtb[, mov_var := zoo::rollapplyr(precip, 1:.N, mean), by = .(cell_id, season)]
meta_print_owda <- 

dta[, precip_scale := scale(precip), .(cell_id, season)]# scaling precip data
dtb[, precip_scale := scale(precip), .(cell_id, season)]


owda <- data.table(readRDS('../../data/input/gridded/owda/owda_1000.rds'))
owda <- data.table(readRDS('../../Projects/2018XEROS/data/input/gridded/owda/owda_1000.rds'))
setnames(owda, old = 'Time', new = 'year')
# owda grid corection
east <- owda[,3] + 0.25
orig <- owda
owda[,Lon := NULL]
owda <- cbind(east, owda)

# creating cell_id based on transfering stations id into grid id
grid_bounds <- readRDS('../../data/geodata/grid_cells.rds')
grid_bounds <- readRDS('../../Projects/2018XEROS/data/geodata/grid_cells.rds')
grid_bounds <- grid_bounds[1:5791, ]
dtc <- grid_bounds[owda, .(cell_id, year, Lat, Lon, scPDSI), 
                      on = .(lat_l <= Lat, lat_u > Lat,  
                                long_l <= Lon, long_u > Lon)]
# choosing only cells that are in dta
dtc <- dtc[cell_id %in% dta$cell_id]

dtc <- dtc[year >= 1697 & year <= 2000] # cutting owda years to be same time period as ghcn_pauling
dtc[, season := factor('su')]
dtc[, precip_scale := scale(scPDSI), cell_id] # scaling owda data separetly
# renaming columns to be same as dta table for easier manipulation in shiny app
setnames(dtc, old = c('Lon', 'Lat', 'scPDSI'), new = c('long', 'lat', 'precip')) 
dtc[year < 1900, period := factor("pre_1900")] # creating periods
dtc[year >= 1900, period := factor("aft_1900")]
dtc[, dataset := factor('OWDA')]
dtc[, n_val := .N, .(cell_id, season, period)]

# cut dta to dtc because owda have smaller area
dta <- dta[cell_id %in% dtc$cell_id]
meta_print_owda <- meta_print[id %in% dtc$cell_id]

dtc <- dtc[dta, .( cell_id, year, season, precip, long, lat, period, n_val, dataset, precip_scale), on = .(cell_id == cell_id, year == year)]
dtc <- unique(dtc)

# merge owda data with ghcn_pauling
dta <- rbind(dta, dtc)

# computing ecdf for both dta and owda
ecdf_plot <- ddply(dta, .(cell_id, season, period, dataset), summarize,
                   precip_scale = unique(precip_scale),
                   ecdf = ecdf(precip_scale)(unique(precip_scale)))


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
      addCircleMarkers(lng = meta_print_owda$Long, 
                       lat = meta_print_owda$Lat, 
                       layerId = meta_print_owda$id, 
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
    #plot whole reconstruction
    output$plot_all <- renderPlot({
      plot(ggplot(dtb[dtb$cell_id %in% store_react$clickedMarker$id, ], 
                  aes(x = year, y = precip_scale)) + 
             geom_line() +
             facet_wrap(~season) +
             theme_bw())
    }) 
  })
})

#-------run shiny---------------------
shinyApp(ui, server)

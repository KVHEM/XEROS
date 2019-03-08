library(shiny) 
library(rgdal)
library(data.table) 
library(ggplot2)
library(ggpubr)

############# GRAPHICS 

# read shapefiles to create map
map_path <- "../../data/geodata/"
wmap <- readOGR(dsn = paste0(map_path, "ne_110m_land"), layer = "ne_110m_land")
wmap_df <- fortify(wmap)
bbox <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_wgs84_bounding_box") 
countries <- readOGR(paste0(map_path,"ne_110m_admin_0_countries"), layer = "ne_110m_admin_0_countries") 
grat <- readOGR(paste0(map_path,"ne_110m_graticules_all"), layer = "ne_110m_graticules_15") 

countries_wintri <- spTransform(countries, CRS("+proj=wintri")) ##Change to wintri projection
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))

# create a blank ggplot theme for maps
map_opts <- list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_rect(fill = "white"),
                       panel.border = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(size = 22),
                       legend.key = element_rect(fill = "white")))

# colors
palettes_bright <- list(
  colset_cheer_brights = c("#C73721", "#31A9B8", "#F5BE41", "#258039"),
  colset_bold_culture = c("#4C3F54", "#D13525", "#F2C057", "#486824"))

proxy.cols <- palettes_bright$colset_cheer_brights
region_cols <- colorRampPalette(palettes_bright$colset_bold_culture)(6)[c(1, 6, 4, 5, 3, 2)]
region_cols <- c(region_cols[1:3], "tan", region_cols[4:6])

################################ SHINY

pages2k_ts <- readRDS("../../data/input/point/pages2k_eu.rds")
pages2k_meta <- readRDS("../../data/input/point/pages2k_eu_meta.rds")

ui <- fluidPage(
  sidebarLayout(
    mainPanel(
  plotOutput("plot1", click = "plot_click"), # get location by clicking
  verbatimTextOutput("info"),
  plotOutput("ts")
)))

server <- function(input, output) {
  output$plot1 <- renderPlot({  # plots map
    ggplot(bbox_wintri, aes(long, lat, group = group)) + 
      geom_polygon(fill = "grey70") +
      geom_polygon(data = countries_wintri, aes(long,lat, group = group, fill = hole)) + 
      geom_path(data = grat_wintri, aes(long, lat, group = group, fill = NULL), 
                linetype = "dashed", color = "grey50") +
      geom_point(data = pages2k_meta, 
                 aes(long_wintri, lat_wintri, group = NULL, fill = NULL, size = 2,
                     col = archive), alpha = I(7 / 10)) +
      scale_size_continuous(guide = FALSE) + 
      scale_color_manual(values = proxy.cols) +
      coord_equal(ratio = 1) + 
      map_opts +
      coord_cartesian(xlim = c(-1250000, 2200000), ylim = c(4100000, 7700000)) + 
      scale_fill_manual(values=c("black", "white"), guide = "none")
  })
  output$ts <- renderPlot({   # plots time series
    ts <- nearPoints(pages2k_meta, input$plot_click, xvar = "long_wintri", yvar = "lat_wintri",  # get the time series when you click at map
                        threshold = 10, maxpoints = 1,
                        addDist = TRUE)
    ts <- pages2k_ts[id %in% ts$id]
    ggplot(ts, aes(x = time, y = temp)) +
      geom_line() +
      geom_point() +
      theme_bw()
  })  
  output$info <- renderPrint({
    ts <- nearPoints(pages2k_meta, input$plot_click, xvar = "long_wintri", yvar = "lat_wintri", 
                        threshold = 10, maxpoints = 1,
                        addDist = TRUE)
    print(data.frame(t(ts[, 1:8])))
    })
} 

shinyApp(ui, server)

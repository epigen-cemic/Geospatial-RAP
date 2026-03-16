source("global.R")

ui <- fluidPage(
  geospatialUI("main_geospatial")
)

server <- function(input, output, session) {
  geospatialServer("main_geospatial")
}

shinyApp(ui, server)

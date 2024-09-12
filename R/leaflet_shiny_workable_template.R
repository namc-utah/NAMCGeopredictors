library(shiny)
library(leaflet)
library(sf)
library(dplyr)



coords_data <- data.frame(id = sf_data$siteId, lng = st_coordinates(sf_data)[,1], lat = st_coordinates(sf_data)[,2])
lines_sf<-isolated_segments
ui <- fluidPage(
  leafletOutput('map'),
  tableOutput('coordsTable')
)

server <- function(input, output, session) {

  coords_rv <- reactiveValues(data = coords_data)

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = coords_data,
                 lng = ~lng, lat = ~lat,
                 layerId = ~id,
                 options = markerOptions(draggable = TRUE)) %>%
      addPolylines(data = lines_sf, color = "blue")  # Add polylines to the map
  })

  observeEvent(input$map_marker_dragend, {

    markerId <- input$map_marker_dragend$id
    lat <- input$map_marker_dragend$lat
    lng <- input$map_marker_dragend$lng

    coords_rv$data[coords_rv$data$id == markerId, c("lat", "lng")] <- c(lat, lng)

    leafletProxy('map') %>%
      clearMarkers() %>%
      addMarkers(data = coords_rv$data,
                 lng = ~lng, lat = ~lat,
                 layerId = ~id,
                 options = markerOptions(draggable = TRUE)) %>%
      addPolylines(data = lines_sf, color = "blue")  # Re-add polylines to maintain display
  })

  output$coordsTable <- renderTable({
    coords_rv$data
    updated_coords <<- coords_rv$data
  })

  session$onSessionEnded(function() {
      # Use <<- to assign to the global variable
    cat("Updated coordinates are now in 'updated_coords'.\nConvert this to an sf object and use in StreamStats delineation")
  })
}

shinyApp(ui, server)

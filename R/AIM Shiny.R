#R Shiny / Leaflet interactive Map
#for AIM WestWide O/E scores
#written by Andrew Caudillo, Lab Manager at NAMC.

#import libraries
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
#read in data straight from database
#adjust model ID if you want to see specific model results
#just change title of map accordingly.
query<-paste0("select m.model_result,
si.longitude as Lon,
si.latitude as Lat,
s.sample_id as sampleId,
strftime('%Y',s.sample_date) as Year,
s.customer_site_code as siteName
from model_results m
join samples s on s.sample_id = m.sample_id
join sites si on si.site_id = s.site_id
where s.box_id in (select box_id from boxes
where customer_id = 4396)
and m.model_result is not null
and m.model_id = 25;")
mydb=DBI::dbConnect(RSQLite::SQLite(),'C://NAMC_S3//LegacyDatabases//instar.sqlite')
data=DBI::dbGetQuery(mydb,query)
#read.csv("C://Users//andrew.caudillo.BUGLAB-I9//Box//NAMC//Database//OE_results_shiny_data.csv")
#ensure the data types will play nicely
data <- data %>%
  mutate(
    Year = as.numeric(Year),
    Lon = as.numeric(Lon),
    Lat = as.numeric(Lat),
    model_result = as.numeric(model_result)
  )

# Define Color Palette
#This is a yellow > green > blue palette. Could play around with other options
color_pal <- colorNumeric(palette = "YlGnBu", domain = data$model_result)

# Define UI
#side bar with years, etc.
ui <- fluidPage(
  titlePanel("AIM Westwide O/E time series"),

  sidebarLayout(
    sidebarPanel(
      #adding sep='' will ensure that year is treated as a year and not a number
      #with commas
      sliderInput("year", "Select Year:",
                  min = min(data$Year), max = max(data$Year),
                  value = min(data$Year), step = 1, animate = TRUE,
                  sep=''),
      h4("Time-Series Data:"),
      plotlyOutput("time_series")  # Time-series plot
    ),
#affects the size of the map. 600px is good.
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive Data: Filter dataset based on selected year
  filtered_data <- reactive({
    #creating a subset of the filtered data
    df <- data %>% filter(Year == input$year)
    #just a check that will not show up on the map
    print(paste("Data points for Year", input$year, ":", nrow(df)))
    return(df)
  })

  # Render Initial Leaflet Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% #could force certain tiles like ESRI imagery etc.
      setView(lng = -100, lat = 40, zoom = 4)  # Adjust center as needed
  })

  # Update Map When Slider Changes
  observe({
    df <- filtered_data()
    leafletProxy("map") %>%
      clearMarkers() %>% #remove old markers when slider is activated
      clearControls() %>% #remove previous legend when slider is activated
      #define the markers
      addCircleMarkers(
        data = df,
        ~Lon, ~Lat, radius = 6,
        color = ~color_pal(model_result), stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste("Site:", siteName, "<br>Year:", Year, "<br>Value:", round(model_result, 2)),
        layerId = ~siteName
      ) %>%
      #legend for interpretation of colors
      addLegend(
        "bottomright",
        pal = color_pal,
        values = data$model_result,  # Full data range
        title = "Westwide O/E score",
        opacity = 1
      )
  })

  # Reactive Value for Storing Clicked Site
  selected_site <- reactiveVal(NULL)

  # Observe Click Events on Map
  observeEvent(input$map_marker_click, {
    selected_site(input$map_marker_click$id)
  })

  # Generate Time-Series Plot for Selected Site
  output$time_series <- renderPlotly({
    req(selected_site())  # Only show plot if a site is selected

    # Filter Data for Selected Site
    site_data <- data %>% filter(siteName == selected_site())

    # Create Plot
    p <- ggplot(site_data, aes(x = Year, y = model_result)) +
      geom_line(color = "blue") +
      geom_point(color = "red", size = 3) +
      labs(title = paste("Time-Series for", selected_site()),
           x = "Year", y = "Model Result") +
      #ggplot will force the year to be a number with a decimal point
      #this forces it to be a whole number
      scale_x_continuous(breaks =~axisTicks(.,log=F))+
      theme_minimal()

    ggplotly(p)  # Convert ggplot to interactive plotly
  })
}

# Run the Shiny App
shinyApp(ui, server)

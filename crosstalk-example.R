# https://stackoverflow.com/questions/54344375/how-do-i-use-crosstalk-in-shiny-to-filter-a-reactive-data-table
library(shiny)
library(crosstalk)
library(leaflet)
library(DT)

# Wrap data frame in SharedData
# Use SharedData like a dataframe with Crosstalk-enabled widgets

sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])

ui <- fluidPage(
  # Create a filter input
  filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250),
  bscols(leafletOutput("map"), DTOutput("table"))
)


server <- function(input,output) {
  
  output$map <- renderLeaflet({ leaflet(sd) %>% addTiles() %>% addMarkers()})
  
  output$table <- renderDT({
    datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
              options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
  }, server = FALSE)
}

shinyApp(ui = ui, server = server)
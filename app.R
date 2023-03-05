library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(thematic)
library(forcats)
library(zoo)

# I am only using Canadian data
data <- read.csv("data/data.csv")

# Reload when saving the app
options(shiny.autoreload = TRUE)

ui <- navbarPage('FeederWatch App',
                 theme = bs_theme(bootswatch = 'lux'),
                 tabPanel('Data Exploration',
                          fluidRow(selectInput(inputId = 'species',
                                               label = 'select the specie:',
                                               choices = unique(data$species_code),
                                               selected = 'norcar'),
                                   dateRangeInput(inputId = 'daterange',
                                                  label = 'Select a range of dates',
                                                  start  = min(data$date),
                                                  end = max(data$date),
                                                  format = "mm/dd/yyyy")),
                          fluidRow(column(6,
                                      leaflet::leafletOutput(outputId = 'map')),
                                   column(6,
                                      DT::DTOutput(outputId = 'table'))
                                   )
                 ),
                 tabPanel('by Province',
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'species2',
                                          label = 'select the specie:',
                                          choices = unique(data$species_code),
                                          selected = 'sonspa'),
                             checkboxGroupInput(inputId = 'provinces',
                                          label = 'select the province:',
                                          choices = unique(data$subnational1_code),
                                          selected = 'CA-BC')
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel('lineplot',
                                         plotlyOutput(outputId = 'lineplot')
                                        ),
                                tabPanel('more...',
                                         'tbd')
                              )
                            )
                          ))
  
)

server <- function(input, output, session) {
  
  data_acc_sps <-  reactive({
    data |> 
      dplyr::filter(species_code == input$species2) })
  
  observeEvent(data_acc_sps(), {
    choices <- unique(data_acc_sps()$subnational1_code)
    updateCheckboxGroupInput(inputId = "provinces",
                      choices = choices)
  })
  
    

  
  ## LINEPLOT
  output$lineplot <- plotly::renderPlotly({ 
    
    req(input$provinces) # trigger the execution with the user selection
    
    thematic::thematic_shiny()
    
    data_acc_sps() |> 
    dplyr::filter(subnational1_code %in% input$provinces) |> 
      dplyr::mutate(yearmon = as.yearmon(as.Date(date))) |> 
      dplyr::group_by(yearmon, subnational1_code) |> 
      dplyr::summarize(n = dplyr::n()) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(subnational1_code) |> 
      dplyr::mutate(cumsum = cumsum(n)) |> 
      ggplot2::ggplot(aes(x = as.Date(yearmon),
                          y = cumsum,
                          color = subnational1_code)) +
      ggplot2::geom_line(size = 0.5,
                         alpha = 0.5) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_x_date(date_breaks = "1 month", 
                            date_labels =  "%b %Y") + 
      #   ggplot2::scale_color_brewer(palette = "Set2") +
      ggplot2::labs(title = paste('Period', min(data$date),
                                  "to",
                                  max(data$date)),
                    x = 'date',
                    y = 'observations') 
  })
  
  filtered_data <-reactive({ 
    
    data |> 
    dplyr::filter(species_code == input$species) |> 
    dplyr::filter(input$daterange[2] > date) |> 
    dplyr::filter (date > input$daterange[1]) |> 
    dplyr::count(loc_id, latitude, longitude)
    
  }) 
  
  ## DT TABLE
  
  output$table <-  renderDT({
    
    # read the documentation for the arguments  
    datatable(filtered_data(),
              caption = 'Table: Observations by location.',
              extensions = 'Scroller',
              options=list(deferRender = TRUE,
                           scrollY = 200,
                           scroller = TRUE))
    
  })
  
  
  ## LEAFLET MAP
  
  output$map <- leaflet::renderLeaflet({
    
    # print(input$daterange)
    # str(input$daterange)
    # print(input$table_rows_selected)
    
    ## color palette
    pal <- leaflet::colorNumeric('viridis', 
                                 domain = filtered_data()$n)
    
    filtered_data() |> 
      leaflet::leaflet() |> 
      leaflet::addProviderTiles(providers$CartoDB.Positron) |> 
      leaflet::addCircleMarkers(
                               lat = ~latitude,
                               lng = ~longitude,
                               radius = ~n,
                               popup = paste(filtered_data()$n,
                                              "bird/s in",
                                              filtered_data()$loc_id),
                                color = ~pal(n),
                                options = popupOptions(closeButton = FALSE))
  })
}

shinyApp(ui, server)
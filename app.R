library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(thematic)
library(zoo)
library(sf)
library(rnaturalearth)

# I am only using Canadian data
data <- read.csv("data/data.csv")

# reading geografic data
poly_canada <- rnaturalearth::ne_states(country = 'canada',
                                        returnclass = c( "sf")) 

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
                                      leaflet::leafletOutput(outputId = 'map'),
                                      verbatimTextOutput('coord')),
                                   column(6,
                                      DT::DTOutput(outputId = 'table'))
                                   )
                 ),
                 tabPanel('by Province',
                          fluidRow(column(6, 
                                          plotlyOutput('plotly_map'),
                                          offset = 0)),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'species2',
                                          label = 'select the specie:',
                                          choices = unique(data$species_code),
                                          selected = 'sonspa'),
                             checkboxGroupInput(inputId = 'provinces',
                                          label = 'select the province:',
                                          choices = unique(data$subnational1_code),
                                          selected = 'CA-BC'),
                             downloadButton('download1')
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
  
  ## DYNAMIC UI - FILTERING SECOND TAB 'by provinces"
  data_acc_sps <-  reactive({
    data |> 
      dplyr::filter(species_code == input$species2) })
  
  observeEvent(data_acc_sps(), {
    choices <- unique(data_acc_sps()$subnational1_code)
    updateCheckboxGroupInput(inputId = "provinces",
                      choices = choices)
  })
  
  
  data_acc_sps_prov <- reactive({ 
    
    data_acc_sps() |> 
    dplyr::filter(subnational1_code %in% input$provinces) |> 
    dplyr::mutate(yearmon = as.yearmon(as.Date(date))) |> 
    dplyr::group_by(yearmon, subnational1_code) |> 
    dplyr::summarize(n = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::group_by(subnational1_code) |> 
    dplyr::mutate(cumsum = cumsum(n)) 
    
    })
    
  ## LINEPLOT - SECOND TAB
  output$lineplot <- plotly::renderPlotly({ 
    
    req(input$provinces) # trigger the execution with the user selection
    
    thematic::thematic_shiny()
    
    data_acc_sps_prov() |> 
      ggplot2::ggplot(aes(x = as.Date(yearmon),
                          y = cumsum,
                          color = subnational1_code)) +
      ggplot2::geom_line(linewidth = 0.5,
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
  

  ## DOWNLOAD BUTTON
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(input$species2, ".csv")
    },
    content = function(file) {
      write.csv(data_acc_sps_prov(), file)
    }
  )
  
  ## FILTERING - FIRST TAB "data exploration"
  filtered_data <-reactive({ 
    

      data |> 
        dplyr::filter(species_code == input$species) |> 
        dplyr::filter(input$daterange[2] > date) |> 
        dplyr::filter (date > input$daterange[1]) |> 
        dplyr::count(loc_id, latitude, longitude)
    
  }) 
  

  
  ## PLOTLY MAP
  

  poly_can_data <- reactive({ 
    
    diversity <- data |> 
      dplyr::group_by(subnational1_code) |> 
      dplyr::summarize(nr_sps = dplyr::n_distinct(species_code),
                sum_effort_hrs_atleast = sum(round(effort_hrs_atleast, 
                                                   digits = 0),
                                             na.rm=TRUE)) 
    
      poly_canada |>
          dplyr::left_join(diversity, 
                by = c('iso_3166_2' = 'subnational1_code'))
    
  })
  
  output$plotly_map <-  renderPlotly({

  plot_ly(poly_can_data(),
          split= ~woe_name,
          color = ~nr_sps,
          text = ~paste(nr_sps,
                        "sps. were recognized in at least",
                        sum_effort_hrs_atleast, 
                        "hours of effort"),
          hoveron = "fills",
          hoverinfo = "text",
          showlegend = FALSE
  )  |>  
    colorbar(title = 'Different species') |> 
    layout(title = "Diversity of birds observed by province")
  })
  
  ## DT TABLE
  output$table <-  renderDT({
    
     datatable(filtered_data(),
              caption = 'Table: Observations by location.',
              extensions = 'Scroller',
              options=list(deferRender = TRUE,
                           scrollY = 200,
                           scroller = TRUE))
     
  }, server = FALSE)
  
  ## LAT LONG
  
  output$coord <- renderPrint({
    
    print(paste("Lat: ", input$map_marker_click$lat,
                "Long: ", input$map_marker_click$lng))
  })

  ## LEAFLET MAP
   output$map <- leaflet::renderLeaflet({
    
    # print(filtered_data())
     print(input$map_marker_click)
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
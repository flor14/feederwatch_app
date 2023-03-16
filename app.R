library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(thematic)
library(zoo)
library(sf)
library(rmarkdown)
library(tidyr)
library(shinycssloaders)
library(here)


# Reading .shp file
poly_canada <- sf::read_sf(here("data", "poly_canada.shp"))

# I am only using Canadian data
all_data <- read.csv(here("data","data.csv"))

species_code <- as.list(unique(all_data$species_code))
american_names <- unique(all_data$american_english_name) |> 
  replace_na("No name")
names(species_code) <- american_names

# subnational1_code <- as.list(unique(all_data$subnational1_code))
# american_names <- unique(all_data$) |> 
#   replace_na("No name")
# names(species_code) <- american_names

rare_sps <- all_data |> 
  dplyr::group_by(species_code) |> 
  dplyr::summarize(n = dplyr::n()) |> 
  dplyr::arrange(n) |> 
  dplyr::filter(n < 3)

freq_sps <- all_data |> 
  dplyr::group_by(species_code) |> 
  dplyr::summarize(n = dplyr::n()) |> 
  dplyr::arrange(n) |> 
  dplyr::filter(n > 3)

data <- dplyr::filter(all_data, species_code %in% freq_sps$species_code)
rare_data <- dplyr::filter(all_data, species_code %in% freq_sps$species_code)

# Reload when saving the app
options(shiny.autoreload = TRUE)

ui <- navbarPage(title = 'FeederWatch App',
                 id = 'navbar',
                 theme = bs_theme(bootswatch = 'lux'),
                 tabPanel(title = 'Data Exploration',
                          fluidRow(selectInput(inputId = 'species',
                                               label = 'select the specie:',
                                               choices = species_code,
                                               selected = 'norcar'),
                                   dateRangeInput(inputId = 'daterange',
                                                  label = 'Select a range of dates',
                                                  start  = min(data$date),
                                                  end = max(data$date),
                                                  format = "mm/dd/yyyy")),
                          fluidRow(column(6,
                                          shinycssloaders::withSpinner( 
                                            leaflet::leafletOutput(outputId = 'map'),
                                            type = 2,
                                            color = 'lightgrey',
                                            color.background = 'white'),
                                          verbatimTextOutput('coord')),
                                   column(6,
                                          shinycssloaders::withSpinner( 
                                            DT::DTOutput(outputId = 'table'),
                                            type = 2,
                                            color = 'lightgrey',
                                            color.background = 'white')))),
                 
                 tabPanel(title = 'Temporal Distribution',
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'species2',
                                          label = 'Select the species:',
                                          choices = species_code,
                                          selected = 'norcar'),
                              selectInput(inputId = 'provinces',
                                          label = 'Select the province or territory:',
                                          choices = unique(data$subnational1_code),
                                          selected = 'CA-BC',
                                          multiple = TRUE),
                              downloadButton('download1',
                                             'Download .CSV'),
                              downloadButton("report",
                                             "Generate report")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel('Annual trend',
                                         shinycssloaders::withSpinner( 
                                           plotlyOutput(outputId = 'lineplot'),
                                           type = 2,
                                           color = 'lightgrey',
                                           color.background = 'white')
                                ),
                                tabPanel('Montly data',
                                         shinycssloaders::withSpinner( 
                                           plotlyOutput(outputId = 'boxplot'),
                                           type = 2,
                                           color = 'lightgrey',
                                           color.background = 'white')
                                )
                              )
                            ))),
                 tabPanel(title = 'Diversity', 
                          sidebarLayout(
                            sidebarPanel(radioButtons('radio',
                                                      'Select:',
                                                      choices = c('region' = 'region_sub',
                                                                  'province' = 'name_en'),
                                                      selected = c('province' = 'name_en')),
                                         width = 2),
                            mainPanel(
                              shinycssloaders::withSpinner( 
                                plotlyOutput('map_plotly',
                                             width = "600px", 
                                             height = "600px"),
                                type = 2,
                                color = 'lightgrey',
                                color.background = 'white')
                            ))),
                 tabPanel(title = 'Rare species', 
                          "..."),
                 tabPanel(title = 'About',
                          "..")
                 
)

server <- function(input, output, session) {
  
  
  # Tab Data Exploration ----------------------------------------------------
  
  filtered_data <-reactive({ 
    
    first_filter <-  all_data |> 
      dplyr::filter(species_code == input$species) |> 
      dplyr::filter(input$daterange[2] > date) |> 
      dplyr::filter (date > input$daterange[1]) 
    
    # Show text to the user if the filtering returns a 0 row dataset   
    validate(
      missing_values(first_filter)
    )
    
    first_filter
    
  })   
  
  missing_values <- function(input_data) {
    if ( nrow(input_data) == 0 ) {
      "There are not values in the dataset for these dates. Please, select a new period"
    } else {
      NULL
    }
  }
  
  ## DT Table
  output$table <-  renderDT({
    
    table_data <-  filtered_data() |>
      select(-latitude, -longitude, -subnational1_code,
             -X, -entry_technique, -sub_id, -PROJ_PERIOD_ID,
             -species_code) 
    
    datatable(table_data,
              caption = 'Table: Observations by location.',
              options=list(deferRender = TRUE,
                           autoWidth = TRUE,
                           paging = TRUE,
                           pageLength = 7),
              fillContainer = TRUE) 
    
  }, server = FALSE)
  
  
  # Latitude and longitude 
  
  output$coord <- renderPrint({
    
    print(paste("Lat: ", input$map_marker_click$lat,
                "Long: ", input$map_marker_click$lng))
  })
  
  output$coord <- renderPrint({
    
    req(input$map_marker_click)
    print(paste("Lat:", 
                input$map_marker_click$lat,
                'Long:',
                input$map_marker_click$lng))
  })
  
  # Leaflet map
  output$map <- leaflet::renderLeaflet({
    
    ## color palette
    pal <- leaflet::colorNumeric('viridis', 
                                 domain = filtered_data()$n)
    
    birds <- filtered_data() |> 
      dplyr::count(loc_id, latitude, longitude) 
    
    birds |> 
      leaflet::leaflet() |> 
      leaflet::addProviderTiles(providers$CartoDB.Positron) |> 
      leaflet::addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = ~n,
        popup = paste(birds$n,
                      "bird/s in",
                      birds$loc_id),
        color = ~pal(n),
        options = popupOptions(closeButton = FALSE))
    
    
  })
  
  # Tab Temporal distribution ------------------------------------------------------------
  
  ## Dynamic UI
  data_acc_sps <-  reactive({
    
    data |> 
      dplyr::filter(species_code == input$species2) })
  
  # Update the province available by species selection 
  observeEvent(data_acc_sps(), {
    choices <- unique(data_acc_sps()$subnational1_code)
    updateSelectInput(inputId = "provinces",
                      choices = choices,
                      selected = unique(data_acc_sps()$subnational1_code))
  })
  
  
  data_acc_sps_prov <- reactive({ 
    
    data_acc_sps() |> 
      dplyr::filter(subnational1_code %in% input$provinces) |> 
      dplyr::mutate(yearmon = as.yearmon(as.Date(date))) 
    
  })
  
  
  # Boxplot - Number of birds by observation for each month and province
  output$boxplot <- renderPlotly({
    
    
    data_acc_sps_prov() |> 
      mutate(month = factor(format(yearmon, "%b"),
                            levels = c("Jan", "Feb", "Mar",
                                       "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", 
                                       "Oct", "Nov", "Dec"))) |> 
      ggplot(aes(month, how_many)) +
      geom_jitter(aes(color = subnational1_code),
                  alpha = 0.3,
                  width = 0.25) +
      geom_violin(alpha = 0.7,
                  draw_quantiles = c(0.25, 
                                     0.5,
                                     0.75))
  })
  
  ## Lineplot - accumulated number of birds by year
  output$lineplot <- plotly::renderPlotly({ 
    
    req(input$provinces) # trigger the execution with the user selection
    
    thematic::thematic_shiny()
    
    data_acc_sps_prov() |> 
      dplyr::group_by(yearmon, subnational1_code) |> 
      dplyr::summarize(n = dplyr::n()) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(subnational1_code) |> 
      dplyr::mutate(cumsum = cumsum(n)) |> 
      ggplot2::ggplot(aes(x = as.Date(yearmon),
                          y = cumsum,
                          color = subnational1_code)) +
      ggplot2::geom_line(linewidth = 0.5,
                         alpha = 0.5) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_x_date(date_breaks = "1 month", 
                            date_labels =  "%b %Y") + 
      ggplot2::labs(title = paste('Period', min(data$date),
                                  "to",
                                  max(data$date)),
                    x = 'date',
                    y = 'observations') 
  })
  
  
  # Download .csv button
  output$download1 <- downloadHandler(
    filename = function() {
      paste0(input$species2, ".csv")
    },
    content = function(file) {
      
      id <- showNotification(
        "Downloading data...", 
        duration = 10, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      write.csv(data_acc_sps_prov(), file)
    }
  )
  
  
  # Report ------------------------------------------------------------------
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_feederwatch-app.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it
      tempReport <- file.path(tempdir(), 
                              'report.Rmd')
      file.copy(from = 'docs/report.Rmd',
                to = tempReport, 
                overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(species_code = input$species2,
                     provinces = input$provinces)
      
      # Notification
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id))
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  # Tab Diversity: Plotly map Canada ----------------------------------------
  
  # Data processing 
  poly_can_data <- reactive({
    diversity <- all_data |> 
      dplyr::group_by(subnational1_code) |> 
      dplyr::summarize(nr_sps = dplyr::n_distinct(species_code),
                       sum_effort_hrs_atleast = sum(round(effort_hrs_atleast, 
                                                          digits = 0),
                                                    na.rm=TRUE)) |>  
      arrange(nr_sps) 
    
    poly_canada |>
      dplyr::left_join(diversity, 
                       by = c('iso_3166_2' = 'subnational1_code'))
    
  })
  
  # Plot region or Province
  decide <- reactive({ 
    
    if(input$radio == 'name_en'){
      poly_can_data()
    }else{ 
      poly_can_data()  |> 
        group_by(region_sub) |> 
        dplyr::summarize(nr_sps = sum(nr_sps,
                                      na.rm = TRUE),
                         sum_effort_hrs_atleast = sum(round(sum_effort_hrs_atleast, 
                                                            digits = 0),
                                                      na.rm=TRUE)) }                                                                 
  }) 
  
  
  
  output$map_plotly <- renderPlotly({
    
    plot_ly(decide(),
            split = ~get(input$radio),
            color = ~nr_sps,
            showlegend = FALSE,
            text = ~ifelse(is.na(nr_sps), 
                           "No data available",
                           paste(nr_sps, 
                                 "sps. observed in at least",
                                 sum_effort_hrs_atleast,
                                 "hs. of effort")
            ),
            hoverinfo = 'text',
            hoveron = 'fills')  |>  
      colorbar(title = 'Nr. of birds species') |> 
      layout(title = paste("Diversity of species in relation to hs. of sampling effort"),
             legend = list(font = list(size = 5)))
    
  })
  
  
  
}

shinyApp(ui, server)
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
library(shinytest2)

# I am only using Canadian data
all_data <- read.csv("data/data.csv")

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

# reading geografic data
poly_canada <- rnaturalearth::ne_states(country = 'canada',
                                        returnclass = c( "sf")) 

# Reload when saving the app
options(shiny.autoreload = TRUE)

ui <- navbarPage(title = 'FeederWatch App',
                 id = 'navbar',
                 theme = bs_theme(bootswatch = 'lux'),
                 tabPanel(title = 'Data Exploration',
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
                 tabPanel(title = 'by Province',
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = 'species2',
                                          label = 'select the specie:',
                                          choices = unique(data$species_code),
                                          selected = 'norcar'),
                             selectInput(inputId = 'provinces',
                                          label = 'select the province:',
                                          choices = unique(data$subnational1_code),
                                          selected = 'CA-BC',
                                          multiple = TRUE),
                             downloadButton('download1')
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel('Annual data',
                                         plotlyOutput(outputId = 'lineplot')
                                        ),
                                tabPanel('Montly data',
                                         plotlyOutput(outputId = 'boxplot')
                                         )
                              )
                            )
                          )),
                 tabPanel(title = 'Diversity', 
                          sidebarLayout(
                            sidebarPanel(radioButtons('radio',
                                               'Select:',
                                               choices = c('region' = 'region_sub',
                                                           'province' = 'name_en'),
                                               selected = c('province' = 'name_en')),
                                         width = 2),
                          mainPanel(plotlyOutput('map_plotly'),
                                    width = 10))),
                 tabPanel(title = 'Rare species', 
                          ),
                 tabPanel(title = 'About',
                          "..")
  
)

server <- function(input, output, session) {
  
  ## DYNAMIC UI - FILTERING SECOND TAB 'by provinces"
  data_acc_sps <-  reactive({
    data |> 
      dplyr::filter(species_code == input$species2) })
  
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
  
  output$boxplot <- renderPlotly({
    
    
    data_acc_sps_prov() |> 
      mutate(month = factor(format(yearmon, "%b"),
                            levels = c("Jan", "Feb", "Mar",
                                       "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep", 
                                       "Oct", "Nov", "Dec"))) |> 
      ggplot(aes(month, how_many)) +
      geom_jitter(aes(color = subnational1_code),
                  alpha = 0.3) +
      geom_boxplot(alpha = 0.7)

    
  })
    
  ## LINEPLOT - SECOND TAB
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
    

    all_data |> 
    dplyr::filter(species_code == input$species) |> 
    dplyr::filter(input$daterange[2] > date) |> 
    dplyr::filter (date > input$daterange[1]) 

    
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

  output$coord <- renderPrint({
    
   req(input$map_marker_click)
   print(paste("Lat:", 
               input$map_marker_click$lat,
               'Long:',
               input$map_marker_click$lng))
  })
  
  ## LEAFLET MAP
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
   
   
   ## PLOTLY MAP
   
   poly_can_data <- reactive({
     
     poly_canada <- rnaturalearth::ne_states(country = 'canada',
                              returnclass = c( "sf")) 
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
           text = ~paste(nr_sps, 
                         "sps. observed in at least",
                         sum_effort_hrs_atleast,
                         "hs. of effort"
           ),
           hoverinfo = 'text',
           hoveron = 'fills')  |>  
     colorbar(title = 'Nr. of birds species') |> 
     layout(title = paste("Diversity of species in relation to hs. of sampling effort"))
   
   })
   
   
}

shinyApp(ui, server)
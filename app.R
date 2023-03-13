library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(thematic)
library(forcats)
library(zoo)
library(rnaturalearth)

# I am only using Canadian data
data <- read.csv("data/data.csv")

# Reload when saving the app
options(shiny.autoreload = TRUE)

ui <- navbarPage('FeederWatch App',
                 id = 'navbar',
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
                                id = 'tabs',
                                tabPanel('lineplot',
                                         plotlyOutput(outputId = 'lineplot')
                                        ),
                                tabPanel('more...',
                                         )
                              )
                            )
                          )),
                 tabPanel('Diversity', 
                          fluidRow(radioButtons('radio',
                                               'Select:',
                                               choices = c('region' = 'region_sub',
                                                           'province' = 'name_en'),
                                               selected = c('province' = 'name_en'),
                                               inline = TRUE)),
                          plotlyOutput('map_plotly')),
                 tabPanel('About',
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
   
   
   ## PLOTLY MAP
   
   poly_can_data <- reactive({
     
     poly_canada <- rnaturalearth::ne_states(country = 'canada',
                              returnclass = c( "sf")) 
     diversity <- data |> 
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
     print(poly_can_data())
     
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
   
 print(decide())
   
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
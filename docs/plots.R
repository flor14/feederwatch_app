library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

data <- read.csv("data/data.csv")

filtered_data <-  data |> 
  dplyr::filter(species_code == 'norcar') |> # user input
  dplyr::filter(max(date) > date) |> 
  dplyr::filter(date > min(date)) |> 
  dplyr::count(loc_id, latitude, longitude) |> # 1 row = 1 observation
  arrange(desc(n))

# read the documentation for the arguments  
datatable(filtered_data,
          caption = 'Table: Observations by location.',
          extensions = 'Scroller',
          options=list(deferRender = TRUE,
                       scrollY = 200,
                       scroller = TRUE))


## LEAFLET MAP

pal <- leaflet::colorNumeric('viridis', 
                             domain = range(filtered_data$n))

filtered_data |> 
leaflet::leaflet() |> 
  leaflet::addProviderTiles(providers$CartoDB.Positron) |> 
  leaflet::addCircleMarkers(radius = ~n,
                            lat = ~latitude,
                            lng = ~longitude,
                            popup = paste0(filtered_data$n, "birds"),
                            color = ~pal(n),
                            options = popupOptions(closeButton = FALSE))


## DATA WRANGLING LINEPLOT AND BOXPLOT
library(zoo)

data_acc_sps_prov <- data |> 
  dplyr::filter(species_code == 'norcar',
                subnational1_code %in% c('CA-QC',
                                         'CA-ON')) |> # User input
  dplyr::mutate(yearmon = as.yearmon(as.Date(date))) |> 
  dplyr::group_by(yearmon, subnational1_code) |> 
  dplyr::summarize(n = dplyr::n()) |> 
  ungroup() |> 
  group_by(subnational1_code) |> 
  dplyr::mutate(cumsum = cumsum(n))

## BOXPLOT
library(zoo)

test <- data |> 
  dplyr::filter(species_code == 'norcar',
                subnational1_code %in% c('CA-QC',
                                         'CA-ON')) |> # User input
  dplyr::mutate(yearmon = as.yearmon(as.Date(date))) |> 
  mutate(month = factor(format(yearmon, "%b"),
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) |> 
  pivot_longer(starts_with("fed_in"),
               values_to = 'fed_in_val' ,
               names_to= 'fed_in_names' ) |> 
  ggplot(aes(month, how_many)) +
  geom_boxplot()+
  geom_jitter(aes(color = subnational1_code))


## LINEPLOT
    plotly::ggplotly(
      data_acc_sps_prov  |> 
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
  )



## BARPLOT (not included in the app)

hab_data <- data |> 
  tidyr::pivot_longer(cols = starts_with("hab_"), 
                      values_to = 'hab_value',
                      names_to = 'hab_name' ) |> 
  dplyr::group_by(hab_name, subnational1_code) |> 
  dplyr::summarize(nro = n(), 
                   .groups = 'keep') |> 
  dplyr::filter(hab_name == 'hab_agricultural') # User input

ggplotly( 
 hab_data |> 
  ggplot(aes(forcats::fct_reorder(subnational1_code, nro), 
             nro)) +
  geom_col() + # it is not a good idea to use aes(fill = species_code)
  coord_flip() +
  labs(title = 'FeederWatch database 2021',
       x = 'provinces',
       y = 'observations'),
tooltip = "nro"
)



## PLOTLY MAP

library(ggplot2)
library(sf)
library(plotly)
library(rnaturalearth)
library(dplyr)


poly_canada <- rnaturalearth::ne_states(country = 'canada',
                                        returnclass = c( "sf")) 
class(poly_canada)
# plot(poly_canada)


diversity <- data |> 
  dplyr::group_by(subnational1_code) |> 
  dplyr::summarize(nr_sps = dplyr::n_distinct(species_code),
                   sum_effort_hrs_atleast = sum(round(effort_hrs_atleast, 
                                                      digits = 0),
                                                na.rm=TRUE)) |>  
  arrange(nr_sps)  

poly_canada_div <- poly_canada |>
  dplyr::left_join(diversity, 
                   by = c('iso_3166_2' = 'subnational1_code'))



if(input$radio == 'name_en'){
  poly_canada_div
}else{ 
  poly_canada_div  |> 
    group_by(region_sub) |> 
    dplyr::summarize(nr_sps = sum(nr_sps,
                                  na.rm = TRUE),
                     sum_effort_hrs_atleast = sum(round(sum_effort_hrs_atleast, 
                                                        digits = 0),
                                                  na.rm=TRUE)) }        

## option 1
ggplotly(
  ggplot(poly_can_div) +
    geom_sf(aes(fill = nr_sps))
)

## option 2
plot_ly(poly_canada_div,
        split= ~woe_name,
        color = ~nr_sps.y,
        text = ~paste(nr_sps, "sps. were recognized in at least", 
                      sum_effort_hrs_atleast.x, "hours of effort"),
        hoveron = "fills",
        hoverinfo = "text",
        # hovertemplate = see docs
        showlegend = FALSE
) |>  
  colorbar(title = 'Different species') |> 
  #layout(title = "Diversity of birds observed by province") |> 
  layout(title = paste("Diversity of species in relation to hs. of sampling effort"),
         legend = list(font = list(size = 5)))
















## RARE

# I am only using Canadian data
all_data <- read.csv("data/data.csv")

rare_sps <- all_data |> 
  group_by(species_code) |> 
  summarize(n = n()) |> 
  arrange(n) |> 
  filter(n < 3)

freq_sps <- all_data |> 
  group_by(species_code) |> 
  summarize(n = n()) |> 
  arrange(n) |> 
  filter(n > 3)

data <- filter(all_data, species_code %in% freq_sps$species_code)
rare_data <- filter(all_data, species_code %in% freq_sps$species_code)



diversity <- data |> 
  group_by(subnational1_code) |> 
  summarize(nr_sps = n_distinct(species_code),
            sum_effort_hrs_atleast = sum(round(effort_hrs_atleast, 
                                               digits = 0),
                                         na.rm=TRUE)) |>  
  arrange(nr_sps) 

poly_can_div <-  poly_canada |>
                     left_join(diversity, 
                               by = c('iso_3166_2' = 'subnational1_code'))




## option 1
ggplotly(
 ggplot(poly_can_div) +
   geom_sf(aes(fill = nr_sps))
)

## option 2
plot_ly(poly_can_div,
        split= ~woe_name,
        color = ~nr_sps,
         text = ~paste(nr_sps, "sps. were recognized in at least", 
                       sum_effort_hrs_atleast, "hours of effort"),
         hoveron = "fills",
         hoverinfo = "text",
        # hovertemplate = see docs
         showlegend = FALSE
) |>  
  colorbar(title = 'Different species') |> 
  layout(title = "Diversity of birds observed by province")




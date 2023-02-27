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


## LINEPLOT

plotly::ggplotly(
  data |> 
    dplyr::filter(species_code == 'norcar') |> # User input
    dplyr::group_by(subnational1_code, date) |> 
    dplyr::summarize(n = n()) |> 
    dplyr::mutate(cumsum = cumsum(n)) |> 
    ggplot2::ggplot(aes(x = as.Date(date),
                        y = cumsum,
                        color = subnational1_code)) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::scale_x_date(date_breaks = "1 month", 
                          date_labels =  "%b %Y") + 
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::labs(title = paste('Period', min(data$date),
                                "-",
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





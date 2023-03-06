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
library(zoo)

    plotly::ggplotly(
    data |> 
    dplyr::filter(species_code == 'norcar',
                    subnational1_code %in% c('CA-QC',
                                             'CA-ON')) |> # User input
      dplyr::mutate(yearmon = as.yearmon(as.Date(date))) |> 
      dplyr::group_by(yearmon, subnational1_code) |> 
      dplyr::summarize(n = dplyr::n()) |> 
      ungroup() |> 
      group_by(subnational1_code) |> 
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

poly_canada <- rnaturalearth::ne_states(country = 'canada',
                                        returnclass = c( "sf")) 
class(poly_canada)
plot(poly_canada)

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

#ggplotly(
#  ggplot(poly_can_div) +
#    geom_sf(aes(fill = nr_sps))
#)

plot_ly(poly_can_div,
        split= ~woe_name,
        color = ~nr_sps,
        text = ~paste(nr_sps, "sps. were recognized in at least", sum_effort_hrs_atleast, "hours of effort"),
        hoveron = "fills",
        hoverinfo = "text",
        showlegend = FALSE
) 



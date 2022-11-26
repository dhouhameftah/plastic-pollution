library(tidyverse)
library(gifski)
library(gganimate)
library(ggplot2)

# graph top 10 companie par pays (shiny)
f_company_pays_year <- function(plastics_ds, pays = "World") {
  plastics_ds %>%
    filter(
      parent_company != "null" &
        parent_company != "NULL" &
        parent_company != "Unbranded" &
        parent_company != "Grand Total" &
        parent_company != "Assorted" & !country == "Empty" &
        !parent_company == "Null"
    ) %>%
    
    filter(country == pays | pays == "World") %>%
    group_by(parent_company) %>%
    summarize(grand_total = sum(grand_total, na.rm = TRUE)) %>% arrange(desc(grand_total)) %>%
    head(10) %>%
    ggplot(aes(
      x = reorder(parent_company, grand_total),
      y = grand_total,
      fill = grand_total
    )) +
    geom_bar(stat = "identity") + labs(
      y = "Quantité de plastique",
      x = "",
      title = "La production de plastique par compagnie en 2019 et 2020",
      caption = "Source: Break Free from Plastic"
    ) +
    coord_flip() + theme_minimal() + #scale_fill_gradient(low="blue", high="red")
    scale_fill_viridis_c (option = "viridis") +
    theme(
      plot.title = element_text(
        color = "black",
        size = 20,
        face = "bold"
      ),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15),
      axis.text.y = element_text(angle = 0)
    )
}
#top10 pays en 2019 et 2020
top_plastics_by_countries <- function (plastics_ds)
{
  plastics_ds %>% group_by(country) %>%
    summarise(grand_total = sum(grand_total, na.rm = TRUE)) %>%
    filter(!country == "Empty") %>%
    arrange(desc(grand_total)) %>% head(10) %>%
    ggplot(aes(
      y = reorder(country, grand_total),
      x = grand_total,
      color = grand_total,
      size = grand_total
    )) +
    geom_point(show.legend = FALSE) + scale_size(range = c(5, 15)) +
    labs(
      y = "",
      x = "Quantité de plastique",
      title = "Les pays les plus consommateurs de plastique en 2019 et 2020",
      caption = "Source: Break Free from Plastic"
    ) +
    scale_x_continuous(breaks = seq(0, 240000, by = 40000)) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        color = "black",
        size = 20,
        face = "bold"
      ),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text = element_text(size = 15),
      axis.text.y = element_text(angle = 0)
    ) + scale_color_gradient(low = "yellow", high = "red")
}
#top10 pays en 2019 et 2020 par valenteer
top_plastics_by_volenteers <- function (plastics_ds)
{
  plastics_ds %>%
    filter(!country == "Empty") %>% group_by(country)  %>%
    summarise(grand_total = sum(grand_total, na.rm = TRUE),
              volunteers = volunteers) %>%
    distinct(country, grand_total, volunteers) %>%
    ungroup() %>% group_by(country, grand_total) %>% summarise(volunteers =
                                                                 sum(volunteers)) %>%
    arrange(desc(volunteers)) %>% mutate(plastics_per_volunteer = grand_total /
                                           volunteers) %>%
    arrange(desc(plastics_per_volunteer)) %>% head(10) %>%
    ggplot(
      aes(
        y = reorder(country, plastics_per_volunteer),
        x = plastics_per_volunteer,
        color = plastics_per_volunteer,
        size = plastics_per_volunteer
      )
    ) +
    geom_point(show.legend = FALSE) + scale_size(range = c(5, 15)) +
    labs(
      y = "",
      x = "Quantité de plastique par volantaire ",
      title = "Les pays les plus consommateurs de plastique en 2019 et 2020",
      caption = "Source: Break Free from Plastic"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(
        color = "black",
        size = 20,
        face = "bold"
      ),
      axis.title.y = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(size = 15),
      axis.text = element_text(size = 15),
      axis.text.y = element_text(angle = 0)
    )
}

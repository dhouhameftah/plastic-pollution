get_popups <- function(plastics_types_ds, country_names)
{
  country_names <- country_names %>% as_tibble()
  p <- as.list(NULL)
  p <- lapply(1:nrow(country_names), function(i) {
    p[[i]] <-
      plastic_type_graph(plastics_types_ds, country_names[i,] %>% as.character())
  })
  return(p)
  
}

type_plastics_data_init <- function(plastic_ds)
{
  plastic_ds$parent_company[plastic_ds$parent_company == 'Null' |
                              plastic_ds$parent_company == 'Unbranded' |
                              plastic_ds$parent_company == 'Grand Total'] <-
    'Autres'
  
  #plot graph
  plastics_types <-
    plastic_ds %>% group_by(country, parent_company) %>%
    summarise(across(empty:volunteers, sum, na.rm = TRUE))  %>%
    pivot_longer(cols = empty:pvc) %>%
    group_by(country, parent_company, name) %>%
    summarize(
      type_total = sum(value, na.rm = TRUE),
      grand_total = sum(grand_total, na.rm = TRUE)
    ) %>%
    mutate(pct_grand_total = type_total / grand_total)
  return(plastics_types)
}

plastic_type_graph <- function(plastics_types_ds, country_name)
{
  #plot graph
  graph <-plastics_types_ds %>%  filter(country == country_name) %>% 
    arrange(desc(grand_total))  %>% head(80) %>% 
    ggplot() +
    geom_col(aes(
      x = parent_company,
      y = pct_grand_total,
      group = name,
      fill = name
    ),
    width = 0.55) +
    labs(x = "", y = "% plastique") +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 11, angle = -90),
      axis.title.y = element_text(size = 11, family = "Arial"),
    ) +
    guides(
      fill = guide_legend(
        title.position = "right",
        title.vjust = 2,
        title.hjust = .5,
        title = "Type de plastiques"
      )
      
    )
  return(graph)
}
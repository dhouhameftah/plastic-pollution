
source("map_popup.R")

init_map <-
  function(plastics_ds,
           type_plastics_ds,
           countries_ds,
           countries_coordinates_ds,
           countries_iso_ds)
  {
    #basemap
    countries_total <-
      plastics_ds %>% group_by(country) %>% summarize(grand_total = sum(grand_total, na.rm = TRUE))
    data_map <-
      countries_ds[countries_ds$ADMIN %in% countries_total$country, ]
    
    map <-
      leaflet(data_map) %>% setView(44.0665, 23.74667, 2) %>% addTiles()
    
    #set bin and color for choropleth map
    bins <- c(0, 500, 1000, 5000, 10000, 15000, 20000, 250000)
    pal <-
      colorBin("YlOrRd", domain = countries_total$grand_total, bins = bins)
    
    lbl_grand_total <-
      data_map$ADMIN %>% as_tibble() %>% rename(country = value) %>% inner_join(countries_total, by =
                                                                                  "country")
    labels <- sprintf(
      "<strong>%s</strong><br/>plastiques: %g<sup></sup>",
      data_map$ADMIN,
      lbl_grand_total$grand_total
    ) %>% lapply(htmltools::HTML)
    
   
    #add polygons,labels and mouse over effect
    map <- map %>% addPolygons(
      fillColor = ~ pal(lbl_grand_total$grand_total),
      weight = 1,
      opacity = 0.5,
      dashArray = '1',
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 1,
        fillOpacity = .6,
        opacity = 0.7,
        bringToFront = FALSE,
        sendToBack = FALSE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 3px"),
        textsize = "12px",
        direction = "auto"
      )
      ,
      popup = popupGraph(
        get_popups(type_plastics_ds, data_map$ADMIN),
        type = "png",
        width = 300,
        height = 300
      )
    ) %>% 
      addLegendBin (
        pal = pal,
        values = lbl_grand_total$grand_total,
        title = "Quantité de plastiques",
        position = 'bottomright'
      )

    # Display Volunteers in maps
    
    
    
    volunteers_info <-
      plastics_ds  %>% distinct(country, year, volunteers, num_events) %>% group_by(country) %>%
      summarize(
        volunteers = sum(volunteers,  na.rm = TRUE),
        num_events = sum(num_events,  na.rm = TRUE)
      ) %>%
      inner_join(countries_iso_ds, by = c("country" = "ADMIN")) %>%
      distinct(country,
               volunteers,
               num_events,
               Latitude..average.,
               Longitude..average.)
    
    volunteers_bins <-
      c(0, 100, 500, 1000, 2500, 5500, 10000 , 20000, 35000)
    radius <-
      findInterval(volunteers_info$volunteers, volunteers_bins) * 5
    # pal_radius <- colorNumeric("blue", domain = volunteers_info$volunteers)
    pal_radius <-
      colorBin(palette = "blue",
               domain = volunteers_info$volunteers,
               bins = volunteers_bins)
    
    circle_popup <-
      '<div style="width: 124px;border: 1px solid green;border-radius: 19px;padding-left: 14px;">
  <p style="
    text-align: center;
"><strong>%s</strong></p>
  <p><img style="margin-left: 15px;" src="https://img.freepik.com/premium-vector/plastic-free-100-percent-logo-eco-organic-sign_349999-1079.jpg?w=1380" alt="" width="62" height="57"><br>Evènements: <strong>%s</strong></p>
<p><img style="margin-left: 15px;" src="https://img.freepik.com/premium-vector/volunteer-trash-clean-up-logo-template-design-vector-emblem-design-concept-creative-symbol-icon_316488-975.jpg?w=2000" alt="" width="71" height="71"><br>Volontaires:<strong>%g</strong></p>
</div>'
    
    circle_popups <- sprintf(
      circle_popup,
      volunteers_info$country ,
      volunteers_info$num_events,
      volunteers_info$volunteers
    ) %>% lapply(htmltools::HTML)
    map <-
      map %>%  addCircleMarkers(
        data = volunteers_info,
        lat = volunteers_info$Latitude..average.,
        lng = volunteers_info$Longitude..average.,
        radius = radius,
        #radius, #20 * volunteers_info$volunteers_info,
        color = "blue",
        opacity = .6,
        fillOpacity = .3,
        weight = 0,
        popup  = circle_popups,
        # label = volunteers_info$volunteers,
        # labelOptions = labelOptions(
        #   noHide = TRUE,
        #   offset = c(0, 0),
        #   style = list("color" = "white",
        #                "padding-left" = "10px"),
        #   textOnly = TRUE
        # ),
        stroke = F
      )
    map <- map %>% addLegendCustom(
      colors =  rep("blue", each = 1),
      labels = c("Nombre de volontaires"),
      sizes = c(15),
      title = ""
    )
    
    return(map)
    
  }

addMarkersMap <- function(map_ds,plastics_ds,countries_iso_ds, markerCompany)
{
  
  
  company_icons <- iconList(
    "The Coca-Cola Company" = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/2/24/Coca-Cola_bottle_cap.svg",
      iconWidth = 20,
      iconHeight = 20
    ),
    "Nestle" = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/fr/1/15/Nestl%C3%A9_Waters.svg",
      iconWidth = 40,
      iconHeight = 30
    ),
    "Unilever" = makeIcon(
      iconUrl = "https://assets.turbologo.com/blog/fr/2021/09/22102011/unilever.png",
      iconWidth = 20,
      iconHeight = 20
    ),
    "Universal Robina Corporation" = makeIcon(
      iconUrl = "https://www.jgsummit.com.ph/images/conglomerate/conglo-urc.svg",
      iconWidth = 30,
      iconHeight = 30,
      
    ),
    
    "La Doo" = makeIcon(
      iconUrl = "https://allure.com.ng/wp-content/uploads/2021/02/Allure-logo1.png",
      iconWidth = 30,
      iconHeight = 30,
      
    )
  )
  
  top_company_markers <- plastics_ds  %>%
    filter(
      parent_company != "null" &
        parent_company != "Null" &
        parent_company != "NULL" &
        parent_company != "Unbranded" &
        parent_company != "Grand Total" &
        parent_company != "Assorted"
    ) %>%
    group_by(parent_company) %>%
    summarise(n = sum(grand_total, na.rm = TRUE)) %>% arrange(desc(n)) %>%
    head(10) %>% inner_join(plastics_ds, by = "parent_company") %>% distinct(parent_company, country) %>%
    inner_join(countries_iso_ds, by = c("country" = "ADMIN")) %>% filter(parent_company ==
                                                                        markerCompany)
  
  
  map_ds<- map_ds %>% addMarkers(
    data = top_company_markers,
    lat = top_company_markers$Latitude..average.,
    lng = top_company_markers$Longitude..average.,
    icon = company_icons[[markerCompany]]
  )
  return(map_ds)
  
}
addLegendCustom <-
  function(map, colors, labels, sizes, title, opacity = 0.5) {
    colorAdditions <-
      paste0(
        colors,
        "; border-radius: 50%;margin-top:7px; width:",
        sizes,
        "px; height:",
        sizes,
        "px"
      )
    labelAdditions <-
      paste0(
        "<div style='display: inline-block;height: ",
        sizes,
        "px;margin-top: 7px;line-height: ",
        sizes,
        "px;'>",
        labels,
        "</div>"
      )
    
    return(
      addLegend(
        map,
        colors = colorAdditions,
        title = title,
        labels = labelAdditions,
        opacity = opacity,
        ,
        position = 'bottomright'
      )
    )
  }

map_data_init <- function()
{
  countries <-
    readOGR(
      "https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson"
    )
  countries_coordinates <-
    read.csv("data/countries_codes_and_coordinates.csv")
  countries_coordinates <-
    countries_coordinates %>% mutate(Alpha.3.code = str_trim(Alpha.3.code))
  data <- list(countries, countries_coordinates)
  return(data)
}

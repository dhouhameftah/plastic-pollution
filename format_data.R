clean_plastics <- function(plastic_ds)
{
  plastics_norma <-
    plastic_ds %>% mutate(
      parent_company = stringi::stri_trans_totitle(parent_company),
      country = stringi::stri_trans_totitle(country)
    )
  
  plastics_norma$parent_company[plastics_norma$parent_company == 'PepsiCo'] <-
    'Pepsico'
  plastics_norma$parent_company[plastics_norma$parent_company == 'Nestlé'] <-
    'Nestle'
  plastics_norma$country[plastics_norma$country == "NIGERIA"] <-
    "Nigeria"
  plastics_norma$parent_company[plastics_norma$parent_company == "Mcdonald'S Corporation"] <-
    "MacDonald"
  plastics_norma$parent_company[plastics_norma$parent_company == "Capri Sun Group Holding"] <-
    "Caprisun"
  plastics_norma$parent_company[plastics_norma$parent_company == "Heineken International"] <-
    "Heineken"
  plastics_norma$country[plastics_norma$country == "United Kingdom of Great Britain & Northern Ireland"] <-
    "United Kingdom"
  plastics_norma$country[plastics_norma$country == "United Kingdom Of Great Britain & Northern Ireland"] <-
    "United Kingdom"
  plastics_norma$country[plastics_norma$country == "Taiwan_ Republic of China (ROC)"] <-
    "Taiwan"
  plastics_norma$country[plastics_norma$country == "Taiwan_ Republic Of China (Roc)"] <-
    "Taiwan"
  plastics_norma$country[plastics_norma$country == "Cote D_ivoire"] <-
    "Ivory Coast"
  plastics_norma$country[plastics_norma$country == "Hong Kong"] <-
    "Hong Kong S.A.R."
  plastics_norma$country[plastics_norma$country == "Korea"] <-
    "South Korea"
  plastics_norma$country[plastics_norma$country == "Serbia"] <-
    "Republic of Serbia"
  plastics_norma$country[plastics_norma$country == "Tanzania"] <-
    "United Republic of Tanzania"
  plastics_norma$country[plastics_norma$country == "United States Of America"] <-
    "United States of America"
  plastics_norma <-
    plastics_norma %>% mutate(country = ifelse(country == "ECUADOR" , "Ecuador", country))
  plastics_norma <-
    plastics_norma %>% mutate(
      parent_company = ifelse(
        parent_company == "Adonko bitters" |
          country == "Adonko Company Ltd" |
          parent_company == "Adonko Company Ltd",
        "Adonko Bitters",
        parent_company
      )
    )
  plastics_norma <-
    plastics_norma %>% mutate(
      parent_company = ifelse(
        parent_company == "JBC Food Corporation" |
          parent_company == "Jbc Corp" |
          parent_company == "Jbc Food Corp" |
          parent_company == "Jbc Foods Corp" |
          parent_company == "Jbc Corp." |
          parent_company == "Jbc Food Corp.",
        "Jbc Food Corporation",
        parent_company
      )
    )
  plastics_norma <-
    plastics_norma %>% mutate(
      parent_company = ifelse(
        parent_company == "Philip Morris International" |
          parent_company ==
          "L&M (Philip Morris)" |
          parent_company == "Chesterfield (Philip Morris)" |
          parent_company == "Heets (Philip Morris)",
        "Philip Morris",
        parent_company
      )
    )
  plastics_norma$parent_company[plastics_norma$parent_company == "Tamil Nadu Co-operative Milk Producers' Federation Ltd"] <-
    'Tamil Nadu'
  plastics_norma$parent_company[plastics_norma$parent_company == "Les Sociétés Anonymes des Brasseries du Cameroun"] <-
    'SABC'
  return(plastics_norma)
}
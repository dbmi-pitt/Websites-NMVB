library(tidyverse)

collapse_col <- function(.data, form_type, col) {
  
  # Collapses values from a column in a repeated form
  # Arguments: .data, a dataframe
  #            form_type, a string representing the repeated form
  #            col, a string representing the column to collapse over
  rtn <- .data %>%
    dplyr::filter(., `Repeat Instrument` == form_type) %>%
    dplyr::select(`Study ID`, `Repeat Instrument`, col) %>%
    dplyr::group_by(`Study ID`, `Repeat Instrument`) %>%
    dplyr::summarize(!!as.name(col) := paste(eval(as.name(col)), collapse = ",")) %>%
    ungroup
  
  return(rtn)
}

form_processing <- function(.data, form) {
  
  # Concatenates multi choice fields for each repeated form
  # Arguments: .data, a dataframe
  #            form, string representing the repeated form
  choice_col <- .data %>%
    dplyr::filter(`Repeat Instrument` == form) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::select(`Study ID`, contains('choice')) %>% names %>% stringr::str_extract(., '[^(]*') %>% unique %>% trimws %>% .[-1]
  # if character(0) then done
  # else call function to collapse each multi type for the form (since there could be many in one form)
  if (length(choice_col) > 0) {
    choice_col %>%
      purrr::map( ~ multi_choice_concat(.data = .data, form = form, form_type = .x)) %>%
      purrr::reduce(full_join)
  }
}

multi_choice_concat <- function(.data, form, form_type) {
  
  # Concatenates a multi choice field represented by many columns
  # Arguments: .data, a dataframe
  #            form, a string representing a repeated form
  #            form_type, a string representing the multi choice field from the form
  rtn <- .data %>%
    dplyr::filter(`Repeat Instrument` == form) %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::select(`Study ID`:`Repeat Instance`, contains(form_type), contains('Complete')) %>%
    tidyr::unite(., !!form_type, -`Study ID`:-`Repeat Instance`, -contains('Complete'), sep = "|", na.rm = TRUE) %>%
    dplyr::mutate_if(is.character, list(~na_if(., "")))
  
  return(rtn)
}

pivot_form <- function(.data, form_type) {
  
  # Pivots data from a repeated form (long) to a wide dataframe
  # Arguments: .data, a dataframe
  #            form_type, a string representing the repeated form to pivot
  cols <- .data %>% dplyr::filter(`Repeat Instrument` == form_type) %>% dplyr::select_if(~!all(is.na(.))) %>% names
  cols <- cols[-1:-2]
  
  cols %>%
    purrr::map( ~ collapse_col(.data = .data, form_type = form_type, col = .x)) %>%
    purrr::reduce(full_join)
}

# read in redcap csv extract
df <- readr::read_csv('~/Downloads/NMVB-full.csv', col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")

# get list of forms in redcap csv extract
forms <- df %>% mutate(`Repeat Instrument` = tidyr::replace_na(`Repeat Instrument`, 'None')) %>% select(`Repeat Instrument`) %>% unique %>% pull %>% .[-1]

# reshape redcap csv extract
reshaped <- df %>% 
  mutate(`Repeat Instrument` = tidyr::replace_na(`Repeat Instrument`, 'None')) %>%
  select(`Repeat Instrument`) %>% 
  unique %>% pull %>%
  map( ~ pivot_form(.data = df %>%
                               mutate(`Repeat Instrument` = tidyr::replace_na(`Repeat Instrument`, 'None')) %>%
                               select(-contains('choice'), -contains('Complete')) %>%
                               full_join(forms %>%
                                           map(~form_processing(.data = df %>% mutate(`Repeat Instrument` = tidyr::replace_na(`Repeat Instrument`, 'None')), form = .x)) %>%
                                           reduce(bind_rows)
                                         ), form_type = .x)
       ) %>% 
  reduce(bind_rows) %>%
  group_by(`Study ID`) %>%
  summarise_all(list(~paste(., collapse = ","))) %>%
  mutate_if(is.character, list(~stringr::str_replace_all(., "NA", ""))) %>%
  mutate_all(list(~gsub(",+", ",", .))) %>%
  mutate_all(list(~gsub("^,", "", .))) %>%
  mutate_all(list(~gsub(",$", "", .)))
  
  
# refactor race and ethnicity codes and reorder columns per Waqas specification  
master <- reshaped %>%
  mutate(Race = forcats::fct_collapse(Race, "Asian" = c("Asian Indian Pakistani", "Chinese",
                                                        "Filipino", "Japanese", "Kampuchean",
                                                        "Korean", "Laotian", "Other Asian",
                                                        "Thai", "Vietnamese"),
                                            "Pacific Islander" = c("Chamorran", "Fiji Islander",
                                                                   "Guamanian", "Hawaiian", "Hmong",
                                                                   "Melanesian", "Micronesian",
                                                                   "New Guinean", "Samon", "Tahitian",
                                                                   "Tongan")
                                      ),
         Ethnicity = forcats::fct_collapse(Ethnicity, "Hispanic or Latino" = c("Other Specified Spanish and Hispanic Origin", 
                                                                               "Cuban", "Mexican including Chicano",
                                                                               "Dominican Republic", "Puerto Rico", 
                                                                               "Spanish Surname Only", "Spanish Hispanic and Latino NOS",
                                                                               "South and Central American and Brazil")
         )) %>%
  select(`Institution`, `Case Type`, Gender, Race, Ethnicity, `Prospective Or Retrospective?`, `Age Range at Diagnosis`, `History of Smoking`,
         `History of Alcohol Use`, `Therapy Type`, `Harmful Exposure Substance`, `Vital Status`, `Histological Type`, `Biopsy Specimen Type`,
         `Resected Specimen Types`, `Resected Specimen Types`, `Blood Products Available`, `Age at Diagnosis`, everything())

# subset first 16 columns for web app
website <- master %>%
  select(1:16)

# save output to csv files
master %>% readr::write_csv('./master.csv')
website %>% readr::write_csv('./website.csv')
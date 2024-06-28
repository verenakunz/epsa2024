## Load required libraries
library(tidyverse)
library(pdftools)

## Get text from PDF
full_programme <- pdf_text("https://epsanet.org/wp-content/uploads/2024/06/EPSA-2024-Conference-Program_061524.pdf")

## Preprocess text
full_programme <- full_programme %>% 
  # Subset to pages containing normal panels
  .[c(11:119, 121:156, 158:172, 174:222, 225:232)] %>% 
  # Remove boilerplate information
  str_remove("Panels by Section\n\n\n\n10xx: Political Behavior\n11xx: Political Representation\n12xx: Political Economy\n13xx: Public Policy and Administration\n14xx: Public Opinion and Political Communication\n15xx: Comparative Politics\n16xx: European Politics\n17xx: Formal Political Theory\n18xx: Party Politics\n19xx: Conflict and Security\n20xx: Political Methodology\n21xx: International Relations\n22xx: Political Sociology\n\n\n\n\nThursday July 4\n\n\n\n") %>% 
  str_remove("Friday July 5\n\n\n\n\n") %>% 
  str_remove("Saturday July 6\n\n\n\n")

## Write function to get panel details
get_panel_details <- function(panel_page) {
  
  # Extract panel information from page
  panel_information <- str_split(panel_page, "\n\n\n")[[1]][1]
  
  # Extract panel details
  panel_infos <- str_split(panel_information, "(\n\n)|\n")
  panel_id <- panel_infos[[1]][1] %>% str_remove("Panel[:space:]+") %>% str_squish()
  panel_title <- panel_infos[[1]][2]
  panel_room <- panel_infos[[1]][3] %>% str_remove("Room[:space:]+") %>% str_squish()
  panel_day_time <- panel_infos[[1]][4] %>% str_remove("Time[:space:]+") %>% str_squish()
  panel_chair <- panel_infos[[1]][5] %>% str_remove("Chair[:space:]+") %>% str_squish()
  panel_discussant <- panel_infos[[1]][6] %>% str_remove("Discussant[:space:]+") %>% str_squish()
  
  panel_details <- tibble(panel_id = panel_id, 
                          panel_title = panel_title, 
                          panel_room = panel_room, 
                          panel_day_time = panel_day_time, 
                          panel_chair = panel_chair, 
                          panel_discussant = panel_discussant)
  
  return(panel_details)
}

## Get panel details
panel_details <- map(full_programme, 
                     get_panel_details) %>% 
  bind_rows()

## Preprocess panel details
panel_details <- panel_details %>% 
  mutate(
    # Add section information
    section = case_when(
      str_detect(panel_id, "^10") ~ "Political Behavior", 
      str_detect(panel_id, "^11") ~ "Political Representation", 
      str_detect(panel_id, "^12") ~ "Political Economy", 
      str_detect(panel_id, "^13") ~ "Public Policy and Administration", 
      str_detect(panel_id, "^14") ~ "Public Opinion and Political Communication", 
      str_detect(panel_id, "^15") ~ "Comparative Politics", 
      str_detect(panel_id, "^16") ~ "European Politics", 
      str_detect(panel_id, "^17") ~ "Formal Political Theory", 
      str_detect(panel_id, "^18") ~ "Party Politics", 
      str_detect(panel_id, "^19") ~ "Conflict and Security", 
      str_detect(panel_id, "^20") ~ "Political Methodology", 
      str_detect(panel_id, "^21") ~ "International Relations", 
      str_detect(panel_id, "^22") ~ "Political Sociology", 
      .default = NA), 
    # Extract weekday
    panel_day = str_extract(panel_day_time, "(Thursday)|(Friday)|(Saturday)"), 
    # Extract time
    panel_time = str_extract(panel_day_time, "[:digit:]{2}:[:digit:]{2}"))

## Write panel details to disk
write_rds(panel_details, "data/panel_details.rds")
write_csv(panel_details, "data/panel_details.csv")
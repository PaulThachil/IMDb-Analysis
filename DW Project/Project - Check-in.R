#Paul Thachil 4/12/24 Final Project - Web Scraping
rm(list = ls())

library(rvest)
library(dplyr)
library(readxl)

#Web Scraping 
get_box_office_data <- function(url) {
  html <- read_html(url)
  table <- html %>%
    html_nodes("table.a-bordered") %>%
    html_table(fill = TRUE)
  
  box_office_data <- table[[1]]
  
  colnames(box_office_data) <- c("Rank", "Movie.Name", "Worldwide", "Domestic", "Domestic %",
                                 "Foreign", "Foreign %")
  
  box_office_data$Movie.Name <- trimws(box_office_data$Movie.Name)
  
  return(box_office_data)
}

# Data from Box Office Mojo for 2020, 2021, 2022, and 2023
url_2020 <- "https://www.boxofficemojo.com/year/world/2020/"
box_office_2020 = get_box_office_data(url_2020)

url_2021 <- "https://www.boxofficemojo.com/year/world/2021/"
box_office_2021 = get_box_office_data(url_2021)

url_2022 <- "https://www.boxofficemojo.com/year/world/2022/"
box_office_2022 = get_box_office_data(url_2022)

url_2023 <- "https://www.boxofficemojo.com/year/world/2023/"
box_office_2023 = get_box_office_data(url_2023)

# Combine data 
boxofficemojo_data <- bind_rows(box_office_2020, box_office_2021, box_office_2022, box_office_2023)

boxofficemojo_data <- boxofficemojo_data %>%
  mutate_all(~ ifelse(. == "-", NA, .))

write.csv(boxofficemojo_data, file = "boxofficemojo_data.csv", row.names = FALSE)


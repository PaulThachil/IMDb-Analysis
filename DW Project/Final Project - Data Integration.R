# Paul Thachil 5/3/24 Final Project - Data Integration
rm(list = ls())  # Clear the environment

library(dplyr)  # For data manipulation
library(stringr)  # For string manipulation
library(readxl)  # For reading Excel files (not used in this part, but kept in case it's needed later)

# Load the datasets
imdb_data <- read.csv("imdb_data_20-23_.csv")
boxofficemojo_data <- read.csv("boxofficemojo_data.csv")

# Trim whitespace in Movie.Name
boxofficemojo_data$Movie.Name <- trimws(boxofficemojo_data$Movie.Name)
imdb_data$Movie.Name <- trimws(imdb_data$Movie.Name)

# Merge datasets on Movie.Name
merged_data <- merge(boxofficemojo_data, imdb_data, by = "Movie.Name", all.x = TRUE)

# Calculate the mean Meta.Score for replacement
meta_score_mean <- mean(merged_data$Meta.Score, na.rm = TRUE)

# Fill in missing Meta.Score values with the mean
merged_data <- merged_data %>%
  mutate(Meta.Score = ifelse(is.na(Meta.Score), meta_score_mean, Meta.Score))

# Functions to clean and convert to numeric
clean_money <- function(value) {
  value <- str_replace_all(value, "\\$", "")  # Remove dollar signs
  value <- str_replace_all(value, ",", "")  # Remove commas
  as.numeric(value)  # Convert to numeric
}

clean_percentage <- function(value) {
  value <- str_replace_all(value, "%", "")  # Remove percent signs
  as.numeric(value)  # Convert to numeric
}

# Rename the columns to avoid special characters
merged_data <- merged_data %>%
  rename(Domestic.Percentage = Domestic.., Foreign.Percentage = Foreign..)

# Convert specified columns to numeric
merged_data <- merged_data %>%
  mutate(
    Worldwide = clean_money(Worldwide),
    Domestic = clean_money(Domestic),
    Domestic.Percentage = clean_percentage(Domestic.Percentage),
    Foreign = clean_money(Foreign),
    Foreign.Percentage = clean_percentage(Foreign.Percentage)
  )

# Remove rows with any missing values
merged_data_clean <- merged_data[complete.cases(merged_data), ]

# Drop unnecessary columns
merged_data_clean <- merged_data_clean %>%
  select(-c(Duration, Rank))

# Write the cleaned data to a CSV file
write.csv(merged_data_clean, "merged_data_clean.csv", row.names = FALSE)

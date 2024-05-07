#Paul Thachil 5/3/24 Final Project - Descriptive Analysis & EDA
rm(list = ls())

library(dplyr)
library(ggplot2) 
library(scales)
library(tidyr)


data <- read.csv("merged_data_clean.csv")

numeric_vars <- data %>%
  select(Worldwide, Domestic, Domestic.Percentage, Foreign, Foreign.Percentage, Rating, Votes, Meta.Score, Year, Duration.Minutes)

# Correlation Matrix
correlation_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

print(correlation_matrix)

#Linear Regression Model 

lm_model <- lm(Worldwide ~ Rating, data = data)

# Summary of the regression model
summary(lm_model)

# Create a scatter plot with a linear regression line
ggplot(data, aes(x = Rating, y = Worldwide)) +
  geom_point() +  # Scatter plot of data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Relationship between IMDb Rating and Worldwide Box Office",
       x = "IMDb Rating",
       y = "Worldwide Box Office Revenue (in 100 Million)") +
  scale_y_continuous(
    breaks = seq(0, max(data$Worldwide, na.rm = TRUE), by = 100e6),  # Y-axis breaks at 100 million
    labels = scales::label_number(scale = 1e-8)  # Format in hundreds of millions
  ) +
 #Theme customizations
   theme(
    plot.title = element_text(hjust = 0.5, family = "Times"),  
    panel.background = element_rect(fill = "grey90"), 
    panel.grid.major = element_line(color = "white"), 
    panel.grid.minor = element_line(color = "white"), 
    axis.title.x = element_text(family = "Times"), 
    axis.title.y = element_text(family = "Times"),  
    axis.text = element_text(family = "Times") 
  )
#First boxplot, IMDb ratings range
ggplot(data, aes(x = factor(1), y = Rating)) +
  geom_boxplot(fill = "gold", color = "darkblue", width = 0.3) +  
  scale_y_continuous(limits = c(0, 10)) +  
  labs(
    title = "Boxplot of IMDb Rating",
    x = "",
    y = "IMDb Rating (0-10)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centered title
    axis.title.x = element_blank(),  # No label for x-axis
    axis.text.x = element_blank(),  # Hide x-axis text
    axis.ticks.x = element_blank()  # Hide x-axis ticks
  )

#Second boxplot, Metascore range
ggplot(data, aes(x = factor(1), y = Meta.Score)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", width = 0.3) +  # Thinner box
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis range from 0 to 100 for Metascore
  labs(
    title = "Boxplot of Metascore",
    x = "",
    y = "Metascore (0-100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centered title
    axis.title.x = element_blank(),  # No label for x-axis
    axis.text.x = element_blank(),  # Hide x-axis text
    axis.ticks.x = element_blank()  # Hide x-axis ticks
  )

# Performing a t-test to check if the correlation is significant
correlation_test <- cor.test(data$Duration.Minutes, data$Worldwide)
print(correlation_test)

correlation_test$p.value

#Final Visualization, Bar Graph
data <- data %>%
  mutate(
    Duration.Minutes = as.numeric(Duration.Minutes),  
    Domestic = as.numeric(Domestic),  
# Segmenting the x axis by 20-minute intervals up to 220 minutes
    Duration.Segment = cut(Duration.Minutes, breaks = seq(80, 220, by = 20), right = FALSE, include.lowest = TRUE)
  )

# Calculate the average domestic box office revenue by duration segment
duration_summary <- data %>%
  group_by(Duration.Segment) %>%
  summarize(Avg.Domestic = mean(Domestic, na.rm = TRUE)) 

# Creates a bar graph to show the average domestic box office sales for each duration segment
ggplot(duration_summary, aes(x = Duration.Segment, y = Avg.Domestic)) +
  geom_bar(stat = "identity", fill = "gold", color = "darkblue") +  
  labs(
    title = "Average Domestic Box Office Sales by Movie Duration Segment",
    x = "Duration Segment (Minutes)",
    y = "Average Domestic Box Office Sales in Millions"
  ) +
  scale_y_continuous(
    limits = c(0, 350000000),  
    labels = scales::label_number(scale = 1e-6)  
  ) +
  scale_x_discrete(drop = FALSE) +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5), 
  )
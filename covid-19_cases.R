# Midpoint Deliverable: Bar chart


# Clears global environment
rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(openintro)
library(plotly)
library(lintr)
library(styler)
style_file("covid-19_cases.R")
# Create the COVID-19 cases dataset from Jan. 23 to May 11
covid19_cases <- read.csv("COVID-19_cases/us_states_covid19_daily.csv")

# What is the total positive cases until May 11?
total_positive_cases <- covid19_cases %>%
  head(56) %>%
  tally(positive) %>%
  pull()

# Which day has the highest increase of positive case?
highest_positiveIncrease_date <- covid19_cases %>%
  mutate(day = ymd(date)) %>%
  group_by(day) %>%
  summarize(
    total_positiveIncrease = sum(positiveIncrease, na.rm = TRUE)
  ) %>%
  filter(total_positiveIncrease == max(total_positiveIncrease,
    na.rm = TRUE
  )) %>%
  pull(day)

# Bar chart of the COVID-19 increase of positive case and currently hositalized vs date
# This chart was intended to present the total positive increase cases  and currently
# hospitalized cases of each day from Jan. 23 to May 11. Through the bar chart,
# we could tell the trend of positive cases and currently hospitalized cases each day
covid19_date <- covid19_cases %>%
  mutate(day = ymd(date))

positiveIncrease_vs_date_chart <- function(covid_df) {
  plot_ly(
    x = ~ covid_df$day, y = ~ covid_df$positiveIncrease,
    type = "scatter", mode = "lines",
    name = "Positive Increase", fill = "tozeroy"
  ) %>%
    add_trace(
      x = ~ covid_df$day, y = ~ covid_df$hospitalizedCurrently,
      name = "Currently Hospitalized",
      fill = "tozeroy"
    ) %>%
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Positive and Hospitalized Cases")
    )
}

positiveIncrease_vs_date_chart(covid19_date)

# Midpoint Deliverable: Bar chart


# Clears global environment
rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyverse)
library(openintro)
# Create the COVID-19 cases dataset
covid19_cases <- read.csv("COVID-19_cases/us_states_covid19_daily.csv")

# Which state has the highest hispitalized rate amongst the total cases
# of the state?
highest_hospitalized_state <- covid19_cases %>%
  group_by(state) %>%
  summarize(
    total_positive = sum(positive, na.rm = TRUE),
    total_current_hospitalized = sum(hospitalizedCurrently, na.rm = TRUE),
    hospitalized_rate = total_current_hospitalized / total_positive
  ) %>%
  filter(hospitalized_rate == max(hospitalized_rate, na.rm = TRUE)) %>%
  pull(state)

# Which state has the most positive cases?
highest_positive_state <- covid19_cases %>%
  abbr2state(state) %>%
  group_by(state) %>%
  summarize(
    total_positive_cases =  sum(positive, na.rm = TRUE)
  ) %>%
  filter(total_positive_cases == max(total_positive_cases, 
                                     na.rm = TRUE)
         ) %>%
  pull(state)

# Bar chart of the COVID-19 hospitalized rate vs state
# This chart was intended to present the current hospitalized rate in each state
# from Jan. 23 to May 11. Through the bar chart, we could tell how each state's
# hospitalized rate differes from each other
state_vs_rate_chart <- function(covid_df){
    ggplot(
      data = covid_df,
      mapping = aes(
        x = state, 
        y = hospitalizedCurrently / positive,
        fill = hospitalizedCurrently / positive
      )
    ) + geom_col() +
    labs(
      x = "State", y = "Hospitalized Rate",
      subtitle = "Hospitalized Rate per State"
    ) +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6))
}

state_vs_rate_chart(covid19_cases)
  

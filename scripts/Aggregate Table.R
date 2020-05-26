# Midpoint Deliverable: Aggregate Table

# This aggregate table collected information from the dataset
# "us_states_covid19_daily.csv" in the data/ directory

rm(list = ls())
library(lintr)
library(styler)
library(dplyr)
library(lubridate)
style_file("Aggregate Table.R")


# Read the "us_state_covid19_daily.csv" file
covid19_cases <- read.csv("data/COVID-19_cases/us_states_covid19_daily.csv",
                          stringsAsFactors = FALSE)

# The aggregate table is grouped by date, arranged from the most recent
# date to the earliest date in record. Through grouping by date, we could 
# tell the trend of positive cases in the US everyday and view the number
# more wholistically by considering recovered number and currently 
# hospitalized number
# 
# The table included information of date, total positive of each day, 
# currently hospitalized, recovered, and positive increase
cases_by_date <- covid19_cases %>%
  mutate(day = ymd(date)) %>%
  group_by(day) %>%
  summarize(
    daily_positive = sum(positive, na.rm = TRUE),
    daily_cur_hospitalized = sum(hospitalizedCurrently, na.rm = TRUE),
    daily_recover = sum(recovered, na.rm = TRUE),
    daily_positive_increase = sum(positiveIncrease, na.rm = TRUE)
  ) %>%
  arrange(desc(day), .by_group = TRUE)
 






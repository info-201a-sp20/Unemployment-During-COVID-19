# Midpoint Deliverable: Aggregate Table

# This aggregate table collected information from the dataset
# "us_states_covid19_daily.csv" in the data/ directory

rm(list = ls())
library(lintr)
library(styler)
library(dplyr)
style_file("Aggregate Table.R")


# Read the "us_state_covid19_daily.csv" file
covid19_cases <- read.csv("data/COVID-19_cases/us_states_covid19_daily.csv",
                          stringsAsFactors = FALSE)

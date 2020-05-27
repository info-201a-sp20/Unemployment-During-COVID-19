# Midpoint Deliverable: Line Chart

library("dplyr")
library("tidyr")
library("ggplot2")

load_data <- read.csv("data/national_unemployment/national_report.csv",
                      stringsAsFactors = FALSE)
# This script helps find the average initial claims in the US
is.data.frame(load_data)

# removed commas in initial_claims
load_data$Initial_Claims <- as.numeric(gsub(",", "", load_data$Initial_Claims))

#changed date into .date format for chronological purposes
load_data$Filed_week_ended <- as.Date(load_data$Filed_week_ended,
                                      format = "%m/%d/%y")

# 50 states were grouped by the date, then took the average of the initial
# claims and assigned new column for average initial claims
first <- load_data %>%
  group_by(Filed_week_ended) %>%
  summarize(average_initial_claims = round(mean(Initial_Claims, 0))
)

# average initial claims throughout the months
months <- mean(first$average_initial_claims, na.rm = TRUE)
print(months)

# What is the highest average initial claim throuhgout the months?
# Found highest average
highest_average <- max(first$average_initial_claims, na.rm = TRUE)

# Found the date it happened
date_of_highest_average <- filter(first,
                                average_initial_claims == highest_average) %>%
  select(Filed_week_ended)


# Claims from January 5, 2019 to May 9, 2020 (Including Pandemic)
national_claims_plot <- ggplot(data = first, aes(x = Filed_week_ended,
                                                 y = average_initial_claims,
                                                 group = 1)) +
   geom_line() +
   geom_point() +
   scale_x_date(breaks = first$Filed_week_ended) +
# code found https://stackoverflow.com/questions/51604367/show-all-date-values-on-ggplot-x-axis-r
# shows all dates in the x axis

  ylim(0, max(first$average_initial_claims) + 500) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# made the data label vertical (hence the 90 degree angle)
# code found: https://gist.github.com/benmarwick/8b95b8fb226986bd86a47ad92e0017f2
  ggtitle("Average Unemployment Claim Rates in the US ") +
  xlab("Date") +
  ylab("Number of Initial Claims")

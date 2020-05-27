washington_covid <- read.csv("data/washington_unemployment/crosstab_real.csv",
                             stringsAsFactors = FALSE)

claims_data <- read.csv("data/washington_unemployment/initial_claims.csv",
                        stringsAsFactors = FALSE)

library("dplyr")
library("ggplot2")
library("plotly")


claims_data$Initial.Claims <- gsub(",", "", claims_data$Initial.Claims)
claims_data$Initial.Claims <- as.numeric(claims_data$Initial.Claims)
claims_plot <- ggplot(data = claims_data, aes(x = Week, y = Initial.Claims,
                                              group = 1)) +
  geom_point() +
  scale_y_continuous(limits = c(5000, 185000)) +
  labs(
    title = "Amount of Initial Claims of Unemployment Per Week",
    x = "Week of Quarantine",
    y = "Initial Claims"
  )

# The plot is a line plot of initial claims of EVERY industry and EVERY
# county that occurred on a weekly basis from weeks 1-19 of the pandemic.
wa_claims_plot <- ggplotly(claims_plot)

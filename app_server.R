library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
source("scripts/national_report.R")

server <- function(input, output) {
# Positive Increase and Hospitalized Cases
output$cases_plot <- renderPlotly({
  cases_plot <- plot_ly(
    data = covid19_cases,
    x = ~ covid19_cases$Date, y = ~ covid19_cases$positiveIncrease,
    type = "scatter", mode = "lines",
    name = "Positive Increase", fill = "tozeroy"
  ) %>%
    add_trace(
      x = ~ covid19_cases$Date, y = ~ covid19_cases$hospitalizedCurrently,
      name = "Currently Hospitalized",
      fill = "tozeroy"
    ) %>%
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Positive and Hospitalized Cases"),
      title = "Jan. 23 to May 11 Positive and Hospitalized Cases"
    )
  return(cases_plot)
})

#National Cases Confirmed
output$national_cases_plot <- renderPlotly({
  # Return the selected column in `case_input`
  selected_col <- covid19_cases %>%
    colnames(input$categories)
  
  national_cases_plot <- plot_ly(
    data = covid19_cases,
    x = ~ input$date,
    y = ~ covid19_cases[selected_col, ],
    type = "scatter", mode = "lines",
    name = input$categories, fill = "tozeroy"
  ) %>%
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Category"),
      title = "Jan. 23 to May 11 Cases"
    )
  return(national_cases_plot)
})

# National Unemployment Claims
df_filtered <- reactive({
  filter(load_data, State == input$state_input)
})

output$national_claims_plot <- renderPlotly({
  national_plot <- ggplot(data = df_filtered(),
                          mapping = aes(x = Filed_week_ended,
                                        y = Initial_Claims)) +
    geom_line() +
    geom_point() +
    scale_x_date(breaks = df_filtered()$Filed_week_ended) +
    # code found https://stackoverflow.com/questions/51604367/show-all-date-values-on-ggplot-x-axis-r
    # shows all dates in the x axis
    ylim(0, max(df_filtered()$Initial_Claims) + 500) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
    # made the data label vertical (hence the 90 degree angle)
    # code found: https://gist.github.com/benmarwick/8b95b8fb226986bd86a47ad92e0017f2
    ggtitle("Average Unemployment Claim Rates in the US ") +
    xlab("Date") +
    ylab("Number of Initial Claims")
    ggplotly(national_plot)
    return(national_plot)
})
output$conclusion_plot <- renderPlotly({
  second <- ggplot(data = first, aes(x = Filed_week_ended,
                                     y = average_initial_claims, group = 1)) +
    geom_line() +
    geom_point() +
    scale_x_date(breaks = first$Filed_week_ended) +
    ylim(0, max(first$average_initial_claims) + 500) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Average Unemployment Claim Rates in the US ") +
    xlab("Date") +
    ylab("Number of Initial Claims")
  ggplotly(second)
  return(second)
})

# washington unemployment plots
output$industry_plot <- renderPlotly({
  plot_data <- impacted_industry %>%
    filter(Row >= input$industry_choice[1], Row <= input$industry_choice[2])
  p <- ggplot(plot_data) +
    geom_col(aes(x = Industry, y = Total, group = 1)) +
    scale_y_continuous(limits = c(0, 35000)) +
    labs(
      title = "Amount of Initial Claims Per Industry",
      x = "Industry",
      y = "Initial Claims"
    ) + scale_x_discrete(labels = seq(1, 94)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 2))
  ggplotly(p)
  return(p)
})
output$week_plot <- renderPlotly({
  p <- ggplot(data = claims_data, aes(x = Week, y = Initial.Claims,
                                      group = 1)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(5000, 185000)) +
    labs(
      title = "Amount of Initial Claims of Unemployment Per Week",
      x = "Week of Quarantine",
      y = "Initial Claims"
    )
  ggplotly(p)
  return(p)
})
}

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

# define inputs

# katie

# shraddha

# joe

# define ui
ui <- tagList(navbarPage(
  title = "Effects of COVID-19 on Unemployment in Washington and United States", 
 
  tabsetPanel(id = "tabs",
    tabPanel("Introduction", value = 0), 
    navbarMenu("Visualizations",
      # katie
      tabPanel("Visualization 1", value = 1),
      # shraddha
      tabPanel("Visualization 2", value = 2),
      # joe
      tabPanel("Visualization 3", value = 3)
    ), 
    tabPanel("Conclusion", value = 4)
  )),
  
  # sidebar only shows up for the visualization pages
  conditionalPanel(condition = "input.tabs != 0 && input.tabs != 4",
    tagList(
      br(),
    sidebarLayout(
      sidebarPanel(
        # katie
        conditionalPanel(condition = "input.tabs == 1", 
                         helpText("desc of visual")),
        # shraddha
        conditionalPanel(condition = "input.tabs == 2", 
                         helpText("desc of visual")),
        # joe
        conditionalPanel(condition = "input.tabs == 3", 
                         helpText("desc of visual"))
      ),
      mainPanel(
        # katie
        conditionalPanel(condition = "input.tabs == 1", helpText("main")),
        # shraddha
        conditionalPanel(condition = "input.tabs == 2", helpText("main")),
        # joe
        conditionalPanel(condition = "input.tabs == 3", helpText("main"))
      )
    )
  ))
)

server <- function(input, output) {
  # katie
  
  # shraddha
  
  # joe
}

shinyApp(ui = ui, server = server)

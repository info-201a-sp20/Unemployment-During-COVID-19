library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

# define inputs

# katie

# shraddha

# joe
claims_data <- read.csv("data/washington_unemployment/initial_claims.csv",
                        stringsAsFactors = FALSE)

claims_data$Initial.Claims <- gsub(",", "", claims_data$Initial.Claims)
claims_data$Initial.Claims <- as.numeric(claims_data$Initial.Claims)

#Converted all relevant columns from type char to type numeric
washington_covid$ADAMS <- as.numeric(washington_covid$ADAMS)
washington_covid$ASOTIN <- as.numeric(washington_covid$ASOTIN)
washington_covid$BENTON <- as.numeric(washington_covid$BENTON)
washington_covid$CHELAN <- as.numeric(washington_covid$CHELAN)
washington_covid$CLALLAM <- as.numeric(washington_covid$CLALLAM)
washington_covid$CLARK <- as.numeric(washington_covid$CLARK)
washington_covid$COLUMBIA <- as.numeric(washington_covid$COLUMBIA)
washington_covid$COWLITZ <- as.numeric(washington_covid$COWLITZ)
washington_covid$DOUGLAS <- as.numeric(washington_covid$DOUGLAS)
washington_covid$FERRY <- as.numeric(washington_covid$FERRY)
washington_covid$FRANKLIN <- as.numeric(washington_covid$FRANKLIN)
washington_covid$GARFIELD <- as.numeric(washington_covid$GARFIELD)
washington_covid$GRANT <- as.numeric(washington_covid$GRANT)
washington_covid$GRAYS.HARBOR <- as.numeric(washington_covid$GRAYS.HARBOR)
washington_covid$ISLAND <- as.numeric(washington_covid$ISLAND)
washington_covid$JEFFERSON <- as.numeric(washington_covid$JEFFERSON)
washington_covid$KING <- as.numeric(washington_covid$KING)
washington_covid$KITSAP <- as.numeric(washington_covid$KITSAP)
washington_covid$KITTITAS <- as.numeric(washington_covid$KITTITAS)
washington_covid$KLICKITAT <- as.numeric(washington_covid$KLICKITAT)
washington_covid$LEWIS <- as.numeric(washington_covid$LEWIS)
washington_covid$LINCOLN <- as.numeric(washington_covid$LINCOLN)
washington_covid$MASON <- as.numeric(washington_covid$MASON)
washington_covid$OKANOGAN <- as.numeric(washington_covid$OKANOGAN)
washington_covid$PACIFIC <- as.numeric(washington_covid$PACIFIC)
washington_covid$PEND.OREILLE <- as.numeric(washington_covid$PEND.OREILLE)
washington_covid$PIERCE <- as.numeric(washington_covid$PIERCE)
washington_covid$SAN.JUAN <- as.numeric(washington_covid$SAN.JUAN)
washington_covid$SKAGIT <- as.numeric(washington_covid$SKAGIT)
washington_covid$SKAMANIA <- as.numeric(washington_covid$SKAMANIA)
washington_covid$SNOHOMISH <- as.numeric(washington_covid$SNOHOMISH)
washington_covid$SPOKANE <- as.numeric(washington_covid$SPOKANE)
washington_covid$STEVENS <- as.numeric(washington_covid$STEVENS)
washington_covid$THURSTON <- as.numeric(washington_covid$THURSTON)
washington_covid$WAHKIAKUM <- as.numeric(washington_covid$WAHKIAKUM)
washington_covid$WALLA.WALLA <- as.numeric(washington_covid$WALLA.WALLA)
washington_covid$WASHINGTON <- as.numeric(washington_covid$WASHINGTON)
washington_covid$WHATCOM <- as.numeric(washington_covid$WHATCOM)
washington_covid$WHITMAN <- as.numeric(washington_covid$WHITMAN)
washington_covid$YAKIMA <- as.numeric(washington_covid$YAKIMA)

impacted_industry <- mutate(washington_covid, Total =
                              rowSums(Filter(is.numeric, washington_covid),
                                      na.rm = TRUE))

impacted_industry <- impacted_industry %>% arrange(Industry) %>% mutate(Row = seq(1,94))

industry_input <- sliderInput(
  inputId = "industry_choice",
  label = "Please Choose a Range of Industries (1-94), all ordered by Alphabetical Order (A-Z)",
  min = head(impacted_industry$Row, 1),
  max = nrow(impacted_industry),
  value = c(1,94)
)

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
      tabPanel("Washington Unemployment Data", value = 3)
    ),
    # ALL OF OUR takeaways in a conslusion page.
    tabPanel("Conclusion", value = 4,
             h1("Conclusions"),
             h2("Takeaway 1: Washington Unemployment Claims"),
             plotlyOutput("week_plot"),
             h4("Purpose of Washington Unemployment Claims Plot"),
             p("From the scatter/line chart above, the goal was
               to see if the state of Washington had been still trending
               upwards for initial unemployment claims filed per week, or to see
               if the state had possibly reached a pinnacle of unemployment claims
               and appeard to either level off or trend downwards."),
             h4("What We Learned from the Data and What We Can Use Our Data To Predict"),
             p("The intial prediction was that as quarantine started, and the economy came to
               a gradual hault, the intial unemploymen claims would skyrocket for the first few
               weeks. However, this was not the case, and as seen in the plot, for the first 9 weeks,
               the unemployment claims lingered around 7000-9000 claims per week. Week 10 took somewhat
               of a rise, and week 11 took a significant rise as the unemployment claims for that week
               approached around 130,000. The pinnacle seemed to be week 12 as the unemployment claims
               reached around 180,000. After that week, the trend was very apparently downards week 15
               when it reached a low of around 80000. From there, there have been a few trends of both
               upwards and downwards throughout the weeks, but never a drastic change. Since week 20 just
               concluded, the Tableau report was updated with the new unemployment claims, and this week
               cocnluded with around 48000 unemployment claims."),
             p("From the data above, we have been able to see trends throughout the weeks of the pandemic
               in the state of Washington. As the data and graph is continuously updated, users will
               be able to see if and when a more consistent trend will arise, and therefore see when
               unemployment claims will gradually come to a low and be compared to normal unemployment claims
               pre pandemic OR if trends will gradually rise again meaning our economy would still be in a hole.
               The only exception to this is if there is a sudden and drastic change in trend due to a
               signifcant event. From week 21, many malls and businesses nationwide were supposed to open and
               therefore decrease unemployment claims. However, due to the George Floyd protests, this
               has haulted and even disrupted many of this businesses which should prevent unemployment
               claims from drastically lowering. Overall, insights can be made and will continually
               be made about the trends of unemployment claims throughout the weeks of unemployment.
               This can be done until the pandemic dies down and life approaches normality in the future.")
             )
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
                         industry_input,
                         helpText("This bar chart describes the initial unemployment claims per
                                  industry in week 19 of the COVID-19 pandemic. There are
                                  a total of 94 industries classified by the state of Washington."))
      ),
      mainPanel(
        # katie
        conditionalPanel(condition = "input.tabs == 1", helpText("main")),
        # shraddha
        conditionalPanel(condition = "input.tabs == 2", helpText("main")),
        # joe
        conditionalPanel(condition = "input.tabs == 3",
                         titlePanel("Washington Unemployment Data"),
                         h3("Plot of Initial Claims Per Industry"),
                         plotlyOutput("industry_plot"),
                         helpText("The bar chart above can be adjusted to
                                  to see a specific range of industries.
                                  The industries are sorted in alphabetical order,
                                  and the bar chart will change and display the specific
                                  industries in accordance to the range inputted into the slider"))
      )
    )
  ))
)

server <- function(input, output) {
  # katie
  
  # shraddha
  
  # joe
  output$industry_plot <- renderPlotly({
    plot_data <- impacted_industry %>%
      filter(Row >= input$industry_choice[1], Row <= input$industry_choice[2] )
    p <- ggplot(plot_data) +
      geom_col(aes(x = Industry, y = Total, group=1)) +
      scale_y_continuous(limits = c(0, 35000)) +
      labs(
        title = "Amount of Initial Claims Per Industry",
        x = "Industry",
        y = "Initial Claims"
      ) + 
      scale_x_discrete(labels=seq(1,94)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 2))
    ggplotly(p)
    return(p)
  })
  output$week_plot <- renderPlotly({
  p <- ggplot(data = claims_data, aes(x = Week, y = Initial.Claims, group=1)) +
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

shinyApp(ui = ui, server = server)

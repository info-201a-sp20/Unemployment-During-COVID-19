library("dplyr")
library("shiny")
library("leaflet")
library("plotly")

washington_covid <- read.csv("data/washington_unemployment/crosstab_real.csv",
                             stringsAsFactors = FALSE)

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

#theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

####################################################################

impacted_industry <- mutate(washington_covid, Total =
                              rowSums(Filter(is.numeric, washington_covid),
                                      na.rm = TRUE))

impacted_industry <- impacted_industry %>% arrange(Industry) %>% mutate(Row = seq(1,94))
industries <- length(impacted_industry$Total)

server <- function(input, output) {
  output$plot <- renderPlotly({
    plot_data <- impacted_industry %>%
      filter(Row > input$industry_choice[1], Row < input$industry_choice[2] )
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
}

industry_input <- sliderInput(
  inputId = "industry_choice",
  label = "Please Choose a Range of Industries (1-94), all ordered by Alphabetical Order (A-Z)",
  min = head(impacted_industry$Row, 1),
  max = industries,
  value = c(1,94)
)

washington_page <- tabPanel(
  "Washington Unemployment Data",
  titlePanel("Washington Unemployment Data"),
  sidebarLayout(
    sidebarPanel(industry_input),
    mainPanel(
      h1("Plot of Initial Claims Per Industry"),
      plotlyOutput("plot")
    )
  )
)

ui <- navbarPage(
  "Final Deliverable",
  washington_page
)

shinyApp(ui = ui, server = server)

###################################################################


claims_data$Initial.Claims <- gsub(",", "", claims_data$Initial.Claims)
claims_data$Initial.Claims <- as.numeric(claims_data$Initial.Claims)
claims_plot <- ggplot(data = claims_data, aes(x = Week, y = Initial.Claims, group=1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(5000, 185000)) +
  labs(
    title = "Amount of Initial Claims of Unemployment Per Week",
    x = "Week of Quarantine",
    y = "Initial Claims"
  )
wa_claims_plot <- ggplotly(claims_plot)
print(wa_claims_plot)

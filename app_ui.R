library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
source("scripts/national_report.R")
# define inputs

covid_cases_df <- read.csv("data/COVID-19_cases/us_states_covid19_daily.csv")

# Add a column `Date` that converts the values in `date` column to date
# variables
covid19_cases <- covid_cases_df %>%
  mutate(Date = ymd(date))

# Select the categories showing in the dropdown widget
selected_categories <- covid19_cases %>%
  select(positive, positiveIncrease, hospitalizedCurrently,
         hospitalizedCumulative, inIcuCurrently, inIcuCumulative,
         onVentilatorCurrently, onVentilatorCumulative, recovered,
         death) 

# This date range selector allows user to select the range of date
date_input <- dateRangeInput(
  inputId = "date",
  label = "Select Date",
  start = "2020-01-23",
  end = "2020-05-11",
  min = "2020-01-23",
  max = "2020-05-11",
  format = "yyyy-mm-dd",
  startview = "month",
  weekstart = 0,
  language = "en",
  separator = " to "
)

# This dropdown allows user to select categories of cases
# in different circumstances
cases_input <- selectInput(
  inputId = "categories",
  label = "Interest of Categories",
  choices = colnames(selected_categories),
  selected = "positiveIncrease"
)


national_input <- selectInput("select",
                              inputId = "state_input",
                              label = "Select State",
                              choices = distinct(load_data, State)
)
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

impacted_industry <- mutate(washington_covid, Total =
                              rowSums(Filter(is.numeric, washington_covid),
                                      na.rm = TRUE))

impacted_industry <- impacted_industry %>%
  arrange(Industry) %>%
  mutate(Row = seq(1, 94))

industry_input <- sliderInput(
  inputId = "industry_choice",
  label = "Choose a Range of Industries (1-94), all ordered by Alphabetical
  Order (A-Z)",
  min = head(impacted_industry$Row, 1),
  max = nrow(impacted_industry),
  value = c(1, 94)
)

# define ui
ui <- tagList(navbarPage(
  title = "Effects of COVID-19 on Unemployment in Washington and United States",
  tabsetPanel(id = "tabs",
    tabPanel("Introduction", value = 0,
      h1("Introduction"), br(),
      h4("Info BA: Katie Wang, Shraddha KC, Joseph Caraan, Chiaki Matsuno"),
      br(),
      h3("The Purpose of This Project"), br(),
      p("Ever since COVID-19 sweeped the United States there has been a lot of
      fear and uncertainty regarding the state of national public health, as
      well as concerns for the future of the economy. Around the globe, many
      countries have taken a hard hit to their economies across all industries
      and many have been faced with unemployment. The United States is no
      stranger to this issue, as delayed responses to the initial spread of the
      virus played a large role in its rapid spread, further accelerating the
      epidemic and the resulting economic downturn."), br(),
      p("This project looks at the effects of COVID-19 on unemployment in
      Washington and the United States as a whole. We will refer to datasets
      that show the spread of the virus, as well as national and local
      unemployment claims for Washington state. This is important because the
      quarantine has had a large impact on the local and national economy, and
      looking at this information can help us be prepared for unemployment
      trends in future waves of the virus."), br(),
      h3("Data Sources"), br(),
      h4(a("COVID-19 in USA",
        href = "https://www.kaggle.com/sudalairajkumar/covid19-in-usa/?select=us_states_covid19_daily.csv"),
        "by Sudalai Raj Kumar"), br(),
      h4(a("US Unemployment Insurance Weekly Claims",
        href = "https://www.arcgis.com/home/item.html?id=b2286e8d03a74206809252a788f35b52&view=list#data"),
      "by Helen Thompson"), br(),
      h4(a("Initial Claims applications for Unemployment Insurance - WA",
        href = "https://public.tableau.com/profile/jeff.robinson#!/vizhome/InitialClaimsapplicationsforUnempIoymentInsurance-WA_ETA539-/Story1"),
      "by Washington Employment Security Department"), br()
      ),
      navbarMenu("Visualizations",
        tabPanel("COVID-19 Cases", value = 1),
        tabPanel("Unemployment Claims in the US", value = 2),
        tabPanel("Washington Unemployment Data", value = 3)
      ),
      # ALL OF OUR takeaways in a conclusion page.
      tabPanel("Conclusion", value = 4,
        h1("Conclusions"), br(),
        h3("Takeaway 1: National Confirmed Cases"), br(),
        plotlyOutput("cases_plot"), br(),
        h4("Purpose of the National Cases Confirmed Area Plot"), br(),
        p("The area plot focused on the trend throughout the period of selected
        date. The data went across from January 23 to May 11. We could see how
        cases in different circumstances changed from the first case reported
        until the most recent case in the data. In the conclusion, we only
        included the positive increase cases and currently hospitalized cases
        since these are the 2 areas that we are most interested in."), br(),
        h4("What We Learned from the Data and What We Can Use Our Data To
        Predict"), br(),
        p("As the data showcase not only the total cases confirmed, we could
        also tell how our healthcare resources, e.g. hospital, ICU, are used
        nationally. We could also track the recovered number to tell how each
        state is reacting with the pandemic. Through the area plot, we could
        tell the peak of increasing cases was in April, with 4 days of more than
        10k of positive increase cases. The rest of the month still remained a
        high number of increasing positive cases in between 5k to 10k. Due to
        the high number of positive increase in single day, the number of
        currently hospitalized became high after cases confirmed. In April,
        there were 6 days with more than 15k people hospitalized."), br(),
        p("According to the data provided, we could predict that as the positive
        cases grow, the growth of hospitalized cases will come after. To prevent
        the healthcare system overload, it is important to flatten the curve of
        the confirmed cases, so the hospitals are able to distribute the more
        resources to each patient. When the number of positive increase cases
        went down, hospitals may have more time to response and purchase more
        supplies. Another major effect of short in healthcare resources was the
        rise in death rate. As hospitals are not able to provide enough
        healthcare until the patient recover, the time for patients to recover
        by themselves become shorter thus causes unnecessary deaths. As the data
        is updated by daily basis, it could help the government to track the
        cases and check which state has been affected the most so far. The
        public sectors are then able determine which state they should fund more
        for the healthcare resources. Through the number of cases in ICU and on
        ventilators, governments are also able to tell which healthcare they
        should purchase/produce more to support the increasing number of
        hospitalized. In conclusion, the insights help the government to puzzle
        out which healthcare resources they are lack in and prepare for similar
        situation in the future."), br(),
        h3("Takeaway 2: National Unemployment Claims"), br(),
        plotlyOutput("conclusion_plot"), br(),
        h4("Purpose of the National Unemployment Claims Line Chart"),
        p("The line chart was created in order to see what the average national
        claim reported the beginning of the pandemic up until a few weeks into
        May."), br(),
        h4("What We Learned from the Data and What We Can Use Our Data To
        Predict"), br(),
        p("Initially we wanted to look at what the unemployment looked like on
        average across the US. While we didn’t brainstorm on a specific data,
        one of the data sources we looked at was provided and updated on a
        weekly basis starting form January 5, 2020. This was the initial
        pandemic date marked as the unemployment claims started to take place.
        The pandemic obviously did not start off with an outrageous number of
        unemployment rates as we believe that as COVID was not impacting
        employment as much as it has recently. Looking at the chart, it is
        apparent that there has been a set number of average claims throughout
        states until March 2020."), br(),
        p("Based on the data provided, we can assume that this is when the
        lockdown measures were taken place, causing many to lose jobs and in
        result, file unemployment cases. There is a significant jump in the
        beginning of March. Alabama for example, shows the claims spike from
        March 14th (4,752 claims), br(), exponentially increases for the next
        few dates (55,335 to 113,641 claims). This data helps us see the trends
        of specific months/weeks that the pandemic impacted heavily on people
        throughout the nation. The data is continuously updated on a weekly
        basis, leading to more opportunities to make predictions in the future.
        This can help provide the government a quick look at what states are
        affected and how badly they are affected. By analyzing the data,
        organizations can predict the amount of resources they may need to set
        up or infer domino chain reactions based on their position. The data
        serves as a possiblity of allowing individuals/groups to see what they
        can offer for those that are affected by the pandemic."), br(),
        h2("Takeaway 3: Washington Unemployment Claims"), br(),
        plotlyOutput("week_plot"), br(),
        h4("Purpose of Washington Unemployment Claims Plot"), br(),
        p("From the scatter/line chart above, the goal was to see if the state
        of Washington had been still trending upwards for initial unemployment
        claims filed per week, or to see if the state had possibly reached a
        pinnacle of unemployment claims and appeared to either level off or
        trend downwards."), br(),
        h4("What We Learned from the Data and What We Can Use Our Data To
        Predict"), br(),
        p("The intial prediction was that as quarantine started and the economy
        came to a gradual halt, the initial unemployment claims would skyrocket
        for the first few weeks. However, this was not the case as the plot
        indicates for the first 9 weeks, unemployment claims lingered around
        7000-9000 claims per week. Week 10 took somewhat of a rise, and week 11
        took a significant rise as the unemployment claims for that week
        approached around 130,000. The pinnacle seemed to be week 12 as the
        unemployment claims reached around 180,000. After that week, the trend
        was very apparently downwards and reached a low of around 80000 on week
        15. Since then there have been a few trends of both upwards and
        downwards throughout the weeks, but never a drastic change. Since week
        20 just concluded, the Tableau report was updated with the new
        unemployment claims, and this week concluded with around 48000
        unemployment claims."), br(),
        p("From the data above, we have been able to see trends throughout the
        weeks of the pandemic in the state of Washington. As the data and graph
        is continuously updated, users will be able to see if and when a more
        consistent trend will arise, and therefore see when unemployment claims
        will gradually come to a low and be compared to normal unemployment
        claims pre-pandemic OR if trends will gradually rise again meaning our
        economy would still be in a hole. The only exception to this is if there
        is a sudden and drastic change in trend due to a signifcant event. From
        week 21, many malls and businesses nationwide were supposed to open and
        therefore decrease unemployment claims. However, due to the George Floyd
        protests, this has halted and even disrupted many of these businesses
        which should prevent unemployment claims from drastically lowering.
        Overall, insights can be made and will continually be made about the
        trends of unemployment claims throughout the weeks of unemployment. This
        can be done until the pandemic dies down and life approaches normality
        in the future."), br()
      )
  )),
  # sidebar only shows up for the visualization pages
  conditionalPanel(condition = "input.tabs != 0 && input.tabs != 4",
    tagList(
    br(),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition = "input.tabs == 1", date_input, cases_input,
        helpText("This area plot depicts the trend of cases in different
        circumstances in selected range of date in all 50 states. Circumstances
        could be selected from the dropdown select box.")),
        conditionalPanel(condition = "input.tabs == 2", national_input,
        helpText("This line chart displays the trend line of the average
        unemployment claim rates within the US. All 50 states are included and
        can be viewed via selecting the state from the select box.")),
        conditionalPanel(condition = "input.tabs == 3", industry_input,
        helpText("This bar chart describes the initial unemployment claims per
        industry in week 19 of the COVID-19 pandemic. There are a total of 94
        industries classified by the state of Washington."))
      ),
      mainPanel(
        conditionalPanel(condition = "input.tabs == 1",
        titlePanel("National Cases Confirmed"),
        h3("Plot of cases through date"),
        plotlyOutput("national_cases_plot"),
        helpText("The area plot portrays the selected categories of interest
        within the selected range of date. The data range box allows users to
        select the period they wanted to focus on. The area plot will be
        adjusted accordingly. The dropdown select box allows user to select the
        categories of interest on y-axis.")),
        conditionalPanel(condition = "input.tabs == 2",
          titlePanel("National Unemployment Claims"),
          h3("Plot of average unemployment claim rates"),
          plotlyOutput("national_claims_plot"),
          helpText("The line chart displays the initial filed claims vs the
          number of initial claims made. The select state box option allows the
          user to select a specifci state which results line chart to adjust
          according to the data values reflecting the chosen state.")),
        conditionalPanel(condition = "input.tabs == 3",
          titlePanel("Washington Unemployment Data"),
          h3("Plot of Initial Claims Per Industry"),
          plotlyOutput("industry_plot"),
          helpText("The bar chart above can be adjusted to see a specific range
          of industries. The industries are sorted in alphabetical order, and
          the bar chart will change and display the specific industries in
          accordance to the range inputted into the slider")
        )
      )
    )
  )
  )
)

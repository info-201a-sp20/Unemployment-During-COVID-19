washington_covid <- read.csv("washington.gov data/crosstab_real.csv",
                             stringsAsFactors = FALSE)

claims_data <- read.csv("washington.gov data/initial_claims.csv",
                        stringsAsFactors = FALSE)

library("dplyr")
library("ggplot2")
library("plotly")


claims_data$Initial.Claims <- gsub(",", "", claims_data$Initial.Claims)
claims_data$Initial.Claims <- as.numeric(claims_data$Initial.Claims)
claims_plot <-
ggplot(data = claims_data) +
  geom_point(mapping = aes(x = Week, y = Initial.Claims)) +
  scale_y_continuous(limits = c(5000, 185000)) +
  labs(
    title = "Amount of Initial Claims of Unemployment Per Week",
    x = "Week",
    y = "Initial Claims"
  )
ggplotly(claims_plot)
str(claims_data)

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

impacted_industry <- mutate(washington_covid, industry_total =
                              rowSums(Filter(is.numeric, washington_covid),
                                      na.rm = TRUE)) %>%
  filter(industry_total == max(industry_total, na.rm = TRUE)) %>%
  pull(Industry)

impacted_industries_king <- filter(washington_covid, KING == KING) %>%
  arrange(-KING) %>%
  head(Industry, n = 5L) %>%
  pull(Industry)
paste(impacted_industries_king)

counties_sum <- rbind(washington_covid,
                      c("Sum", " ", colSums(Filter(is.numeric,
                                                   washington_covid),
                                            na.rm = TRUE)))
impacted_county <- names(counties_sum)[max.col(counties_sum,
                                               ties.method = "first")]

str(counties_sum)

library(tidyverse)
library(highcharter)
source('ga-auth.R')


mf <- met_filter('sessions', 'GREATER_THAN', 5)
mf2 <- met_filter('avgSessionDuration', 'LESS_THAN', 1)
ga_id <- 168826535

fc <- filter_clause_ga4(list(mf, mf2), operator = 'AND')

SessionsByDate <- google_analytics(
  ga_id, 
  date_range = c("2018-01-01","2018-11-01"),
  dimensions=c('date', 'deviceCategory'), 
  metrics = c("users","sessions","bounceRate",
              "avgSessionDuration")
)


View(referral_spam_duration)

hchart(SessionsByDate, 'line', hcaes(x = date, 
                                     y = sessions, 
                                     group = deviceCategory))

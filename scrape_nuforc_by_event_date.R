library(tidyverse)
library(httr)
library(rvest)
library(lubridate)

by_date_url <- "http://www.nuforc.org/webreports/ndxevent.html"

events_by_date <- GET(by_date_url) 

event_months <- by_date_url %>% read_html() %>%
  html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>% html_text()

event_month_urls <- by_date_url %>% read_html() %>%
  html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>% xml_attrs() %>%
  unlist %>% paste0("http://www.nuforc.org/webreports/", .)

event_month_table <- tibble(month = event_months, month_url = event_month_urls)

## Out to here, next bit works but clean it up and make sure you join in the deposition month

nuforc_events <- lapply(event_month_urls, function(url) {
  nuforc_for_date <- GET(url)
  events <- nuforc_for_date %>% read_html() %>% html_nodes(xpath = "/html/body/table") %>%
    html_table() %>% as.data.frame %>% tbl_df
  events$event_url <- nuforc_for_date %>% read_html() %>% html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>%
    xml_attrs() %>% unlist %>% paste0("http://www.nuforc.org/webreports/", .)
  events %>% mutate(month_url = url)
}) %>% bind_rows %>% inner_join(event_month_table, .) %>% select(-month_url)

nuforc_events$Sighting_Report <- sapply(nuforc_events$event_url, function(url) {
  x <- GET(url) %>% read_html() %>% html_nodes(xpath = "/html/body/table") %>%
    html_table()
  x[[1]][2,1]
})

final <- nuforc_events %>%
  separate(`Date...Time`, c("Date", "Time"), " ", convert = T) %>%
  separate(Date, c("Month", "Day", "Year"), convert = T) %>% select(-Month, -Year) %>%
  separate(month, c("Month", "Year"), convert = T) %>%
  mutate(Event_Date = ymd(paste(Year, Month, Day, sep = "/")),
         Event_Time = hm(Time),
         Hour = hour(Event_Time),
         Minute = minute(Event_Time)) %>%
  select(Event_Date, Year, Month, Day, Event_Time, Hour, Minute, City, State, Shape, Duration, Summary, Sighting_Report)

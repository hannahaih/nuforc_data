library(tidyverse)
library(httr)
library(rvest)

by_date_url <- "http://www.nuforc.org/webreports/ndxevent.html"

events_by_date <- GET(by_date_url) 

event_months <- by_date_url %>% read_html() %>%
  html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>% html_text()

event_month_urls <- by_date_url %>% read_html() %>%
  html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>% xml_attrs() %>%
  unlist %>% paste0("http://www.nuforc.org/webreports/", .)

event_month_table <- tibble(month = event_months, month_url = event_month_urls)

## Out to here, next bit works but clean it up and make sure you join in the deposition month


nuforc_events <- lapply(head(nuforc_event_urls), function(url) {
  nuforc_for_date <- GET(url)
  events <- nuforc_for_date %>% read_html() %>% html_nodes(xpath = "/html/body/table") %>%
    html_table() %>% as.data.frame %>% tbl_df
  events$url <- nuforc_for_date %>% read_html() %>% html_nodes(xpath = "/html/body/table/tbody/tr/td/font/a") %>%
    xml_attrs() %>% unlist %>% paste0("http://www.nuforc.org/webreports/", .)
  events
}) %>% bind_rows

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

# Too many errors for now...
# nuforc_events$Sighting_Report <- sapply(nuforc_events$event_url, function(url) {
#   x <- GET(url) %>% read_html() %>% html_nodes(xpath = "/html/body/table") %>%
#     html_table()
#   x[[1]][2,1]
# })

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

final <- nuforc_events %>%
  separate(`Date...Time`, c("Date", "Time"), " ", convert = T) %>%
  separate(Date, c("Month", "Day", "Year"), convert = T) %>% select(-Month, -Year) %>%
  separate(month, c("Month", "Year"), convert = T) %>%
  separate(Time, c("Hour", "Minute"), convert = T, remove = F) %>%
  mutate(Event_Time = as.POSIXct(paste(paste(Year, Month, Day, sep = "-"), Time), format = "%Y-%m-%d %H:%M"),
         Event_Date = ymd(paste(Year, Month, Day, sep = "-")),
         Shape = capwords(tolower(Shape)),
         Shape = ifelse(Shape == "NANA", NA, Shape)) %>%
  mutate_at(vars(Year, Month, Day, Hour, Minute), as.integer) %>%
  select(Event_Time, Event_Date, Year, Month, Day, Hour, Minute, City, State, Shape, Duration, Summary, Event_URL = event_url)
  # select(Event_Date, Year, Month, Day, Hour, Minute, City, State, Shape, Duration, Summary, Sighting_Report)

write_csv(final, "nuforc_events.csv")

# Nice day for a UFO sighting!
final %>% filter(!is.na(Month), !is.na(Day)) %>% group_by(Month, Day) %>% tally %>% arrange(desc(n))

final %>% filter(!is.na(Year), !is.na(Shape), Year > 1938) %>%
  group_by(Shape) %>% mutate(Shape_Disp = ifelse(n() > 5000, Shape, "Other")) %>%
  # group_by(Year, Shape) %>% tally %>%
  ggplot(aes(Year, fill = Shape_Disp)) + geom_area(aes(y = ..count..), stat = "bin", position = "fill") +
  theme_bw() + scale_y_continuous("Proportion of sightings", labels = scales::percent) +
  scale_fill_brewer("Shape", palette = "Dark2")


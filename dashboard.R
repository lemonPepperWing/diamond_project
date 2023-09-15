### DASHBOARD WITH SPARKLINE

# load libraries
library(tidyverse)
library(tidytuesdayR)
library(plotly)
library(formattable)
library(sparkline)

# load datasets 
tt <- tidytuesdayR::tt_load("2020-05-12")

volcano <- tt$volcano

eruptions <- tt$eruptions 

events <- tt$events %>%
  select(volcano_number, volcano_name, eruption_number, event_type)

# get info about the volcanoes in each country and region
volcano_info <- volcano %>%
  select(volcano_number, volcano_name, country, region, population_within_100_km)

# combine the volcano info with the eruption info
dat <- eruptions %>%
  filter(eruption_category == "Confirmed Eruption") %>%
  inner_join(volcano_info)  %>%
  filter(!is.na(start_year)) %>%
  mutate(
    end   = as.Date(
      paste(end_year, ifelse(end_month == 0, "01", end_month), 
            ifelse(end_day == 0, "01", end_day), sep = "-")),
    start = as.Date(
      paste(start_year, ifelse(start_month == 0, "01", start_month), 
            ifelse(start_day == 0, "01", start_day), sep = "-"))
  ) %>% 
  mutate(
    Length_of_Eruption = as.numeric(difftime(end, start, units = "days"))
  )

# add the information about the eruption events
dat <- dat %>%
  left_join(events, multiple = "all") %>%
  filter(between(start_year, 1950, 2010))

# check to see how many NAs are in the dataset.
dat %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  gather() %>% 
  print(n = Inf)

# EDA

# which countries have the most volcanoes?
dat %>%
  count(country, sort = T) %>%
  head() %>%
  ggplot(aes(x = n, y = reorder(country, n))) +
  geom_col(aes(fill = country)) +
  geom_label(aes(label = n)) +
  theme(legend.position = "none")

# how many volcano eruptions have taken place starting in 1950?
dat %>%
  count(start_year, sort = T) %>%
  ggplot(aes(x = start_year, y = n)) +
  geom_col(fill = "blue",
           alpha = 0.6,
           color = "black")

# which countries have had volcanoes most often since 1950?
dat %>%
  filter(country %in% c("Japan",
                        "Indonesia",
                        "United States",
                        "Russia",
                        "Papua New Guinea",
                        "New Zealand")) %>%
  mutate(decade = plyr::round_any(start_year, 10, floor)) %>% 
  group_by(country, decade) %>%
  summarize(n_eruptions = n()) %>%
  ggplot(aes(x = decade, 
             y = n_eruptions,
             color = country)) +
  geom_line(size = 1.1) +
  scale_x_continuous (breaks = seq(1950, 2010, 10))

# how many total eruptions have occurred for the different volcanoes?
dat %>%
  count(volcano_name, sort = T) %>%
  top_n(n = 10) %>%
  ggplot(aes(x = n, y = reorder(volcano_name, n))) +
  geom_col(aes(fill = volcano_name)) +
  geom_label(aes(label = n)) +
  theme(legend.position = "none") +
  labs(y = "")

# number of eruptions per country since 1950?
dat %>%
  distinct(volcano_name, .keep_all = T) %>%
  group_by(country) %>%
  summarize(n_eruptions = n()) %>%
  arrange(desc(n_eruptions))

# what is the vei rating for the volcanoes?
dat %>%
  filter(!is.na(vei)) %>%
  count(vei) %>%
  ggplot(aes(x = vei, y = n)) +
  geom_col(aes(fill = vei))

## Build a Dashboard

# volcanoes per decade data frame
decades <- dat %>%
  mutate(decade = plyr::round_any(start_year, 10, floor)) %>% 
  group_by(country, 
           decade) %>%
  summarize(n_volcano = length(unique(volcano_name))) %>%
  arrange(decade) %>%
  pivot_wider(data = .,
              names_from = decade,
              values_from = n_volcano,
              values_fill = 0)

# the number of different volcanoes that erupted per country
volcanoes <- dat %>%
  group_by(country) %>% 
  distinct(., volcano_name) %>%
  summarize(eruption_number = n())

# percent volcano explosivity index (vei) rating of 2 
vei <- dat %>%
  group_by(country, vei)%>%
  distinct(., volcano_name) %>%
  group_by(country) %>%
  count(vei) %>%
  mutate(pct_vei = n / sum(n)) %>%
  filter(vei == 2) %>%
  select(country, pct_vei) 

## join tables
volcano_final <- decades %>%
  left_join(volcanoes, multiple = "all") %>%
  left_join(vei, multiple = "all") %>%
  mutate(pct_vei = ifelse(is.na(pct_vei), 0, pct_vei),
         pct_vei = round(pct_vei, 2)) 

volcano_final %>% glimpse()

#Build Dashboard

## create sparkline line 
volcano_final$trend_line <- 
  apply(volcano_final[, 2:8], 
        MARGIN = 1, 
        FUN = function(x){
          as.character(
            htmltools::as.tags(
              sparkline(as.numeric(x), type = "line")
            ))
        })

# create a percent total function
pct_total <- function(x){
  p = x / sum(x)
  return(p)
}

# create plot as an html widget
output_dashboard <- volcano_final %>%
  rename(eruptions = eruption_number,
         pct_vei_R2 = pct_vei) %>% 
  arrange(desc(eruptions)) %>%
  formattable(
    align = c("l", rep("c", NCOL(volcano_final) - 4), rep("r", 2)),  
    list(
      eruptions = color_bar("salmon", fun = pct_total),
      pct_vei_R2 = percent)
  ) %>% 
  as.htmlwidget()

# add sparkline dependencies
output_dashboard$dependencies <- c(
  output_dashboard$dependencies, 
  htmlwidgets::getDependency("sparkline", "sparkline"))

# run dashboard
output_dashboard


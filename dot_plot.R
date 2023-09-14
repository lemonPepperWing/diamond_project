### DOT PLOT 

#load libraries
library(tidyverse)
library(rvest)

# read in the data from baseball reference with a bit of cleaning.
url <- read_html("https://www.baseball-reference.com/players/gl.fcgi?id=willite01&t=b&year=1941") %>% 
  html_table(header = TRUE) %>% 
  pluck(5) %>%
  janitor::clean_names() %>%
  filter(!rk %in% c("May" , "Jun","Jul", "Aug", "Sept" , "")) %>%
  select(-rk) %>%
  mutate(games = 1:143)

# check out the dataset.
glimpse(url)

# prep the dataset for better ready use
ted_data <- url %>%
  mutate(batting_avg = as.numeric(as.character(ba)),
         team = as_factor(tm),
         hits = as.character(h),
         year = paste(date, "1941", sep = " ")) %>%
  separate(col = year, into = c("month", "day", "year"), sep = " " ) %>%
  mutate(year = case_when(year == "(1)" ~ "1941",
                          year == "(2)" ~ "1941",
                          TRUE ~ year)) %>% 
  mutate(month = as.factor(month)) %>%
  select(-date, -tm)

# check to see how many NAs are in the dataset.
ted_data %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>%
  gather() %>%
  print(n = Inf)

#order the month factor levels correctly in the month column
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ted_data$month <- factor(ted_data$month, levels = month_levels)

# create a dot plot for Ted Williams hits per month
ted_data %>% 
  ggplot(aes(x=month, group=hits, fill=hits)) +
  geom_dotplot(aes(y=..count..),
               stackdir = "up", stackratio = 1, stackgroups = TRUE,  
               binpositions="all", binwidth= 0.4,
               dotsize = 0.4) + 
  scale_color_manual(values =  c("cyan", "magenta3","dodgerblue",
                                 "darkorange", "red")) +
  theme_bw() + 
  theme(plot.title = element_text(size = 18), 
        text = element_text(size=16),    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.background = element_rect(fill = "lightyellow",color = "lightyellow")) + 
  xlab("") +    
  ylab("") +
  ggtitle("Ted Williams hits per month")


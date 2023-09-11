### LINE CHART 

#load libraries
library(tidyverse)
library(gapminder)

# check out the datasets
glimpse(gapminder)

# filter Japan's numeric variables for each year and convert to a long data frame
japan_longer <- gapminder %>% 
  group_by(year) %>%
  arrange(desc(lifeExp)) %>%
  filter(country == "Japan") %>%
  pivot_longer(4:6, names_to = "variable", values_to = "values")

# convert original data to long data frame
gapminder_longer <- gapminder %>%
  pivot_longer(4:6, names_to = "variable", values_to = "values")

# create a line chart  highlighting Japan's stats data
japan_plot <- gapminder_longer %>% 
  ggplot(aes(x = year,
             y = values)) +
  geom_line(aes(
    color = country == "Japan",
    group = country,
    alpha = ifelse(country == "Japan", 1, 0.8)
  )) + 
  theme_bw() +
  scale_color_manual(values = c("lightgrey", "red")) +
  labs(x = "",
       y = "",
       title = "Data on GDP per capita, life expectancy and population by country.",
       subtitle = "Hightlight of Japan data") +
  scale_y_continuous(labels = scales::comma) + 
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y"
  ) +
  theme(panel.background = element_rect(fill = "#333333", color = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_line(color = "#333333"),
        panel.border = element_rect(color = "white", fill = NA),
        legend.position = "none",
        axis.text.x = element_text(color = "white", 
                                   face = "bold", 
                                   size = 12),
        axis.text.y = element_text(color = "white",
                                   face = "bold", 
                                   size = 12),
        axis.title.x = element_text(color = "white",
                                    face = "bold", 
                                    size = 13,
                                    vjust = 2),
        plot.title = element_text(color = "white",
                                  size = 20),
        plot.subtitle = element_text(color = "white", 
                                     size = 14),
        plot.caption = element_text(color = "white",
                                    face = "bold"),
        panel.grid.major.y = element_line(color = "#e6e6e6"),
        panel.grid.major.x = element_blank())

japan_plot

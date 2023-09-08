### LOLLIPOP PLOTS 

#load libraries
library(tidyverse)
library(patchwork)

# check out the dataset.
glimpse(mpg)

# remove all duplicates from the tibble
distinct(mpg, .keep_all = TRUE)

# do some data prep to create meaningful statistical variables
city_advantage <- mpg %>%
  mutate(manufacturer = as_factor(manufacturer),
         city_avg = mean(cty)) %>%
  mutate(city_margin = cty - city_avg) %>%
  relocate(c(cty, city_avg, city_margin), .after = cyl) %>%
  group_by(manufacturer) %>% 
  summarize(city_edge = mean(city_margin)) %>% 
  arrange(desc(city_edge)) %>%
  mutate(
    manufacturer = fct_reorder(manufacturer, city_edge)
  )

# do some data prep to create meaningful statistical variables
model_advantage <- mpg %>%
  mutate(manufacturer = as_factor(manufacturer),
         model = as_factor(model),
         city_avg = mean(cty)) %>%
  mutate(city_margin = cty - city_avg) %>%
  group_by(manufacturer, model) %>% 
  summarize(city_edge = mean(city_margin)) %>% 
  arrange(desc(city_edge)) %>% 
  filter(manufacturer %in% c("honda", "volkswagen", "dodge", "lincoln")) %>%
  ungroup() %>%
  mutate(
    model = fct_reorder(model, city_edge)
  ) 

# create a lollipop plot for car manufacturer MPG edge in city driving
city_plot <- city_advantage %>% 
  ggplot(aes(x = manufacturer, y = city_edge)) +
  geom_hline(aes(yintercept = 0), size = 1.2) +
  geom_segment(aes(x = manufacturer, xend = manufacturer, y = 0, yend = city_edge)) +
  geom_point(size = 12, color = "dodgerblue") +
  geom_text(aes(label = round(city_edge, 1)), color = "black") +
  labs(title = "Car manufacturer MPG edge in city driving",
       subtitle = "Honda has the biggest advantage",
       x = "Manufactuer",
       y = "City MPG") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        panel.grid.major = element_line(color = "linen"),
        panel.background = element_rect(fill = "linen",color = "linen"),
        
  )

city_plot

# create a lollipop plot for car model MPG edge in city driving
model_plot <- model_advantage %>% 
  ggplot(aes(x = model, y = city_edge)) +
  geom_hline(aes(yintercept = 0), size = 1.2) +
  geom_segment(aes(x = model, xend = model, y = 0, yend = city_edge)) +
  geom_point(size = 12, color = "cyan") +
  geom_text(aes(label = round(city_edge, 1)), color = "black") +
  labs(title = "Car model MPG edge in city driving",
       subtitle = "Models: honda, volkswagen, dodge, lincoln",
       x = "Models",
       y = "City MPG") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        panel.grid.major = element_line(color = "linen"),
        panel.background = element_rect(fill = "linen",color = "linen"),
        
  )

model_plot

# patchwork the lollipop plots all together
city_plot/model_plot  & 
  theme(
    plot.background = element_rect(fill = "light grey", colour = "light grey")
  )


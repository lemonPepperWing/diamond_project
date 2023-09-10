### GGPLOTLY

# load libraries
library(tidyverse)
library(plotly)

# check out the dataset.
glimpse(mpg)

# remove all duplicates from the tibble
distinct(mpg, .keep_all = TRUE)

# inspect the numerical variables for relevant correlation
mpg_num <- keep(mpg, is.numeric)
mpg_num

cor(mpg_num, use = "complete.obs")
pairs(mpg_num)  


# create a boxplot of the highway per gallon ~ the class of the vehicle
ggplot(mpg) +
  geom_boxplot(aes(x = reorder(class, hwy, FUN = mean),
                   y = hwy,
                   color = class)) +
  coord_flip() +
  labs(x = "class")

# create a boxplot of the highway per gallon ~ the drive of the vehicle
ggplot(mpg) +
  geom_boxplot(aes(x = reorder(drv, hwy, FUN = mean),
                   y = hwy,
                   color = drv)) +
  coord_flip() +
  labs(x = "drive")

# create a ggplotly plot of Highway MPG ~ Engine Displacement (L)
mpg_plot <- mpg %>%
  group_by(manufacturer, model) %>%
  summarize(
    displ = round(mean(displ), 1),
    hwy = round(mean(hwy), 1),
    class = first(class),
    drv = first(drv)
  ) %>% 
  ungroup() %>% 
  mutate(
    vehicle = paste( model,"<br>",
                     "mean_displ:", displ,"<br>",
                     "mean_hwy:", hwy,"<br>",
                     "class:", class,"<br>",
                     "drv:", drv)
  ) %>% 
  ggplot(aes(x = displ, 
             y = hwy,
             size = hwy,
             color = class,
             label = vehicle)) +
  geom_point() +
  labs(title = "Car Manufacturer and Model information",
       x = "Engine Displacement (L)", 
       y = "Highway MPG"
  ) +
  theme_light() +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16),
        legend.position = "none")

ggplotly(mpg_plot, tooltip = "label")

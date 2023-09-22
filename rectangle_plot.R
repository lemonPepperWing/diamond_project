### RECTANGLES

# load libraries
library(tidyverse)
library(patchwork)

# glimpse the dataset
glimpse(iris)

# do some data prep for better ease of use.
iris <- as_tibble(iris) %>%
  janitor::clean_names()

# check to see how many NAs are in the dataset
iris %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  gather() 

# create a rectangle plot for The measurements in centimeters of the Iris species

sepal <- iris %>%
  ggplot(aes(x = sepal_width, y = sepal_length)) +
  geom_rect(aes(xmin = 2.0, ymin = 4.3, xmax = sepal_width, ymax = sepal_length), 
            alpha = 0.1, fill = "#8d84dd", colour = "#3627fc", size = 1.2) +
  scale_x_continuous(limits = c(2.0, 5.0)) +
  scale_y_continuous(limits = c(4.3, 8)) +
  facet_wrap(~species, nrow = 1) +
  labs(x = "Sepal Width (cm)",
       y = "Sepal Height (cm)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "lightyellow", fill = "lightyellow"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24,
                                  face = "bold",
                                  ),
        plot.margin = unit(c(1,1,1,1), "cm"))  

# create a rectangle plot for the measurements in centimeters of the Iris species
petal <- iris %>%
  ggplot(aes(x = petal_width, y = petal_length)) +
  geom_rect(aes(xmin = 0, ymin = 1.0, xmax = petal_width, ymax = petal_length), 
            alpha = 0.1, fill = "#8d84dd", colour = "#3627fc", size = 1.2) +
  scale_x_continuous(limits = c(0, 3.0)) +
  scale_y_continuous(limits = c(1.0, 7.0)) +
  facet_wrap(~species, nrow = 1) +
  labs(x = "Petal Width (cm)",
       y = "Petal Height (cm)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = "lightyellow", fill = "lightyellow"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5,
                                  size = 24,
                                  face = "bold",
        ),
        plot.margin = unit(c(1,1,1,1), "cm"))  

# create a patchwork for all of the plots
(sepal / petal) + 
  plot_annotation(
    title = "The measurements(cm) of the Iris species",
    theme =  theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.background = element_blank(),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
    )
  )




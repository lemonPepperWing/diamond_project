### MAP - STATEBINS

# load libraries
library(tidyverse)
library(statebins) 
library(patchwork)

# glimpse the dataset
glimpse(USArrests)

# do some data prep for better ease of use.
USArrests <- rownames_to_column(USArrests) %>% 
  rename(State = `rowname`) %>%
  janitor::clean_names() %>%
  as_tibble()

# check to see how many NAs are in the dataset
USArrests %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  gather() %>% 
  print(n = Inf)

# pivot dataset longer to prep for visualization
USArrest <- USArrests %>%
  pivot_longer(cols = 2:5, names_to = "crime", values_to = "value")

# create a map for Murder arrests per 100,000 by State in 1973
murder <- USArrest %>%
  filter(crime == "murder") %>% 
  ggplot(aes(state = state, fill = value)) + 
  geom_statebins(radius = grid::unit(5, "pt"), 
                 na.rm = FALSE, 
                 border_size = 2, 
                 size = 1, 
                 border_col = "midnightblue") +
  scale_fill_gradient(
    high = "red1", 
    low = "springgreen1" 
  ) +
  labs(title = "Murder arrests per 100,000 by State in 1973") + 
  theme(
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA)
  )

# create a map for Assault arrests per 100,000 by State in 1973
assault <- USArrest %>%
  filter(crime == "assault") %>% 
  ggplot(aes(state = state, fill = value)) + 
  geom_statebins(radius = grid::unit(5, "pt"), 
                 na.rm = FALSE, 
                 border_size = 2, 
                 size = 1, 
                 border_col = "midnightblue") +
  scale_fill_gradient(
    high = "red1", 
    low = "springgreen1" 
  ) +
  labs(title = "Assault arrests per 100,000 by State in 1973") + 
  theme(
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA)
  )

# create a map for Urban population by State in 1973
urban_pop <- USArrest %>%
  filter(crime == "urban_pop") %>% 
  ggplot(aes(state = state, fill = value)) + 
  geom_statebins(radius = grid::unit(5, "pt"), 
                 na.rm = FALSE, 
                 border_size = 2, 
                 size = 1, 
                 border_col = "midnightblue") +
  scale_fill_gradient(
    high = "red1", 
    low = "springgreen1" 
  ) +
  labs(title = "Urban population by State in 1973") + 
  theme(
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA)
  )

# create a map for Rape arrests per 100,000 by State in 1973
rape <- USArrest %>%
  filter(crime == "rape") %>% 
  ggplot(aes(state = state, fill = value)) + 
  geom_statebins(radius = grid::unit(5, "pt"), 
                 na.rm = FALSE, 
                 border_size = 2, 
                 size = 1, 
                 border_col = "midnightblue") +
  scale_fill_gradient(
    high = "red1", 
    low = "springgreen1" 
  ) +
  labs(title = "Rape arrests per 100,000 by State in 1973") + 
  theme(
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA)
  )

# create a patchwork for all of the plots
((urban_pop / assault) | (murder / rape)) + 
  plot_annotation(
    title = "Violent Crime Rates by US State in 1973",
    theme =  theme(
      plot.title = element_text(size = 20),
      plot.background = element_rect(color = "grey", fill = "grey"),
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
    )
  )


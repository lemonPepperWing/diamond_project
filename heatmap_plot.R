### HEATMAPS

# load libraries
library(tidyverse)
library(palmerpenguins)

# check out the datasets.
glimpse(penguins_raw)
glimpse(penguins)

# do some data prep in order to join both datasets.
penguins_raw <- penguins_raw %>%
  janitor::clean_names() %>% 
  mutate(sex = str_to_lower(sex),
         sex = as_factor(sex),
         island = as_factor(island),
         flipper_length_mm = as.integer(flipper_length_mm),
         body_mass_g = as.integer(body_mass_g))


penguins_raw <- penguins_raw %>%
  mutate(
    species = case_when(
      species %in% "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species %in% "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
      species %in% "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
      TRUE ~ species)) %>%
  mutate(species = as_factor(species))

# left join both penguin datasets together and select the variable to use for analysis.
penguins_cleaned <- 
  left_join(penguins, penguins_raw, multiple = "first") %>%
  select(individual_id, species, starts_with("bill_"), flipper_length_mm,
         body_mass_g) %>%
  mutate(individual_id = as_factor(individual_id)) 

# create z-score function
z_score <- function(x){
  z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(z)
}

# convert numeric data to  a standardize score (z-score)
penguins_cleaned <- penguins_cleaned %>%
  group_by(species) %>%
  mutate_at(vars(bill_length_mm:body_mass_g), .funs = z_score) %>%
  ungroup()

# convert to long data frame
penguins_long <- penguins_cleaned %>%
  pivot_longer(cols = 3:6, names_to = "variable", values_to = "value")

# create a function to select a subset of penguins n
subset_id <- function(individual_id, n, seed ){
  if(!missing(seed)){
    set.seed(seed)
  }
  individual_id %in% sample(unique(individual_id), size = n, replace = F)
}

# create a heat map for penguin measurements for each species
penguins_long %>%
  group_by(species) %>%
  mutate(
    keep_penguin = subset_id(individual_id, 10, seed = 84)
  ) %>%
  ungroup() %>%
  filter(keep_penguin == TRUE) %>% 
  ggplot(aes(x = variable, y = individual_id)) +
  geom_tile(aes(fill = value), color = "white") +
  geom_text(aes(label = round(value,1)),
            check_overlap = T) +
  scale_fill_gradient( low = "blue", high = "red") + 
  facet_wrap(~species, scale = "free_y") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(face = "bold", color = "white"),
    legend.position = "bottom"
  ) +
  labs(title = "Palmers Penguin Heat Map",
       subtitle = "Penguin measurements for each species",
       x = "",
       y = "Penguin Id")

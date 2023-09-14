### DUMBBELL PLOT

library(tidyverse)
library(ggalt)
library(palmerpenguins)

# check out the dataset.
glimpse(penguins)

# check to see how many NAs are in the dataset.
penguins %>% 
  summarize(across(everything(), ~ sum(is.na(.)))) %>% 
  gather()

# do some data prep to create more statistical variables
penguins_cleaned <- penguins %>%
  na.omit() %>%
  select(species,
         bill_length_mm,
         flipper_length_mm,
         body_mass_g) %>%
  mutate(body_mass_kg = body_mass_g / 1000,
         billLength_per_mass = bill_length_mm / body_mass_kg,
         flipperLength_per_mass = flipper_length_mm /body_mass_kg) %>%
  mutate(billLength_per_mass = round(billLength_per_mass, 1),
         flipperLength_per_mass = round(flipperLength_per_mass, 1)) %>%
  group_by(species) %>%
  summarise(
    species = species[which.max(body_mass_kg)],
    across(where(is.numeric), ~ mean(.x))
  )

penguins_cleaned %>% glimpse()

# create a dumbbell plot for Penguins bill length & flipper length ~ body mass(kg)

penguins_cleaned %>%
  ggplot(
    aes(
      x = billLength_per_mass,
      xend = flipperLength_per_mass,
      y = reorder(species, desc(flipperLength_per_mass))
    )
  ) +
  geom_dumbbell(
    color = "black",
    size = 1.2,
    size_x = 10,
    size_xend = 10,
    colour_x =  "cyan",
    colour_xend = "gold"
  ) +
  labs(
    title = "Penguins bill length ~ flipper length by body mass(kg)",
    subtitle = "Cyan = bill length, Gold = flipper length",
    x = "",
    y = "") +
  theme(
    axis.text = element_text(size = 12,face = "bold",color = "black"),
    plot.title = element_text(size = 20,color = "black"),
    plot.subtitle = element_text(size = 17,color = "black"),
    panel.background = element_rect(fill = "lightgrey"),
    plot.background = element_rect(fill = "dodgerblue"),
    panel.grid.major.x = element_line(color = "lightyellow")
    
  )
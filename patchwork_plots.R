### PATCHWORK & INTERACTIVE GRAPHICS

# load libraries
library(tidyverse)
library(plotly)
library(patchwork)
library(palmerpenguins)

# glimpse the two datasets
glimpse(penguins)
glimpse(penguins_raw)

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
  left_join(penguins, penguins_raw, multiple = "first")

# create a scatter plot for bill length ~ flipper length.
penguins_length <- penguins_cleaned %>%
  group_by(species, sex) %>%
  relocate(c(sex,flipper_length_mm), .after = island) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, label = sex)) +
  geom_point(aes(color = species)) +
  theme_bw() +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 14)) +
  labs(title = "Palmers Penguins",
       subtitle = "Bill length ~ Flipper length")

penguins_length

# make the plot interactive
peng_len_interactive <- highlight(ggplotly(penguins_length), on = "plotly_selected")
peng_len_interactive

# create a box plot for body mass per species and sex
penguins_mass <- penguins_cleaned %>%
  filter(!is.na(sex)) %>%
  relocate(c(sex, body_mass_g), .after = island) %>%
  ggplot(aes(x = reorder(species, body_mass_g), y = body_mass_g)) +
  geom_boxplot(aes(fill = sex), na.rm = T) +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size  = 11, face = "bold")) +
  labs(x = "", 
       y = "Body Mass", 
       title = "Palmers Penguins",
       subtitle = "Body Mass per Species and Sex")

penguins_mass

# create a scatter plot comparasion of body parts to body mass by sex.
penguins_body <- penguins_cleaned %>%
  filter(species == "Gentoo") %>%
  mutate(bill_body = bill_length_mm/body_mass_g, 
         flipper_body = flipper_length_mm/body_mass_g ,
         bill_z = scale(bill_body),
         flipper_z = scale(flipper_body)) %>% 
  ggplot(aes(x = bill_z, y = flipper_z, label = sex)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 1.1) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 1.1) +
  geom_point(size = 3, color = "blue", alpha = 0.6) +
  theme(axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size  = 11, face = "bold")) +
  labs(x = "bill_len/mass_g",
       y = "flipper_len/mass_g",
       title = "Palmers Penguins",
       subtitle = "Comparasion of Body Parts to Body Mass by Sex")

penguins_body

# make the plot interactive
peng_body_interactive <- highlight(ggplotly(penguins_body), "plotly_selected")
peng_body_interactive

# patchwork the static plots together all together
penguins_length | (penguins_mass / penguins_body)

# combine interactive plots in one figure
subplot(peng_len_interactive, penguins_mass, penguins_body, nrows = 2)

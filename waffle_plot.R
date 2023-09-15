### WAFFLE PLOT

# load libraries
library(tidyverse)
library(ggpubr)
library(janitor)
library(ggwaffle)
library(nwslR)

# join nwsl tables together
adv_stats <- adv_team_stats %>% 
  filter(season == "2019") %>% 
  select(team_id, season, game_id) %>%
  left_join(adv_player_stats, by = c("team_id", "game_id"))

# calculate the % of shots for each player on their team  
player_shots <- adv_stats %>%
  select(team_id , season, first_name, last_name, total_shots, game_id) %>% 
  mutate(player = paste(first_name, last_name, sep = " ")) %>%
  relocate(player, .after = season) %>% 
  unnest(season) %>% 
  group_by(team_id) %>%
  mutate(team_touches = sum(total_shots)) %>%
  select(-4:-5) %>%
  group_by(player, team_id) %>%
  mutate(shots_distrib = total_shots / team_touches) %>% 
  ungroup()

# what is the distribution of shots across the NWSL?
shots_quant <- ggtexttable(data.frame(
  round(quantile(player_shots$shots_distrib), 3) %>% t()),
  rows = NULL)

# create a density plot for the distribution of shots across National Women Soccer
player_shots %>%
  ggplot(aes(x = shots_distrib)) +
  geom_density(fill = "light blue", alpha = 0.4,
               size = 1.2) +
  geom_vline(aes(xintercept = median(shots_distrib)),
             color = "red",
             size = 1.3,
             linetype = "dotted") +
  #annotation_custom(ggplotGrob(shots_quant),
  #                  xmin = 0.21,
  #                  xmax = 0.27,
  #                  ymin = 0.4) +
  labs(x = "Shots Distribution",
       y = "Density",
       title = "Distribution of Shots Across National Women Soccer League",
       subtitle = "2019 Season")

# select a team for the waffle plot
team_shots <- player_shots %>%
  filter(team_id == "HOU") %>%
  select(player, total_shots) %>% 
  arrange(player) %>%
  uncount(weights = total_shots) %>% 
  mutate(player = fct_lump(player, 5))

# prepare the waffle plot
waffle_df <- team_shots %>% 
  mutate(player = as.numeric(player)
  ) %>% 
  waffle_iron(
    rows = 25,
    aes_d(group = player)
  ) %>% 
  mutate(
    group = factor(levels(team_shots$player)[group], levels = levels(team_shots$player))
  )

# create a waffle plot for the players with the most shots taken for the HOU team in the NWSL
ggplot(waffle_df, aes(x = x, y = y, fill = group)) +
  geom_waffle() +
  theme_waffle() 

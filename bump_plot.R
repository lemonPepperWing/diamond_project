### BUMP PLOT

#load libraries
library(tidyverse)

# check out the dataset.
glimpse(gss_cat)

# check to see how many NAs are in the dataset.
gss_cat %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>%
  gather()

# do some data prep to the religion variable for ease of use
gss_cat <- gss_cat %>%
  filter(!is.na(tvhours)) %>%
  mutate(#relig = as_factor(relig),
    relig = fct_lump(relig, n = 6)) %>%
  mutate(relig = if_else(relig == "None", "Atheist", relig))

# summarize and rank the data for the tv hours
tv_rank <- gss_cat %>%
  group_by(year, relig) %>%
  summarize(avg_tv = mean(tvhours, na.rm = T)) %>%
  group_by(year) %>%
  mutate(rank_tv = rank(avg_tv, ties.method = "random")) %>%
  arrange(year, rank_tv)

# create a bump plot to show tv consumption by religion as rank ~ year  
tv_rank %>%
  ggplot(aes(
    x = year,
    y = rank_tv,
    color = relig,
    group = relig
  )) +
  geom_line(size = 1.1) +
  geom_point(
    shape = 19, 
    size = 3.5
  ) +
  scale_y_reverse(
    lim=c(6,1),
    breaks = seq(from = 1, to = 6, by = 1)
  ) +
  scale_x_discrete(limits = unique(tv_rank$year)) +
  scale_color_manual(values =  c("dodgerblue3", "darkorange2", "cyan", 
                                 "magenta3", "red", "gold")) +
  labs(x = "",
       y = "",
       title = "Ranked hours of tv/day watched by religion") +
  theme(plot.title = element_text(size = 20,
                                  color = "white"),
        plot.subtitle = element_text(size = 17, 
                                     color = "white"),
        plot.caption = element_text(face = "bold", 
                                    size = 12,
                                    color = "white"),
        axis.text = element_text(size = 12, 
                                 face = "bold",
                                 color = "white"),
        legend.text = element_text(face = "bold",
                                   color = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black", color="black"),
        panel.grid = element_line(color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        plot.margin = margin(5, 5, 5, 5)) +
  guides(color = guide_legend(ncol= 6,
                              nrow = 1,
                              byrow = TRUE,
                              title = NULL))


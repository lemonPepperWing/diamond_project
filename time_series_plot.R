#TIME SERIES PLOTS

library(tidyverse)
library(gapminder)
library(modelr)
library(broom)

# check out the dataset.
glimpse(gapminder)

# check to see how many NAs are in the dataset.
gapminder %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  gather()

# check what type of correlation is in the numeric data
gapminder_num <- keep(gapminder, is.numeric)
gapminder_num

cor(gapminder_num, use = "complete.obs")

# plot the data of year ~ life expectancy
ggplot(gapminder,  aes(year, lifeExp, group = country)) +
  geom_line(alpha = 2 / 3) 

# nest dataframe for individual country statistics
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

by_country$data[[1]] # check the first country tibble

# create a function for fitting a linear model
country_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

# apply linear model and residuals to the dataframe.
by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals))

# unnest the dataframe with residual data
resids <- unnest(by_country, resids) %>%
  select(-data:-model) 

# create a line plot of year ~ residuals data
resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

# create a line plot of year ~ residuals data facet wrap by continent
resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~ continent)

# create a glance column to see the summary statistics
glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  select(-data:-resids) %>%
  unnest(glance)

glance %>%
  arrange(r.squared) 

# create a jitter plot of the continent ~ r.squared
glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

# filter the data that doesn't fit the trend well.
bad_fit <- filter(glance, r.squared < 0.25)

# plot the bad fit data for year ~ life expectancy
gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line(size = 1.2) +
  labs(
    x = "Life expectancy",
    y = "Year",
    title = "Countries with the lowest life expectancy"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 15),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(ncol= 6,
                              nrow = 1,
                              byrow = TRUE,
                              title = NULL))


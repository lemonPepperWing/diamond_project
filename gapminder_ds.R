###Gapminder data on life expectancy, GDP per capita, and population by country.

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(gapminder)
  library(modelr)
  library(broom)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?gapminder

#glimpse to determine the type of variables and data type in the dataset.
glimpse(gapminder)
head(gapminder)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which predictor variables contribute the most to 
#the response(lifeExp) variable.


## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.
#the source of this data come from the website: www.gapminder.org/data/

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(gapminder)

#check the summary for abnormal data.
summary(gapminder)

#check for duplicate data and remove.
#there are no duplicate data.
gapminder %>% n_distinct()

#check the factor data type for string errors.
#determine what (Other) is for the country variable.
gapminder %>%
  group_by(country) %>%
  count() %>%
  View()

gapminder %>%
  group_by(continent) %>%
  count()

##ANALYSE

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset.
numeric_data <- keep(gapminder, is.numeric)

#observe the correlation between numeric data.
#note that the life expectancy has a correlation with the year.
#note that the life expectancy has a correlation with the gdpPercap.
cor(numeric_data, use = "complete.obs")

#plot the pairs of numeric data.
pairs(numeric_data)

#lifeExp and gdpPercap have a positive correlation.
#determine how lifeExp will change over time(year) for each country? 
cor(numeric_data, use = "complete.obs")

#plot the pairs of numeric data.
pairs(numeric_data)

#plot year versus lifeExp.
#note that overall lifeExp has gone up throughout the years, 
#but there are some that have not follow this pattern.
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

#plot gdpPercap versus lifeExp.
#note that overall gdpPercap has gone up relative to increased lifeExp.
#note we'll work with year versus lifeExp correlation gonna forward.
gapminder %>%
  ggplot(aes(year, gdpPercap, group = country)) +
  geom_line(alpha = 1/3)

#nest dataframe for easier extraction of individual countries data.
(by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest())

#extract the first country to observe the data.
by_country$data[[1]]

#create a function to fit linear regression model.
mod_country <- function(df){
  lm(lifeExp ~ year, data = df)
}

#apply model fitting function to the dataframe.
(by_country <- by_country %>%
  mutate(model = map(data, mod_country)))

#filter the first country to observe the data.
by_country %>%
  filter(country == "Afghanistan")

#apply the residuals to the dataframe.
(by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals)))

#unnest the dataframe with residuals
(resids <- unnest(by_country, resids))

#plot the residuals versus the year.
#note that some  countries that don't fit the smoothing pattern(gam) 
#and have extremely high residuals.
resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1/3) +
  geom_smooth(se = FALSE)

#group the continents and plot the with facets.
#note that the African model has some poor fits with large residuals 
#followed by the Asian model.
resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~ continent)

#apply the summary statistics for the model to the dataframe.
(glance <- by_country %>%
  mutate(glance = map(model, glance)) %>%
  unnest(glance)) %>%
  select(-data:-resids) %>%
  arrange(r.squared)

##SHARE

#the story that was told within this dataset.

#plot the r.squared versus the continents.
#Africa has the worst models.
glance %>%
  ggplot(aes(continent, r.squared, color=continent)) +
  geom_jitter(width = 0.5) +
  labs(
    title = paste("Africa has the worst trend when it comes to life expectancy"),
    subtitle = paste("Continents determined by their r.squared value"),
    caption = paste("Data from www.gapminder.org/data/")) +
  theme(legend.position = "none")

#filter and plot countries with the lowest r.squared values.
#the lowest r.squared reveals that HIV/AIDs and the Rwandan genocide 
#contributed to the lowest birth rate among rising  yearly birthrates.
bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line() +
  labs(
    title = paste("HIV/AIDs & Rwandan genocide contributed to lowest birth rate"),
    subtitle = paste("Countries with the lowest r.squared values"),
    caption = paste("Data from www.gapminder.org/data/")) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(nrow = 1, override.aes = list(size = 2))
  )

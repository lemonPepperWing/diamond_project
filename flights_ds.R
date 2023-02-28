###On-time data for all flights that departed NYC (i.e. JFK, LGA or EWR) in 2013.

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(nycflights13)
  library(lubridate)
  library(modelr)
  
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?flights

#glimpse to determine the type of variables and data type in the dataset.
glimpse(flights)
head(flights)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll use the time and date variable to uncover hidden information.

##PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.
#it also comes from the RITA, Bureau of transportation statistics, 
#https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(flights)

#look at the help content of each variable in the dataset.
?flights

#check the summary for abnormal data.
summary(flights)

#check for duplicate data and remove.
#there is no duplicate data.
flights %>% n_distinct()

#inspect dataset for any NA values.
#large amount of data with NA values, span across multiple columns. 
#will remove if necessary.


#create function to format the individual date-time columns(integer) 
#and make it one column with the correct format(dttm).
  
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(arr_time), !is.na(dep_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"), year:day)

#glimpse the new format dataset.
glimpse(flights_dt)

#check the dimensions.
#8713 data observations were removed.
dim(flights_dt)

#check the character data type for string errors.
#no errors found.
flights_dt %>% count(origin)
flights_dt %>% count(dest) %>% View()


##ANALYSE

#analyse dataset to find relevant data. 

#count the number of flights per day.
(daily <- flights_dt %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarise(n = n()))

#plot the number of flights per day.
#note certain days out of the year have less amount of flights.
ggplot(daily, aes(date, n)) +
  geom_line() 

#plot the number of flights by the day of the week. 
#note the weekend have less flights than during the week. 
(daily <- daily %>%
  mutate(wday = wday(date, label = TRUE)))

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  labs(title = "weekend have less flights than during the week 
       due to business travel")

#fit linear regression model. 
#check the summary for statistical significance.
#note that there is statistical evidence(p-value<0.05) to reject H0.
mod_wday <- lm(n ~ wday, data = daily)
summary(mod_wday)

#create data grid to overlay on the plot.
#add the predictions. 
(grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod_wday, "n"))

#plot the relationship between day of the week(wday) and count with 
#the prediction point for the response variable(count).
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "blue", size = 4)

#add the residual variable for price to the dataset.
(daily <- daily %>%
  add_residuals(mod_wday))

#plot the date versus residual.
#note that some days have less flights than expected. 
#the model could not capture some of the residuals.
#note the longer term trend(yearly) is more smooth and with normal random residuals. 
daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color = "grey50") +
  geom_smooth(se = FALSE, span = 0.20)

#plot the date versus the residual separated by days of the week.
#note that the model does not predict flights on Saturday that well.
daily %>%
  ggplot(aes(date, resid, color=wday)) +
  geom_ref_line(h = 0) +
  geom_line()

#the interquartile range(IQR) for Saturday is the largest by far.
tapply(daily$resid, daily$wday, IQR)

#filter the outlier residuals that the model could not capture well.
#note some holidays stand out such as July 4th, American Thanksgiving, Christmas and NYE.
daily %>%
  filter(resid < -100)

#why does Saturday have such a large IQR in comparasion to every other day 
#of the week? 

#filter and plot Saturday only.
#note the straight consecutive trend points in spring, summer, and fall, 
#that coincide with the school terms.
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  )

#create a function that breaks the year into three school terms.
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall"))
}

daily <- daily %>%
  mutate(term = term(date))


##SHARE


#filter and plot the three school terms for Saturday.

#note:
#spring term - march break has the most flights.
#summer term - out of school summer months has the most flights.
#fall term - back to school has the lowest flights heading into the holidays
#then there is increased flights.
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  labs(
    title = paste("The three school terms for Saturday")
  )

#fit a robust linear model 
library(splines)
mod_dates <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

#plot the model and group the days of the week.
#note the seasonal effect of Saturday knowing the three school terms that exist 
#throughout the year.
daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod_dates) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point() +
  labs(
    title = paste("The seasonal effect of Saturday knowing the 
    three school terms")
  )


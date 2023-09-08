### TREEMAP PLOT

# load libraries
library(tidyverse)
library(treemap)

# check out the dataset.
glimpse(mpg)

# remove all duplicates from the tibble
distinct(mpg, .keep_all = TRUE)

# change variables used  for plotting into factors
mpg <- mpg %>% 
  mutate(class = as_factor(class),
         manufacturer = as_factor(manufacturer)) %>%
  relocate(class, .after = model)

# create a table to see where each category matches up
table(mpg$class, mpg$manufacturer)

# create a tibble with the selected count
car_count <- mpg %>% count(class, manufacturer)

treemap(car_count,
        index=c("class","manufacturer"),
        vSize="n",
        type="index",
        title="Car Manufacturer by Class Size",
        fontsize.title = 16,        
        fontsize.labels=c(15,11),   
        fontcolor.labels="black",  
        fontface.labels=c(2,2),  
        overlap.labels=1,          
        palette="Dark2",         #RColorBrewer::display.brewer.all()
        align.labels= c("center", "center")) 

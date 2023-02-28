###Fuel economy data from 1999 to 2008 for 38 popular models of cars.

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(ggrepel)
  library(modelr)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?mpg

#glimpse to determine the type of variables and data type in the dataset.
glimpse(mpg)
head(mpg)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which predictor variables contribute the most to 
#the response(engine) variable.

## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.
#it also comes from the US Government website  https://fueleconomy.gov/

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset
dim(mpg)

#check the summary for abnormal data.
summary(mpg)

#check for duplicate data and remove.
#note removed 9 observations.
mpg %>% n_distinct()
mpg <- distinct(mpg, .keep_all = TRUE)

#check the character data type for string errors.
mpg %>% count(manufacturer)
mpg %>% count(model) %>% view()
mpg %>% count(trans)
mpg %>% count(drv)
mpg %>% count(fl)
mpg %>% count(class) 

#rename some variables with more clear names. 
(renamed_mpg <- mpg %>%
  rename(engine = displ,
         cylinder = cyl,
         city = cty,
         fuel = fl))

#rename data for analysis.
mpg2 <- renamed_mpg

##ANALYSE 

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset.
numeric_data <- keep(mpg2, is.numeric)

#observe the correlation between numeric data.
#note that engine(L) has a negative correlation with city(mpg) and hwy(mpg).
#engine and cylinder have a strong correlation. 
cor(numeric_data, use = "complete.obs")
cor(mpg2$cylinder, mpg2$engine)

#plot the pairs between the numeric values. 
#note that city and hwy have an interaction effect between predictors(collinearity).
#furthermore the collinearity requires that only one variable is needed 
#for analysis with the engine(L).
pairs(numeric_data)  

#plot and look for correlation with numeric data.
#note that engine and city have a good correlation. 
#note that engine and hwy have a good correlation. 
#note that city and hwy have an interaction effect between predictors(collinearity).
plot(mpg2$city, mpg2$engine,
     main = "the correlation between engine(L) vs. city(mpg)")

plot(mpg2$hwy, mpg2$engine,
     main = "the correlation between engine(L) vs. hwy(mpg)")

#plot the engine(L) versus the cylinder. 
#note that as the engine(L) size increases so too does the 
# number of cylinders.
boxplot(mpg2$engine~mpg2$cylinder, xlab = "cylinder", ylab = "engine")
points(1:4, tapply(mpg2$engine, mpg2$cylinder, mean), pch=4, col="red")


#fit linear regression model. 
#check the summary for statistical significance.
#note that there is statistical evidence(p-value<0.05) to reject H0.
#note that since city and hwy are collinearity with each other from here on out 
#we'll only use hwy in the analysis process.
mod_city <- lm(engine~city, data = mpg2)
summary(mod_city)

mod_hwy <- lm(engine~hwy, data = mpg2)
summary(mod_hwy)

mod_cylinder <- lm(engine~as.factor(cylinder), data = mpg2)
summary(mod_cylinder)

#plot the hwy(mpg) versus the class of the vehicle.
#plot shows smaller class cars(type) gets better gas efficiency in highway 
#driving note that 2 seater sports car is the exception to this rule.
ggplot(mpg2) +
  geom_boxplot(
    aes(x = reorder(class, hwy, FUN = median), 
        y = hwy)) +
  coord_flip() +
  labs(x = "class")

#plot the highway(mpg) versus the drive of the vehicle.
#plot shows which drive gets better gas efficiency in highway driving.
ggplot(mpg2) +
  geom_boxplot(
    aes(x = reorder(drv, hwy, FUN = median),
        y = hwy)) +
  coord_flip() +
  labs(x = "drive")


##SHARE

#the story that was told within this dataset.

#plot engine(L) size versus fuel economy. 
#shows that fuel economy decreases with engine size.
#fit data with loess method(smoothing curve). 
#note the curve in the fit is due to the two seater sports cars.
ggplot(mpg2, aes(engine, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste("Fuel efficiency generally decreases with engine size"),
    subtitle = paste("Two seater sports cars are an exeption because of their 
                     light weight"),
    caption = paste("Data from fueleconomy.gov"),
    x = "Engine displacement (L)",
    y = "HIghway fuel economy (mpg)",
    color = "Car type"
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(nrow = 1, override.aes = list(size = 4))
  ) +
  scale_y_continuous(breaks = seq(15, 40, by = 5)) 

#filter and  plot the engine(L) size versus the fuel economy, factoring the
#class of the vehicle.
#shows that 2 seater sports car exception to the rule more clearly.
class_avg <- mpg2 %>%
  group_by(class) %>%
  summarise(
    engine = median(engine),
    hwy = median(hwy)
  )

ggplot(mpg2, aes(engine, hwy, color = class)) +
  geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6, label.size = 0
  ) +
  geom_point() +
  theme(legend.position = "none") +
  labs(
    title = paste("Increasing engine size is related to decreasing fuel economy"),
    subtitle = paste(
      "Two seater (sports cars) are an exeption because of their light weight"),
    x = "Engine displacement (L)",
    y = "HIghway fuel economy (mpg)"
  ) +
  scale_y_continuous(breaks = seq(15, 40, by = 5)) 

#filter and plot class with the best fuel economy.
best_in_class <- mpg2 %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg2, aes(engine, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  geom_label_repel(
    aes(label = model),
    data = best_in_class
  ) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with engine size"
    ),
    subtitle = paste(
      "Two seater sports cars are an exeption because of their light weight"
    ),
    caption = paste("Data from fueleconomy.gov"
    ),
    x = "Engine displacement (L)",
    y = "HIghway fuel economy (mpg)",
    color = "Class type"
  ) +
  scale_y_continuous(breaks = seq(15, 40, by = 5)) 


#END OF PROJECT


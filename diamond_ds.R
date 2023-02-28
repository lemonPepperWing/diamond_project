###Prices of over 50,000 round cut diamonds

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(modelr)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?diamonds

#glimpse to determine the type of variables and data type in the dataset.
glimpse(diamonds)
head(diamonds)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which predictor variables contribute the most to 
#the response(price) variable.


## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(diamonds)

#check the summary for abnormal data.
summary(diamonds)

#check for duplicate data and remove.
#note removed 146 observations.
diamonds %>% n_distinct()
diamonds <- distinct(diamonds, .keep_all = TRUE)

#rename x, y, z variables with more clear names. 
renamed_diamonds <- diamonds %>%
  rename(length = x,
         width = y,
         depth = z,
         tot_depth = depth)

#plot and check the length, width and depth variables. 
#note the graph reveals some sizes(mm) that are highly unusual when y scale 
#is magnified. it occurs in the upper and/or lower limit.
ggplot(renamed_diamonds) +
  geom_histogram(aes(width), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(renamed_diamonds) +
  geom_histogram(aes(length), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(renamed_diamonds) +
  geom_histogram(aes(depth), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

#filter only normal data to use for analysis.
#note 22 data observations were removed.
normal_diamonds <- renamed_diamonds %>%
  filter(between(width, 3, 20 )) %>%
  filter(between(length, 3, 11)) %>%
  filter(between(depth, 1,10)) %>%
  arrange(width) 

#check new dimensions
dim(normal_diamonds)

#keep normal data for analysis.
diamonds2 <- normal_diamonds

#determine what (Other) is for the clarity variable.
#inspecting clarity ordered factor data type for errors.
#no errors found.
diamonds2 %>% count(clarity) 


##ANALYSE

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset.
numeric_data <- keep(diamonds2, is.numeric)

#observe the correlation between numeric data.
cor(numeric_data, use = "complete.obs")
cor(diamonds2$carat, diamonds2$price)

#plot and look for correlation with numeric data.
#note that price and carat have a strong correlation. 
#it will be difficult to interpret between the individual coefficients.
plot(diamonds2$carat, diamonds2$price,
     main = "the strong correlation between price vs. carat ")

#skip pairs(numeric_data) if your computer is slow. Long output time.
#pairs(numeric_data)  

#plot carat distribution.
#note there are not many observations of diamonds after 3 carats 
#because that weight would be extremely rare and expensive.
ggplot(diamonds2) +
  geom_histogram(aes(carat), bins = 40) +
  coord_cartesian(ylim = c(0, 7500)) 

#cut the data into bins to show the distribution of the carats.
diamonds2 %>%
  count(
    cut_width(carat, 0.5))

#filter and plot the dataset of diamonds with less than 3 carats.
#note 603 data observations were removed.
#note this will be the new normal dataset used for analysis.
diamonds3 <- diamonds2 %>%
  filter(
    between(carat, 0.25, 3)) %>%
  arrange(
    desc(carat))

hist(diamonds3$carat, main = "diamonds less than 3 carats")

#plot the price versus the ordered factors. 
#note that the price does not reflect an increase in quality 
#relative to the quality of the ordered factors.
boxplot(diamonds3$price~diamonds3$cut)
points(1:5, tapply(diamonds3$price, diamonds3$cut, mean), pch=4, col="red")

boxplot(diamonds3$price~diamonds3$clarity)
points(1:8, tapply(diamonds3$price, diamonds3$clarity, mean), pch=4, col="red")

boxplot(diamonds3$price~diamonds3$color)
points(1:7, tapply(diamonds3$price, diamonds3$color, mean), pch=4, col="red")

#fit linear regression model. 
#produce this for each ordered factor variable.
#check the summary for statistical significance.
#note that there is statistical evidence(p-value<0.05) to reject H0.
mod_cut <- lm(price~cut, data = diamonds3)
summary(mod_cut)

mod_clarity <- lm(price~clarity, data = diamonds3)
summary(mod_clarity)

mod_color <- lm(price~color, data = diamonds3)
summary(mod_color)
 
#add the log transform for the price and carat variables to the dataset.
#note that the scale is now normalize between price and carat.
diamonds3 <- diamonds3 %>%
  mutate(lprice = log2(price), 
         lcarat = log2(carat))

#plot the relationship between lprice and lcarat which will be on a log scale.
#note that there is a strong interaction effect between price and carat.
#show strong positive correlation.
ggplot(diamonds3, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

cor(diamonds3$lcarat, diamonds3$lprice)


#fit a simple linear regression  model.
#check the summary for model details.
##there is statistical evidence(p-value<0.05) to reject H0.
mod_carat <- lm(lprice ~ lcarat, data = diamonds3)
summary(mod_carat)

#create data grid to overlay on the plot.
#add the predictions. 
(grid <- diamonds3 %>%
    data_grid(carat = seq_range(carat, 20)) %>%
    mutate(lcarat = log2(carat)) %>%
    add_predictions(mod_carat, "lprice") %>%
    mutate(price = 2 ^ lprice))

#plot the relationship between price and carat with the prediction line for 
#the response variable(price).
#note that some of the larger diamonds are cheaper than expected, this probably
#could be due to no price being over $19000.
ggplot(diamonds3, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", linewidth = 1)

#add the residual variable for price to the dataset.
diamonds3 <- diamonds3 %>%
  add_residuals(mod_carat, "lresid")

#plot the residual to see that we removed the strong effect(correlation) of 
#carat from price.
#note the residuals are scattered in a normal random pattern along lresid.
ggplot(diamonds3, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

#fit a more complicated model including the categorical variables of cut, clarity
#and color. it has statistical significance and yields a higher R-squared.
#this model is more difficult to plot due to more predictor variables.
mod_diamonds <-  lm(lprice ~ lcarat +cut+clarity+color, data = diamonds3)
summary(mod_diamonds)

#create data grid and plot the prediction of cut.
(grid2 <- diamonds3 %>%
  data_grid(cut, .model =  mod_diamonds) %>%
  add_predictions(mod_diamonds))

ggplot(grid2, aes(cut, pred)) + 
  geom_point()

#add the residual variable for price to the dataset.
diamonds3 <- diamonds3 %>%
  add_residuals(mod_diamonds, "lresid2")

#plot the residual to see that we removed the strong effect(correlation) of 
#carat from price, while also including the ordered factor variables.
#note the residuals are scattered in a normal random pattern along lresid but
#there are a few more larger residuals. 
#note lresid is on a log scale, where the 2 is 4X the expected price.
ggplot(diamonds3, aes(lcarat, lresid2)) +
  geom_hex(bins =50)

#filter the larger residuals to determine if there's an opportunity to buy 
#diamonds priced incorrectly.
#nothing really stands out as an opportunity.
diamonds3 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamonds) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table) %>%
  arrange(price)


##SHARE

#the story that was told within this dataset.

#plot the residual(lresid) instead of price to get the desired relationship
#that was expected for the cut, clarity, and color.
#note as the quality of diamond increases, so too does price.
#note lresid is on a log scale, where the -1 are half the expected price and 1
#is twice the expected price.
ggplot(diamonds3, aes(cut, lresid)) + 
  geom_boxplot() +
   labs(
    title = paste("The quality of diamond cut increases, so too does price."),
    subtitle = paste(
      "Quality of the cut (Fair, Good, Very Good, Premium, Ideal)")
    )
   
ggplot(diamonds3, aes(clarity, lresid)) + 
  geom_boxplot() +
  labs(
    title = paste("The quality of diamond clarity increases, so too does price."),
    subtitle = paste(
      "Quality of diamond clarity, from I1 (worst), SI2, SI1, VS2, VS1, VVS2, 
      VVS1, IF (best)")
  )
  
ggplot(diamonds3, aes(color, lresid)) + 
  geom_boxplot() +
  labs(
    title = paste("The quality of diamond color decreases, so too does price."),
      subtitle = paste("Quality of the color from D (best) to J (worst)")
  )


# END OF PROJECT





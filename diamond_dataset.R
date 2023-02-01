###Prices of over 50,000 round cut diamonds

#create R prompt
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(ggrepel)
  library(modelr)
  print("The libraries have been loaded.")
}
LoadLibraries()


#attach the diamonds dataset to the loaded packages.
#search the for visual confirmation of diamonds dataset.
#glimpse to determine the type of variables and data type in the dataset.
attach(diamonds)
search()
glimpse(diamonds)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
head(diamonds)

## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.


## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#look at the help content of each variable in the dataset.
#there are no missing values in the summary.
#the x, y, z variables have a minimum of zero which can't be possible.
#also the variables should be renamed.
?diamonds
summary(diamonds)
dim(diamonds)

#renamed x, y, z variables with clearer names 
a <- rename(diamonds, length_mm = x)
b <- rename(a, width_mm = y)
c <- rename(b, depth_mm = z)
renamed_diamonds <- rename(c, depth_total = depth)

#plot and check the size(mm) of diamonds. 
#the observations have some sizes that are highly unusual when magnified.
#they are in the upper and lower bound.
ggplot(renamed_diamonds) +
  geom_histogram(aes(width_mm), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(renamed_diamonds) +
  geom_histogram(aes(length_mm), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

ggplot(renamed_diamonds) +
  geom_histogram(aes(depth_mm), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

#determine where the minimum of normal data is for depth_mm
renamed_diamonds %>%
  count(depth_mm) %>%
  head(10)

#filter only normal data.
#23 data observations were removed.
unusual_diamonds <- renamed_diamonds %>%
  filter(between(width_mm, 3, 20 )) %>%
  filter(between(length_mm, 3, 11)) %>%
  filter(between(depth_mm, 1,10)) %>%
  arrange(width_mm) 

dim(unusual_diamonds)

#keep normal data for analysis.
diamonds2 <- unusual_diamonds

#look at the contents of each variable in the dataset.
#determine what (Other) is for the clarity column.
summary(diamonds2)

#inspecting clarity ordered factor data type for errors.
#no errors found.
diamonds2 %>%
  count(clarity) 


##ANALYSE AND SHARE

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset
numeric_data <- keep(diamonds2, is.numeric)

#plot and look for correlation with numeric data.
#skip pairs(numeric_data) if your computer is slow. Long output time.
pairs(numeric_data)  
cor(numeric_data, use = "complete.obs")
cor(carat, price)
plot(carat, price)

#plot price versus carat(weight), it has a strong correlation.
ggplot(diamonds2, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 1/20)

#there are not many counts of diamonds after 3 carats because that weight would 
#be extremely rare and expensive.
#cut into bins to show a numeric distribution.
ggplot(diamonds2) +
  geom_histogram(aes(carat), bins = 40) +
  coord_cartesian(ylim = c(0, 7500))

diamonds2 %>%
  count(cut_width(carat, 0.5))

#filter the dataset of diamonds to less than 3 carats.
#40 data observations were removed.
#plot the filtered distribution.
diamonds3 <- diamonds2 %>%
  filter(carat < 3)

ggplot(diamonds3, aes(x = carat)) +
  geom_histogram(bins = 40)

#the cut and clarity variable in the diamonds dataset is ordered by increasing 
#quality.show the rank of quality of the diamonds.
levels(cut)
levels(clarity)
levels(color)

#therefore the quality of the cut should be go up in median price, but the 
#trend from the boxplot does not reflect those results.
#the straight line in the boxplot is the median price.
ggplot(diamonds3, aes(cut, price)) +
  geom_boxplot()

tapply(diamonds3$price, INDEX = diamonds3$cut, FUN = median)

#similarly the quality of the clarity should be go up in median price, but the 
#trend from the boxplot does not reflect those results.
#the straight line in the boxplot is the median price.
ggplot(diamonds3, aes(clarity, price)) +
  geom_boxplot()

tapply(diamonds3$price, INDEX = diamonds3$clarity, FUN = median)

#the results for color are similar to the cut and clarity examples above.

#log transform the carat and price variables.
diamonds4 <- diamonds3 %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

#plot the relationship between lprice and lcarat.
#as known there is a strong linear relationship.
ggplot(diamonds4, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

#perform a simple linear regression to fit the model.
#check the summary for model details. p-value shows statistical significance.
mod_diamond <- lm(lprice ~ lcarat, data = diamonds4)

summary(mod_diamond)

#plot the relationship between price and carat with the prediction line.
#this fits the model and accounts for the curve in the linear model.
#note that some of the large diamonds are cheaper than expected, this could be 
#due probably to no price being over $19000.
grid <- diamonds4 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)

ggplot(diamonds4, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

#check the residuals to see that we removed the strong linear pattern.
#the residuals are scattered in a normal pattern along lresid.
#also no standard deviation greater than +/- 2.
diamonds4 <- diamonds4 %>%
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds4, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

#plot the residual instead of price to get the desired relationship
#that was expected for the cut, clarity, and color.
#the quality of diamond increases, so to does price.
ggplot(diamonds4, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds4, aes(clarity, lresid)) + geom_boxplot()
ggplot(diamonds4, aes(color, lresid)) + geom_boxplot()

#perform a multiple linear regression to fit the model.
#check the summary for model details. p-value shows statistical significance.
mod_diamond2 <- lm(
  lprice ~ lcarat + color + cut + clarity, data = diamonds4
)

summary(mod_diamond2)

#plot the prediction line for cut. any ordered factor could have been used.
grid2 <- diamonds4 %>%
  data_grid(cut, .model =  mod_diamond2) %>%
  add_predictions(mod_diamond2)

ggplot(grid2, aes(cut, pred)) +
  geom_point()

#check the residuals to see if it is scattered in a normal pattern along lresid2.
#there are some observations that are greater than +2 standard deviation.
diamonds4 <- diamonds4 %>%
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds4, aes(lcarat, lresid2)) +
  geom_hex(bins =50)

#filter unusual values to see if there is anything useful to note.
#nothing really stands out
diamonds4 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, length_mm:depth_mm) %>%
  arrange(price)

levels(cut)
levels(clarity)
levels(color)

#END OF PROJECT
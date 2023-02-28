###Major League Baseball Data from the 1986 and 1987 seasons.

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(ISLR2)
  library(glmnet)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?Hitters

#glimpse to determine the type of variables and data type in the dataset.
glimpse(Hitters)
head(Hitters)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which variables contribute the most to salary.


## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.
#this dataset was taken from the StatLib library which is maintained at 
#Carnegie Mellon University. This is part of the data that was used in the 
#1988 ASA Graphics Section Poster Session. The salary data were originally from 
#Sports Illustrated, April 20, 1987. The 1986 and career statistics were obtained 
#from The 1987 Baseball Encyclopedia Update published by Collier Books, 
#Macmillan Publishing Company, New York.

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(Hitters)

#check the summary for abnormal data.
summary(Hitters)

#check for duplicate data and remove.
#there are no duplicates.
Hitters %>% n_distinct()

#check the names of  the variables.
names(Hitters)

#rename variables to work with better.
hitters <- Hitters %>% 
  rename_all(.funs = function(.x){
    .x %>% str_to_lower()
  })

#check the dimensions
dim(hitters)


##ANALYSE

#analyse dataset to find relevant data. 

#plot the salary variable.
#note everything seems normal. 
plot(hitters$salary)

#keep only the data type numeric for the dataset.
numeric_data <- keep(hitters, is.numeric)

#look for correlation with numeric data.
#note that there is nothing that stands out as key predictor variables.
#note will use ridge and lasso regression to find key predictor variables.
cor(numeric_data, use = "complete.obs")

#remove NA from dataset (salary) variable.
hitters <- hitters %>% drop_na()
sum(is.na(hitters))

#create a model matrix and y vector
x <- model.matrix(salary ~ ., hitters)[, -1]
y <- hitters$salary

#create a grid range of values for lambda.
grid <- 10^seq(10, -2, length = 100)

#fit a ridge regression model.
mod_ridge <- glmnet(x, y, alpha = 0, lambda = grid)
names(mod_ridge)

#check dimension of coefficients.
dim(coef(mod_ridge))

#check a lambda sample.
mod_ridge$lambda[50]
coef(mod_ridge)[, 50]
sqrt(sum(coef(mod_ridge)[-1, 50]^2))

#predict and check ridge regression coefficients for lambda sample.
predict(mod_ridge , s = 50, type = "coefficients")[1:20, ]

#split the samples into a training set and a test set in order
#to estimate the test error of ridge regression.
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y_test <- y[test]

#use cross-validation to choose the tuning parameter(lambda).
#note the smallest cross validation error is 326.
set.seed (1)
cv_ridge <- cv.glmnet(x[train , ], y[train], alpha = 0)

names(cv_ridge)
bestlam0 <- cv_ridge$lambda.min

#determine the test mean squares error(MSE) associated with this 
#value of lambda.
pred_ridge <- predict(mod_ridge , s = bestlam0 , newx = x[test , ])

mean ((pred_ridge - y_test)^2)

#refit the ridge regression model on the full data set, using the value of 
#lambda chosen by cross-validation, and examine the coefficient estimates.
#note all coefficients are predictor variables. 
#note use lasso regression to remove unnecessary variables.
mod_output0 <- glmnet(x, y, alpha = 0)
predict(mod_output0 , type = "coefficients", s = bestlam0)[1:20, ]

#fit a lasso regression model
mod_lasso <- glmnet(x, y, alpha = 1, lambda = grid)
names(mod_lasso)

#use cross-validation to choose the tuning parameter(lambda).
#note the smallest cross validation error is 9.28.
set.seed (1)
cv_lasso <- cv.glmnet(x[train , ], y[train], alpha = 1)
bestlam1 <- cv_lasso$lambda.min

#determine the test mean squares error(MSE) associated with this 
#value of lambda.
#note that lasso has a better MSE than ridge regression.
pred_lasso <- predict(mod_lasso , s = bestlam1 , newx = x[test , ])

mean ((pred_lasso - y_test)^2)

#refit the lasso regression model on the full data set, using the value of 
#lambda chosen by cross-validation, and examine the coefficient estimates.
#note  lasso removed unnecessary variables.
mod_output1 <- glmnet(x, y, alpha = 1)
coef_lasso <- predict(mod_output1 , type = "coefficients", s = bestlam1)[1:20, ]

#the lasso model with lambda chosen by cross-validation contains 
#only nine variables.
coef_lasso[coef_lasso != 0]

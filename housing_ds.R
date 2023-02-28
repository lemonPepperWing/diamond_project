###A data set containing housing values in 506 suburbs of Boston.

#create R prompt.
#load the libraries need for the project.
options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(ISLR2)
  library(tree)
  library(randomForest)
  library(gbm)
  library(BART)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?Boston

#glimpse to determine the type of variables and data type in the dataset.
glimpse(Boston)
head(Boston)


## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which predictor variables that contribute the most to 
#the response median value(medv) variable.


## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(Boston)

#check the summary for abnormal data.
summary(Boston)

#check for duplicate data and remove.
#there are no duplicates.
Boston %>% n_distinct()


##ANALYSE 

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset.
numeric_data <- keep(Boston, is.numeric)

#look for correlation with numeric data.
#note that there is good correlation with lower status of the population(lstat).
#note that there is good correlation with average number of rooms per dwelling(rm).
#note will use tree-based methods to determine most important predictor variables.
cor(numeric_data, use = "complete.obs")

#fit a regression tree to the Boston data set.
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree_boston<- tree(medv ~ ., Boston, subset = train)
summary(tree_boston)

#plot the tree
plot(tree_boston)
text(tree_boston, pretty = 0)

#determine whether pruning the tree will improve performance 
#with cross validation.
#note in this case, the most complex tree under consideration is 
#selected by cross validation.
cv_boston <- cv.tree(tree_boston)
names(cv_boston)
plot(cv_boston$size, cv_boston$dev, type = "b")

#use unpruned tree to make predictions on the test set(medv).
pred_boston <- predict(tree_boston, newdata = Boston[-train, ])
test_medv <- Boston[-train, "medv"]

#the test set mean squared error(MSE) associated with the 
#regression tree is 35.29.
#note the square root of the MSE is therefore around 5.941, indicating that this
#model leads to test predictions that are (on average) within approximately
#$5,941 of the true median home value for the census tract.
mean((pred_boston - test_medv)^2)

#fit a bagged regression tree to the Boston data set.
set.seed(1)
bag_boston <- randomForest(medv ~ ., data = Boston,
                           subset = train, mtry = 12, importance = TRUE)
bag_boston

#use unpruned tree to make predictions on the test set.
#the test set MSE associated with the bagged regression tree is 23.42.
pred_bag <- predict(bag_boston, newdata = Boston[-train, ])
mean((pred_bag - test_medv)^2)

#fit a random forest regression tree to the Boston data set.
#use p/3 variables when building a random forest of regression trees.
set.seed(1)
rf_boston <- randomForest(medv ~ ., data = Boston,
                          subset = train, mtry = 6, importance = TRUE)

#use unpruned tree to make predictions on the test set.
#the test set MSE associated with the bagged regression tree is 19.55.
#note this indicates that random forests has better improvement over bagging.
pred_rf <- predict(rf_boston, newdata = Boston[-train, ])
mean((pred_rf - test_medv)^2)

#view the importance of each variable.
#note the wealth of the community (lstat) and the house size (rm) are  
#the two most important variables.
importance(rf_boston)

#fit a boosting regression tree to the Boston data set.
set.seed(1)
boost_boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4)

#produce the relative influence statistics
summary(boost_boston)

#produce partial dependence plots for these two variables
#median house prices are increasing with rm and decreasing with lstat.
plot(boost_boston, i = "rm")
plot(boost_boston, i = "lstat")

#use unpruned tree to make predictions on the test set.
#the test set MSE associated with the boosting regression tree is 17.03.
#note the test MSE is superior to the test MSE of random forests and bagging.
pred_boost <- predict(boost_boston,
                      newdata = Boston[-train, ], n.trees = 5000)
mean((pred_boost - test_medv)^2)

#create matrices of predictors for the training and test data.
x <- Boston[, 1:12]
y <- Boston[, "medv"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]

#fit a Bayesian additive regression tree model to the Boston housing data set.
set.seed(1)
bart_boston <- gbart(xtrain, ytrain, x.test = xtest)

#check BART summary statistics.
names(bart_boston)

#use unpruned tree to make predictions on the test set.
#the test set MSE associated with the boosting regression tree is 15.94.
#note BART is lower than the test error of random forests and boosting.
pred_bart <- bart_boston$yhat.test.mean
mean((ytest - pred_bart)^2)

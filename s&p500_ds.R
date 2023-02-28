###Daily percentage returns for the S&P 500 stock index between 2001 and 2005.

options(prompt = "R> ")

LoadLibraries <- function () {
  library(tidyverse)
  library(ISLR2)
  library(MASS)
  library(e1071)
  print("The libraries have been loaded.")
}
LoadLibraries()

#look at the help content of each variable in the dataset.
?Smarket

#glimpse to determine the type of variables and data type in the dataset.
glimpse(Smarket)
head(Smarket)

## ASK

#is there a potential business problem in this dataset to be solved?
#is there a story that could be told within this dataset? 
#note that we'll determine which predictor variables and fit of classification
#models contribute the best to the binary response(direction) variable.


## PREPARE

#the dataset used for this project is one that has been installed in R. 
#therefore the dataset is a reliable source that can be used.
#the source of the raw data comes from S&P 500 Yahoo finance.

## PROCESS

#check to see if the data is clean and ready to be used for data analysis.

#check the dimension of the dataset.
dim(Smarket)

#check the summary for abnormal data.
summary(Smarket)

#check for duplicate data and remove.
#there are no duplicate data.
Smarket %>% n_distinct()

#rename all the variables to lowercase. 
smarket <- Smarket %>%
  rename_all(.funs = function(.x){
    .x %>% str_to_lower()
  })


##ANALYSE

#analyse dataset to find relevant data. 

#keep only the data type numeric for the dataset.
numeric_data <- keep(smarket, is.numeric)

#plot and look for correlation with numeric data.
#there is some correlation with year and volume.
cor(numeric_data, use = "complete.obs")
cor(smarket$year, smarket$volume)

#plot the volume.
#note that volume is increasing over time.
plot(smarket$volume)

#create a training dataset of observations from 2001-2004.
#create a testing dataset of observations from 2005.
train <- (smarket$year < 2005)
test <- smarket[!train, ]

#create observations for which train is FALSE.
test_direction <- smarket$direction[!train]

#fit and train the logistic regression model. 
#check the summary for statistical significance.
#note that there is no statistical evidence(p-value<0.05) to reject H0.
mod_logr <- glm(direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + volume,
  data = smarket, family = binomial, subset = train)
summary(mod_logr)

#determine which binomial direction is up.
contrasts(smarket$direction)

#predict and test that the probability that the market will go up 
#given values of the predictors.
#note that these prediction values correspond to the probability 
#of the market going up.
pred_logr <- predict(mod_logr, test, type = "response")
pred_logr[1:10]

#create a vector of class predictions based on whether the predicted 
#probability of a market increase is greater than or less than 0.5.
pred_direction <- rep("Down", 252)
pred_direction[pred_logr > .5] = "Up"

#create a confusion matrix in order to determine how many
#observations were correctly or incorrectly classified.
table(pred_direction, test_direction)

#calculate the correct predictions.
#logistic regression correctly predicted the movement of the market 
#48% of the time.
(77 + 44) / 252
mean(pred_direction == test_direction)

#calculate the test error rate. it is 52%.
mean(pred_direction != test_direction)


#fit the logistic regression using just lag1 and lag2.
mod_logr1 <- glm(direction ~ lag1 + lag2, data = smarket,
                family = binomial, subset = train)

#predict and test that the probability that the market will go up 
#given values of the predictors.
pred_logr1 <- predict(mod_logr1, test,
                     type = "response")

#create a vector of class predictions based on whether the predicted 
#probability of a market increase is greater than or less than 0.5.
pred_direction1 <- rep("Down", 252)
pred_direction1[pred_logr1 > .5] <- "Up"

#create a confusion matrix in order to determine how many
#observations were correctly or incorrectly classified.
table(pred_direction1, test_direction)

#logistic regression correctly predicted the movement of the market 
#56% of the time.
mean(pred_direction1 == test_direction)


#fit the Linear Discriminant Analysis(LDA) using just Lag1 and Lag2.
mod_lda <- lda(direction ~ lag1 + lag2, data = smarket,
               subset = train)

#observe summary statistic of LDA.
#note 49.2% of the training observations correspond to days during which the
#market went down.
names(mod_lda)
mod_lda$prior

#compute the predictions for 2005 and compare them to the actual movements
#of the market over that time period.
pred_lda <- predict(mod_lda, test)
names(pred_lda)

#class contains LDA predictions about the movement of the market.
class_lda <- pred_lda$class


#create a confusion matrix in order to determine how many
#observations were correctly or incorrectly classified.
table(class_lda, test_direction)

##calculate correct predictions.
#LDA correctly predicted the movement of the market 56% of the time.
mean(class_lda == test_direction)


#fit the Quadratic Discriminant Analysis(QDA) using just Lag1 and Lag2.
mod_qda <- qda(direction ~ lag1 + lag2, data = smarket,
               subset = train)

#observe summary statistic of QDA.
names(mod_qda)
mod_qda$prior

#compute the predictions for 2005 and compare them to the actual movements
#of the market over that time period.
pred_qda <- predict(mod_qda, test)
names(pred_qda)

#class contains QDA predictions about the movement of the market.
class_qda <- pred_qda$class

#create a confusion matrix in order to determine how many
#observations were correctly or incorrectly classified.
table(class_qda, test_direction)

##calculate correct predictions.
#QDA correctly predicted the movement of the market 60% of the time.
mean(class_qda == test_direction)


#fit the Naive Bayes using just Lag1 and Lag2.
mod_nb <- naiveBayes(direction ~ lag1 + lag2, data = smarket,
                     subset = train)

#observe summary statistic of NB.
names(mod_nb)
mod_nb$apriori

#compute the predictions for 2005 and compare them to the actual movements
#of the market over that time period.
pred_direction2 <- predict(mod_nb, test)

#create a confusion matrix in order to determine how many
#observations were correctly or incorrectly classified.
table(pred_direction2, test_direction)

##calculate correct predictions.
#naive Bayes correctly predicted the movement of the market 59% of the time.
mean(pred_direction2 == test_direction)


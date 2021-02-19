

rm(list=ls())


getwd()
setwd("/Users/mayankpandey/Downloads")


install.packages("tidyverse")
install.packages("broom")
install.packages("glmnet")
install.packages("ISLR")

library(ISLR)
library(glmnet)
library(broom)
library(tidyverse)
insurance = read.csv("insurance.csv")
is.na(insurance)
insurance <- na.omit(insurance)
head(insurance)



######################################################################

# Compute RMSE and R Square from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - (SSE / SST)
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}



########################################################

install.packages("caret")
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(insurance$charges, p = .8,
                                  list = FALSE,
                                  times = 1)
Train <- insurance[ trainIndex,]
Test <- insurance[-trainIndex,]


x_train = data.matrix(Train[,-7])
y_train = Train$charges

x_test= data.matrix(Test[,-7])
y_test = Test$charges




#############################################################################
############# Linear regression #########################
linearMod <- lm(Train$charges ~Train$age + Train$sex+ Train$bmi+ Train$children+ Train$smoker+ Train$region,data = insurance )  # build linear regression model on full data
linearMod
plot(linearMod)
train_modellin <- predict(linearMod, newdata = Train, type = "response")
test_modellin <- predict(linearMod, newdata = Test, type = "response")
## finding RMSE and Rsquare for training and testing data
eval_results(Train$charge,train_modellin,Train)
eval_results(Test$charge,test_modellin,Test)

###################### generalized linear regression model ####################

hist(insurance$charges)

glmmod <- glm(charges ~ .,  data= Train)
glmmod
summary(glmmod)
plot(glmmod)

train_modelglm <- predict(glmmod, newdata = Train, type = "response")
test_modelglm <- predict(glmmod, newdata = Test, type = "response")

## finding RMSE and Rsquare for training and testing data
eval_results(Train$charges,train_modelglm,Train)
eval_results(Test$charges,test_modelglm,Test)

###########  Ridge Regression   ####################


lambdas <- 10^seq(3, -2, by = -.1)

ridgemod <- cv.glmnet(x_train,y_train, alpha = 0, lambda = lambdas,type.measure = "mse")

summary(ridgemod)
plot(ridgemod)
ridgemod$lambda.min
ridgemod$lambda.1se
coef(ridgemod)



train_modelridge <- predict(ridgemod, newx = x_train, type = "response")
head(train_model)
summary(train_model)


test_modelridge <- predict(ridgemod, newx = x_test, type = "response")
head(test_model)
summary(test_model)


# evaluation on train data
eval_results(Train$charges,train_modelridge,Train)



# evaluation on test data
eval_results(Test$charges,test_modelridge,Test)



################   Lasso Regression    ###########################

lambdas <- 10^seq(3, -2, by = -.1)

lassomod <- cv.glmnet(x_train,y_train, alpha = 1, lambda = lambdas,type.measure = "mse")

summary(lassomod)
plot(lassomod)
lassomod$lambda.min
lassomod$lambda.1se
coef(lassomod)


train_modellasso <- predict(lassomod, newx = x_train, type = "response")
head(train_model)
summary(train_model)


test_modellasso <- predict(lassomod, newx = x_test, type = "response")
head(test_model)
summary(test_model)


# evaluation on train data
eval_results(Train$charges,train_modellasso,Train)


# evaluation on test data
eval_results(Test$charges,test_modellasso,Test)

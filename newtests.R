library(tidyverse)
library(dplyr)
library(readxl)
library(glmnet)
library(reshape2)
library(ggplot2)
library(rpart)

# reads the dataset and skips unnecessary information
# dataset <- read.csv("new_crops.csv", header = TRUE, row.names=NULL, skip = 14)




dataset <- read.csv("finalcombdata.csv", header = TRUE)
head(dataset)

tail(dataset)



melted_matrix <- melt(correlation_matrix)
ggplot(data = melted_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90))

#comparison with other studies using similar datasets: 

#https://www.sciencedirect.com/science/article/pii/S0303243421000581
#https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0070816
#https://www.mdpi.com/2071-1050/8/11/1123
#https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0245467

# linmod <- lm(MEAN.COUNT ~ SAMPLE.VALUE + SAMPLE.COUNT + # MEAN.VALUE + 
#                ANOM.VALUE + prec, data = df2)
# summary(linmod)
# y_predicted <- predict(linmod, newx = X)
# # R-squared value
# sst <- sum((y - mean(y))^2)
# sse <- sum((y_predicted - y)^2)
# rsq <- 1 - sse/sst
# rsq

dt = sort(sample(nrow(dataset), nrow(dataset)*.5))
train<-dataset[dt,]
test<-dataset[-dt,]

X <- data.matrix(train[, c('MEAN.COUNT', 'SAMPLE.COUNT', 'ANOM.VALUE', 'prec')])
y <- train$MEAN.COUNT

X_test <- data.matrix(test[, c('MEAN.COUNT', 'SAMPLE.COUNT', 'ANOM.VALUE', 'prec')])
y_test <- test$MEAN.COUNT
#Lasso Regression: 
cv_model <- cv.glmnet(X, y, alpha = 1)
#find optimal lambda value that minimizes test MSE
optimal_lambda <- cv_model$lambda.min
optimal_lambda
#produce plot of test MSE by lambda value
plot(cv_model)

model <- glmnet(X, y, alpha = 1, lambda = optimal_lambda)
coef(model)

y_predicted <- predict(model, s = optimal_lambda, newx = X_test)
# R-squared value
sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_predicted - y_test)^2)
rsq <- 1 - sse/sst
rsq
# 0.9991501

# Ridge regression
cv_model <- cv.glmnet(X, y, alpha = 0)
plot(cv_model)
optimal_lambda <- cv_model$lambda.min
optimal_lambda
ridge_model <- glmnet(X, y, alpha = 0, lambda = optimal_lambda)
plot(ridge_model,xvar="lambda",label=TRUE)
coef(ridge_model)

y_predicted <- predict(ridge_model, s = optimal_lambda, newx = X_test)
# R-squared value
sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_predicted - y_test)^2)
rsq <- 1 - sse/sst
rsq
#0.9972781
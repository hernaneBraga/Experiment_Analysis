library(caret)
library(tidyverse)

fulldata <- read.csv2("teste.csv", sep=',',dec = ".", header = FALSE)

X <- as.matrix(fulldata[,1:4])
Y <- as.factor(fulldata[,5])

#idx <- createDataPartition(Y,p = 0.7,  list = FALSE)
idx <- createTimeSlices(Y, 100, horizon = 100, fixedWindow = TRUE, skip = 0)

X_train <- as.matrix(fulldata[idx,1:4])
Y_train <- as.factor(fulldata[idx,5])

X_test <- as.matrix(fulldata[-idx,1:4])
Y_test <- as.factor(fulldata[-idx,5])

# Treinamento dos Modelos

# Random Forest
# randomf <- train(X_train, Y_train, method = 'ranger',
#                preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
randomf <- train(X_train, Y_train, method = 'ranger',
                 preProcess = 'scale',
                 trControl = trainControl(method = "timeslice",initialWindow = 400,
                                          horizon = 400, fixedWindow = TRUE, skip = 0))
print(randomf)

# Multi Layer Perceptron
mlperceptron <- train(X_train, Y_train, method = 'mlp', 
                 preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(mlperceptron)

# SVM Kernel Polinomial
svmrbf <- train(X_train, Y_train, method = 'lssvmRadial', 
                      preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(svmrbf)

fuzzy <- train(X_train, Y_train, method = 'FRBCS.CHI', 
                preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(fuzzy)

# Erro dos modelos
models <- list(randomf, mlperceptron, svmrbf, fuzzy)
yhat <- predict.train(models[[1]], newdata = X_test)
e <- mean(as.numeric(yhat) == as.numeric(Y_test))

# yhat <- predict(mlperceptron,X_test)
# e <- postResample(pred = yhat, obs = Y_test)
# e <- e[1];


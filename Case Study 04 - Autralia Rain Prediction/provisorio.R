library(caret)
library(tidyverse)

fulldata <- read.csv2("teste.csv", sep=',',dec = ".", header = FALSE)

X <- as.matrix(fulldata[,1:4])
Y <- as.factor(fulldata[,5])

idx <- createDataPartition(Y,p = 0.7,  list = FALSE)

X_train <- as.matrix(fulldata[idx,1:4])
Y_train <- as.factor(fulldata[idx,5])

X_test <- as.matrix(fulldata[-idx,1:4])
Y_test <- as.factor(fulldata[-idx,5])

# Treinamento dos Modelos

# Random Forest
randomf <- train(X_train, Y_train, method = 'ranger', 
               preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(model)

# Multi Layer Perceptron
mlperceptron <- train(X_train, Y_train, method = 'mlp', 
                 preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(mlperceptron)

# SVM Kernel Polinomial
svmrbf <- train(X_train, Y_train, method = 'lssvmRadial', 
                      preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
print(svmrbf)



# Erro dos modelos
yhat <- predict.train(svm, newdata = X_test)
e <- mean(as.numeric(yhat) == as.numeric(Y_test))


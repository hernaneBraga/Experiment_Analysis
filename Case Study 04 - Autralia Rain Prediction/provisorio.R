library(caret)
library(tidyverse)

# fulldata <- read.csv2("teste.csv", sep=',',dec = ".", header = FALSE)
# 
# X <- as.matrix(fulldata[,1:4])
# Y <- as.factor(fulldata[,5])

# idx <- createDataPartition(Y,p = 0.7,  list = FALSE)

fulldata <- read.csv("data/train.csv", header = TRUE)
fulldata_test <- read.csv("data/test.csv", header = TRUE)
loc <- unique(fulldata$Location)

localdata <- fulldata[fulldata$Location == loc[1],]
localdata_test <- fulldata_test[fulldata_test$Location == loc[1],]

X_train <- localdata[,2:21]
Y_train <- as.factor(localdata[,22])

X_test <- localdata_test[,2:21]
Y_test <- as.factor(localdata_test[,22])


## TESTE DE CROSSVALIDATION ##
X_train <- fulldata[,2:21]
Y_train <- as.factor(fulldata[,22])

randomf <- train(X_train, Y_train, method = 'ranger', 
                      preProcess = 'scale',trControl = trainControl(method = "timeslice",
                                                                    initialWindow = 1000,
                                                                    horizon = 1000,
                                                                    fixedWindow = TRUE,
                                                                    skip = 1000))
print(randomf)

mlperceptron <- train(X_train, Y_train, method = 'mlp', 
                      preProcess = 'scale',trControl = trainControl(method = "timeslice",
                                                                    initialWindow = 1000,
                                                                    horizon = 1000,
                                                                    fixedWindow = TRUE,
                                                                    skip = 1000))
print(mlperceptron)

svmrbf <- train(X_train, Y_train, method = 'lssvmRadial',
                      preProcess = 'scale',trControl = trainControl(method = "timeslice",
                                                                    initialWindow = 1000,
                                                                    horizon = 1000,
                                                                    fixedWindow = TRUE,
                                                                    skip = 1000))
print(svmrbf)

fuzzy <- train(X_train, Y_train, method = 'FRBCS.CHI', 
                      preProcess = 'scale',trControl = trainControl(method = "timeslice",
                                                                    initialWindow = 1000,
                                                                    horizon = 1000,
                                                                    fixedWindow = TRUE,
                                                                    skip = 1000))
print(fuzzy)






###################################


#idx <- createTimeSlices(Y, 100, horizon = 100, fixedWindow = TRUE, skip = 0)

# X_train <- as.matrix(fulldata[idx,1:4])
# Y_train <- as.factor(fulldata[idx,5])
# 
# X_test <- as.matrix(fulldata[-idx,1:4])
# Y_test <- as.factor(fulldata[-idx,5])

# Treinamento dos Modelos

# Random Forest
# randomf <- train(X_train, Y_train, method = 'ranger',
#                preProcess = 'scale',trControl = trainControl(method = "cv", number = 5))
randomf <- train(X_train, Y_train, method = 'ranger',
                 preProcess = 'scale',
                 trControl = trainControl(method = "none"))
print(randomf)

# Multi Layer Perceptron
mlperceptron <- train(X_train, Y_train, method = 'mlp', 
                 preProcess = 'scale',trControl = trainControl(method = "none"))
print(mlperceptron)

# SVM Kernel Polinomial
svmrbf <- train(X_train, Y_train, method = 'lssvmRadial', 
                      preProcess = 'scale',trControl = trainControl(method = "none"))
print(svmrbf)

# Fuzzy Inference System
fuzzy <- train(X_train, Y_train, method = 'FRBCS.CHI', 
                preProcess = 'scale',trControl = trainControl(method = "none"))
print(fuzzy)

# Erro dos modelos
# models <- list(randomf, mlperceptron, svmrbf, fuzzy)
# yhat <- predict.train(randomf, newdata = X_test)
# e <- mean(as.numeric(yhat) == as.numeric(Y_test))

yhat <- predict(randomf,X_test)
cm <- confusionMatrix(data = yhat, reference = Y_test)
acc <- cm$overall['Accuracy']
sst <- cm$byClass['Sensitivity']
spc <- cm$byClass['Specificity']
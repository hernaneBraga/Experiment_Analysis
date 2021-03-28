library(caret)
library(tidyverse)

fulldata <- read.csv2("teste.csv", sep=',',dec = ".", header = FALSE)
X <- as.matrix(fulldata[,1:4])
Y <- as.matrix(fulldata[,5])

k <- kfold(fulldata, k=5)
e_it <- c()

lambda <- c(0.01,0.10,0.20,0.30,0.40,0.50)

# Inserir for aqui (1 até 5)

X_train <- as.matrix(fulldata[k!=1,1:4])
Y_train <- as.factor(fulldata[k!=1,5])

X_test <- as.matrix(fulldata[k==1,1:4])
Y_test <- as.factor(fulldata[k==1,5])

# Treinar Modelos com Lambda

mse <- 0 # erro
e_it <- cbind(e_it,mse) # salvar os erros

# fechar for

e_m <- mean(e_it) # Erro médio


# Modelos
train(X_train,Y_train,method = 'ranger',preProcess = 'scale')

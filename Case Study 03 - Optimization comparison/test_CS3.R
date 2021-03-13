Y_est <- read.csv(".\\data\\param_data.csv", sep=';', dec = ",", header = TRUE)
Y_est <- data.frame(Y_est)
model <- aov(resultado~bloco+config, data=Y_est)
summary(model)

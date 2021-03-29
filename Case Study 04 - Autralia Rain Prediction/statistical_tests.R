library(CAISEr)
library(multcomp)
library(ggplot2)

## Potência
Ncalc <- calc_instances(3,d = 0.5, ninstances = 23,power = NULL,  sig.level = 0.05, alternative = "two.sided", test = "t.test")

fulldata <- read.csv("data/inst_3modelos.csv",sep =';',dec = ',', header = TRUE)

## Tratamento de Dados
aggdata <- with(fulldata, aggregate(x=cbind(acuracia,sensibilidade,especificidade),
                                    by=list(modelo,localidade),
                                    FUN=mean))
for(i in 1:2) aggdata[,i] <- as.factor(aggdata[,i])
names(aggdata) <- c("Modelo","Localidade","Acuracia","Sensibilidade","Especificidade")
summary(aggdata)

## Análise Anova 
mdl_acc <- aov(Acuracia~Modelo+Localidade, data = aggdata)
summary(mdl_acc)



aggdata$Resultado[1] <- 0.0001 # Log de 0 dá -inf
mdl2 <- aov(log(Resultado)~Configuracao+Instancia, data = aggdata)

summary(mdl2)




## Plots

# Plot Acurácia
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Acuracia, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)


par(mfrow = c(2, 2))
plot(mdl_acc, pch = 20, las = 1)

# Plot Sensibilidade
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Sensibilidade, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)

# Plot Especificidade
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Especificidade, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)

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

# Acurácia
mdl_acc <- aov(Acuracia~Modelo+Localidade, data = aggdata)
summary(mdl_acc)
summary.lm(mdl_acc)$r.squared

# Sensibilidade
mdl_ssb <- aov(Sensibilidade~Modelo+Localidade, data = aggdata)
summary(mdl_ssb)
summary.lm(mdl_ssb)$r.squared

# Especificidade
mdl_spc <- aov(Especificidade~Modelo+Localidade, data = aggdata)
summary(mdl_spc)
summary.lm(mdl_spc)$r.squared

## Comparações Múltiplas

# Acurácia
dtest_acc <- glht(mdl_acc, linfct = mcp(Modelo = "Tukey"))
dtestIC_acc <- confint(dtest_acc,level = 0.95)

# Sensibilidade
dtest_ssb <- glht(mdl_ssb, linfct = mcp(Modelo = "Tukey"))
dtestIC_ssb <- confint(dtest_ssb,level = 0.95)

# Especificidade
dtest_spc <- glht(mdl_spc, linfct = mcp(Modelo = "Tukey"))
dtestIC_spc <- confint(dtest_spc,level = 0.95)


## Plots

# Acurácia -- Localidades 
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Acuracia, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)

# Acurácia -- Informações ANOVA 
par(mfrow = c(2, 2))
plot(mdl_acc, pch = 20, las = 1)

# Acurácia -- Comparações 
par(mar = c(5, 10, 3, 2), las = 1)
plot(dtestIC_acc,xlim = c(-0.15,0.15))

# Sensibilidade -- Localidades 
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Sensibilidade, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)

# Sensibilidade -- Informações ANOVA 
par(mfrow = c(2, 2))
plot(mdl_ssb, pch = 20, las = 1)

# Sensibilidade -- Comparações 
par(mar = c(5, 10, 3, 2), las = 1)
plot(dtestIC_ssb,xlim = c(-0.1,0.1))

# Especificidade -- Localidades 
p <- ggplot(aggdata, aes(x = Localidade, 
                         y = Especificidade, 
                         group = Modelo, 
                         colour = Modelo))
p + geom_line(linetype = 2) + geom_point(size=5)

# Especificidade -- Informações ANOVA 
par(mfrow = c(2, 2))
plot(mdl_spc, pch = 20, las = 1)

# Especificidade -- Comparações 
par(mar = c(5, 10, 3, 2), las = 1)
plot(dtestIC_spc,xlim = c(-0.2,0.6))

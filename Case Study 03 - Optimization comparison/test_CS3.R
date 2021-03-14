library(CAISEr)
library(multcomp)

# Número de instancias
Ncalc <- calc_instances(1,power = 0.8, d = 0.5, sig.level = 0.05, alternative = "two.sided", test = "t.test")

# "Tratamento" dos Dados
fulldata <- read.csv2(".\\data\\inst_data_complete.csv", sep=';', header = TRUE)
aggdata <- with(fulldata, aggregate(x=resultado,
                                    by=list(config,instancia),
                                    FUN=mean))
names(aggdata) <- c("Configuracao","Instancia","Resultado")
for(i in 1:2) aggdata[,i] <- as.factor(aggdata[,i])
levels(aggdata$Configuracao) <- c("Config1","Config2")
summary(aggdata)

library(ggplot2)
p <- ggplot(aggdata, aes(x = Instancia, 
                         y = Resultado, 
                         group = Configuracao, 
                         colour = Configuracao))
p + geom_line(linetype = 2) + geom_point(size=5)

# Análise Anova 
mdl1 <- aov(Resultado~Configuracao+Instancia, data = aggdata)
aggdata$Resultado[1] <- 0.0001 # Log de 0 dá -inf
mdl2 <- aov(log(Resultado)~Configuracao+Instancia, data = aggdata)
summary(mdl1)
summary(mdl2)

# Não tenho certeza, algum gráfico kk
par(mfrow = c(2, 2))
plot(mdl1, pch = 20, las = 1)
par(mfrow = c(2, 2))
plot(mdl2, pch = 20, las = 1)

# Comparação Múltipla (caso dê diferença)
dtest1 <- glht(mdl1, linfct = mcp(Configuracao = "Dunnet"))
dtestIC1 <- confint(dtest1,level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
plot(dtestIC1,xlim = c(-10^7,10^7))

dtest2 <- glht(mdl2, linfct = mcp(Configuracao = "Dunnet"))
dtestIC2 <- confint(dtest2,level = 0.95)
par(mar = c(5, 8, 4, 2), las = 1)
plot(dtestIC2,xlim = c(-10,10))


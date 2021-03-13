library(CAISEr)
library(multcomp)

# Número de instancias
Ncalc <- calc_instances(1,power = 0.8, d = 0.5, sig.level = 0.05, alternative = "two.sided", test = "t.test")

# Análise Anova 
mdl <- aov(resultado~config+instancia, data = Y)
summary(mdl)

# Não tenho certeza, algum gráfico kk
par(mfrow = c(2, 2))
plot(mdl, pch = 20, las = 1)

# Comparação Múltipla (caso dê diferença)
mdl$model$config = as.factor(config)
dtest <- glht(model, linfct = mcp(config = "Dunnett"))
dtestIC <- confint(dtest,level = 0.95)
plot(dtestIC)
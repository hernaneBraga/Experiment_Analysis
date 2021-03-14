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

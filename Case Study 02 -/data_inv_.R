# Function to convert absolute values on series to percentual
abs_serie_to_perct <- function(serie) {
  
  size = length(serie)
  serie_perct = numeric(size-1)
  
  for(i in 1:size-1) {
    vi = serie[i]
    vf = serie[i+1]
    perct_delta = (vf-vi)/vi
    serie_perct[i] <- perct_delta
  }
  
  return(serie_perct)
}

# Loading dataset
raw_data <- read.csv('DadosAcoesGrupoA.csv', sep=',', header = FALSE)

# Reverse each series 
df <- raw_data[rev(1:nrow(raw_data)), ]

s1 <- df[,1]
s2 <- df[,2]
s3 <- df[,3]
s4 <- df[,4]
s5 <- df[,5]

sp1 <- abs_serie_to_perct(s1)
sp2 <- abs_serie_to_perct(s2)
sp3 <- abs_serie_to_perct(s3)
sp4 <- abs_serie_to_perct(s4)
sp5 <- abs_serie_to_perct(s5)

#par(mfrow=c(1,1)) 
#par(cex=0.7, mai=c(0.6,0.6,0.2,0.2))

# Plot time series (fixed axis)
plot(1:36,s1,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250),col = 'blue',type = 'b')
lines(1:36,s2,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250),col = 'red',type = 'b')
lines(1:36,s3,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250),col = 'orange',type = 'b')
lines(1:36,s4,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250),col = 'purple',type = 'b')
lines(1:36,s5,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250),col = 'dark green',type = 'b')
leg.txt <- c("Ação 1","Ação 2","Ação 3","Ação 4","Ação 5")
legend(-0.7,259,leg.txt,pch = "oooo", col = c('blue','red','orange','purple','darkgreen'), cex =0.8)

# Plot percentage time series (fixed axis)
plot(1:35,sp1,xlab = 'Mês',ylab = 'Percentual de Variação da Ação',xlim = c(0,35),ylim = c(-0.05,0.05),col = 'blue',type = 'b')
legend(31.7,0.052,leg.txt[1],pch = "o", col = c('blue'), cex =0.8)
plot(1:35,sp2,xlab = 'Mês',ylab = 'Percentual de Variação da Ação',xlim = c(0,35),ylim = c(-0.05,0.05),col = 'red',type = 'b')
legend(31.7,0.052,leg.txt[2],pch = "o", col = c('red'), cex =0.8)
plot(1:35,sp3,xlab = 'Mês',ylab = 'Percentual de Variação da Ação',xlim = c(0,35),ylim = c(-0.05,0.05),col = 'orange',type = 'b')
legend(31.7,0.052,leg.txt[3],pch = "o", col = c('orange'), cex =0.8)
plot(1:35,sp4,xlab = 'Mês',ylab = 'Percentual de Variação da Ação',xlim = c(0,35),ylim = c(-0.05,0.05),col = 'purple',type = 'b')
legend(31.7,0.052,leg.txt[4],pch = "o", col = c('purple'), cex =0.8)
plot(1:35,sp5,xlab = 'Mês',ylab = 'Percentual de Variação da Ação',xlim = c(0,35),ylim = c(-0.05,0.05),col = 'dark green',type = 'b')
legend(31.7,0.052,leg.txt[5],pch = "o", col = c('dark green'), cex =0.8)


# Plot time series (automatic axis)
plot(1:36,s2,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,25),col = 'red',type = 'b')
lines(1:36,s4,xlab = 'Mês',ylab = 'Valor da Ação',col = 'purple',type = 'b')
lines(1:36,s5,xlab = 'Mês',ylab = 'Valor da Ação',col = 'dark green',type = 'b')
leg.txt <- c("Ação 1","Ação 2","Ação 3","Ação 4","Ação 5")
legend(32.7,2.7,leg.txt,pch = "oooo", col = c('blue','red','orange','purple','darkgreen'), cex =0.8)

##################################################################
# Análise Anova
################################

# 5 Ações (Dados Brutos)

X <- rep(c("A1","A2","A3","A4","A5"),each=36);
Y <- c(s1,s2,s3,s4,s5);
dataDBT <- data.frame(Y,X);

anovaDBT <- lm(Y ~ X, data= dataDBT)
summary(anovaDBT)
anova(anovaDBT) # Pelo Pvalue, as médias são diferentes (?)
with(dataDBT,tapply(Y,X,mean)) # mostra a média de cada população

## Será Realizado o Procedimento de Benjamini-Hochberg
# Ação 1 e 2
X12 <- rep(c("A1","A2"),each=36);
Y12 <- c(s1,s2);
dataDB12 <- data.frame(Y12,X12);
anovaDB12 <- aov(Y12 ~ X12, data= dataDB12)
summary(anovaDB12)
a12 <- anova(anovaDB12);
p12 <- a12$`Pr(>F)`;

# Ação 1 e 3
X13 <- rep(c("A1","A3"),each=36);
Y13 <- c(s1,s3);
dataDB13 <- data.frame(Y13,X13);
anovaDB13 <- aov(Y13 ~ X13, data= dataDB13)
summary(anovaDB13)
a13 <- anova(anovaDB13);
p13 <- a13$`Pr(>F)`;

# Ação 1 e 4
X14 <- rep(c("A1","A4"),each=36);
Y14 <- c(s1,s4);
dataDB14 <- data.frame(Y14,X14);
anovaDB14 <- aov(Y14 ~ X14, data= dataDB14)
summary(anovaDB14)
a14 <- anova(anovaDB14);
p14 <- a14$`Pr(>F)`;

# Ação 1 e 5
X15 <- rep(c("A1","A5"),each=36);
Y15 <- c(s1,s5);
dataDB15 <- data.frame(Y15,X15);
anovaDB15 <- aov(Y15 ~ X15, data= dataDB15)
summary(anovaDB15)
a15 <- anova(anovaDB15);
p15 <- a15$`Pr(>F)`;

# Ação 2 e 3
X23 <- rep(c("A2","A3"),each=36);
Y23 <- c(s2,s3);
dataDB23 <- data.frame(Y23,X23);
anovaDB23 <- aov(Y23 ~ X23, data= dataDB23)
summary(anovaDB23)
a23 <- anova(anovaDB23);
p23 <- a23$`Pr(>F)`;

# Ação 2 e 4
X24 <- rep(c("A2","A4"),each=36);
Y24 <- c(s2,s4);
dataDB24 <- data.frame(Y24,X24);
anovaDB24 <- aov(Y24 ~ X24, data= dataDB24)
summary(anovaDB24)
a24 <- anova(anovaDB24);
p24 <- a24$`Pr(>F)`;

# Ação 2 e 5
X25 <- rep(c("A2","A5"),each=36);
Y25 <- c(s2,s5);
dataDB25 <- data.frame(Y25,X25);
anovaDB25 <- aov(Y25 ~ X25, data= dataDB25)
summary(anovaDB25)
a25 <- anova(anovaDB25);
p25 <- a25$`Pr(>F)`;

# Ação 3 e 4
X34 <- rep(c("A3","A4"),each=36);
Y34 <- c(s3,s4);
dataDB34 <- data.frame(Y34,X34);
anovaDB34 <- aov(Y34 ~ X34, data= dataDB34)
summary(anovaDB34)
a34 <- anova(anovaDB34);
p34 <- a34$`Pr(>F)`;

# Ação 3 e 5
X35 <- rep(c("A3","A5"),each=36);
Y35 <- c(s3,s5);
dataDB35 <- data.frame(Y35,X35);
anovaDB35 <- aov(Y35 ~ X35, data= dataDB35)
summary(anovaDB35)
a35 <- anova(anovaDB35);
p35 <- a35$`Pr(>F)`;

# Ação 4 e 5
X45 <- rep(c("A4","A5"),each=36);
Y45 <- c(s4,s5);
dataDB45 <- data.frame(Y45,X45);
anovaDB45 <- aov(Y45 ~ X45, data= dataDB45)
summary(anovaDB45)
a45 <- anova(anovaDB45);
p45 <- a45$`Pr(>F)`;

# Método B-H (Dados Brutos)
PDB <- sort(c(p12,p13,p14,p15,p23,p24,p25,p34,p35,p45))
alfaDB <- 1:10*(0.05/10)
BHDB <- 0
for(i in 1:10){ 
  BHDB[i] <- (PDB[i] < alfaDB[i]) 
}
# Rejeitar todas as hipóteses antes do primeiro FALSE


#####################################################
# 5 Ações (Variação Percentual)

Xp <- rep(c("A1","A2","A3","A4","A5"),each=35);
Yp <- c(sp1,sp2,sp3,sp4,sp5);
dataDp <- data.frame(Yp,Xp);

anovaDp <- lm(Yp ~ Xp, data= dataDp)
summary(anovaDp)
anova(anovaDp) # Pelo Pvalue, as médias são diferentes (?)
with(dataDp,tapply(Yp,Xp,mean)) # mostra a média de cada população

## Será Realizado o Procedimento de Benjamini-Hochberg
# Ação 1 e 2
Xp12 <- rep(c("A1","A2"),each=35);
Yp12 <- c(sp1,sp2);
dataDp12 <- data.frame(Yp12,Xp12);
anovaDp12 <- aov(Yp12 ~ Xp12, data= dataDp12)
summary(anovaDp12)
ap12 <- anova(anovaDp12);
pp12 <- ap12$`Pr(>F)`;

# Ação 1 e 3
Xp13 <- rep(c("A1","A3"),each=35);
Yp13 <- c(sp1,sp3);
dataDp13 <- data.frame(Y13,X13);
anovaDp13 <- aov(Yp13 ~ Xp13, data= dataDp13)
summary(anovaDp13)
ap13 <- anova(anovaDp13);
pp13 <- ap13$`Pr(>F)`;

# Ação 1 e 4
Xp14 <- rep(c("A1","A4"),each=35);
Yp14 <- c(sp1,sp4);
dataDp14 <- data.frame(Yp14,Xp14);
anovaDp14 <- aov(Yp14 ~ Xp14, data= dataDp14)
summary(anovaDp14)
ap14 <- anova(anovaDp14);
pp14 <- ap14$`Pr(>F)`;

# Ação 1 e 5
Xp15 <- rep(c("A1","A5"),each=35);
Yp15 <- c(sp1,sp5);
dataDp15 <- data.frame(Yp15,Xp15);
anovaDp15 <- aov(Yp15 ~ Xp15, data= dataDp15)
summary(anovaDp15)
ap15 <- anova(anovaDp15);
pp15 <- ap15$`Pr(>F)`;

# Ação 2 e 3
Xp23 <- rep(c("A2","A3"),each=35);
Yp23 <- c(sp2,sp3);
dataDp23 <- data.frame(Yp23,Xp23);
anovaDp23 <- aov(Yp23 ~ Xp23, data= dataDp23)
summary(anovaDp23)
ap23 <- anova(anovaDp23);
pp23 <- ap23$`Pr(>F)`;

# Ação 2 e 4
Xp24 <- rep(c("A2","A4"),each=35);
Yp24 <- c(sp2,sp4);
dataDp24 <- data.frame(Yp24,Xp24);
anovaDp24 <- aov(Yp24 ~ Xp24, data= dataDp24)
summary(anovaDp24)
ap24 <- anova(anovaDp24);
pp24 <- ap24$`Pr(>F)`;

# Ação 2 e 5
Xp25 <- rep(c("A2","A5"),each=35);
Yp25 <- c(sp2,sp5);
dataDp25 <- data.frame(Yp25,Xp25);
anovaDp25 <- aov(Yp25 ~ Xp25, data= dataDp25)
summary(anovaDp25)
ap25 <- anova(anovaDp25);
pp25 <- ap25$`Pr(>F)`;

# Ação 3 e 4
Xp34 <- rep(c("A3","A4"),each=35);
Yp34 <- c(sp3,sp4);
dataDp34 <- data.frame(Yp34,Xp34);
anovaDp34 <- aov(Yp34 ~ Xp34, data= dataDp34)
summary(anovaDp34)
ap34 <- anova(anovaDp34);
pp34 <- ap34$`Pr(>F)`;

# Ação 3 e 5
Xp35 <- rep(c("A3","A5"),each=35);
Yp35 <- c(sp3,sp5);
dataDp35 <- data.frame(Yp35,Xp35);
anovaDp35 <- aov(Yp35 ~ Xp35, data= dataDp35)
summary(anovaDp35)
ap35 <- anova(anovaDp35);
pp35 <- ap35$`Pr(>F)`;

# Ação 4 e 5
Xp45 <- rep(c("A4","A5"),each=35);
Yp45 <- c(sp4,sp5);
dataDp45 <- data.frame(Yp45,Xp45);
anovaDp45 <- aov(Yp45 ~ Xp45, data= dataDp45)
summary(anovaDp45)
ap45 <- anova(anovaDp45);
pp45 <- ap45$`Pr(>F)`;

# Método B-H (Dados Brutos)
PDp <- sort(c(pp12,pp13,pp14,pp15,pp23,pp24,pp25,pp34,pp35,pp45))
alfaDp <- 1:10*(0.05/10)
BHDp <- 0
for(i in 1:10){ 
  BHDp[i] <- (PDp[i] < alfaDp[i]) 
}
# Resultou em médias iguais para 1-3 e 4-5
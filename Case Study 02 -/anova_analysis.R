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
raw_data <- read.csv('data/DadosAcoesGrupoA.csv', sep=',', header = FALSE)

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

total_months = length(sp1)

sp1_long <- sp1[13:total_months]
sp2_long <- sp2[13:total_months]
sp3_long <- sp3[13:total_months]
sp4_long <- sp4[13:total_months]
sp5_long <- sp5[13:total_months]

sp1_short <- sp1[(total_months-5):total_months]
sp2_short <- sp2[(total_months-5):total_months]
sp3_short <- sp3[(total_months-5):total_months]
sp4_short <- sp4[(total_months-5):total_months]
sp5_short <- sp5[(total_months-5):total_months]

################################
######### Análise Anova ########
################################

# 5 Ações Variação Percentual Curto Prazo

Xp <- rep(c("A1","A2","A3","A4","A5"),each=length(sp1_short));
Yp <- c(sp1_short,sp2_short,sp3_short,sp4_short,sp5_short);
dataDp <- data.frame(Yp,Xp);

anovaDp <- lm(Yp ~ Xp, data= dataDp)
summary(anovaDp)
anova(anovaDp) # Pelo Pvalue as médias são diferentes
with(dataDp,tapply(Yp,Xp,mean)) # mostra a média de cada população

## Será Realizado o Procedimento de Benjamini-Hochberg
# Ação 1 e 2
Xp12 <- rep(c("A1","A2"),each=length(sp1_short));
Yp12 <- c(sp1_short,sp2_short);
dataDp12 <- data.frame(Yp12,Xp12);
anovaDp12 <- aov(Yp12 ~ Xp12, data= dataDp12)
summary(anovaDp12)
ap12 <- anova(anovaDp12);
pp12 <- ap12$`Pr(>F)`;

# Ação 1 e 3
Xp13 <- rep(c("A1","A3"),each=length(sp1_short));
Yp13 <- c(sp1_short,sp3_short);
dataDp13 <- data.frame(Yp13,Xp13);
anovaDp13 <- aov(Yp13 ~ Xp13, data= dataDp13)
summary(anovaDp13)
ap13 <- anova(anovaDp13);
pp13 <- ap13$`Pr(>F)`;

# Ação 1 e 4
Xp14 <- rep(c("A1","A4"),each=length(sp1_short));
Yp14 <- c(sp1_short,sp4_short);
dataDp14 <- data.frame(Yp14,Xp14);
anovaDp14 <- aov(Yp14 ~ Xp14, data= dataDp14)
summary(anovaDp14)
ap14 <- anova(anovaDp14);
pp14 <- ap14$`Pr(>F)`;

# Ação 1 e 5
Xp15 <- rep(c("A1","A5"),each=length(sp1_short));
Yp15 <- c(sp1_short,sp5_short);
dataDp15 <- data.frame(Yp15,Xp15);
anovaDp15 <- aov(Yp15 ~ Xp15, data= dataDp15)
summary(anovaDp15)
ap15 <- anova(anovaDp15);
pp15 <- ap15$`Pr(>F)`;

# Ação 2 e 3
Xp23 <- rep(c("A2","A3"),each=length(sp1_short));
Yp23 <- c(sp2_short,sp3_short);
dataDp23 <- data.frame(Yp23,Xp23);
anovaDp23 <- aov(Yp23 ~ Xp23, data= dataDp23)
summary(anovaDp23)
ap23 <- anova(anovaDp23);
pp23 <- ap23$`Pr(>F)`;

# Ação 2 e 4
Xp24 <- rep(c("A2","A4"),each=length(sp1_short));
Yp24 <- c(sp2_short,sp4_short);
dataDp24 <- data.frame(Yp24,Xp24);
anovaDp24 <- aov(Yp24 ~ Xp24, data= dataDp24)
summary(anovaDp24)
ap24 <- anova(anovaDp24);
pp24 <- ap24$`Pr(>F)`;

# Ação 2 e 5
Xp25 <- rep(c("A2","A5"),each=length(sp1_short));
Yp25 <- c(sp2_short,sp5_short);
dataDp25 <- data.frame(Yp25,Xp25);
anovaDp25 <- aov(Yp25 ~ Xp25, data= dataDp25)
summary(anovaDp25)
ap25 <- anova(anovaDp25);
pp25 <- ap25$`Pr(>F)`;

# Ação 3 e 4
Xp34 <- rep(c("A3","A4"),each=length(sp1_short));
Yp34 <- c(sp3_short,sp4_short);
dataDp34 <- data.frame(Yp34,Xp34);
anovaDp34 <- aov(Yp34 ~ Xp34, data= dataDp34)
summary(anovaDp34)
ap34 <- anova(anovaDp34);
pp34 <- ap34$`Pr(>F)`;

# Ação 3 e 5
Xp35 <- rep(c("A3","A5"),each=length(sp1_short));
Yp35 <- c(sp3_short,sp5_short);
dataDp35 <- data.frame(Yp35,Xp35);
anovaDp35 <- aov(Yp35 ~ Xp35, data= dataDp35)
summary(anovaDp35)
ap35 <- anova(anovaDp35);
pp35 <- ap35$`Pr(>F)`;

# Ação 4 e 5
Xp45 <- rep(c("A4","A5"),each=length(sp1_short));
Yp45 <- c(sp4_short,sp5_short);
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
# Resultou em médias iguais para 2-5, 1-3, 2-4 e 4-5

####################################################
####################################################

# 5 Ações Variação Percentual Curto Prazo

Xp <- rep(c("A1","A2","A3","A4","A5"),each=length(sp1_long));
Yp <- c(sp1_long,sp2_long,sp3_long,sp4_long,sp5_long);
dataDp <- data.frame(Yp,Xp);

anovaDp <- lm(Yp ~ Xp, data= dataDp)
summary(anovaDp)
anova(anovaDp) # Pelo Pvalue, as médias são diferentes (?)
with(dataDp,tapply(Yp,Xp,mean)) # mostra a média de cada população

## Será Realizado o Procedimento de Benjamini-Hochberg
# Ação 1 e 2
Xp12 <- rep(c("A1","A2"),each=length(sp1_long));
Yp12 <- c(sp1_long,sp2_long);
dataDp12 <- data.frame(Yp12,Xp12);
anovaDp12 <- aov(Yp12 ~ Xp12, data= dataDp12)
summary(anovaDp12)
ap12 <- anova(anovaDp12);
pp12 <- ap12$`Pr(>F)`;

# Ação 1 e 3
Xp13 <- rep(c("A1","A3"),each=length(sp1_long));
Yp13 <- c(sp1_long,sp3_long);
dataDp13 <- data.frame(Yp13,Xp13);
anovaDp13 <- aov(Yp13 ~ Xp13, data= dataDp13)
summary(anovaDp13)
ap13 <- anova(anovaDp13);
pp13 <- ap13$`Pr(>F)`;

# Ação 1 e 4
Xp14 <- rep(c("A1","A4"),each=length(sp1_long));
Yp14 <- c(sp1_long,sp4_long);
dataDp14 <- data.frame(Yp14,Xp14);
anovaDp14 <- aov(Yp14 ~ Xp14, data= dataDp14)
summary(anovaDp14)
ap14 <- anova(anovaDp14);
pp14 <- ap14$`Pr(>F)`;

# Ação 1 e 5
Xp15 <- rep(c("A1","A5"),each=length(sp1_long));
Yp15 <- c(sp1_long,sp5_long);
dataDp15 <- data.frame(Yp15,Xp15);
anovaDp15 <- aov(Yp15 ~ Xp15, data= dataDp15)
summary(anovaDp15)
ap15 <- anova(anovaDp15);
pp15 <- ap15$`Pr(>F)`;

# Ação 2 e 3
Xp23 <- rep(c("A2","A3"),each=length(sp1_long));
Yp23 <- c(sp2_long,sp3_long);
dataDp23 <- data.frame(Yp23,Xp23);
anovaDp23 <- aov(Yp23 ~ Xp23, data= dataDp23)
summary(anovaDp23)
ap23 <- anova(anovaDp23);
pp23 <- ap23$`Pr(>F)`;

# Ação 2 e 4
Xp24 <- rep(c("A2","A4"),each=length(sp1_long));
Yp24 <- c(sp2_long,sp4_long);
dataDp24 <- data.frame(Yp24,Xp24);
anovaDp24 <- aov(Yp24 ~ Xp24, data= dataDp24)
summary(anovaDp24)
ap24 <- anova(anovaDp24);
pp24 <- ap24$`Pr(>F)`;

# Ação 2 e 5
Xp25 <- rep(c("A2","A5"),each=length(sp1_long));
Yp25 <- c(sp2_long,sp5_long);
dataDp25 <- data.frame(Yp25,Xp25);
anovaDp25 <- aov(Yp25 ~ Xp25, data= dataDp25)
summary(anovaDp25)
ap25 <- anova(anovaDp25);
pp25 <- ap25$`Pr(>F)`;

# Ação 3 e 4
Xp34 <- rep(c("A3","A4"),each=length(sp1_long));
Yp34 <- c(sp3_long,sp4_long);
dataDp34 <- data.frame(Yp34,Xp34);
anovaDp34 <- aov(Yp34 ~ Xp34, data= dataDp34)
summary(anovaDp34)
ap34 <- anova(anovaDp34);
pp34 <- ap34$`Pr(>F)`;

# Ação 3 e 5
Xp35 <- rep(c("A3","A5"),each=length(sp1_long));
Yp35 <- c(sp3_long,sp5_long);
dataDp35 <- data.frame(Yp35,Xp35);
anovaDp35 <- aov(Yp35 ~ Xp35, data= dataDp35)
summary(anovaDp35)
ap35 <- anova(anovaDp35);
pp35 <- ap35$`Pr(>F)`;

# Ação 4 e 5
Xp45 <- rep(c("A4","A5"),each=length(sp1_long));
Yp45 <- c(sp4_long,sp5_long);
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

# Resultou em médias iguais para 4-5, 1-3, 2-4 e 2-5



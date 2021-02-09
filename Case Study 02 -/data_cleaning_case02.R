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



# Plot time series (fixed axis)
plot(1:36,s1,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250))
plot(1:36,s2,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250))
plot(1:36,s3,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250))
plot(1:36,s4,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250))
plot(1:36,s5,xlab = 'Mês',ylab = 'Valor da Ação',xlim = c(0,36),ylim = c(0,250))


# Plot time series (automatic axis)
plot(1:36,s1,xlab = 'Mês',ylab = 'Valor da Ação')
plot(1:36,s2,xlab = 'Mês',ylab = 'Valor da Ação')
plot(1:36,s3,xlab = 'Mês',ylab = 'Valor da Ação')
plot(1:36,s4,xlab = 'Mês',ylab = 'Valor da Ação')
plot(1:36,s5,xlab = 'Mês',ylab = 'Valor da Ação')

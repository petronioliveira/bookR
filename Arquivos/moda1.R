# Fun��o Moda
# Calcula a moda
# Fun��o criada pelo Professor Petr�nio Fagundes de Oliveira Filho
# Criada em R 4.1.0

# Criar a fun��o:

moda <- function(x) {
  z <- table(as.vector(x))
  names(z)[z == max(z)]
}

# Para usar a fun��o bastas baixar os dados e coloc�-los no objeto x.

x <- c(6, 8,  6,  7, 11,  8,  6,  7,  4,  5,  8,  4,  8,  8, 11)

# Para ativar a fun��o :

# source ("C:/Users/T.S/Dropbox/Estat�stica/Estat�stica R/Fun��es/moda.R")

# moda (x)

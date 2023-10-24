# Função Moda
# Calcula a moda
# Função criada pelo Professor Petrônio Fagundes de Oliveira Filho
# Criada em R 4.1.0

# Criar a função:

moda <- function(x) {
  valor.unico <- unique(x)
valor.unico[which.max(tabulate(match(x, valor.unico)))]
}

# Para usar a função bastas baixar os dados e colocá-los no objeto x.

x <- c(6, 8,  6,  7, 11,  8,  6,  7,  4,  5,  8,  4,  8,  8, 11)

# Para ativar a fun??o :

# source ("C:/Users/T.S/Dropbox/Estat?stica/Estat?stica R/Fun??es/moda.R")

# moda (x)

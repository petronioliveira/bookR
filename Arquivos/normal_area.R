####################################################################
# Função sombrear área curva normal                                #
# Faz o sobreamento da abaixo da curva normal (P<=x)               #
# Função criada pelo Professor Petrônio Fagundes de Oliveira Filho #
# Criada em R 4.2.1                                                #
# ##################################################################
# media: média da variável Normal
# dp: desvio padrão da variável Normal
# linf: limite inferior da área
# lsup: limite superior da área
# cor: cor da área
# ...: argumentos adicionais a serem passados para a função "lines"
# ##################################################################

normal_area <- function(media = 0, dp = 1, linf, lsup, cor = "lightgray", ...) {
  x <- seq(media - 3 * dp, media + 3 * dp, length = 100) 
  
  if (missing(linf)) {
    lb <- min(x)
  }
  if (missing(lsup)) {
    ub <- max(x)
  }
  
  x2 <- seq(linf, lsup, length = 100)    
  plot(x, dnorm(x, media, dp), type = "n", ylab = "")
  
  y <- dnorm(x2, media, dp)
  polygon(c(linf, x2, lsup), c(0, y, 0), col = cor)
  lines(x, dnorm(x, media, dp), type = "l", ...)
}


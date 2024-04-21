x <- 1:500
y <- cumsum (sample(0:1,500,rep=TRUE))

plot(x,
     y/1:500, 
     ylab = "Probabilidade",
     xlab = "Nº de sorteios",
     ylim=c(0,1), 
     xlim=c(0, 500), 
     pch=20, 
     col = "red")

abline (h = 0.50, lty = 2)

# ou
# https://digfir-published.macmillanusa.com/stats_applet/stats_applet_10_prob.html
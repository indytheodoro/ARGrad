restaurante <- function(amostras){
  
  numeroDeMesas <- round(rtriangle(amostras, 40, 120, 60))

  for(i in 1:amostras) {
    valorPorMesa <- rtriangle(numeroDeMesas[i], 90, 250, 130)
    lucroPorMesa <- rtriangle(numeroDeMesas[i], 0.15, 0.30, 0.22)
    lucroDiario[i] <- sum(valorPorMesa*lucroPorMesa)
  }
  
  print(c("Média lucro diário: ", mean(lucroDiario)))
  print(c("Variância lucro diário: ", var(lucroDiario)))
  
  mediaMensal <- 22*mean(lucroDiario)
  varMensal <- 22*var(lucroDiario)
  
  print(c("Média lucro mensal: ", mediaMensal))
  print(c("Variância lucro mensal: ", varMensal))
  
  s<-1:12
  dv<-(1.0/(1.012))^s
  
  mediaAnualDescontada <- sum(mediaMensal*dv)
  varianciaAnualDescontada <- sum(varMensal*(dv^2))
  
  print(c("Média lucro anual: ", mediaAnualDescontada))
  print(c("Variância lucro anual: ", varianciaAnualDescontada))
  
  eixoX = seq(mediaAnualDescontada - 3*sqrt(varianciaAnualDescontada),mediaAnualDescontada + 3*sqrt(varianciaAnualDescontada),0.1)
  eixoY = dnorm(eixoX, mediaAnualDescontada, sqrt(varianciaAnualDescontada))
  plot(eixoX, eixoY, type="l")
  
}

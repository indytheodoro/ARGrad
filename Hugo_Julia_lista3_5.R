taxi<-function(amostras){
  # MODELO 1: Simulacao do gasto diário de cada taxi
  frota<-20
  consumoIndividual<-rtriangle(frota, 40, 60, 58)
  custoIndividual<-rtriangle(frota, 3.1, 4.0, 3.8) 
  
  custoCadaTaxi<-consumoIndividual*custoIndividual
  print(custoCadaTaxi)
  #hist(custoCadaTaxi, main = "Custo Individual")
  
  mediaIndividual<-mean(custoCadaTaxi)
  varIndividual<-var(custoCadaTaxi)
  
  eixoX1 <- seq(mediaIndividual-3*sqrt(varIndividual), mediaIndividual+3*sqrt(varIndividual), 0.1)
  eixoY1 <- dnorm(eixoX1, mediaIndividual, sqrt(varIndividual))
  plot(eixoX1, eixoY1, type="l", main="Risco Individual")

  # MODELO 2: Simulacao do gasto diário com TCL
  consumoGasolinaTCL<-rtriangle(amostras, 40, 60, 58)
  custoGasolinaTCL<-rtriangle(amostras, 3.1, 4.0, 3.8) 
  
  custoCadaTaxiTCL<-consumoGasolinaTCL*custoGasolinaTCL
  mediaTCL<-20*mean(custoCadaTaxiTCL)
  varTCL<-20*var(custoCadaTaxiTCL)
  
  eixoX2 <- seq(mediaTCL-3*sqrt(varTCL), mediaTCL+3*sqrt(varTCL), 0.1)
  eixoY2 <- dnorm(eixoX2, mediaTCL, sqrt(varTCL))
  plot(eixoX2, eixoY2, type="l", main="Risco TCL")  

  # Modelo 3: Simulacao produto das VA's por TCL

  consumoGasolinaTCL<-rtriangle(amostras, 40, 60, 58)
  custoGasolinaTCL<-rtriangle(amostras, 3.1, 4.0, 3.8) 
  
  mediaCadaTaxiTCL<-mean(consumoGasolinaTCL)*mean(custoGasolinaTCL)
  varCadaTaxiTCL<-(mean(consumoGasolinaTCL)**2)*var(custoGasolinaTCL) + (mean(custoGasolinaTCL)**2)*var(consumoGasolinaTCL)

  mediaCustoTCL<-20*mediaCadaTaxiTCL
  varCustoTCL<-20*varCadaTaxiTCL
  
  eixoX3 <- seq(mediaTCL-3*sqrt(varCustoTCL), mediaTCL+3*sqrt(varCustoTCL), 0.1)
  eixoY3 <- dnorm(eixoX3, mediaCustoTCL, sqrt(varCustoTCL))
  plot(eixoX3, eixoY3, type="l", main="Risco TCL VAs")  
  
}

navio<-function(amostras){
  placas<-562
  
  tempoRebitador<-rtriangle(amostras, 3.45, 5.3, 4.15) 
  custoRebitador<-7.5
  
  risco<-tempoRebitador*custoRebitador*placas
  
  mediaRisco<-mean(risco)
  varRisco<-var(risco)
  
  eixoX <- seq(mediaRisco-3*sqrt(varRisco), mediaRisco+3*sqrt(varRisco), 0.1)
  eixoY <- dnorm(eixoX, mediaRisco, sqrt(varRisco))
  plot(eixoX, eixoY, type="l", main="Risco Rebitagem")
  
}

almoco<-function(amostras){
  executivos<-rtriangle(amostras, 16, 22, 18)
  consumoPorExecutivo<-rtriangle(amostras, 25, 36, 28)
  sextas<-rtriangle(amostras, 40, 42, 41)
  
  custoTotal<-executivos*consumoPorExecutivo*sextas
  
  mediaCusto<-mean(custoTotal)
  varCusto<-var(custoTotal)
  
  print("A média do custo total é:")
  print(mediaCusto)
  
  print("A variança do custo total é:")
  print(varCusto)
  
  eixoX <- seq(mediaCusto-3*sqrt(varCusto), mediaCusto+3*sqrt(varCusto), 0.1)
  eixoY <- dnorm(eixoX, mediaCusto, sqrt(varCusto))
  plot(eixoX, eixoY, type="l", main="Risco Almoço")
  
}

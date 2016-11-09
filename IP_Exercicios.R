################## Lista 4 ####################

#Biblioteca
library(triangle)

### Exercício 1 ###

taxi<-function(amostras){
  
  #Fixo
  frota<-20
  
  #Variaveis para todos as simulacoes
  consumoG<-rtriangle(amostras, 40, 60, 58)
  custoG<-rtriangle(amostras, 3.1, 4.0, 3.8) 
  
  gastoTaxi<-consumoG*custoG
  
  ## Simulacao do gasto individual por taxi ##
  mediaI<-mean(gastoTaxi)
  varI<-var(gastoTaxi)
  eixoX1<-seq(mediaI-3*sqrt(varI), mediaI+3*sqrt(varI), 0.1)
  eixoY1<-dnorm(eixoX1, mediaI, sqrt(varI))
  plot(eixoX1, eixoY1, type="l", main="Gasto Individual por Taxi")
  
  ## Simulacao do gasto diário pela Frota com TCL ##
  mediaD<-frota*mean(gastoTaxi)
  varD<-frota*var(gastoTaxi)
  eixoX2<-seq(mediaD-3*sqrt(varD), mediaD+3*sqrt(varD), 0.1)
  eixoY2<-dnorm(eixoX2, mediaD, sqrt(varD))
  plot(eixoX2, eixoY2, type="l", main="Gasto Diário pela Frota")  
  
  
  ## Simulacao produto do gasto da das 2 VA's por TCL
  mediaTaxi<-mean(consumoG)*mean(custoG)
  varTaxi<-(mean(consumoG)**2)*var(custoG) + (mean(custoG)**2)*var(consumoG)
  
  mediaGasto<-frota*mediaTaxi
  varGasto<-frota*varTaxi
  
  eixoX3<-seq(mediaGasto-3*sqrt(varGasto), mediaGasto+3*sqrt(varGasto), 0.1)
  eixoY3<-dnorm(eixoX3, mediaGasto, sqrt(varGasto))
  plot(eixoX3, eixoY3, type="l", main="Gasto Diário pela Frota 2 VA's")  
  
}

### Exercício 2 ###

navio<-function(amostras){
  
  #Fixos
  placas<-562
  custoReb<-7.5
  
  #Variavel
  tempoReb<-rtriangle(amostras, 3.45, 5.3, 4.15) 
  
  #Solicitado na questão, TCL
  riscoMO<-tempoReb*custoReb*placas
  mRiscoMO<-mean(riscoMO)
  vRiscoMO<-var(riscoMO)
  
  #Main
  print(mRiscoMO)
  print(vRiscoMO)
  eixoX<-seq(mRiscoMO-3*sqrt(vRiscoMO), mRiscoMO+3*sqrt(vRiscoMO), 0.1)
  eixoY<-dnorm(eixoX, mRiscoMO, sqrt(vRiscoMO))
  plot(eixoX, eixoY, type="l", main="Risco Mao de Obra Rebitagem")
  
}

### Exercício 3 ###

almoco<-function(amostras){
  
  #Variaveis_Distribuicoes
  clientes<-rtriangle(amostras, 16, 22, 18)
  consumoClientes<-rtriangle(amostras, 25, 36, 28)
  sextas<-rtriangle(amostras, 40, 42, 41)
  
  #Solicitacao da questao
  custo<-clientes*consumoClientes*sextas
  mediaC<-mean(custo)
  varC<-var(custo)
  
  #Main
  print(mediaC)
  print(varC) 
  eixoX<-seq(mediaC-3*sqrt(varC), mediaC+3*sqrt(varC), 0.1)
  eixoY<-dnorm(eixoX, mediaC, sqrt(varC))
  plot(eixoX, eixoY, type="l", main="TOTAL GASTO DA NEW ENGLAND")
}

 

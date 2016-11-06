# Rota de 260 ou 290
# 290 com 35% a 40% de chance
# Tubulacao de 8m 

custo<-function(amostras){
  
  # Estimativas de custo
  tubulacao<-rtriangle(amostras, 725, 790, 740)
  tempoVala<-rtriangle(amostras, 12, 25, 16)
  custoHora<-rtriangle(amostras, 17, 23, 18.5)
  transporteTubulacao<-rtriangle(amostras, 6.1, 7.4, 6.6)
  tempoSoldagem<-rtriangle(amostras, 4, 5, 4.5)
  custoFiltragem<-rtriangle(amostras, 165000,188000,173000)
  custoAcabamento<-rtriangle(amostras, 14000,17000,15000)
  
  # Probabilidade de precisar da rota alternativa
  probRotaAlternativa<-rbinom(amostras, 1, 0.4)
  comprimentoRota<-probRotaAlternativa*30000+260000
  # hist(comprimentoRota)
  
  # Numero de tubos necessarios
  nTubos<-comprimentoRota/8
  
  # Calculo do custo do material
  custoMaterial<-nTubos*tubulacao
  
  # Calculo do custo de mao de obra
  custoValas<-tempoVala*custoHora
  custoSoldas<-(nTubos-1)*tempoSoldagem*custoHora
  custoMaoDeObra<-custoValas+custoSoldas
  
  # Calculo do custo de mão de serviço 
  custoAcab<-custoAcabamento*(comprimentoRota/1000)
  custoTransporte<-transporteTubulacao*nTubos
  custoMaoDeServico<-custoFiltragem+custoAcab+custoTransporte
  
  # Custo Total
  custoTotal<-custoMaterial+custoMaoDeObra+custoMaoDeServico
  print(custoTotal)
}

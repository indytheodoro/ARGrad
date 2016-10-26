#Lista 3


restaurante<-function(amostras){
  
  #library
  library(triangle)
  
  #variáveis
  nmesas<-rtriangle(amostras,40,120,60)
  lucratividaded<-vector(length = amostras)
  
  #Lucratividade diaria
  LD<-function(){
    for(i in 1:amostras){
      fatd<-rtriangle(nmesas[i],90, 250, 130)
      lucrod<-rtriangle(nmesas[i], 0.15,0.30,0.22)
      lucratividaded[i]<-sum(fatd*lucrod)
    }  
    lucratividaded
  }
  
  #Lucratividade mensal
  LM<-function(){
    mediam<-22*(mean(LD()))
    varm<-22*(var(LD()))
    retorno<-list(mediam, varm)
  }
  
  #Lucratividade anual
  LA<-function(){
    mes<-1:12
    desc<-(1/1.012)^mes
    medA<-sum(LM()[[1]]*desc)
    varA<-sum(LM()[[2]]*desc^2)
    lucroa<-list(medA,sqrt(varA))
  }
  
  #MAIN
  minX<-LA()[[1]]-3*LA()[[2]]
  maxX<-LA()[[1]]+3*LA()[[2]]
  x<-seq(from = minX, to = maxX, length.out = 100)
  y<-dnorm(x, LA()[[1]], LA()[[2]])
  plot(x,y, type = "l")
  
}

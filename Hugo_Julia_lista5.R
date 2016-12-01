library("triangle")

calcula_duracao<-function(NS){
  durN<-matrix(nrow=NS, ncol=6)
  
  durN[,1]<-rtriangle(NS,4,5,4.3)
  durN[,2]<-rtriangle(NS,9,12,10.9)
  durN[,3]<-rtriangle(NS,1.5,3.2,2.2)
  durN[,4]<-rtriangle(NS,5,7.5,6.7)
  durN[,5]<-rtriangle(NS,15,18,16.7)
  durN[,6]<-rtriangle(NS,6.5,9,7.6)
  
  durN
}

calcula_impacto<-function(NS){
  CT<-matrix(nrow=NS, ncol=6)
  
  vetor_probabilidades<-c(0.125,0.3125,0.25,0.1875,0.125)
  
  CT[,1]<-sample(c(1.4,1.28,1.0,1.0,0.98), size=NS, prob=vetor_probabilidades, replace=TRUE)
  CT[,2]<-sample(c(1.3,1.2,1.0,0.94,0.90), size=NS, prob=vetor_probabilidades, replace=TRUE)
  CT[,3]<-sample(c(1.1,1.04,1.0,1.0,0.97), size=NS, prob=vetor_probabilidades, replace=TRUE)
  CT[,4]<-sample(c(1.4,1.25,1.0,0.88,0.82), size=NS, prob=vetor_probabilidades, replace=TRUE)
  CT[,5]<-sample(c(1.1,1.04,1.0,1.0,0.98), size=NS, prob=vetor_probabilidades, replace=TRUE)
  CT[,6]<-sample(c(1.2,1.08,1.0,0.96,0.94), size=NS, prob=vetor_probabilidades, replace=TRUE)
  
  CT
}

calcula_tempo_cenario<-function(NS, durN, CT){
  tempoTotal<-vector(length = NS)
  
  for (i in 1:NS) {
    tempoTotal[i]<-sum(durN[i,]*CT[i,])
  }
  
  tempoTotal
}

main<-function(NS){
  duracao<-calcula_duracao(NS)
  impacto<-calcula_impacto(NS)
  
  tempo<-calcula_tempo_cenario(NS, duracao, impacto)
  hist(tempo)
  
  cumulativaTempo<-ecdf(tempo)
  plot(cumulativaTempo)
}
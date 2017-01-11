library("triangle")

#função para sortear quais projetos entraram
sorteiaChanceProjetos<-function(NS){
 vetorChances<-matrix(nrow=NS, ncol=8)
 
 vetorChances[,1]<-1
 vetorChances[,2]<-1
 vetorChances[,3]<-rbinom(NS, 1, 0.3)
 vetorChances[,4]<-rbinom(NS, 1, 0.4)
 vetorChances[,5]<-rbinom(NS, 1, 0.2)
 vetorChances[,6]<-rbinom(NS, 1, 0.4)
 vetorChances[,7]<-rbinom(NS, 1, 0.4)
 vetorChances[,8]<-rbinom(NS, 1, 0.5)
 
 vetorChances
}

#função para sortear os inicios
sorteiaInicio<-function(NS, chances){
  vetorInicio<-matrix(data = 0, nrow=NS, ncol=8)
  
  vetorInicio[,1]<-1
  vetorInicio[,2]<-1
  
  for(i in 1:NS){
    if(chances[i,3]!=0) vetorInicio[i,3]<-rtriangle(1, 2, 5, 3) 
    if(chances[i,4]!=0) vetorInicio[i,4]<-rtriangle(1, 1.5, 3, 2)
    if(chances[i,5]!=0) vetorInicio[i,5]<-rtriangle(1, 2, 4, 2)
    if(chances[i,6]!=0) vetorInicio[i,6]<-rtriangle(1, 2, 6, 3)
    if(chances[i,7]!=0) vetorInicio[i,7]<-rtriangle(1, 1, 3.5, 2)
    if(chances[i,8]!=0) vetorInicio[i,8]<-rtriangle(1, 2.5, 5, 3)
  }

  vetorInicio
}

#função para gerar as durações
sorteiaDuracao<-function(NS, chances){
  vetorDuracao<-matrix(data = 0, nrow=NS, ncol=8)
  
  for(i in 1:NS){
    if(chances[i,1]!=0) vetorDuracao[i,1]<-rtriangle(1, 2, 5, 3)
    if(chances[i,2]!=0) vetorDuracao[i,2]<-rtriangle(1, 6, 9, 7)
    if(chances[i,3]!=0) vetorDuracao[i,3]<-rtriangle(1, 4, 5, 4)
    if(chances[i,4]!=0) vetorDuracao[i,4]<-rtriangle(1, 2, 3.5, 2.5)
    if(chances[i,5]!=0) vetorDuracao[i,5]<-rtriangle(1, 3, 5, 3)
    if(chances[i,6]!=0) vetorDuracao[i,6]<-rtriangle(1, 2, 3, 2)
    if(chances[i,7]!=0) vetorDuracao[i,7]<-rtriangle(1, 2, 5, 3)
    if(chances[i,8]!=0) vetorDuracao[i,8]<-rtriangle(1, 3, 4.5, 3.5)
  }
  
  vetorDuracao
}

#função para gerar os faturamentos
sorteiaFaturamento<-function(NS){
  vetorFaturamento<-matrix(nrow=NS, ncol=8)
  
  vetorFaturamento[,1]<-rtriangle(NS, 400, 525, 425)
  vetorFaturamento[,2]<-rtriangle(NS, 375, 550, 480)
  vetorFaturamento[,3]<-rtriangle(NS, 525, 780, 600)
  vetorFaturamento[,4]<-rtriangle(NS, 625, 800, 700)
  vetorFaturamento[,5]<-rtriangle(NS, 250, 370, 300)
  vetorFaturamento[,6]<-rtriangle(NS, 425, 550, 475)
  vetorFaturamento[,7]<-rtriangle(NS, 480, 550, 500)
  vetorFaturamento[,8]<-rtriangle(NS, 550, 700, 600)
  
  vetorFaturamento
}

#função para gerar as margens
sorteiaMargem<-function(NS){
  vetorMargem<-matrix(nrow=NS, ncol=8)
  
  vetorMargem[,1]<-rtriangle(NS, .14, .21, .18)
  vetorMargem[,2]<-rtriangle(NS, .17, .26, .22)
  vetorMargem[,3]<-rtriangle(NS, .14, .21, .18)
  vetorMargem[,4]<-rtriangle(NS, .14, .21, .18)
  vetorMargem[,5]<-rtriangle(NS, .17, .26, .22)
  vetorMargem[,6]<-rtriangle(NS, .14, .21, .18)
  vetorMargem[,7]<-rtriangle(NS, .17, .26, .22)
  vetorMargem[,8]<-rtriangle(NS, .17, .26, .22)
  
  vetorMargem
}

calculaFaturamentoMensal<-function(NS, faturamento, inicio, duracao){
  # matrix de 12 meses
  fatMensal<-matrix(nrow=NS, ncol=12, data = 0)
  
  #i representa a i-ésima amostra
  for(i in 1:NS){
    #j representa o j-ésimo mês
    for(j in 1: ncol(fatMensal)){
      for(k in 1: 8){
        if((inicio[i,k] < j) && (inicio[i,k]+duracao[i,k] > j)){
          fatMensal[i,inicio[i,k]:(inicio[i,k]+duracao[i,k])] <- fatMensal[i,inicio[i,k]:(inicio[i,k]+duracao[i,k])] + faturamento[i,k]/duracao[i,k]
        }  
      }
    }
  }
  
  fatMensal
}

mediaFatMensal<-function(NS, fatMensal){
  sim_fatur<-vector(length = NS)

  for(i in 1: NS){
    sim_fatur[i]<-mean(fatMensal[i,])
  }
  
  sim_fatur
}

normalizadora<-function(vetor, cabecalho){
  
  media <- mean(vetor)
  dv <- sqrt(var(vetor))
  
  eixoX = seq(media - 3*dv,media + 3*dv,0.1)
  eixoY = dnorm(eixoX, media, dv)
  plot(eixoX, eixoY, type="l", main=cabecalho)
  
}

probabilidadeMensal<-function(NS, simMensal, title){
  for(i in 1:12){
    # hist(simMensal[,i], main=paste("Histograma de", title, "Mês:", i))
    fatMensalCDF<-ecdf(simMensal[,i])
    plot(fatMensalCDF, main=paste("Cumulativa de", title, "Mês:", i))
  }
}

calculaMargemMensal<-function(NS, faturamento, margem, inicio, duracao){
  margemMensal<-matrix(nrow=NS, ncol=12, data = 0)
  
  #i representa a i-ésima amostra
  for(i in 1:NS){
    #j representa o j-ésimo mês
    for(j in 1: ncol(margemMensal)){
      for(k in 1: 8){
        if((inicio[i,k] < j) && (inicio[i,k]+duracao[i,k] > j)){
          margemMensal[i,inicio[i,k]:(inicio[i,k]+duracao[i,k])] <- margemMensal[i,inicio[i,k]:(inicio[i,k]+duracao[i,k])] + faturamento[i,k]/duracao[i,k]*margem[i,k]
        }  
      }
    }
  }
  
  margemMensal
}

main<-function(){
  NS<-3000
  
  chances<-sorteiaChanceProjetos(NS)
  
  inicio<-sorteiaInicio(NS, chances)
  duracao<-sorteiaDuracao(NS, chances)
  
  faturamento<-sorteiaFaturamento(NS)
  margem<-sorteiaMargem(NS)
  
  sim_fatMensal<-calculaFaturamentoMensal(NS, faturamento, inicio, duracao)
  probabilidadeMensal(NS, sim_fatMensal, "Faturamento")
  
  sim_fatur<-mediaFatMensal(NS, sim_fatMensal)
  normalizadora(sim_fatur, "Faturamento")
  
  sim_margemMensal<-calculaMargemMensal(NS, sim_fatMensal, margem, inicio, duracao)
  probabilidadeMensal(NS, sim_margemMensal, "Margem")
}

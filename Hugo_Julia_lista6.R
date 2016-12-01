library("igraph")
library("triangle")
library("MASS")

#   Atividade 1: Projeto do sistema
#   Atividade 2: Projeto Modulo 1
#   Atividade 3: Codificacao Modulo 1
#   Atividade 4: Teste Modulo 1
#   Atividade 5: Projeto Modulo 2
#   Atividade 6: Codificacao Modulo 2
#   Atividade 7: Teste Modulo 2
#   Atividade 8: Projeto Modulo 3
#   Atividade 9: Codificacao Modulo 3
#   Atividade 10: Teste Modulo 3
#   Atividade 11: Projeto Modulo 4
#   Atividade 12: Codificacao Modulo 4
#   Atividade 13: Teste Modulo 4
#   Atividade 14: Integração
#   Atividade 15: Teste do sistema
#   Atividade 16: Teste de aceitação
#   Atividade 17: Retrabalho 1
#   Atividade 18: Teste de retrabalho 1
#   Atividade 19: Retrabalho 2
#   Atividade 20: Teste de retrabalho 2
#   Atividade 20: Fim

atividades<-make_graph(c(1,2, 2,3, 3,4, 4,14,
                         1,5, 6,7, 7,8, 8,14,
                         1,8, 8,9, 9,10, 10,14,
                         1,11, 11,12, 12,13, 13,14,
                         14,15, 15,16, 16,21,
                         16,17, 17,18, 18,21,
                         18,19, 19,20, 20,21))

plot(atividades)

#   Evento 1: Retrabalho 1
#   Evento 2: Retrabalho 2 

gera_eventos<-function(NS){
  e<-matrix(nrow=NS, ncol=2)
  
  e[,1]<-rbinom(NS, 1, 0.3)
  e[,2]<-rbinom(NS, 1, 0.05)
  
  e
}

gera_duracao<-function(NS){
  dur<-matrix(nrow = NS, ncol = 21)
  
  # Duracao da atividade 1 
  dur[,1]<-rtriangle(NS, 4, 7, 5)
  
  # Duracao das atividades correlacionadas 2 a 13
  matrixCorrelacao <- matrix(c(1,0.7,0,0.7,1,0.5,0,0.5,1), ncol=3)
  mu<-c(0,0,0) # DUVIDA AQUI: deveria ser um vetor de médias (?) <-------------------------------------------------------------------------------
  
  Z<-mvrnorm(NS, mu=rep(0,3), Sigma=matrixCorrelacao)
  U<-pnorm(Z)
  
    # Modulo 1
  dur[,2]<-qtriangle(U[,1], 2, 5, 2.5)
  dur[,3]<-qtriangle(U[,2], 4, 6.5, 5)
  dur[,4]<-qtriangle(U[,3], 1.5, 3, 2)
  
    # Modulo 2
  dur[,5]<-qtriangle(U[,1], 2, 5, 2.5)
  dur[,6]<-qtriangle(U[,2], 4, 6.5, 5)
  dur[,7]<-qtriangle(U[,3], 1.5, 3, 2)
  
    # Modulo 3
  dur[,8]<-qtriangle(U[,1], 2, 5, 2.5)
  dur[,9]<-qtriangle(U[,2], 4, 6.5, 5)
  dur[,10]<-qtriangle(U[,3], 1.5, 3, 2)
  
    # Modulo 4
  dur[,11]<-qtriangle(U[,1], 2, 5, 2.5)
  dur[,12]<-qtriangle(U[,2], 4, 6.5, 5)
  dur[,13]<-qtriangle(U[,3], 1.5, 3, 2)
  
  # Duracao das atividades 14 a 21
  dur[,14]<-rtriangle(NS, 4, 7, 5)
  dur[,15]<-rtriangle(NS, 2, 5, 2)
  dur[,16]<-rtriangle(NS, 1, 7, 1)
  dur[,17]<-rtriangle(NS, 2, 5, 3)
  dur[,18]<-rtriangle(NS, 1, 3, 1)
  dur[,19]<-rtriangle(NS, 1, 3, 1)
  dur[,20]<-rtriangle(NS, 1, 3, 1)
  dur[,21]<-0
  
  dur
}

gera_grafo_cenario<-function(i, cenario_evento){
  arestas<-c()
  
  if(cenario_evento[i,1] == 0){
    arestas <- c(arestas, 20, 21)
  }
  
  if(cenario_evento[i,2] == 0){
    arestas <- c(arestas, 23, 24)
  }
  
  grafoCenario<-delete_edges(atividades, arestas)
  
  grafoCenario
}

calcula_duracao<-function(NS, duracao, evento){
  vetorDuracaoCenarios<-vector(length = NS)
  
  for(i in 1:NS){
    durMaiorCaminho<-0
    
    grafoCenario<-gera_grafo_cenario(i, evento)
    caminhos<-all_simple_paths(grafoCenario,from=1,to=21)
    
    for(j in 1:length(caminhos)){
      durCaminhoAtual<-sum(duracao[i,][caminhos[[j]]])
      
      if(durCaminhoAtual > durMaiorCaminho){
        durMaiorCaminho<-durCaminhoAtual
      }
    }
    
    vetorDuracaoCenarios[i]<-durMaiorCaminho
  }
  
  vetorDuracaoCenarios
}

main<-function(NS){
  mEventos<-gera_eventos(NS)
  mDuracao<-gera_duracao(NS)
  
  duracao<-calcula_duracao(NS, mDuracao, mEventos)
  
  duracaoCDF<-ecdf(duracao)
  plot(duracaoCDF)
  
}
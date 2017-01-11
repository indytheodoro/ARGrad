library("igraph")
library("triangle")
library("MASS")

# Atividade 1: inicio do projeto
# Atividade 2: A
# Atividade 3: B
# Atividade 4: C
# Atividade 5: D
# Atividade 6: E
# Atividade 7: F
# Atividade 8: G
# Atividade 9: fim do projeto

atividades<-make_graph(c(1,2, 1,3, 1,4, 2,5, 2,7, 3,8, 4,6, 5,8, 6,8, 8,9))

plot(atividades)


#   Evento 1: Atividade 7 acontece

gera_eventos<-function(NS){
  e<-rbinom(NS, 1, 0.25)
  
  e
}

sorteia_custos<-function(NS){
  matriz_custos<-matrix(nrow=NS, ncol=9)
  
  matriz_custos[,1]<-0
  matriz_custos[,2]<-rtriangle(NS, 75, 110, 90)
  matriz_custos[,3]<-rtriangle(NS, 120, 190, 150)
  matriz_custos[,4]<-rtriangle(NS, 60, 85, 75)
  matriz_custos[,5]<-rtriangle(NS, 45, 80, 60)
  matriz_custos[,6]<-rtriangle(NS, 160, 240, 200)
  matriz_custos[,7]<-rtriangle(NS, 210, 300, 250)
  matriz_custos[,8]<-rtriangle(NS, 85, 140, 120)
  matriz_custos[,9]<-0
  
  matriz_custos
  
}

gera_duracao<-function(NS){
  dur<-matrix(nrow = NS, ncol = 9)
  
  # Duracao das atividades correlacionadas
  matrixCorrelacao <- matrix(c(1,0.4,0,0,0.4,1,0,0,0,0,1,0.7,0,0,0.7,1), ncol=4)
  
  Z<-mvrnorm(NS, mu=rep(0,4), Sigma=matrixCorrelacao)
  U<-pnorm(Z)
  
  dur[,1]<-0
  dur[,2]<-rtriangle(NS, 1, 4, 2)
  dur[,3]<-qtriangle(U[,1], 5, 7, 6)
  dur[,4]<-rtriangle(NS, 2, 5, 4)
  dur[,5]<-qtriangle(U[,2], 1, 4, 3)
  dur[,6]<-qtriangle(U[,3], 4, 7, 5)
  dur[,7]<-qtriangle(U[,4], 3, 5, 4)
  dur[,8]<-rtriangle(NS, 1, 3, 2)

  dur[,9]<-0
  
  dur
}

gera_grafo_cenario<-function(i, cenario_evento){
  arestas<-c()
  
  if(cenario_evento[i] == 0){
    arestas <- c(arestas, 2, 7)
  }
  
  grafoCenario<-delete_edges(atividades, arestas)
  
  grafoCenario
}

calcula_duracao<-function(NS, duracao, evento){
  vetorDuracaoCenarios<-vector(length = NS)
  
  for(i in 1:NS){
    durMaiorCaminho<-0
    
    grafoCenario<-gera_grafo_cenario(i, evento)
    caminhos<-all_simple_paths(grafoCenario,from=1,to=9)
    
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

custo_cenario<-function(cenario_custo, cenario_evento){
  custo<-vector()
  
  for(i in 1:nrow(cenario_custo)){
    #soma todos os custos
    custo[i]<-sum(cenario_custo[i,])
    
    #retira os custos que não aconteceram de acordo com a tabela de eventos
    if(cenario_evento[i] == 0){
      custo[i] = custo[i] - cenario_custo[i,7]
    }
  }
  
  custo  
}

normalizadora<-function(vetor, cabecalho){
  media <- mean(vetor)
  dv <- sqrt(var(vetor))
  
  eixoX = seq(media - 3*dv,media + 3*dv,0.1)
  eixoY = dnorm(eixoX, media, dv)
  plot(eixoX, eixoY, type="l", main=cabecalho)
}

preparaTornado<-function(cenario, cenario_evento){ 
  # Retorna vetor de matriz de custos com colunas zeradas
  
  for(i in 1:nrow(cenario)){
    if(cenario_evento[i] == 0){
      cenario[i,7] = 0
    }
  }
    cenario
}
  
geraTornado<-function(matrix_cenarios, resultados_cenarios, tituloGrafico){
  N_atividades<-9
  c<-vector(length=N_atividades)
  
  # Calcula correlação entre as colunas e o resultado final
  # Exceto nas colunas onde o desvio padrão é zero
  for(i in 1:N_atividades){
    if(sd(matrix_cenarios[,i]) != 0){
      c[i]<-cor(resultados_cenarios, matrix_cenarios[,i], method="spearman")
    }
    else{
      c[i]<-0
    }
  }
  
  # Normalizando o vetor de correlação
  c<-c/sum(c)
  
  # Matriz de entrada pro tornado [para cada item do vetor de correlacao, valorAbsoluto, sinal]
  m<-matrix(c(1:length(c), abs(c), ifelse(c>=0, 1, -1)), ncol=3)
  
  # Preparando plot do tornado
  o<-order(m[,2])
  sm<-m[o,][4:9,] # Cinco variáveis mais criticas
  yname<-sm[,1]
  barplot(sm[,2]*sm[,3], beside=TRUE, horiz=TRUE, xlim=c(-1,1),
          names.arg=yname, main = paste(tituloGrafico, "Analise de Sensibilidade"),
          xlab="Coeficiente de correlação normalizado", ylab="Item")
}

clusterizacao<-function(vetor_custo, vetor_duracao){
  matrix_custo_dur<-cbind(vetor_custo, vetor_duracao)
  
  clusters <- kmeans(matrix_custo_dur, 2)
  plot(matrix_custo_dur, col = clusters$cluster)
}  

main<-function(NS){
  mEventos<-gera_eventos(NS)
  mDuracao<-gera_duracao(NS)
  
  custo<-sorteia_custos(NS)
  
  sim_custo<-custo_cenario(custo, mEventos)
  normalizadora(sim_custo, "custo")
  custoCDF<-ecdf(sim_custo)
  plot(custoCDF)
  
  duracao<-calcula_duracao(NS, mDuracao, mEventos)
  
  hist(duracao)
  
  duracaoCDF<-ecdf(duracao)
  plot(duracaoCDF)
  
  # Relação entre as duas variáveis
  correlation<-cor(sim_custo, duracao, use = "everything", method = c("pearson", "kendall", "spearman"))
  
  # Tornado de custo
  custoTornado<-preparaTornado(custo, mEventos)
  geraTornado(custoTornado, sim_custo, "Custo")
  
  # Tornado de prazo
  duracaoTornado<-preparaTornado(mDuracao, mEventos)
  geraTornado(duracaoTornado, duracao, "Prazo")
  
  # Plota Custo vs Cenario
  clusterizacao(sim_custo, duracao)
  
}

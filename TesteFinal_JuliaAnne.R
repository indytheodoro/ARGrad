library("igraph")
library("triangle")
library("MASS")

geraGrafico<-function(){
  # Função de geração do gráfico
  
  # 1: Dummy de início
  # 2: Atividade A
  # 3: Atividade B
  # 4: Atividade C
  # 5: Atividade A
  # 6: Atividade E
  # 7: Atividade F
  # 8: Atividade G
  # 9: Dummy de fim
  
  grafico<-make_graph(c(1,2,1,3,1,4,2,5,2,7,3,8,4,6,5,8,6,8,7,9,8,9))
  
  plot(grafico)
  
  grafico
}

geraEvento<-function(NS){
  # Geração do evento da atividade F para cada cenario
  e<-matrix(nrow=NS, ncol=1)
  
  e[,1]<-rbinom(NS, 1, 0.25)
  
  e
}

geraCusto<-function(NS){
  # Geração de custo
  custo<-matrix(nrow=NS, ncol=9)
  
  # Gera matrix de correlações
                    #  E  F  B  D
  matrixCor<-matrix(c(1, .7, 0, 0,
                      .7, 1, 0, 0,
                      0, 0, 1, .4,
                      0, 0, .4, 1), 
                      ncol=4)
  mu<-c(0,0,0)
  
  Z<-mvrnorm(NS, mu=rep(0,4), Sigma=matrixCor)
  U<-pnorm(Z)
  
  # Atividades 3, 5, 6 e 7, respectivamente, B, D, E e F gerando com correlação
  custo[,1]<-0
  custo[,2]<-rtriangle(NS, 75, 110, 90)
  custo[,3]<-qtriangle(U[,3], 120, 190, 150)
  custo[,4]<-rtriangle(NS, 60, 85, 75)
  custo[,5]<-qtriangle(U[,4], 45, 80, 60)
  custo[,6]<-qtriangle(U[,1], 160, 240, 200)
  custo[,7]<-qtriangle(U[,2], 210, 300, 250) 
  custo[,8]<-rtriangle(NS, 85, 140, 120)
  custo[,9]<-0
  
  custo
}

geraPrazo<-function(NS){
  # Geração de prazo
  dur<-matrix(nrow=NS, ncol=9)
  
  # Geraç matrix de correlação
                    #  E  F  B  D
  matrixCor<-matrix(c(1, .7, 0, 0,
                      .7, 1, 0, 0,
                      0, 0, 1, .4,
                      0, 0, .4, 1), 
                    ncol=4)
  mu<-c(0,0,0)
  
  Z<-mvrnorm(NS, mu=rep(0,4), Sigma=matrixCor)
  U<-pnorm(Z)
  
  # Atividades 3, 5, 6 e 7, respectivamente, B, D, E e F gerando com correlação
  dur[,1]<-0
  dur[,2]<-rtriangle(NS, 1, 4, 2)
  dur[,3]<-qtriangle(U[,3], 5, 7, 6)
  dur[,4]<-rtriangle(NS, 2, 5, 4)
  dur[,5]<-qtriangle(U[,4], 1, 4, 3)
  dur[,6]<-qtriangle(U[,1], 4, 7, 5)
  dur[,7]<-qtriangle(U[,2], 3, 5, 4)
  dur[,8]<-rtriangle(NS, 1, 3, 2)
  dur[,9]<-0
  
  dur
}

calculaCusto<-function(NS, custo, evento){
  # Função retorna o custo total de cada cenario
  custoCenarios<-matrix(nrow=NS, ncol=1)
  
  for(i in 1:NS){
    custoCenarios[i,]<-sum(custo[i,])
    
    # Remove o custo caso evento não ocorra
    if(evento[i,1] == 0){
      custoCenarios[i,] = custoCenarios[i,] - custo[i,7]
    }
  }
  
  custoCenarios
}

calculaGraficoCenario<-function(i, grafico, cenario_evento){
  remover<-c()
  
  if(cenario_evento[i,1] == 0){
    remover <- c(remover, 2, 7)
  }
  
  grafico_cenario<-delete_edges(grafico, remover)
  
  grafico_cenario
}

calculaPrazo<-function(NS, grafico, prazo, evento){
  # Função retorna os prazos de cada cenário
  prazoCenarios<-matrix(nrow=NS, ncol=1)
  
  # Calculo do maior caminho para cada cenario
  for(i in 1:NS){
    durMaiorCaminho<-0
    
    # calcula todos os caminhos para o gráfico do cenario em questão, 
    # removendo as arestas caso o evento não aconteça, eliminando assim o caminho que não exista
    graficoCenario<-calculaGraficoCenario(i, grafico, evento)
    caminhos<-all_simple_paths(graficoCenario,from=1,to=9)
    
    # Para cada caminho do grafo, calcula o maior caminho
    for(j in 1:length(caminhos)){
      durCaminhoAtual<-sum(prazo[i,][caminhos[[j]]])
      
      if(durCaminhoAtual > durMaiorCaminho){
        durMaiorCaminho<-durCaminhoAtual
      }
    }
    
    # Seta maior caminho
    prazoCenarios[i,]<-durMaiorCaminho
  }
  
  prazoCenarios
}

normalize<-function(vetor, cabecalho){
  # Normalizando: Variaveis tendem a uma distribuição normal
  media <- mean(vetor)
  dv <- sqrt(var(vetor))
  
  eixoX = seq(media - 3*dv,media + 3*dv,0.1)
  eixoY = dnorm(eixoX, media, dv)
  plot(eixoX, eixoY, type="l", main=cabecalho)
}

clusters<-function(custo, duracao){
  # Calculo da correlacao
  correlation<-cor(custo, duracao, use = "everything", method = c("pearson", "kendall", "spearman"))
  
  # Clusterizando e plotando
  custoVsPrazo<-cbind(custo, duracao)
  clusters <- kmeans(custoVsPrazo, 2)
  plot(custoVsPrazo, col = clusters$cluster, main = paste("Correlacao:", correlation))
}

preparaTornado<-function(NS, matrix, evento){
  # Função retorna o matrix com as colunas zeradas caso não ocorra o evento
  
  for(i in 1:NS){
    if(evento[i,1] == 0){
      matrix[i,7] = 0
    }
  }
    
  matrix
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
  yname<-m[,1][o]
  barplot(m[,2][o], beside=TRUE, horiz=TRUE, xlim=c(-1,1),
          names.arg=yname, main = paste("Analise de Sensibilidade de", tituloGrafico),
          xlab="Coeficiente de correlação normalizado", ylab="Item")
}

main<-function(){
  # Numero de amostras
  NS<-3000
  
  # Geração do grafo de atividades
  grafico<-geraGrafico()
  
  # Geração de eventos, custo e prazo
  evento<-geraEvento(NS)
  custo<-geraCusto(NS)
  prazo<-geraPrazo(NS)
  
  # Calculo de prazo e custo, respectivamente
  prazoSimulado<-calculaPrazo(NS, grafico, prazo, evento)
  custoSimulado<-calculaCusto(NS, custo, evento)

  # Cumulativa de prazo e custo
  plot(ecdf(prazoSimulado), main=paste("Cumulativa de Prazo"))
  plot(ecdf(custoSimulado), main=paste("Cumulativa de Custo"))
  
  # Probabilidade de prazo e custo
  hist(prazoSimulado, main=paste("Histograma de Prazo"))
  normalize(prazoSimulado, "Prazo")
  hist(custoSimulado, main=paste("Histograma de Custo"))
  normalize(custoSimulado, "Custo")
  
  
  # Divisão por clusters Custo vs Prazo
  clusters(custoSimulado, prazoSimulado)
  
  # Tornado de Prazo
  prazoTornado<-preparaTornado(NS, prazo, evento)
  geraTornado(prazoTornado, prazoSimulado, "Prazo")
  
  # Tornado de Custo
  custoTornado<-preparaTornado(NS, custo, evento)
  geraTornado(custoTornado, custoSimulado, "Custo")
  
 
}
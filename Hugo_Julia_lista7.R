library("igraph")
library("triangle")

#   atividade 1: assinatura do contrato (inicio)
#   atividade 2: projeto
#   atividade 3: alterações no projeto (adicional de (3,6,4) semanas)
#   atividade 4: terraplanagem
#   atividade 5: obtenção de artefatos arqueológicos (adicional de (8,10,14) semanas)
#   atividade 6: fundações
#   atividade 7: tempo de espera de secagem da fundação
#   atividade 8: estrutura piso 1
#   atividade 9: estrutura piso 2
#   atividade 10: estrutura piso 3
#   atividade 11: espera após o término do piso 1
#   atividade 12: espera após o término do piso 2
#   atividade 13: espera após o término do piso 3
#   atividade 14: envoltória piso 1
#   atividade 15: envoltória piso 1 caso mude a empresa
#   atividade 16: envoltória piso 2
#   atividade 17: envoltória piso 2 caso mude a empresa
#   atividade 18: envoltória piso 3
#   atividade 19: envoltória piso 3 caso mude a empresa
#   atividade 20: dummy (espera 0 para envoltoria 1)
#   atividade 21: dummy (espera 0 para envoltoria 2)
#   atividade 22: dummy (espera 0 para envoltoria 3)
#   atividade 23: serviço 1
#   atividade 24: acabamento 1
#   atividade 25: serviço 2
#   atividade 26: acabamento 2
#   atividade 27: serviço 3
#   atividade 28: acabamento 3
#   atividade 29: telhado 29
#   atividade 30: limpeza 30
#   atividade 31: ajuste 1
#   atividade 32: ajuste 2
#   atividade 33: fim

#   evento 1: adicional do projeto
#   evento 2: encontrar artefatos
#   evento 3: recusa da envoltória
#   evento 4: primeiros acertos
#   evento 5: segundos acertos

fases<-make_graph(c(
  1,2, 1,4, 2,3, 2,8, 3,8, 
  4,6, 4,5, 5,6, 6,7, 7,8,
  8,11, 11,14, 11,15, 14,20, 15,20, 
  20,23, 20,24, 9,12, 12,16, 12,17, 
  16,21, 17,21, 21,25, 21,26, 10,13, 
  13,18, 13,19, 18,22, 19,22, 22,27, 
  22,28, 10,29, 23,30, 24,30, 25,30, 
  26,30, 27,30, 28,30, 29,30, 30,31, 
  30,33, 31,32, 31,33, 32,33
))

sorteia_evento<-function(NS){
  e<-matrix(nrow=NS, ncol=5)
  e[,1]<-rbinom(NS, 1, 0.2)
  e[,2]<-rbinom(NS, 1, 0.3)
  e[,3]<-rbinom(NS, 1, 0.9)
  e[,4]<-rbinom(NS, 1, 0.4)
  e[,5]<-rbinom(NS, 1, 0.05)
  
  e
}

sorteia_duracao<-function(NS){
  dur<-matrix(nrow=NS, ncol=33)
  
  dur[,1]<-0
  dur[,2]<-rtriangle(NS, 14,21,16)
  dur[,3]<-rtriangle(NS, 3,6,4)
  dur[,4]<-rtriangle(NS, 3,7,4)
  dur[,5]<-rtriangle(NS, 8,14,10)
  dur[,6]<-rtriangle(NS, 6,8,7)
  dur[,7]<-rtriangle(NS, 3,6,4)
  dur[,8]<-rtriangle(NS, 4,6,4.5)
  dur[,9]<-rtriangle(NS, 4,6,4.5)
  dur[,10]<-rtriangle(NS, 4,6,4.5)
  dur[,11]<-3
  dur[,12]<-3
  dur[,13]<-3
  dur[,14]<-rtriangle(NS, 7,9,8)
  dur[,15]<-rtriangle(NS, 6,11,8)
  dur[,16]<-rtriangle(NS, 7,9,8)
  dur[,17]<-rtriangle(NS, 6,11,8)
  dur[,18]<-rtriangle(NS, 7,9,8)
  dur[,19]<-rtriangle(NS, 6,11,8)
  dur[,20]<-0
  dur[,21]<-0
  dur[,22]<-0
  dur[,23]<-rtriangle(NS, 8,13,10)
  dur[,24]<-rtriangle(NS, 9,13,11)
  dur[,25]<-rtriangle(NS, 8,13,10)
  dur[,26]<-rtriangle(NS, 9,13,11)
  dur[,27]<-rtriangle(NS, 8,13,10)
  dur[,28]<-rtriangle(NS, 9,13,11)
  dur[,29]<-rtriangle(NS, 7,10,8)
  dur[,30]<-2
  dur[,31]<-rtriangle(NS, 0.2,5,2)
  dur[,32]<-rtriangle(NS, 0.5,1.5,1)
  dur[,33]<-0
  
  dur
}

sorteia_custos<-function(NS, matrixDuracao){
  custo<-matrix(nrow=NS, ncol=33)
  
  custo[,1]<-0
  custo[,2]<-160000
  custo[,3]<-12000*matrixDuracao[,3]
  custo[,4]<-rtriangle(NS, 4200,4700,4500)*matrixDuracao[,4]
  custo[,5]<-0
  custo[,6]<-rtriangle(NS, 2800,3300,3300)*matrixDuracao[,6] + rtriangle(NS, 37000,40000,38500)
  custo[,7]<-0
  custo[,8]<-rtriangle(NS, 4700,5500,5200)*matrixDuracao[,8] + rtriangle(NS, 17200,18000,17500)
  custo[,9]<-rtriangle(NS, 4700,5500,5200)*matrixDuracao[,9] + rtriangle(NS, 17200,18000,17500)
  custo[,10]<-rtriangle(NS, 4700,5500,5200)*matrixDuracao[,10] + rtriangle(NS, 17200,18000,17500)
  custo[,11]<-0
  custo[,12]<-0
  custo[,13]<-0
  custo[,14]<-rtriangle(NS, 36000,40000,37000) + 9800 + 197000
  custo[,15]<-rtriangle(NS, 36000,40000,37000) + 9800 + 209000
  custo[,16]<-rtriangle(NS, 36000,40000,37000) + 197000
  custo[,17]<-rtriangle(NS, 36000,40000,37000) + 209000
  custo[,18]<-rtriangle(NS, 36000,40000,37000) + 197000
  custo[,19]<-rtriangle(NS, 36000,40000,37000) + 209000
  custo[,20]<-0
  custo[,21]<-0
  custo[,22]<-0
  custo[,23]<-0
  custo[,24]<-rtriangle(NS, 106000,114000,112000)
  custo[,25]<-0
  custo[,26]<-rtriangle(NS, 106000,114000,112000)
  custo[,27]<-0
  custo[,28]<-rtriangle(NS, 106000,114000,112000)
  custo[,29]<-172000
  custo[,30]<-4000
  custo[,31]<-0
  custo[,32]<-0
  custo[,33]<-0
  
  custo
}

calcula_grafo_cenario<-function(i, cenario_evento){
  remover<-c()
  
  if(cenario_evento[i,1] == 0){
    remover <- c(remover, 3, 5)
  }
  
  if(cenario_evento[i,2] == 0){
    remover <- c(remover, 7, 8)
  }
  
  if(cenario_evento[i,3] == 1){
    remover <- c(remover, 13, 15, 20, 22, 27, 29)
  } else {
    remover <- c(remover, 12, 14, 19, 21, 26, 28)
  }
  
  if(cenario_evento[i,4] == 0){
    remover <- c(remover, 40, 43)
  }
  
  if(cenario_evento[i,5] == 0){
    remover <- c(remover, 42, 44)
  }
  
  grafoCenario<-delete_edges(fases, remover)
  #plot(grafoCenario)
  
  grafoCenario
}

calcula_custo_cenario<-function(cenario_custo, cenario_evento){
  custo<-vector()
  
  for(i in 1:nrow(cenario_custo)){
    #soma todos os custos
    custo[i]<-sum(cenario_custo[i,])
    
    #retira os custos que não aconteceram de acordo com a tabela de eventos
    if(cenario_evento[i,1] == 0){
      custo[i] = custo[i] - cenario_custo[i,3]
    }
    
    if(cenario_evento[i,2] == 0){
      custo[i] = custo[i] - cenario_custo[i,5]
    }
    
    if(cenario_evento[i,3] == 1){
      custo[i] = custo[i] - cenario_custo[i,15] - cenario_custo[i,17] - cenario_custo[i,19]
    } else {
      custo[i] = custo[i] - cenario_custo[i,14] - cenario_custo[i,16] - cenario_custo[i,18]
    }
    
    if(cenario_evento[i,4] == 0){
      custo[i] = custo[i] - cenario_custo[i,31]
    }
    
    if(cenario_evento[i,5] == 0){
      custo[i] = custo[i] - cenario_custo[i,32]
    }
    
  }
  
  custo  
}

calcula_duracao_cenarios<-function(NS, dur, evento){
  vetorDuracaoCenarios<-vector(length = NS)
  
  for(i in 1:NS){
    durMaiorCaminho<-0
    
    grafoCenario<-calcula_grafo_cenario(i, evento)
    caminhos<-all_simple_paths(grafoCenario,from=1,to=33)
    
    for(j in 1:length(caminhos)){
      durCaminhoAtual<-sum(dur[i,][caminhos[[j]]])
      
      if(durCaminhoAtual > durMaiorCaminho){
        durMaiorCaminho<-durCaminhoAtual
      }
    }
    
    vetorDuracaoCenarios[i]<-durMaiorCaminho
  }
  
  vetorDuracaoCenarios
}

geraTornado<-function(matrix_cenarios, resultados_cenarios, tituloGrafico){
  N_atividades<-33
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
  sm<-m[o,][28:33,] # Cinco variáveis mais criticas
  yname<-sm[,1]
  barplot(sm[,2]*sm[,3], beside=TRUE, horiz=TRUE, xlim=c(-1,1),
          names.arg=yname, main = paste(tituloGrafico, "Analise de Sensibilidade"),
          xlab="Coeficiente de correlação normalizado", ylab="Item")
}

preparaTornado<-function(cenario, cenario_evento){ 
  # Retorna vetor de matriz de custos com colunas zeradas
  
  for(i in 1:nrow(cenario)){
    if(cenario_evento[i,1] == 0){
      cenario[i,3] = 0
    }
    
    if(cenario_evento[i,2] == 0){
      cenario[i,5] = 0
    }
    
    if(cenario_evento[i,3] == 1){
      cenario[i,15] = 0
      cenario[i,17] = 0
      cenario[i,19] = 0
    } else {
      cenario[i,14] = 0
      cenario[i,16] = 0
      cenario[i,18] = 0
    }
    
    if(cenario_evento[i,4] == 0){
      cenario[i,31] = 0
    }
    
    if(cenario_evento[i,5] == 0){
      cenario[i,32] = 0
    }
    
  }
  
  cenario
}

clusterizacao<-function(vetor_custo, vetor_duracao){
  matrix_custo_dur<-cbind(vetor_custo, vetor_duracao)
  
  clusters <- kmeans(matrix_custo_dur, 2)
  plot(matrix_custo_dur, col = clusters$cluster)
}

main<-function(NS){
  # Prepara os cenários
  evento<-sorteia_evento(NS)
  dur<-sorteia_duracao(NS)
  custo<-sorteia_custos(NS, dur)
  
  # Calculo custo de cada cenário e geração de histograma e CDF
  sim_custo<-calcula_custo_cenario(custo, evento)
  hist(sim_custo, main=paste("Histograma de Custo"))
  custoCDF<-ecdf(sim_custo)
  plot(custoCDF, main=paste("Cumulativa de Custo"))
  
  # Calculo duracao de cada cenário e geração de histograma e CDF
  sim_duracao<-calcula_duracao_cenarios(NS, dur, evento)
  hist(sim_duracao, main=paste("Histograma de Duração"))
  durCDF<-ecdf(sim_duracao)
  plot(durCDF, main=paste("Cumulativa de Duração"))
  
  # Relação entre as duas variáveis
  correlation<-cor(sim_custo, sim_duracao, use = "everything", method = c("pearson", "kendall", "spearman"))
  
  # Tornado de custo
  custoTornado<-preparaCustoTornado(custo, evento)
  geraTornado(custoTornado, sim_custo, "Custo")
  
  # Tornado de prazo
  duracaoTornado<-preparaCustoTornado(dur, evento)
  geraTornado(duracaoTornado, sim_duracao, "Prazo")
  
  # Plota Custo vs Cenario
  clusterizacao(sim_custo, sim_duracao)
}

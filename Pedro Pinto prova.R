#Calcula o evento que, se ocorrer, inclui a etapa F no grafo
evento<-function(amostras){
  mevento<-rbinom(amostras,1,0.25)
  mevento
}
  
#Calcula a duracao de cada etapa do projeto
duracao<-function(amostras, mevento){
  mduracao<-matrix(nrow = amostras, ncol = 8)

  mduracao[,1]<-0 #inicio
  mduracao[,2]<-rtriangle(amostras,1,4,2) #Atividade A
  mduracao[,3]<-rtriangle(amostras,5,7,6) #Atividade B
  mduracao[,4]<-rtriangle(amostras,2,5,4) #Atividade C
  mduracao[,5]<-rtriangle(amostras,1,4,3) #Atividade D
  mduracao[,6]<-rtriangle(amostras,4,7,5) #Atividade E
  mduracao[,7]<-rtriangle(amostras,3,5,4)*mevento #Atividade F(Condicional)
  mduracao[,8]<-rtriangle(amostras,1,3,2) #Atividade G(Final)

  mduracao
} 

#Calcula o custo de cada etapa  
custo<-function(amostras, mevento){
  
  mcusto<-matrix(nrow = amostras, ncol = 8)
  
  mcusto[,1]<-rtriangle(amostras,75,110,90) #Atividade A
  mcusto[,2]<-rtriangle(amostras,120,190,150) #Atividade B
  mcusto[,3]<-rtriangle(amostras,60,85,75) #Atividade C
  mcusto[,4]<-rtriangle(amostras,45,80,60) #Atividade D
  mcusto[,5]<-rtriangle(amostras,160,240,200) #Atividade E
  mcusto[,6]<-rtriangle(amostras,210,300,250)*mevento #Atividade F(condicional)
  mcusto[,7]<-rtriangle(amostras,85,140,120) #Atividade G

    #Essa ultima coluna contem a soma dos custos de todas as etapas anteriores
  mcusto[,8]<-mcusto[,1]+mcusto[,2]+mcusto[,3]+mcusto[,4]+mcusto[,5]+mcusto[,6]+mcusto[,7]
  
  mcusto
} 

#Funcao principal
main<-function(amostras){
    
  #Bibliotecas
  library(triangle)
  library(igraph)
  
  #Guarda os resultados das funcoes criadas anteriormente em variaveis
  mevento<-evento(amostras)
  mduracao<-duracao(amostras,mevento)
  mcusto<-custo(amostras,mevento)

  #Este vetor será usado para guardar as duracoes de cada cenario.
  #Nao foi necessario criar um vetor para custo pois na propria matriz ja foi feita uma coluna com os totais
  vduracao<-vector(length=amostras)
  
  #Loop que calcula o grafo de cada  cenario
  for(i in 1:amostras){
    #Se o evento ocorrer, a atividade F será incluída pela aresta (2,7)
    if (mevento[i]==1){
      caminho<-c(1,2,1,3,1,4,2,5,4,6,2,7,3,8,5,8,6,8)
    }
    #Se nao, a aresta nao entrara no grafo
    else{
      caminho<-c(1,2,1,3,1,4,2,5,4,6,3,8,5,8,6,8)
    }
  
    #Cria o grafo do respectivo cenario atravez do caminho calculado
    g<-make_graph(caminho)
    
    #Calcula o caminho mais longo em relacao ao tempo
    p<-all_simple_paths(g, from=1, to=8)
    np<-length(p)
    maior<-0
    tempo<-mduracao
    for (j in 1:np){
      t<-sum(tempo[i,][p[[j]]])
      if(t>maior){
        maior<-t
      }
    }
    #O maior caminho de cada cenario sera adicionado ao vetor de duracoes
    vduracao[i]<-maior
  }
  
  #Para que o programa nao fique muito lento, apenas um grafo sera plotado
  plot(g)
  #O histograma da duracao parece se aproximar de uma normal. A ocorrencia condicional da atividade F nao influencia no maior caminho.
  hist(vduracao)
  #no histograma do custo, pode-se notar um "buraco".Isso mostra que os cenarios se dividiram em dois grups em relacao ao custo: com atividade F e sem atividade F.
  hist(mcusto[,8])
  #As curvas de densidade se mostraram coerentes com os histogramas, pode-se notar uma quebra de padrao no custo e nao na de duracao.
  plot(ecdf(vduracao))
  plot(ecdf(mcusto[,8]))
  
  #Por fim, foi calculada a correlacao entre custo e duracao, mas ela nao se mostrou relevante.
  correlacao<-cor(mcusto[,8], vduracao, use = "everything", method = c("pearson", "kendall", "spearman"))
  
  correlacao
}
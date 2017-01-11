teste<-function(amostras){
  
  #bibliotecas
  library(triangle)
  library(MASS)
  library(igraph)

  eventos<-function(amostras){
    
    evento<-matrix(nrow = amostras, ncol = 9)
    
    evento[,1]<-1                                                 #Inicio
    evento[,2]<-1                                                 #A
    evento[,3]<-1                                                 #B
    evento[,4]<-1                                                 #C
    evento[,5]<-1                                                 #D
    evento[,6]<-1                                                 #E
    evento[,7]<-rbinom(amostras,1,0.25)                           #F
    evento[,8]<-1                                                 #G
    evento[,9]<-1                                                 #Fim
    
    evento
  }
  
  duração<-function(amostras){
    
    duracao<-matrix(nrow = amostras, ncol = 9)
     
    duracao[,1]<-0                                                 #Inicio
    duracao[,2]<-rtriangle(amostras,1,4,2)                         #A
    duracao[,3]<-rtriangle(amostras,5,7,6)                         #B
    duracao[,4]<-rtriangle(amostras,2,5,4)                         #C
    duracao[,5]<-rtriangle(amostras,1,4,3)                         #D
    duracao[,6]<-rtriangle(amostras,4,7,5)                         #E
    duracao[,7]<-rtriangle(amostras,3,5,4)*rbinom(amostras,1,0.25) #F
    duracao[,8]<-rtriangle(amostras,1,3,2)                         #G
    duracao[,9]<-0                                                 #Fim
    
    duracao
    
  }
  
  custo<-function(amostras){
    
    preco<-matrix(nrow = amostras, ncol = 9)

    preco[,1]<-0                               #Inicio
    preco[,2]<-rtriangle(amostras,75,110,90)   #A
    preco[,3]<-rtriangle(amostras,120,190,150) #B
    preco[,4]<-rtriangle(amostras,60,85,75)    #C
    preco[,5]<-rtriangle(amostras,45,80,60)    #D
    preco[,6]<-rtriangle(amostras,160,240,200) #E
    preco[,7]<-rtriangle(amostras,210,300,250) #F
    preco[,8]<-rtriangle(amostras,85,140,120)  #G
    preco[,9]<-0                               #Fim
    
    preco    
  }
  
  #chamada funções
  neventos<-eventos(amostras)
  nprazo<-duração(amostras)
  nvalor<-custo(amostras)
  
  duracao<-vector(length=amostras)
  totalcusto<-vector(length=amostras)
  
  caminho<-(c(1,2,2,3,3,4,4,5,5,6))
  
  #Calcula direção do grafo
  for(i in 1:amostras){
    totalcusto[i]<-sum(nvalor[i,])
    if (neventos[i,7]==1){
      caminho<-c(caminho,c(6,7,7,8,8,9))
      
    }
    else {
      caminho<-c(caminho,c(6,8,8,9))
      totalcusto[i]<-totalcusto[i]-nvalor[i,7]
      }
  
    
    g<-make_graph(caminho)
    p<-all_simple_paths(g, from=1, to=9)
    np<-length(p)
    
    tempo<-nprazo
    
    #variável temporária
    maior<-0
   
    #Calcular maior duração do projeto
    for (j in 1:np){
      t<-sum(tempo[i,][p[[j]]])
      if(t>maior){
        maior<-t
      }
    }
    duracao[i]<-maior
  }
  
  duracao
  totalcusto
  plot(g)
  
  #####Distribuição Duração
  m_duração<-mean(duracao)
  var_duração<-var(duracao)
  dp_duração<-sd(duracao)
  #print(m_duração)
  probTempo<-duracao*neventos
  pTempo<-pnorm(probTempo, m_duração, dp_duração)
  hist(pTempo)
  cumulativa<-(ecdf(pTempo))
  plot(cumulativa)
  
  #####Distribuição Custo
  m_custo<-mean(totalcusto)
  var_custo<-var(totalcusto)
  dp_custo<-sd(totalcusto)
  #print(m_custo)
  probCusto<-totalcusto*neventos
  #print(probCusto)
  pCusto<-pnorm(probCusto, m_custo, dp_custo)
  hist(pCusto)
  cumulativa<-(ecdf(pCusto))
  plot(cumulativa)
  
  #####Criando gráfico tornado
  #geração dos cenários
  impacto_custos<-vector(length = amostras)
  impacto_prazo<-vector(length = amostras)
  impacto_custos<-neventos*nvalor
  impacto_prazo<-neventos*nprazo
  
  c<-vector(length = 9)
  for (i in 1:9){
    if (sd(impacto_custos[,i]) != 0) {
      c[i]<-cov(totalcusto, impacto_custos[,i],method="spearman")
    }
  }
  
  d<-vector(length = 9)
  for (i in 1:9){
    if (sd(impacto_prazo[,i]) != 0) {
      d[i]<-cov(duracao, impacto_prazo[,i],method="spearman")
    }
  }
  
  #normalization custo
  c<-c/sum(c)
  m<-matrix(c(1:9,c),ncol=2)  
  
  #normalization prazo
  d<-d/sum(d)
  n<-matrix(c(1:9,d),ncol=2)
  
  #preparando tornado plot custo
  o1<-order(m[,2])
  yname1<-m[,1][o1]
  barplot(m[,2][o1],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
          names.arg=yname1, main = "Cost sensitivity analysis",
          xlab="Normalized correlation coeficient",ylab="Cost item")
  
  #preparando tornado plot prazo
  o2<-order(n[,2])
  yname2<-n[,1][o2]
  barplot(n[,2][o2],beside=TRUE,horiz=TRUE,xlim=c(-1,1),
          names.arg=yname2, main = "Deadline sensitivity analysis",
          xlab="Normalized correlation coeficient",ylab="Deadline item")
  
  
  
  
  # #####Correlação entre custo x prazo
  # corr<-function(amostras){
  #   média<-c(0,0)
  #   matrixCorr<-matrix(c(0.7,0.7,0.4,0.4), ncol = 4)
  #   
  #   ev<-matrix(nrow = amostras, ncol = 4)
  #   
  #   ev[,1]<-rtriangle(amostras,120,190,150) #B
  #   ev[,2]<-rtriangle(amostras,45,80,60)    #D
  #   ev[,3]<-rtriangle(amostras,160,240,200) #E
  #   ev[,4]<-rtriangle(amostras,210,300,250) #F
  #   
  #   
  #   covariancia<-mvrnorm(amostras,mu=média,Sigma=matrixCorr)
  #   acumulada<-pnorm(covariancia)
  #   plot(acumulada)
    
  # }
  
  #corr_custo_prazo<-corr(amostras)
  
  
}
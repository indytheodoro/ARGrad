# ============================ LISTA 6 ===============================

# 1: Projeto de sistema
# 2: Projeto de módulo
# 3: Projeto de módulo
# 4: Projeto de módulo
# 5: Projeto de módulo
# 6: Codificação de módulo
# 7: Codificação de módulo
# 8: Codificação de módulo
# 9: Codificação de módulo
# 10: Teste de módulo
# 11: Teste de módulo
# 12: Teste de módulo
# 13: Teste de módulo
# 14: Integração
# 15: Teste do sistema
# 16: Teste de aceitação

# 17: Retrabalho1
# 18: testeRetrabalho1
# 19: Retrabalho2
# 20: testeRetrabalho2

# 21: Fim

projeto<-function(amostras){
  
  # Bibliotecas
  library("triangle")
  library("MASS")
  library("igraph")
  
  neventos<-eventos(amostras)
  nprazo<-atividades(amostras)
  duracao<-vector(length=amostras)
 
  caminho<-c(1,2,1,3,1,4,1,5,
             2,6,3,7,4,8,5,9,
             6,10,7,11,8,12,9,13,
             10,14,11,14,12,14,13,14,
             14,15,15,16)
    
  for(i in 1:amostras){
    if (neventos[i,1]==1){
      caminho<-c(caminho,c(16,17,17,18))
      
      if (neventos[i,2]==1){
        caminho<-c(caminho,c(18,19,19,20,20,21))
        
      }
      
      else{
        caminho<-c(caminho,c(18,21))
      }
    }
    else{
      caminho<-c(caminho,c(16,21))
    }
    
    g<-make_graph(caminho)
    p<-all_simple_paths(g, from=1, to=21)
    np<-length(p)
    maior<-0
    tempo<-nprazo
    
    for (j in 1:np){
      t<-sum(tempo[i,][p[[j]]])
      if(t>maior){
        maior<-t
      }
    }
    duracao[i]<-maior
  }
  duracao
  
}

atividades<-function(amostras){

  média<-c(0,0,0)
  matrixCorr<-matrix(c(1,0.7,0,0.7,1,0.5,0,0.5,1), ncol = 3)
  
  covariancia<-mvrnorm(amostras,mu=média,Sigma=matrixCorr)
  acumulada<-pnorm(covariancia)
  
  t<-matrix(nrow = amostras, ncol = 3)
  
  t[,1]<-qtriangle(acumulada[,1],2,5,2.5)
  t[,2]<-qtriangle(acumulada[,2],4,6.5,5)
  t[,3]<-qtriangle(acumulada[,3],1.5,3,2)
  
  nprazo<-matrix(nrow = amostras, ncol = 21)
  
  nprazo[,1]<-rtriangle(amostras,4,7,5)
  
  nprazo[,2]<-t[,1]
  nprazo[,3]<-t[,1]
  nprazo[,4]<-t[,1]
  nprazo[,5]<-t[,1]
  
  nprazo[,6]<-t[,2]
  nprazo[,7]<-t[,2]
  nprazo[,8]<-t[,2]
  nprazo[,9]<-t[,2]
  
  nprazo[,10]<-t[,3]
  nprazo[,11]<-t[,3]
  nprazo[,12]<-t[,3]
  nprazo[,13]<-t[,3]
  
  nprazo[,14]<-rtriangle(amostras,4,7,5)
  nprazo[,15]<-rtriangle(amostras,2,5,2)
  nprazo[,16]<-rtriangle(amostras,1,7,1)
  
  nprazo[,17]<-rtriangle(amostras,2,5,3) #Retrabalho1
  nprazo[,18]<-rtriangle(amostras,1,3,1) #TesteRetrabalho1
  nprazo[,19]<-rtriangle(amostras,1,3,1) #Retrabalho2
  nprazo[,20]<-rtriangle(amostras,1,3,1) #TesteRetrabalho2
  
  nprazo[,21]<-0  
  nprazo
  
}

eventos<-function(amostras){
  neventos<-matrix(nrow = amostras, ncol = 2)
  neventos[,1]<-rbinom(amostras, 1, 0.3)  #Retrabalho1
  neventos[,2]<-rbinom(amostras,1,0.05)   #Retrabalho2
  neventos
  
}

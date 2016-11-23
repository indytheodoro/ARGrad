################## Lista 4 ####################

#Documentação:

#(*)Nós grafo_base

#Seção 0 - AssinaturaContrato (0) = 1*

#Seção 1 - Projeto (14,21,16) = 2*
#Seção 1 - Reprojeto (3,4,6) = 3

#Seção 2 - Terraplanagem (3,4,7) = 4*
#Seção 2 - Inspeção (8,10,14) = 5
#Seção 3 - Fundações (6,7,8) = 6*
#Seção 4 - Estrutura_Espera (3,6,4) = 7*

#Seção 4 - Piso1 (4,4.5,6) = 8*
#Seção 4 - Piso1_Espera (3) = 9*
#Seção 5 - Envoltória1 (7,8,9) = 10
#Seção 5 - Envoltória1_nova (6,8,11) = 11
#Seção 5 - Envoltória_Espera (0) = 12*
#Seção 6 - Serviços (8,10,13) = 13*
#Seção 6 - Acabamento (9,11,13) = 14*

#Seção 4 - Piso2 (4,4.5,6) = 15*
#Seção 4 - Piso2_Espera (3) = 16*
#Seção 5 - Envoltória2 (7,8,9) = 17
#Seção 5 - Envoltória2_nova (6,8,11) = 18
#Seção 5 - Envoltória_Espera (0) = 19*
#Seção 6 - Serviços (8,10,13) = 20*
#Seção 6 - Acabamento (9,11,13) = 21*

#Seção 4 - Piso3 (4,4.5,6) = 22*
#Seção 4 - Piso3_Espera (3) = 23*
#Seção 5 - Envoltória3 (7,8,9) = 24
#Seção 5 - Envoltória3_nova (6,8,11) = 25
#Seção 5 - Envoltória_Espera (0) = 26*
#Seção 6 - Serviços (8,10,13) = 27*
#Seção 6 - Acabamento (9,11,13) = 28*

#Seção 4 - Telhado (7,8,10) = 29*
#Seção 7 - Limpeza (2) = 30*
#Seção 7 - Acertos1 (0.2,2,5) = 31
#Seção 7 - Acertos2 (0.5,1,1.5) = 32
#Seção 7 - Fim (0) = 33*


main<-function(amostras){
  
  #Biblioteca
  library(igraph)
  library(triangle)
  
  
  
  grafoCompleto<-(c(1,2,1,4,2,3,4,5,4,6,5,6,6,7,7,8,2,8,3,8,
                    8,9,9,10,9,11,10,12,11,12,12,13,12,14,13,30,14,30,
                    8,15,15,16,16,17,16,18,17,19,18,19,19,20,19,21,20,30,21,30,
                    15,22,22,23,23,24,23,25,24,26,25,26,26,27,26,28,27,30,28,30,
                    22,29,29,30,
                    30,31,30,33,31,32,32,33
                    ))
  
  grafoBase<-(c(1,2,1,4,6,7,7,8,8,9,12,13,12,14,13,30,14,30,
                8,15,15,16,19,20,19,21,20,30,21,30,
                15,22,22,23,26,27,26,28,27,30,28,30,
                22,29,29,30
                ))
  duracao<-vector(length = amostras)
  custo<-vector(length = amostras)
  caminho<-grafoBase
  evento<-eventos(amostras)
  tempo<-prazo(amostras)
  preco<-custos(tempo, amostras)
  for(i in 1:amostras){
    custo[i]<-sum(preco[i,])
    if (evento[i,1]==1){
      caminho<-c(caminho,c(2,3,3,8))
    }
    else{
      caminho<-c(caminho,c(2,8))
      custo[i]<-custo[i]-preco[i,3]
    }
    if (evento[i,2]==1){
      caminho<-c(caminho,c(4,5,5,6))
    }
    else{
      caminho<-c(caminho,c(4,6))
      custo[i]<-custo[i]-preco[i,5]
    }
    
    if (evento[i,3]==1){
      caminho<-c(caminho,c(9,11,11,12,16,18,18,19,23,25,25,26))
      custo[i]<-custo[i]-preco[i,10]-preco[i,17]-preco[i,24]
    }
    else{
      caminho<-c(caminho,c(9,10,10,12,16,17,17,19,23,24,24,26))
      custo[i]<-custo[i]-preco[i,11]-preco[i,18]-preco[i,25]
    }
    
    if (evento[i,4]==1){
      caminho<-c(caminho,c(30,31))
      if (evento[i,5]==1){
        caminho<-c(caminho,c(31,32,32,33))
      }
      else{
        caminho<-c(caminho,c(31,33))
        custo[i]<-custo[i]-preco[i,32]
      }
    }
    else{
      caminho<-c(caminho,c(30,33))
      custo[i]<-custo[i]-preco[i,31]-preco[i,32]
    }
    
    g<-make_graph(caminho)
    p<-all_simple_paths(g, from=1, to=33)
    np<-length(p)
    maior<-0
    for (j in 1:np){
      t<-sum(tempo[i,][p[[j]]])
      if(t>maior){
        maior<-t
      }

    }
    
    
    duracao[i]<-maior
  }
  list(mean(custo), var(custo), mean(duracao), var(duracao))
  

}

eventos<-function(amostras){
  
  neventos<-matrix(nrow = amostras, ncol = 5)
  
  neventos[,1]<-rbinom(amostras, 1, 0.2) #reprojeto
  neventos[,2]<-rbinom(amostras,1,0.3)   #inspeção
  neventos[,3]<-rbinom(amostras,1,0.1)   #empresas
  neventos[,4]<-rbinom(amostras,1,0.4)   #acertos
  neventos[,5]<-rbinom(amostras,1,0.05)  #2ª chamada
  
  neventos
}
                  
prazo<-function(amostras){
  
  nprazo<-matrix(nrow = amostras, ncol = 33)
  
  nprazo[,1]<-0
  nprazo[,2]<-rtriangle(amostras,14,21,16)
  nprazo[,3]<-rtriangle(amostras,3,6,4)
  nprazo[,4]<-rtriangle(amostras,3,7,4)
  nprazo[,5]<-rtriangle(amostras,8,14,10)
  nprazo[,6]<-rtriangle(amostras,6,8,7)
  nprazo[,7]<-rtriangle(amostras,3,6,4)
  nprazo[,8]<-rtriangle(amostras,4,6,4.5)
  nprazo[,9]<-3
  nprazo[,10]<-rtriangle(amostras,7,9,8)
  nprazo[,11]<-rtriangle(amostras,6,11,8)
  nprazo[,12]<-0
  nprazo[,13]<-rtriangle(amostras,8,13,10)
  nprazo[,14]<-rtriangle(amostras,9,13,11)
  nprazo[,15]<-rtriangle(amostras,4,6,4.5)
  nprazo[,16]<-3
  nprazo[,17]<-rtriangle(amostras,7,9,8)
  nprazo[,18]<-rtriangle(amostras,6,11,8)
  nprazo[,19]<-0
  nprazo[,20]<-rtriangle(amostras,8,13,10)
  nprazo[,21]<-rtriangle(amostras,9,13,11)
  nprazo[,22]<-rtriangle(amostras,4,6,4.5)
  nprazo[,23]<-3
  nprazo[,24]<-rtriangle(amostras,7,9,8)
  nprazo[,25]<-rtriangle(amostras,6,11,8)
  nprazo[,26]<-0
  nprazo[,27]<-rtriangle(amostras,8,13,10)
  nprazo[,28]<-rtriangle(amostras,9,13,11)
  nprazo[,29]<-rtriangle(amostras,7,10,8)
  nprazo[,30]<-2
  nprazo[,31]<-rtriangle(amostras,0.2,5,2)
  nprazo[,32]<-rtriangle(amostras,0.5,1.5,1)
  nprazo[,33]<-0
  
  nprazo
}

custos<-function(pr, amostras){
  
  ncusto<-matrix(nrow = amostras, ncol = 33)
  
  ncusto[,1]<-0
  ncusto[,2]<-160.000
  ncusto[,3]<-12.000*pr[,3]
  ncusto[,4]<-rtriangle(amostras,4200,4700,4500)*pr[,4]
  ncusto[,5]<-0
  ncusto[,6]<-rtriangle(amostras,2800,3300,3300)*pr[,6]+rtriangle(amostras,3700,40000,38500)
  ncusto[,7]<-0
  ncusto[,8]<-rtriangle(amostras,4700,5500,5200)*pr[,8]+rtriangle(amostras,17200,18500,17500)
  ncusto[,9]<-0
  ncusto[,10]<-rtriangle(amostras,36000,40000,37000)+197000+9800
  ncusto[,11]<-rtriangle(amostras,36000,40000,37000)+209000+9800
  ncusto[,12]<-0
  ncusto[,13]<-0
  ncusto[,14]<-rtriangle(amostras,106000,114000,112000)
  ncusto[,15]<-rtriangle(amostras,4700,5500,5200)*pr[,8]+rtriangle(amostras,17200,18500,17500)
  ncusto[,16]<-0
  ncusto[,17]<-rtriangle(amostras,36000,40000,37000)
  ncusto[,18]<-rtriangle(amostras,36000,40000,37000)
  ncusto[,19]<-0
  ncusto[,20]<-0
  ncusto[,21]<-rtriangle(amostras,106000,114000,112000)
  ncusto[,22]<-rtriangle(amostras,4700,5500,5200)*pr[,8]+rtriangle(amostras,17200,18500,17500)
  ncusto[,23]<-0
  ncusto[,24]<-rtriangle(amostras,36000,40000,37000)
  ncusto[,25]<-rtriangle(amostras,36000,40000,37000)
  ncusto[,26]<-0
  ncusto[,27]<-0
  ncusto[,28]<-rtriangle(amostras,106000,114000,112000)
  ncusto[,29]<-172000
  ncusto[,30]<-4000
  ncusto[,31]<-0
  ncusto[,32]<-0
  ncusto[,33]<-0
  
  ncusto
}

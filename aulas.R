# ########################################################
# #                   DIA 14/09/2016
# ########################################################
# 
# xnorm<-rnorm(1000)
# hist(x)
# ?set.seed
# set.seed(111)
# min(xnorm)
# max(xnorm)
# str(xnorm)
# yunif<-runif(100)
# hist(yunif)
# rbinom(10,1,0.5)
# library(triangle)
# ztri<-rtriangle(10,10,300,150)
# 
# 
# ########################################################
# #                   DIA 21/09/2016
# ########################################################
# 
# #1-Simule um jogo de cara ou coroa. (i) verifique a freq??ncia do n?mero de 
# #caras com: 10,100 e 1000 lan?amentos. (ii) repita cada um dos experimentos 
# #10 vezes e compare com os resultados anteriores.
# set.seed(123)
# normal<-rnorm(1000)
# summary(normal)
# s_norm<-sort(normal)
# s_norm[1]
# s_norm[250]
# #Fun??o de Distribui??o Normal
# #F(x) = 12 * e-x2
# 
# #2-Simule o lan?amento simult?neo de 20 moedas. Calcule a freq??ncia relativa 
# #do "n?mero total de caras" para 10, 100 e 1000 lan?amentos. Calcule a m?dia
# #e a vari?ncia do "n?mero total de caras".
# moeda<-rbinom(1000,1,0.5)
# media<-mean(moeda)
# media
# variancia<-var(moeda)
# variancia
# 
# #3-Usando o Teorema Central do Limite (TCL), obtenha uma aproximacao analitica 
# #para a variavel aleatoria soma de 12 VAs que seguem uma distribuicao 
# #uniforme 0..1. Calcule a m?dia e a variancia desta distribuicao e 
# #esboce um grafico dessa distribuicao.
# 
# 
# 
# 
# #-----------------------------------------------------------------------------  
#   
# f0<-(1/2*pi^0.5)*exp(0)
# f3<-(1/(2*pi)^0.5)*exp(-9)
# min(normal)
# max(normal)
# var(normal)
# sd(normal)
# mean(normal)
# median(normal)
# hist(normal)
# sum(normal)
# 
# rnorm(3)
# dnorm(3)
# pnorm(3)
# qnorm(0.9986501)
# 
# runif(3)
# dunif(3)
# punif(3)
# qunif(0.9986501)
# 
# plot(normal)
# x1<-seq(from=-1, to=1, by=0.001)
# dx1<-dnorm(x1)
# plot(x1, dx1)
# px1<-pnorm(x1)
# plot(x1, px1)
# qx1<-qnorm(x1)
# plot(x1, qx1)
# 
# x3<-seq(from=-3, to=3, by=0.001)
# dx3<-dnorm(x3)
# plot(x3, dx3)
# px3<-pnorm(x3)
# plot(x3, px3)
# qx3<-qnorm(x3)
# plot(x3, qx3)
# 
# 
# ########################################################
# #                   DIA 28/09/2016
# ########################################################
# x<-runif(1000)
# y<-runif(1000)
# z<-vector(length=1000)
# for(i in 1:1000){
#   if(sqrt((x[i]^2)+(y[i]^2)) <= 1){
#     z[i]<-1
#   }
# }  
# pi<-sum(z/1000)*4
# pi
# 
# #-------------------------------------------------------------------------------  
# #LISTA 1:
# 
# # 1-Simule o lan?amento simult?neo de 30 moedas, anotando o n?mero de caras. 
# # Repita 1000 vezes e construa uma aproxima??o emp?rica para os par?metros desta 
# # distribui??o. Compare com os resultados estimados pelo TCL. 
# moeda30<-rbinom(1000,30,0.5)
# mean(moeda30)
# var(moeda30)
# max(moeda30)
# min(moeda30)
# media30<-30*0.5
# sd30<-sqrt(30*0.25)
# hist(moeda30)
# plot(moeda30)
# xplot30<-seq(from=(media30-(3*sd30)), to=(media30+(3*sd30)), by=0.1)
# yplot30<-(dnorm(xplot30,mean=media30,sd=sd30))
# plot(xplot30, yplot30)
# 
# # 2-Usando o TCL, obtenha uma aproxima??o anal?tica para a vari?vel aleat?ria 
# # "soma de 12 VAs" que seguem uma distribui??o uniforme U(0,1). Calcule a 
# # m?dia e a vari?ncia desta distribui??o e esboce um gr?fico dessa distribui??o. 
# # Compare com os resultados obtidos usando simula??o MC.
# 
# r12<-vector(length = 1000)
# z12<-matrix(runif(12*1000),ncol=12)
# for(i in 1:1000){
#   r12[i]<-sum(z12[i,])-6
# }
# mean(r12)
# var(r12)
# sd(r12)
# hist(r12)
# 
# # 3- Usando o TCL, obtenha uma aproxima??o anal?tica (TCL) para a soma de 10 distribui??es
# # triangulares. Suponha que a distribui??o i tem como par?metros (i-3, i, i+5). 
# # Compare os resultados com aqueles obtidos por simula??o.
# 
# library(triangle)
# r10<-vector(length = 1000)
# z10<-matrix(nrow = 1000, ncol = 10)
# for(j in 1:10){
#   z10[,j]<-rtriangle(1000,j-3,j+5,j)
# }
# for(i in 1:1000){
#   r10[i]<-sum(z10[i,])
# }
# min(r10)
# mean(r10)
# var(r10)
# sd(r10)
# hist(r10)
# 
# 
# # 4-Obtenha um aproxima??o emp?rica (MC) para a fun??o M?ximo( Xi) (i=5) que representa a
# # distribui??o de probabilidade do "m?ximo dentre i VAs" cada uma delas seguindo uma
# # Normal(0,1).
# 
# r5<-vector(length = 1000)
# z5<-matrix(rnorm(5*1000), ncol = 5)
# for(i in 1:1000){
#   r5[i]<-max(z5[i, ])
# }
# mean(r5)
# hist(r5)
# 
# # 5-Suponha um fluxo de caixa com 20 valores, cada um deles seguindo uma triangular 
# # com valores (8,10,12) descontados a uma taxa de 1% por per?odo. Obtenha uma 
# # aproxima??o emp?rica para a distribui??o de probabilidade do VPL e compare com 
# # o resultado obtido usando o TCL. O valor presente (VP) de um elemento de fluxo 
# # de caixa futuro (VF), n per?odos a frente, com taxa de juros t, ?: VP=VF/(1+t)^n
# 
# library(triangle)
# r20<-vector(length = 1000)
# desc20<-vector(length = 20)
# z20<-matrix(rtriangle(20000,8,12,10), ncol = 20)
# for(i in 1:20){
#   desc20[i]<-1/(1+0.01)^i
# }
# for(i in 1:1000){
#   r20[i]<-sum(z20[i, ]*desc20)
# }
# mean(r20)
# sd(r20)
# hist(r20)

########################################################
#                   DIA 05/10/2016
########################################################

#============== Lista 2 - Risco de custo ================

################## VERSÃƒO 1 ############################
# #biblioteca
# library(triangle)
# 
# #variÃ¡veis
# op<-vector(length=1000)
# comprimento<-vector(length=1000)
# ntubos<-vector(length=1000)
# ctubo<-vector(length=1000)
# cmat<-vector(length=1000)
# tsolda<-vector(length=1000)
# ctrabalho<-vector(length=1000)
# csoldas<-vector(length=1000)
# tcavar<-vector(length=1000)
# ccavar<-vector(length=1000)
# cmo<-vector(length=1000)
# cfiltragem<-vector(length=1000)
# cacabamento<-vector(length=1000)
# cser<-vector(length=1000)
# ctotal<-vector(length=1000)
# ctransporte<-vector(length=1000)
# 
# for (i in 1:1000){
#   op[i]<-rbinom(1,1,0.35)
#   if(op[i] == 1){
#     comprimento[i] <- 290000
#   }
#   else{
#     comprimento[i] <- 260000
#   }
#   ntubos[i]<-comprimento[i]/8
#   ctubo[i]<-rtriangle(1,725,790,740)
#   cmat[i]<-ctubo[i]*ntubos[i]
#   tsolda[i]<-rtriangle(1,4,5,4.5)
#   ctrabalho[i]<-rtriangle(1,17,23,18.5)
#   csoldas[i]<-(ntubos[i]-1)*tsolda[i]*ctrabalho[i]
#   tcavar[i]<-rtriangle(1,12,25,16)*ntubos[i]
#   ccavar[i]<-tcavar[i]*ctrabalho[i]
#   cmo[i]<-ccavar[i]+csoldas[i]
#   cfiltragem[i]<-rtriangle(1,165000,188000,173000)
#   cacabamento[i]<-rtriangle(1,14000,17000,15000)*(comprimento[i]/1000)
#   ctransporte[i]<-rtriangle(1,6.1,7.4,6.6)*ntubos
#   cser[i]<-cfiltragem[i]+cacabamento[i]+ctransporte[i]
#   ctotal[i]<-cmat[i]+cmo[i]+cser[i]
# } 
# 
# mean(ctotal)
# hist(ctotal)

#################### VERSAO 2 ############################
# 
# obra<-function(amostras=1000,p=0.40){
#   
#   #Biblioteca
#   library(triangle)
#   
#   #Variaveis
#   ctubo<-rtriangle(amostras,725,790,740)
#   tcavar<-rtriangle(amostras,12,25,16)
#   ctrabalho<-rtriangle(amostras,17,23,18.5)
#   ctransporte<-rtriangle(amostras,6.1,7.4,6.6)
#   tsolda<-rtriangle(amostras,4,5,4.5)
#   cfiltragem<-rtriangle(amostras,165000,188000,173000)
#   cacabamento<-rtriangle(amostras,14000,17000,15000)
#   op<-rbinom(amostras,1,p)
#   comprimento<-op*30000+260000
#   cmat<-vector(length = amostras)
#   cmo<-vector(length = amostras)
#   cser<-vector(length = amostras)
#   ctotal<-vector(length = amostras)
# 
#   #Calcular os custos com base em dois possíveis comprimentos: 260.000m ou 290.000m
# 
#   #Custo Material
#   CustoMat<-function(i){ 
#     ntubos<-comprimento[i]/8
#     retorno<-ctubo[i]*ntubos
#     retorno
#   }
#   
#   #Custo MÃ£o de Obra  
#   CustoMO<-function(i){ 
#     ntubos<-comprimento[i]/8
#     ccavar<-tcavar[i]*ctrabalho[i]
#     csoldas<-(ntubos-1)*tsolda[i]*ctrabalho[i]
#     retorno<-ccavar+csoldas
#     retorno
#   }  
#   
#   #Custo MÃ£o de ServiÃ§o
#   CustoSer<-function(i){
#     ntubos<-comprimento[i]/8
#     fim<-cacabamento[i]*(comprimento[i]/1000)
#     transp<-ctransporte[i]*ntubos
#     retorno<-cfiltragem[i]+fim+transp 
#     retorno
#   }
#   
#   for(i in 1:amostras){
#     cmat[i]<-CustoMat(i)
#     cmo[i]<-CustoMO(i)
#     cser[i]<-CustoSer(i)
#     ctotal[i]<-cmat[i]+cmo[i]+cser[i]
#   }
#   resultado<-list(ctotal,cmat,cmo,cser)
#   resultado
# }  
# 

#============================================================================================

# # Lista 3
# #resultado tem que ser o valor presente liquido do lucro
# 
# restaurante<-function(amostras){
# 
#   #library
#   library(triangle)
# 
#   #variáveis
#   nmesas<-rtriangle(amostras,40,120,60)
#   lucratividaded<-vector(length = amostras)
# 
#   #Lucratividade diaria
#   LD<-function(){
#     for(i in 1:amostras){
#       fatd<-rtriangle(nmesas[i],90, 250, 130)
#       lucrod<-rtriangle(nmesas[i], 0.15,0.30,0.22)
#       lucratividaded[i]<-sum(fatd*lucrod)
#     }
#     lucratividaded
#   }
# 
#   #Lucratividade mensal
#   LM<-function(){
#     mediam<-22*(mean(LD()))
#     varm<-22*(var(LD()))
#     retorno<-list(mediam, varm)
#   }
# 
#   #Lucratividade anual
#   LA<-function(){
#     mes<-1:12
#     desc<-(1/1.012)^mes
#     medA<-sum(LM()[[1]]*desc)
#     varA<-sum(LM()[[2]]*desc^2)
#     lucroa<-list(medA,sqrt(varA))
#   }
# 
#   #MAIN
#   minX<-LA()[[1]]-3*LA()[[2]]
#   maxX<-LA()[[1]]+3*LA()[[2]]
#   x<-seq(from = minX, to = maxX, length.out = 100)
#   y<-dnorm(x, LA()[[1]], LA()[[2]])
#   plot(x,y, type = "l")
# 
# }


#=============================================================================
# ################## Exercícios Sessão 4 ####################
# 
#    #Biblioteca
#    library(triangle)
# 
# ### Exercício 1 ###
# 
# taxi<-function(amostras){
#   
#   #Fixo
#   frota<-20
#   
#   #Variaveis para todos as simulacoes
#   consumoG<-rtriangle(amostras, 40, 60, 58)
#   custoG<-rtriangle(amostras, 3.1, 4.0, 3.8) 
#   
#   gastoTaxi<-consumoG*custoG
# 
#   ## Simulacao do gasto individual por taxi ##
#   mediaI<-mean(gastoTaxi)
#   varI<-var(gastoTaxi)
#   eixoX1<-seq(mediaI-3*sqrt(varI), mediaI+3*sqrt(varI), 0.1)
#   eixoY1<-dnorm(eixoX1, mediaI, sqrt(varI))
#   plot(eixoX1, eixoY1, type="l", main="Gasto Individual por Taxi")
# 
#   ## Simulacao do gasto diário pela Frota com TCL ##
#   mediaD<-frota*mean(gastoTaxi)
#   varD<-frota*var(gastoTaxi)
#   eixoX2<-seq(mediaD-3*sqrt(varD), mediaD+3*sqrt(varD), 0.1)
#   eixoY2<-dnorm(eixoX2, mediaD, sqrt(varD))
#   plot(eixoX2, eixoY2, type="l", main="Gasto Diário pela Frota")  
#   
#   
#   ## Simulacao produto do gasto da das 2 VA's por TCL
#   mediaTaxi<-mean(consumoG)*mean(custoG)
#   varTaxi<-(mean(consumoG)**2)*var(custoG) + (mean(custoG)**2)*var(consumoG)
#   
#   mediaGasto<-frota*mediaTaxi
#   varGasto<-frota*varTaxi
#   
#   eixoX3<-seq(mediaGasto-3*sqrt(varGasto), mediaGasto+3*sqrt(varGasto), 0.1)
#   eixoY3<-dnorm(eixoX3, mediaGasto, sqrt(varGasto))
#   plot(eixoX3, eixoY3, type="l", main="Gasto Diário pela Frota 2 VA's")  
#   
# }
# 
# ### Exercício 2 ###
#   
# navio<-function(amostras){
#   
#   #Fixos
#   placas<-562
#   custoReb<-7.5
#   
#   #Variavel
#   tempoReb<-rtriangle(amostras, 3.45, 5.3, 4.15) 
#   
#   #Solicitado na questão, TCL
#   riscoMO<-tempoReb*custoReb*placas
#   mRiscoMO<-mean(riscoMO)
#   vRiscoMO<-var(riscoMO)
#   
#   #Main
#   print(mRiscoMO)
#   print(vRiscoMO)
#   eixoX<-seq(mRiscoMO-3*sqrt(vRiscoMO), mRiscoMO+3*sqrt(vRiscoMO), 0.1)
#   eixoY<-dnorm(eixoX, mRiscoMO, sqrt(vRiscoMO))
#   plot(eixoX, eixoY, type="l", main="Risco Mao de Obra Rebitagem")
#   
# }
# 
# ### Exercício 3 ###
# 
# almoco<-function(amostras){
#   
#   #Variaveis_Distribuicoes
#   clientes<-rtriangle(amostras, 16, 22, 18)
#   consumoClientes<-rtriangle(amostras, 25, 36, 28)
#   sextas<-rtriangle(amostras, 40, 42, 41)
#   
#   #Solicitacao da questao
#   custo<-clientes*consumoClientes*sextas
#   mediaC<-mean(custo)
#   varC<-var(custo)
# 
#   #Main
#   print(mediaC)
#   print(varC) 
#   eixoX<-seq(mediaC-3*sqrt(varC), mediaC+3*sqrt(varC), 0.1)
#   eixoY<-dnorm(eixoX, mediaC, sqrt(varC))
#   plot(eixoX, eixoY, type="l", main="TOTAL GASTO DA NEW ENGLAND")
# }


# ########################################################
# #                   DIA 09/11/2016
# ########################################################
# 
# #Biblioteca
# library(igraph)
# library(triangle)
# 
# #criar grafo direcionado (origem,destino)
# #g<-make_graph(c(1,2,1,3,2,4,3,4,3,5,4,6,5,7,6,7))
# #plo
# 
# #retorna a lista de caminhos possíveis
# #lista<-all_simple_paths(g,from=1,to=7) 
# 
# 
# #Exercicio aula
# 
# ####### CORRIGIR, VALORES CERTOS ENTRE 14 E 19 ##########
# 
# prazo<-function(amostras){
#   #'''Calcular a distribuicao probabilidade 
#   #'com o menor prazo de execucao do projeto'''
#   
#   #Biblioteca
#   library(igraph)
#   library(triangle)
#   
#   amostras<-3000
#   grafo<-make_graph(c(1,2,1,3,2,4,3,4,3,5,4,6,5,7,6,7))
#   lista<-all_simple_paths(grafo,from=1,to=7) 
#   
#   #cenarios<-rtriangle(amostras, min, max, med)
#   
#   M<-matrix(nrow = amostras, ncol = 7)
#   #[,1] quer dizer todas as linhas da primeira coluna
#   #[1,] quer dizer todas as colunas da primeia linha
#   
#   #nos do grafo
#   M[,1]<-rtriangle(amostras, 0.5, 1.5, 1)
#   M[,2]<-rtriangle(amostras, 3, 5, 4)
#   M[,3]<-rtriangle(amostras, 4, 6, 5)
#   M[,4]<-rtriangle(amostras, 6, 8, 7)
#   M[,5]<-rtriangle(amostras, 1, 3, 2)
#   M[,6]<-rtriangle(amostras, 2, 4, 3)
#   M[,7]<-rtriangle(amostras, 0.5, 1.5, 1)
#   
#   duracao<-function(M, lista, amostras){
#     tam<-length(lista)
#     retorno<-vector(length = amostras)
#     for(i in 1:amostras){
#       dmax<-0
#       for (j in 1:tam){
#         temp<-lista[[j]]
#         somacaminho<-sum(M[i,][temp])
#         if (dmax < somacaminho) {
#           dmax<-somacaminho
#         }
#       }
#       retorno[i]<-dmax
#     }
#     retorno
#   }
#   
#   
#   #MAIN
#   resposta<-duracao(M,lista,amostras)
#   hist(resposta)
# }

#===============================================================================
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


main<-function(amostras=1){
  
  #Biblioteca
  library(igraph)
  library(triangle)
  
  grafoCompleto<-make_graph(c(1,2,1,4,2,3,4,5,4,6,5,6,6,7,7,8,2,8,3,8,
                      8,9,9,10,9,11,10,12,11,12,12,13,12,14,13,30,14,30,
                      8,15,15,16,16,17,16,18,17,19,18,19,19,20,19,21,20,30,21,30,
                      15,22,22,23,23,24,23,25,24,26,25,26,26,27,26,28,27,30,28,30,
                      22,29,29,30,
                      30,31,30,33,31,32,32,33
                      ))
  
  grafoBase<-make_graph(c(1,2,1,4,6,7,7,8,8,9,12,13,12,14,13,30,14,30,
                          8,15,15,16,19,20,19,21,20,30,21,30,
                          15,22,22,23,26,27,26,28,27,30,28,30,
                          22,29,29,30
                          ))
  
  caminho<-grafoBase
  evento<-eventos(amostras)
  for(i in 1:amostras){
    if (evento[i,1]==1){
      caminho<-caminho+edges(2,3,3,8)
    }
    else{
      caminho<-caminho+edges(2,8)
    }
    if (evento[i,2]==1){
      caminho<-caminho+edges(4,5,5,6)
    }
    else{
      caminho<-caminho+edges(4,6)
    }
    
    if (evento[i,3]==1){
      caminho<-caminho+edges(9,11,11,12,16,18,18,19,23,25,25,26)
    }
    else{
      caminho<-caminho+edges(9,10,10,12,16,17,17,19,23,24,24,26)
    }
    
    if (evento[i,4]==1){
      caminho<-caminho+edges(30,31)
      if (evento[i,5]==1){
        caminho<-caminho+edges(31,32)
      }
      else{
        caminho<-caminho+edges(31,33)
      }
    }
    else{
      caminho<-caminho+edges(30,33)
    }
  }
  plot(caminho)
  
  # lista<-all_simple_paths(grafoNOif,from=1,to=33) 
  # plot(grafoCompleto)
  
  #if()....
  
  
  #a partir dos eventos monta o grafo e retorna o grafo novo  
  #e os nós participantes do cenários de cada caminho
  #         somacaminho<-sum(grafo[i,][nósparticipantes])
  
  
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
                  
prazo<-function(){
  
  nprazo<-matrix(nrow = amostras, ncol = 33)
  
  nprazo[,1]<-0
  nprazo[,2]<-rtriangle(amostras,14,21,16)
  nprazo[,3]<-rtriangle(amostras,3,6,4)
  nprazo[,4]<-rtriangle(amostras,3,7,4)
  nprazo[,5]<-rtriangle(amostras,8,14,10)
  nprazo[,6]<-rtriangle(amostras,6,8,7)
  nprazo[,7]<-rtriangle(amostras,3,6,4)
  nprazo[,8]<-rtriangle(amostras,4,4.5,6)
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
}

custos<-function(pr){
  
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
}

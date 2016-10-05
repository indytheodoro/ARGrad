obra<-function(amostras=1000,p=0.40){
  
  #Biblioteca
  library(triangle)
  
  #Variáveis
  ctubo<-rtriangle(amostras,725,790,740)
  tcavar<-rtriangle(amostras,12,25,16)
  ctrabalho<-rtriangle(amostras,17,23,18.5)
  ctransporte<-rtriangle(amostras,6.1,7.4,6.6)
  tsolda<-rtriangle(amostras,4,5,4.5)
  cfiltragem<-rtriangle(amostras,165000,188000,173000)
  cacabamento<-rtriangle(amostras,14000,17000,15000)
  op<-rbinom(amostras,1,p)
  comprimento<-op*30000+260000
  cmat<-vector(length = amostras)
  cmo<-vector(length = amostras)
  cser<-vector(length = amostras)
  ctotal<-vector(length = amostras)
  
  #Calcular os custos com base em dois possíveis comprimentos: 260.000m ou 290.000m
  
  #Custo Material
  CustoMat<-function(i){ 
    ntubos<-comprimento[i]/8
    retorno<-ctubo[i]*ntubos
    retorno
  }
  
  #Custo Mão de Obra  
  CustoMO<-function(i){ 
    ntubos<-comprimento[i]/8
    ccavar<-tcavar[i]*ctrabalho[i]
    csoldas<-(ntubos-1)*tsolda[i]*ctrabalho[i]
    retorno<-ccavar+csoldas
    retorno
  }  
  
  #Custo Mão de Serviço
  CustoSer<-function(i){
    ntubos<-comprimento[i]/8
    fim<-cacabamento[i]*(comprimento[i]/1000)
    transp<-ctransporte[i]*ntubos
    retorno<-cfiltragem[i]+fim+transp 
    retorno
  }
  
  for(i in 1:amostras){
    cmat[i]<-CustoMat(i)
    cmo[i]<-CustoMO(i)
    cser[i]<-CustoSer(i)
    ctotal[i]<-cmat[i]+cmo[i]+cser[i]
  }
  resultado<-list(ctotal,cmat,cmo,cser)
  resultado
}  



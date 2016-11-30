# ==================== LISTA 5 ===================

# DESCRIÇÃO:
# 1: Arqueologia
# 2: Excavação
# 3: Formas
# 4: Fundação
# 5: Paredes
# 6: Telhado


main<-function(amostras){
  
  # Bibliotecas
  library(triangle)
  library(MASS)
  
  prazo<-prazo(amostras)
  impacto <-impacto(amostras)
  
  result<-prazo*impacto
  t<-vector(length = amostras)
  
  for(i in 1:amostras){t[i]<-sum(result[i,])}
  hist(t)
  cumulativa<-(ecdf(t))
  plot(cumulativa)
  cumulativa
}

prazo<-function(amostras){
  
  nprazo<-matrix(nrow = amostras, ncol = 6)
  
  nprazo[,1]<-rtriangle(amostras,1,5,4)
  nprazo[,2]<-rtriangle(amostras,9,12,10.9)
  nprazo[,3]<-rtriangle(amostras,1.5,3.2,2.2)
  nprazo[,4]<-rtriangle(amostras,5,7.5,6.7)
  nprazo[,5]<-rtriangle(amostras,15,18,16.7)
  nprazo[,6]<-rtriangle(amostras,6.5,9,7.6)
}

impacto<-function(amostras){
  
  imp<-matrix(nrow = amostras, ncol = 6)
  
  # Fixos:
  MR<-0.125
  Ruim<-0.3125
  Normal<-0.25
  Bom<-0.1875
  MB<-0.125
  
  probabildade<-c(MR,Ruim,Normal,Bom,MB)
  
  imp[,1]<-sample(c(1.4, 1.28, 1, 1, 0.98), size=1, replace = TRUE, probabildade)
  imp[,2]<-sample(c(1.3, 1.2, 1, 0.94, 0.9), size=1, replace = TRUE, probabildade)
  imp[,3]<-sample(c(1.1, 1.04, 1, 1, 0.97), size=1, replace = TRUE, probabildade)
  imp[,4]<-sample(c(1.4, 1.25, 1, 0.88, 0.82), size=1, replace = TRUE, probabildade)
  imp[,5]<-sample(c(1.1, 1.04, 1, 1, 0.98), size=1, replace = TRUE, probabildade)
  imp[,6]<-sample(c(1.2, 1.08, 1, 0.96, 0.94), size=1, replace = TRUE, probabildade)
  
  imp
  
}





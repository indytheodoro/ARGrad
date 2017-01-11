tornadoExemplo<-function(Ns=10,cont=FALSE,p=0.2){
  #geração de dados para análise de sensibilidade
  #modelo tem uma variável constingenciada  
  
  #Bibliotecas
  library(igraph)
  library(triangle)
  library (MASS)
  library(cluster)
  library(fpc)
  
  
  #run the model  
  Ni<-33
  
  #resulting scenarios matrix
  rs<-matrix(0,nrow=Ns,ncol=Ni+1)
  
  #vetor de eventos
  ev<-vector(length=Ns)
  
  #geração dos cenários
  impacto_custos<-vector(length = Ns)
  impacto_prazo<-vector(length = Ns)
  prazos_reais<-vector(length = Ns)

  Vcusto<-main4(Ns)[[1]]
  Vprazo<-main4(Ns)[[2]]
  
  
  neventos<-eventos(Ns)
  nprazo<-prazo(Ns)
  ncustos<-custos(nprazo, Ns)
  
  impacto_custos<-neventos*ncustos
  impacto_prazo<-neventos*nprazo

  c<-vector(length = 33)
  for (i in 1:33){
    if (sd(impacto_custos[,i]) != 0) {
      c[i]<-cov(Vcusto, impacto_custos[,i],method="spearman")
    }
  }
  
  d<-vector(length = 33)
  for (i in 1:33){
    if (sd(impacto_prazo[,i]) != 0) {
      d[i]<-cov(Vprazo, impacto_prazo[,i],method="spearman")
    }
  }

  #normalization custo
  c<-c/sum(c)
  m<-matrix(c(1:Ni,c),ncol=2)  
  
  #normalization prazo
  d<-d/sum(d)
  n<-matrix(c(1:Ni,d),ncol=2)
  
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

  plot(Vprazo, Vcusto)
  kmatriz<-cbind(Vprazo, Vcusto)
  km<-kmeans(kmatriz,2)
  plotcluster(kmatriz, km)
 
}
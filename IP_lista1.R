#LISTA 1: Ingrhid Theodoro e Pedro Pinto

# 1-Simule o lançamento simultâneo de 30 moedas, anotando o número de caras. 
# Repita 1000 vezes e construa uma aproximação empírica para os parâmetros desta 
# distribuição. Compare com os resultados estimados pelo TCL. 
moeda30<-rbinom(1000,30,0.5)
mean(moeda30)
var(moeda30)
max(moeda30)
min(moeda30)
media30<-30*0.5
sd30<-sqrt(30*0.25)
hist(moeda30)
plot(moeda30)
xplot30<-seq(from=(media30-(3*sd30)), to=(media30+(3*sd30)), by=0.1)
yplot30<-(dnorm(xplot30,mean=media30,sd=sd30))
plot(xplot30, yplot30)

# 2-Usando o TCL, obtenha uma aproximação analítica para a variável aleatória 
# "soma de 12 VAs" que seguem uma distribuição uniforme U(0,1). Calcule a 
# média e a variância desta distribuição e esboce um gráfico dessa distribuição. 
# Compare com os resultados obtidos usando simulação MC.

r12<-vector(length = 1000)
z12<-matrix(runif(12*1000),ncol=12)
for(i in 1:1000){
  r12[i]<-sum(z12[i,])-6
}
mean(r12)
var(r12)
sd(r12)
hist(r12)

# 3- Usando o TCL, obtenha uma aproximação analítica (TCL) para a soma de 10 distribuições
# triangulares. Suponha que a distribuição i tem como parâmetros (i-3, i, i+5). 
# Compare os resultados com aqueles obtidos por simulação.

library(triangle)
r10<-vector(length = 1000)
z10<-matrix(nrow = 1000, ncol = 10)
for(j in 1:10){
  z10[,j]<-rtriangle(1000,j-3,j+5,j)
}
for(i in 1:1000){
  r10[i]<-sum(z10[i,])
}
min(r10)
mean(r10)
var(r10)
sd(r10)
hist(r10)


# 4-Obtenha um aproximação empírica (MC) para a função Máximo( Xi) (i=5) que representa a
# distribuição de probabilidade do "máximo dentre i VAs" cada uma delas seguindo uma
# Normal(0,1).

r5<-vector(length = 1000)
z5<-matrix(rnorm(5*1000), ncol = 5)
for(i in 1:1000){
  r5[i]<-max(z5[i, ])
}
mean(r5)
hist(r5)

# 5-Suponha um fluxo de caixa com 20 valores, cada um deles seguindo uma triangular 
# com valores (8,10,12) descontados a uma taxa de 1% por período. Obtenha uma 
# aproximação empírica para a distribuição de probabilidade do VPL e compare com 
# o resultado obtido usando o TCL. O valor presente (VP) de um elemento de fluxo 
# de caixa futuro (VF), n períodos a frente, com taxa de juros t, é: VP=VF/(1+t)^n

library(triangle)
r20<-vector(length = 1000)
desc20<-vector(length = 20)
z20<-matrix(rtriangle(20000,8,12,10), ncol = 20)
for(i in 1:20){
  desc20[i]<-1/(1+0.01)^i
}
for(i in 1:1000){
  r20[i]<-sum(z20[i, ]*desc20)
}
mean(r20)
sd(r20)
hist(r20)
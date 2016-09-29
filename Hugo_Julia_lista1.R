exercicio 1:

ex1<-function(){
	x<-rbinom(1000,30,0.5)
	
	print(c("Média = ",mean(x)))
	print(c("Desvio Padrão = ",sd(x)))
	hist(x)
	
	eixoX <- seq(mean(x) - 3*sd(x),mean(x) + 3*sd(x),0.1)
	eixoY <- dnorm(eixoX, mean(x), sd(x))
	plot(eixoX, eixoY)
}

ex2<-function(){
	z<-matrix(runif(12000),ncol = 12)
	for(i in 1:1000){r[i] <- sum(z[i,]) - 6}
		
	print("RESULTADOS EXPERIMENTAIS")
	print(c("Média = ", mean(r)))
	print(c("Variância = ", var(r)))
}

ex3<-function(){
	z<-matrix(ncol = 10, nrow=1000)
	for(i in 1:10){
		z[,i] <- rtriangle(1000, i-3,i+5,i)
		for(j in 1:1000){
			r[j] <- sum(z[j,])
		}
	}
		
	print("RESULTADOS EXPERIMENTAIS")
	print(c("Média = ", mean(r)))
	print(c("Variância = ", var(r)))
	hist(r)
}

ex4<-function(){
	z<-matrix(ncol = 5, nrow=5000)
	for(i in 1:5){
		z[,i] <- rnorm(5000)
		for(j in 1:1000){
			r[j] <- max(z[j,])
		}
	}
		
	print("RESULTADOS EXPERIMENTAIS")
	print(c("Média = ", mean(r)))
	print(c("Variância = ", var(r)))
	hist(r)
}

ex5<-function(){
	desc<-vector(length=20)
	for(i in 1:20){desc[i]<-1/((1.01)^i)}
	fc<-matrix(ncol=20, nrow=1000)
	for(i in 1:20){
		fc[,i] <- rtriangle(1000,8,12,10)
		for(j in 1:1000){
			r[j] <- sum(fc[j,]*desc)
		}
	}
		
	print("RESULTADOS EXPERIMENTAIS")
	print(c("Média = ", mean(r)))
	print(c("Variância = ", var(r)))
	hist(r)	
}

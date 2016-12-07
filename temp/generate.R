#p: probability; gen: generate dataset

#set values
#n = 100
#betavec = c(0,1,-1,-2)
#lambda = 0.5

source("/home/susan/Dropbox/code/powdist.R")


#function that generates data sets from ppl link
genpl <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppl(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genplr <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = pplr(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpp <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppn(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genppr <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppnr(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpc <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppc(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpcr <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppcr(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpcll <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppgmi(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpcllr <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppgmir(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpll <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppgma(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}

genpllr <- function(n,betavec,lambda){
	beta0 = betavec[1]
	beta = c(betavec[-1])
	set.seed(123)
	X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
	prob = ppgmar(beta0+X%*%beta,lambda)
	rm(list=".Random.seed", envir=globalenv()) 
	y = numeric()
	for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
	return(data.frame(cbind(X,y,prob)))
}
ppower <- function(type,eta,lambda){
require(VGAM)
probs <- list(
  pl = function(eta,lambda) plogis(eta)**lambda,
  plr = function(eta,lambda) 1-plogis(-eta)**lambda,
  pp = function(eta,lambda) pnorm(eta)**lambda,
  ppr = function(eta,lambda) 1-pnorm(-eta)**lambda,
  pc = function(eta,lambda) pcauchy(eta)**lambda,
  pcr = function(eta,lambda) 1-pcauchy(-eta)**lambda,
  pll = function(eta,lambda) pgumbel(eta)**lambda,
  pllr = function(eta,lambda) 1-pgumbel(-eta)**lambda,
  pcll = function(eta,lambda) (1-pgumbel(-eta))**lambda,
  pcllr = function(eta,lambda) 1-(1-pgumbel(eta))**lambda
)
return(probs[[type]](eta,lambda))
}

genpower <- function(n,betavec,lambda,type){
  beta0 = betavec[1]
  beta = c(betavec[-1])
  set.seed(123)
  X = matrix(runif(n*NROW(beta),-3,3),ncol=NROW(beta))
  prob = ppower(type,beta0+X%*%beta,lambda)
  rm(list=".Random.seed", envir=globalenv()) 
  y = numeric()
  for(i in 1:n){y[i] = rbinom(1,1,prob[i])}
  return(data.frame(cbind(X,y,prob)))
}

lists <- function(dataset){
    dat = list(n = NROW(dataset),
               y = dataset[,NCOL(dataset)-1],
               X = as.matrix(dataset[,1:(NCOL(dataset)-2)],NCOL=NCOL(dataset-2,NROW=nrow(dataset))),
               k = NCOL(dataset)-2)
}
  
listj <- function(dataset){
    dat = list(n = NROW(dataset),
               y = dataset[,NCOL(dataset)-1],
               X = dataset[,1])
}

filestan <- function(type) {
  switch(type,
         pl = "pl.stan",
         plr = "plr.stan",
         pp = "pp.stan",
         ppr = "ppr.stan",
         pll = "pll.stan",
         pllr = "pllr.stan",
         pcll = "pcll.stan",
         pcllr = "pcllr.stan",
         pc = "pc.stan",
         pcr = "pcr.stan")
}

filebugs <- function(type) {
  switch(type,
         pl = "pl.bug",
         plr = "plr.bug",
         pp = "pp.bug",
         ppr = "ppr.bug",
         pll = "pll.bug",
         pllr = "pllr.bug",
         pcll = "pcll.bug",
         pcllr = "pcllr.bug",
         pc = "pc.bug",
         pcr = "pcr.bug")
}
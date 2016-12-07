source("/home/susan/Dropbox/Susan/code/tools.R")

spower <- function(n,betavec,lambda,type,chains,iter,warmup,thin){
  require(R2jags)
  require(rstan)
  require(batchmeans)

  file_s <- filestan(type)
  file_j <- filebugs(type)  

  dat <- genpower(n, betavec, lambda, type)

  time_s <- system.time(fit_s <- stan(file = file_s, data = lists(dat),
            chains = chains, iter = iter, warmup = warmup, thin = thin))
  time_j <- system.time(fit_j <- jags(model.file = file_j, data = listj(dat),
  					n.chains= chains, n.iter = iter, n.burnin = warmup, n.thin = thin,
  					parameters.to.save = c('beta0', 'beta1','lambda')))
  
  output <- c(mean(as.vector(extract(fit_s, "beta0",permuted=F))), mean(as.vector(fit_j$BUGSoutput$sims.matrix[,1])),
              mean(as.vector(extract(fit_s, "beta[1]",permuted=F))), mean(as.vector(fit_j$BUGSoutput$sims.matrix[,2])),
              median(as.vector(extract(fit_s, "lambda",permuted=F))), median(as.vector(fit_j$BUGSoutput$sims.matrix[,4])),
              ess(as.vector(extract(fit_s, "beta0",permuted=F))), ess(as.vector(fit_j$BUGSoutput$sims.matrix[,1])),
              ess(as.vector(extract(fit_s, "beta[1]",permuted=F))), ess(as.vector(fit_j$BUGSoutput$sims.matrix[,2])),
              ess(as.vector(extract(fit_s, "lambda",permuted=F))), ess(as.vector(fit_j$BUGSoutput$sims.matrix[,4])),
              time_s[3],time_j[3],n)
  
  return(output)
}

rpower = function(M,n,betavec,lambda,type,chains,iter,warmup,thin){
  replica=matrix(0,M,15)
  for(i in 1:M){
    replica[i,]=spower(n,betavec,lambda,type,chains,iter,warmup,thin) 
  }
  
  return(replica)
}

sumy <-function(rep,betavec,lambda){
  pre <- function(i,param){
    return(c(mean(rep[,i])-param, sd(rep[,i])/sqrt(NROW(rep)), mean((rep[,i]-param)^2), mean(rep[,i+6])))
  }
  resumen = matrix(c(pre(1,betavec[1]),pre(2,betavec[1]),pre(3,betavec[2]),pre(4,betavec[2]),pre(5,lambda),pre(6,lambda)),3,8,byrow=TRUE)
  rownames(resumen) = c('b0','b1','lambda')
  colnames(resumen)=c('vies_s','dp_s','eqm_s','tme_s','vies_j','dp_j','eqm_j','tme_j')
  tiempo = matrix(c(mean(rep[,13]),mean(rep[,14])),1,2,byrow=TRUE)
  colnames(tiempo)=c('mean_s','mean_j')
  rownames(tiempo)=c('tiempo')
  tamano = matrix(mean(rep[,15]),1,1,byrow = TRUE)
  
  return(output = list(resumen=resumen,tiempo=tiempo, tamano = tamano))
}
#pl = spower(n,betavec,lambda,"pl",chains,iter,warmup,thin)
#plr = spower(n,betavec,lambda,"plr",chains,iter,warmup,thin)
#pp = spower(n,betavec,lambda,"pp",chains,iter,warmup,thin)
#ppr = spower(n,betavec,lambda,"ppr",chains,iter,warmup,thin)
#pc = spower(n,betavec,lambda,"pc",chains,iter,warmup,thin)
#pcr = spower(n,betavec,lambda,"pcr",chains,iter,warmup,thin)
#pcll = spower(n,betavec,lambda,"pcll",chains,iter,warmup,thin)
#pcllr = spower(n,betavec,lambda,"pcllr",chains,iter,warmup,thin)
#pll = spower(n,betavec,lambda,"pll",chains,iter,warmup,thin)
#pllr = spower(n,betavec,lambda,"pllr",chains,iter,warmup,thin)
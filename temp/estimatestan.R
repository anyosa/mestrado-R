library(rstan)
#dat = listan(dados, dados$y, cbind(dados$V1))
#chains = 1
#iter = 8000
#warmup = 4000
#thin = 4

lStan.time<-system.time(lStan <- stan(file = "lStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pStan.time<-system.time(pStan <- stan(file = "pStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

cllStan.time<-system.time(cllStan <- stan(file = "cllStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

llStan.time<-system.time(llStan <- stan(file = "llStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

cStan.time<-system.time(cStan <- stan(file = "cStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

plStan.time<-system.time(plStan <- stan(file = "plStan.stan", data = dat,
                                          chains = chains, iter = iter, warmup = warmup, thin = thin))

plrStan.time<-system.time(plrStan <- stan(file = "plrStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

ppStan.time<-system.time(ppStan <- stan(file = "ppStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pprStan.time<-system.time(pprStan <- stan(file = "pprStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pcllStan.time<-system.time(pcllStan <- stan(file = "pcllStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pcllrStan.time<-system.time(pcllrStan <- stan(file = "pcllrStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pllStan.time<-system.time(pllStan <- stan(file = "pllStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pllrStan.time<-system.time(pllrStan <- stan(file = "pllrStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

pcStan.time<-system.time(pcStan <- stan(file = "pcStan.stan", data = dat,
                                       chains = chains, iter = iter, warmup = warmup, thin = thin))

pcrStan.time<-system.time(pcrStan <- stan(file = "pcrStan.stan", data = dat,
                                      chains = chains, iter = iter, warmup = warmup, thin = thin))

#until this i have 15 stanfits
#save.image("pcrfit.Rda")
#load("fit.Rda")
#colMeans(extract (pllStan, c("beta0","beta","lambda"),permuted=F))
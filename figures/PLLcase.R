source("/home/susan/Dropbox/code/powdist.R")
library(ggplot2)
library(gridExtra)
library(Cairo)

eta=seq(-10,10,by=0.01)

dat1 = data.frame(eta,p=ppll(eta,1/5),Lambda=rep("1/5",NROW(eta)))
dat2 = data.frame(eta,p=ppll(eta,1/3),Lambda=rep("1/3",NROW(eta)))
dat3 = data.frame(eta,p=ppll(eta,1),Lambda=rep("1",NROW(eta)))
dat4 = data.frame(eta,p=ppll(eta,2),Lambda=rep("2",NROW(eta)))
dat5 = data.frame(eta,p=ppll(eta,3),Lambda=rep("3",NROW(eta)))
datf =rbind(dat1,dat2,dat3,dat4,dat5)

datf = data.frame(dif = ppll(eta,1)-ppll(eta,2))

ggplot(datf, aes(x=dif)) +
  geom_histogram(col="black", bins = 20)



ggplot(datf,aes(p,color=Lambda, fill = Lambda)) +geom_histogram(col="black", bins = 80)
+ labs(x=expression(eta[i]),y=expression(P[i]),linetype=expression(lambda))+ scale_linetype_manual(values=c(2,3,1,4,5)) 

# Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")



ggplot(dat, aes(x=propor, color=Lambda, fill = Lambda)) +
  geom_histogram(col="black", bins = 80) +
  scale_color_grey()+scale_fill_grey() + labs(x="Proporção", y = "Frequência") + theme_light() + 
  coord_cartesian(xlim = c(0,1)) + geom_hline(yintercept = 0) +
  theme(axis.text=element_text(size=12), # increase size of axe
        axis.title=element_text(size=14)) #increase size of title of axe

eta=seq(-10,10)
ppll1=ppll(eta,1)
ppll2=ppll(eta,2)
plot(eta,ppll1,type="l")
lines(eta,ppll2)

datppll= data.frame(ppll1,ppll2,eta)

hist(ppll2-ppll1)
#######################

lambda=2
o7=qpll(0.875,lambda)
o4=qpll(0.5,lambda)
o1=qpll(0.125,lambda)

A8 <- function(o7,o4,o1){
  A8=((o7-o4)-(o4-o1))/(o7-o1)
  return(A8)
}

A8(o7,o4,o1)


p=seq(0,1,0.01)
q1=qpll(p,1)
q2=qpll(p,2)
plot(p,q1,type="l")
lines(p,q2)

q1-q2


q = seq(-4,4,by=0.01)
plot(q,ppll(q,1),type="l")
lines(q,ppll(q,2))
abline(h=0.3678794)
abline(h=0.13533528)
abline(v=0)


datf = data.frame(dif = qpll(eta,5)-qpll(eta,1),eta)

ggplot(datf, aes(x=dif)) +
  geom_histogram(col="black", bins = 20)


#####
eta=seq(-4,4,by=0.01)

datf = rbind(data.frame(eta,p=ppll(eta,1),Lambda=rep("1",NROW(eta))),
             data.frame(eta,p=ppll(eta,2),Lambda=rep("2",NROW(eta))))
df1 <- data.frame(x1 = 0.125, x2 = 0.125, y1 = -0.03895219, y2 = -0.7320994)
df2 <- data.frame(x1 = 0.5, x2 = 0.5, y1 = 1.05966, y2 = 0.3665129)
df3 <- data.frame(x1 = 0.875, x2 = 0.875, y1 = 2.706566, y2 = 2.013419)

ggplot(datf) +  geom_line(aes(p,eta,linetype=Lambda)) + theme_light() +
  labs(x=expression(p[i]),y=expression(q[i]),linetype=expression(lambda)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df1) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = df3) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))
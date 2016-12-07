source("/home/susan/Dropbox/Susan/code/powdist.R")
library(ggplot2)
library(gridExtra)
library(Cairo)

lambda = seq(0.01,10,by=0.01)

datf = rbind(data.frame(lambda,p=ppl(-2.5,lambda),Eta=rep("-2.5",NROW(lambda))),
             data.frame(lambda,p=ppl(0,lambda),Eta=rep("0",NROW(lambda))),
             data.frame(lambda,p=ppl(2.5,lambda),Eta=rep("2.5",NROW(lambda))))

plotp = ggplot(datf) +  geom_line(aes(lambda,p,linetype=Eta)) + theme_light() +
  labs(x=expression(lambda),y=expression(p),linetype=expression(eta)) + ggtitle("(a)") +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

#theme(legend.margin=unit(1,"mm"))

datf = rbind(data.frame(lambda,p=pplr(-2.5,lambda),Eta=rep("-2.5",NROW(lambda))),
             data.frame(lambda,p=pplr(0,lambda),Eta=rep("0",NROW(lambda))),
             data.frame(lambda,p=pplr(2.5,lambda),Eta=rep("2.5",NROW(lambda))))

plotpr = ggplot(datf) +  geom_line(aes(lambda,p,linetype=Eta)) + theme_light() +
  labs(x=expression(lambda),y=expression(p),linetype=expression(eta)) + ggtitle("(b)") +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

grid.arrange(plotp, plotpr, nrow=1, ncol=2)

### 

datf = rbind(
  data.frame(lambda,dpdl=ppl(-2.5,lambda)*log(ppl(-2.5,1)),Eta=rep("-2.5",NROW(lambda))),
  data.frame(lambda,dpdl=ppl(0,lambda)*log(ppl(0,1)),Eta=rep("0",NROW(lambda))),
  data.frame(lambda,dpdl=ppl(2.5,lambda)*log(ppl(2.5,1)),Eta=rep("2.5",NROW(lambda)))
)

plotp = ggplot(datf) +  geom_line(aes(lambda,dpdl,linetype=Eta)) + theme_light() + ggtitle("(a)") +
  labs(x=expression(lambda),y=expression(frac(paste(partialdiff,p),paste(partialdiff,lambda))),linetype=expression(eta)) + 
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

datf = rbind(
  data.frame(lambda,dpdl=-ppl(2.5,lambda)*log(ppl(2.5,1)),Eta=rep("-2.5",NROW(lambda))),
  data.frame(lambda,dpdl=-ppl(0,lambda)*log(ppl(0,1)),Eta=rep("0",NROW(lambda))),
  data.frame(lambda,dpdl=-ppl(-2.5,lambda)*log(ppl(-2.5,1)),Eta=rep("2.5",NROW(lambda)))
)

plotpr = ggplot(datf) +  geom_line(aes(lambda,dpdl,linetype=Eta)) + theme_light() + ggtitle("(b)") +
  labs(x=expression(lambda),y=expression(frac(paste(partialdiff,p),paste(partialdiff,lambda))),linetype=expression(eta)) + 
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

grid.arrange(plotp, plotpr, nrow=1, ncol=2)

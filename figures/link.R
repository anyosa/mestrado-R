source("/home/susan/Dropbox/Susan/code/powdist.R")
library(ggplot2)
library(Cairo)

eta=seq(-10,10,by=0.01)

### Ligacoes Comuns ###
datf = rbind(data.frame(eta,p=ppl(eta,1),Lambda=rep("logit",NROW(eta))),
             data.frame(eta,p=ppn(eta,1),Lambda=rep("probit",NROW(eta))),
             data.frame(eta,p=ppc(eta,1),Lambda=rep("cauchit",NROW(eta))),
             data.frame(eta,p=ppgmi(eta,1),Lambda=rep("cloglog",NROW(eta))),
             data.frame(eta,p=ppgma(eta,1),Lambda=rep("loglog",NROW(eta))))

plot  = ggplot(datf) +  geom_line(aes(eta,p,linetype=Lambda)) + theme_light() +
        labs(x=expression(eta[i]),y=expression(p[i]),linetype=NULL) + 
        theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
              legend.text = element_text(size = 16), axis.title = element_text(size = 16),
              legend.title = element_text(size=16), plot.title = element_text(size = 18), 
              axis.text = element_text(size = 12))

CairoPDF(file="ligacoes.pdf", width=11, height=8.5, pointsize=11)
plot
dev.off()
#+ scale_linetype_manual(values=c(2,3,1,4,5))

### Ligacoes Potencia e Potencia reversa ###
datf = rbind(data.frame(eta,p=ppgvmar(eta,1/5),Lambda=rep("1/5",NROW(eta))),
             data.frame(eta,p=ppgvmar(eta,1/3),Lambda=rep("1/3",NROW(eta))),
             data.frame(eta,p=ppgvmar(eta,1),Lambda=rep("1",NROW(eta))),
             data.frame(eta,p=ppgvmar(eta,2),Lambda=rep("2",NROW(eta))),
             data.frame(eta,p=ppgvmar(eta,3),Lambda=rep("3",NROW(eta))))

#linetype
plot = ggplot(datf) +  geom_line(aes(eta,p,linetype=Lambda)) + theme_light() + 
  labs(x=expression(eta[i]),y=expression(p[i]),linetype=expression(lambda))+ 
  scale_linetype_manual(values=c(2,3,1,4,5)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18), 
        axis.text = element_text(size = 12))

CairoPDF(file="pgvmar.pdf", width=11, height=8.5, pointsize=11)
plot
dev.off()

#color
#library(ggplot2)
#library(Cairo)
#CairoPDF(file="pll.pdf", width=11, height=8.5, pointsize=11)
#ggplot(datf) +  geom_line(aes(eta,p,color=Lambda)) + theme_light() + 
#labs(x=expression(eta[i]),y=expression(P[i]),color=expression(lambda))
#dev.off()
#ggplot(datf) +  geom_line(aes(eta,p,color=Lambda,linetype=Lambda)) + theme_light() + labs(x=expression(eta[i]),y=expression(P[i]),color=expression(lambda),linetype=expression(lambda)) + scale_color_brewer(palette="Set1")
#ggplot(datf) +  geom_line(aes(eta,p,color=Lambda,linetype=Lambda),size=0.8) + theme_light() + labs(x=expression(eta[i]),y=expression(P[i]),color=expression(lambda),linetype=expression(lambda)) + scale_color_hue(l=60)
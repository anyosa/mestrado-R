source("/home/susan/Dropbox/Susan/code/powdist.R")
library(ggplot2)
library(gridExtra)
library(Cairo)

eta=seq(-5,5,by=0.01)

datf = rbind(data.frame(eta,d=dpl(eta,1/5),Lambda=rep("1/5",NROW(eta))),
             data.frame(eta,d=dpl(eta,1/3),Lambda=rep("1/3",NROW(eta))),
             data.frame(eta,d=dpl(eta,1),Lambda=rep("1",NROW(eta))),
             data.frame(eta,d=dpl(eta,2),Lambda=rep("2",NROW(eta))),
             data.frame(eta,d=dpl(eta,3),Lambda=rep("3",NROW(eta))))

plotp = ggplot(datf) +  geom_line(aes(eta,d,linetype=Lambda)) + theme_light() +
  labs(x=expression(z),y=expression(f(z)),linetype=expression(lambda)) + 
  scale_linetype_manual(values=c(2,3,1,4,5)) + ggtitle("(a)") + coord_cartesian(ylim = c(0,0.4)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

datf = rbind(data.frame(eta,d=dplr(eta,1/5),Lambda=rep("1/5",NROW(eta))),
             data.frame(eta,d=dplr(eta,1/3),Lambda=rep("1/3",NROW(eta))),
             data.frame(eta,d=dplr(eta,1),Lambda=rep("1",NROW(eta))),
             data.frame(eta,d=dplr(eta,2),Lambda=rep("2",NROW(eta))),
             data.frame(eta,d=dplr(eta,3),Lambda=rep("3",NROW(eta))))

plotpr = ggplot(datf) +  geom_line(aes(eta,d,linetype=Lambda)) + theme_light() + 
  labs(x=expression(z),y=expression(f(z)),linetype=expression(lambda)) + 
  scale_linetype_manual(values=c(2,3,1,4,5)) + ggtitle("(b)") + coord_cartesian(ylim = c(0,0.4)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

CairoPDF(file="dpl.pdf", width=11, height=8.5, pointsize=11)
grid.arrange(plotp, plotpr, nrow=1, ncol=2)
dev.off()

###

datf = rbind(data.frame(eta,d=dpgvma(eta,1/5),Lambda=rep("1/5",NROW(eta))),
             data.frame(eta,d=dpgvma(eta,1/3),Lambda=rep("1/3",NROW(eta))),
             data.frame(eta,d=dpgvma(eta,1),Lambda=rep("1",NROW(eta))),
             data.frame(eta,d=dpgvma(eta,2),Lambda=rep("2",NROW(eta))),
             data.frame(eta,d=dpgvma(eta,3),Lambda=rep("3",NROW(eta))))

plot = ggplot(datf) +  geom_line(aes(eta,d,linetype=Lambda)) + theme_light() + labs(x=expression(z),y=expression(f(z)),linetype=expression(lambda)) + 
  scale_linetype_manual(values=c(2,3,1,4,5)) + coord_cartesian(ylim = c(0,0.4)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
        axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

CairoPDF(file="dpgvma.pdf", width=11, height=8.5, pointsize=11)
plot
dev.off()

###

eta=seq(-5,5,by=0.01)

datf = rbind(data.frame(eta,d=dpl(eta,1),Lambda=rep("L",NROW(eta))),
             data.frame(eta,d=dpn(eta,1),Lambda=rep("N",NROW(eta))),
             data.frame(eta,d=dpc(eta,1),Lambda=rep("C",NROW(eta))))

plot = ggplot(datf) +  geom_line(aes(eta,d,linetype=Lambda)) + theme_light() +
  labs(x=expression(eta[i]),y=expression(p[i]),linetype=NULL) + coord_cartesian(ylim = c(0,0.4)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18), 
        axis.text = element_text(size = 12))

CairoPDF(file="fdp_sim.pdf", width=11, height=8.5, pointsize=11)
plot
dev.off()

###

datf = rbind(data.frame(eta,d=dpgma(eta,1),Lambda=rep("GVMa",NROW(eta))),
             data.frame(eta,d=dpgmi(eta,1),Lambda=rep("GVMi",NROW(eta))))

plot = ggplot(datf) +  geom_line(aes(eta,d,linetype=Lambda)) + theme_light() +
  labs(x=expression(eta[i]),y=expression(p[i]),linetype=NULL) + coord_cartesian(ylim = c(0,0.4)) +
  theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        legend.text = element_text(size = 16), axis.title = element_text(size = 16),
        legend.title = element_text(size=16), plot.title = element_text(size = 18), 
        axis.text = element_text(size = 12))

CairoPDF(file="fdp_assim.pdf", width=11, height=8.5, pointsize=11)
plot
dev.off()
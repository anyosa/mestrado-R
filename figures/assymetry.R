source("/home/susan/Dropbox/Susan/code/powdist.R")
library(ggplot2)
library(gridExtra)
library(Cairo)

A8 <- function(o7,o4,o1){
  A8=((o7-o4)-(o4-o1))/(o7-o1)
  return(A8)
}

lambda=seq(0.01,10,by=0.01)

plotp = ggplot(data.frame(lambda, CA= A8(qpgvma(0.875,lambda), qpgvma(0.5,lambda), qpgvma(0.125,lambda))) ) +  
        geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y=expression(A[O])) + 
        coord_cartesian(ylim = c(-1, 1))  + ggtitle("(a)") +
        theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
              legend.text = element_text(size = 16), axis.title = element_text(size = 16),
              legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
              axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

plotpr = ggplot(data.frame(lambda, CA= A8(qpgvmar(0.875,lambda), qpgvmar(0.5,lambda), qpgvmar(0.125,lambda))) ) +  
        geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y=expression(A[O])) + 
        coord_cartesian(ylim = c(-1, 1))  + ggtitle("(b)") +
        theme(axis.title.y = element_text( margin = unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
              legend.text = element_text(size = 16), axis.title = element_text(size = 16),
              legend.title = element_text(size=16), plot.title = element_text(size = 18, hjust = +0.5), 
              axis.text = element_text(size = 12), legend.key = element_rect(colour = "gray"))

grid.arrange(plotp, plotpr, nrow=1, ncol=2)





CairoPDF(file="a8pp.pdf", width=11, height=8.5, pointsize=11)
grid.arrange(ggplot(data.frame(lambda, CA= A8(qpp(0.875,lambda), qpp(0.5,lambda), qpp(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1))  + ggtitle("PP"), 
             ggplot(data.frame(lambda, CA= A8(qppr(0.875,lambda), qppr(0.5,lambda), qppr(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1)) + ggtitle("PPR"), 
             nrow=1, ncol=2)
dev.off()

CairoPDF(file="a8pll.pdf", width=11, height=8.5, pointsize=11)
grid.arrange(ggplot(data.frame(lambda, CA= A8(qpll(0.875,lambda), qpll(0.5,lambda), qpll(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1))  + ggtitle("PLL"), 
             ggplot(data.frame(lambda, CA= A8(qpllr(0.875,lambda), qpllr(0.5,lambda), qpllr(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1)) + ggtitle("PLLR"), 
             nrow=1, ncol=2)
dev.off()

CairoPDF(file="a8pcll.pdf", width=11, height=8.5, pointsize=11)
grid.arrange(ggplot(data.frame(lambda, CA= A8(qpcll(0.875,lambda), qpcll(0.5,lambda), qpcll(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1))  + ggtitle("PCLL"), 
             ggplot(data.frame(lambda, CA= A8(qpcllr(0.875,lambda), qpcllr(0.5,lambda), qpcllr(0.125,lambda))) ) +  
               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
               coord_cartesian(ylim = c(-1, 1)) + ggtitle("PCLLR"), 
             nrow=1, ncol=2)
dev.off()
#CairoPDF(file="a8pc.pdf", width=11, height=8.5, pointsize=11)
#%grid.arrange(ggplot(data.frame(lambda, CA= A8(qpc(0.875,lambda), qpc(0.5,lambda), qpc(0.125,lambda))) ) +  
#               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
#               coord_cartesian(ylim = c(-1, 1))  + ggtitle("PC"), 
#             ggplot(data.frame(lambda, CA= A8(qpcr(0.875,lambda), qpcr(0.5,lambda), qpcr(0.125,lambda))) ) +  
#               geom_line(aes(lambda,CA)) + theme_light() + labs(x=expression(lambda),y="Assimetria octil") + 
#               coord_cartesian(ylim = c(-1, 1)) + ggtitle("PCR"), 
#             nrow=1, ncol=2)
#dev.off()
#datf = rbind(
#  data.frame(lambda, CA= A8(qpl(0.875,lambda), qpl(0.5,lambda), qpl(0.125,lambda)), modelo= rep("PL",NROW(lambda))),
#  data.frame(lambda, CA= A8(qplr(0.875,lambda), qplr(0.5,lambda), qplr(0.125,lambda)), modelo= rep("PLR",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpp(0.875,lambda), qpp(0.5,lambda), qpp(0.125,lambda)), modelo= rep("PP",NROW(lambda))),
#  data.frame(lambda, CA= A8(qppr(0.875,lambda), qppr(0.5,lambda), qppr(0.125,lambda)), modelo= rep("PPR",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpcll(0.875,lambda), qpcll(0.5,lambda), qpcll(0.125,lambda)), modelo= rep("PCLL",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpcllr(0.875,lambda), qpcllr(0.5,lambda), qpcllr(0.125,lambda)), modelo= rep("PCLLR",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpll(0.875,lambda), qpll(0.5,lambda), qpll(0.125,lambda)), modelo= rep("PLL",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpllr(0.875,lambda), qpllr(0.5,lambda), qpllr(0.125,lambda)), modelo= rep("PLLR",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpc(0.875,lambda), qpc(0.5,lambda), qpc(0.125,lambda)), modelo= rep("PC",NROW(lambda))),
#  data.frame(lambda, CA= A8(qpcr(0.875,lambda), qpcr(0.5,lambda), qpcr(0.125,lambda)), modelo= rep("PCR",NROW(lambda)))
#)
#ggplot(datf) + geom_line(aes(lambda,CA,linetype= modelo)) + theme_light() + 
#labs(x=expression(lambda),y="Assimetria octil",linetype="Ligações") + theme(legend.text = element_text(size = 12)) +
#coord_cartesian(ylim = c(-1, 1))
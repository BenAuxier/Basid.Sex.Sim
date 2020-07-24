library("ggplot2")
library("tidyr")
library(gridExtra)


# sample ------------------------------------------------------------------

x <- seq(0,1,0.0001)

d <- data.frame(x=x,
                y=x,
                y1=1-(1-x^2)^0.5,
                y2=(1-((1-x)^2))^0.5,
                y3=1-(1-x^1.5)^0.667,
                y4=(1-((1-x)^1.5))^0.667,
                y5=1-(1-x^1.25)^0.8,
                y6=(1-((1-x)^1.25))^0.8)

p <- ggplot(data=d)+ #geom_line(aes(x=x,y=y ),lty=5,color="grey70") +
  #                geom_line(aes(x=x,y=y1),color="grey50") +
  #                geom_line(aes(x=x,y=y2),color="grey50") +
  #                geom_line(aes(x=x,y=y3),color="grey50") +
  #                geom_line(aes(x=x,y=y4),color="grey50") +
  #                geom_line(aes(x=x,y=y5),color="grey50") +
  #                geom_line(aes(x=x,y=y6),color="grey50") +
  theme_classic() + xlab("") +theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.y=element_blank()
  )
p1 <- p + geom_line(aes(x=x,y=y1),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 2")))+ theme(axis.title.y = element_text()) + ylab("Transformed\nFitness")
p2 <- p + geom_line(aes(x=x,y=y3),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 1.5")))
p3 <- p + geom_line(aes(x=x,y=y5),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 1.25")))
p4 <- p + geom_line(aes(x=x,y=y ),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 1")))+ theme(axis.title.x = element_text()) + xlab("Starting Fitness")
p5 <- p + geom_line(aes(x=x,y=y6),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 0.8")))
p6 <- p + geom_line(aes(x=x,y=y4),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 0.667")))
p7 <- p + geom_line(aes(x=x,y=y2),size=1.5,color="black") + geom_line(aes(x=x,y=y ),lty=5,color="grey70") + ggtitle(expression(paste("     ",beta," = 0.5"))) 

#pdf("/Users/user/Downloads/grid.betas.pdf",width=10,height=1.5)
samples <- arrangeGrob(p1,p2,p3,p4,p5,p6,p7,nrow=1,widths=c(1.7,1,1,1,1,1,1))
#dev.off()





# prep --------------------------------------------------------------------


setwd("/Users/user/Desktop/Basid.Sex.Sim/Fig4_competition/")
#loads dataset
d1 <- read.csv("output.grouped.V5.3.beta.csv",row.names = NULL)
d1$trt <- sub("temp_","",d1$trt)
d2 <- separate(d1,trt,c("linkage","role","beta"),sep="_")
d2$role[d2$role==0] <- "Dominant"
d2$role[d2$role==1] <- "Co-Dominant"
d2$role[d2$role==2] <- "Recessive"
d2$role[d2$role==3] <- "No Dikaryon\nMale Role"
d2$role <- factor(d2$role,c("Dominant","Co-Dominant","Recessive","No Dikaryon\nMale Role"))
#betarotion maters is 
d2$non_zero <- (d2$zero1+d2$lt151+d2$gt151)-d2$zero1
d2$p_maters <- (d2$gt151+d2$gt152)/(d2$lt151+d2$gt151+d2$lt152+d2$gt152)
d2$p_maters1 <- (d2$gt151)/(d2$lt151+d2$gt151)
d2$p_maters2 <- (d2$gt152)/(d2$lt152+d2$gt152)
d2$mat      <- (d2$mat1+d2$mat2)/(d2$tot1+d2$tot2)
d_link   <- d2[d2$linkage=="lbeta",]
d_unlink <- d2[d2$linkage=="ubeta",]

# linked ------------------------------------------------------------------
lmaters1 <- ggplot(data=d_link,aes(x=gen,y=p_maters1)) +
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,colour="grey90"),
                     legend.position = "none",
                     strip.background=element_blank()) + 
  ggtitle("Linkage = 1.0 (no recombination)") +
  ylab("")
  #ylab("betaortion of\nmale function\nmating types 1")
lmaters2 <- ggplot(data=d_link,aes(x=gen,y=p_maters2)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text = element_blank(),
                     strip.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("")
  #ylab("betaortion of\nmale function\nmating types 2")
lparasites1 <- ggplot(data=d_link,aes(x=gen,y=mat1/tot1)) + geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_bw() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("")
  #ylab(" >66% Mating\nFitness\nNucleus 1")
lparasites2 <- ggplot(data=d_link,aes(x=gen,y=mat2/tot2)) + geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_bw() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  labs(lty="Initial betaortion of non-maters")+
  xlab("Generations") + ylab("")

lpop_size <- ggplot(data=d_link,aes(x=gen,y=non_zero)) + geom_line(aes(group=beta,lty=beta)) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-200,90200),breaks=c(0,30000,60000,90000))+
  theme_bw() + ylab("") + labs(lty=expression(beta))+
  theme(
    panel.grid.minor=element_blank(),
    axis.text.y=element_blank(),
    panel.grid.major=element_line(size=0.3,color="grey90"),
    strip.text.x=element_blank(),
    legend.position="bottom",
    legend.title=element_text(size=18)
  )



# unlinked ----------------------------------------------------------------
umaters1 <- ggplot(data=d_unlink,aes(x=gen,y=p_maters1)) +
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,colour="grey90"),
                     legend.position = "none",
                     strip.background = element_blank()) + 
  ggtitle("Linkage = 0.0 (free recombination)") +
  ylab("Proportion of\nmale function\nmating types 1")
umaters2 <- ggplot(data=d_unlink,aes(x=gen,y=p_maters2)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  theme_bw() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     strip.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("betaortion of\nmale function\nmating types 2")

uparasites1 <- ggplot(data=d_unlink,aes(x=gen,y=mat1/tot1)) + geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_bw() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab(" >66% Mating\nFitness\nNucleus 1")
uparasites2 <- ggplot(data=d_unlink,aes(x=gen,y=mat2/tot2)) + geom_line(aes(group=beta,lty=beta),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_bw() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  labs(lty="Initial betaortion of non-maters")+
  xlab("Generations") + ylab(">66% Mating\nFitness\nNucleus 2")

upop_size <- ggplot(data=d_unlink,aes(x=gen,y=non_zero)) + geom_line(aes(group=beta,lty=beta)) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-200,90200),breaks=c(0,30000,60000,90000))+
  theme_bw() + ylab("Population\nsize") + labs(lty=expression(beta))+
  theme(
    panel.grid.minor=element_blank(),
    panel.grid.major=element_line(size=0.3,color="grey90"),
    strip.text.x=element_blank(),
    legend.position="bottom",
    legend.title=element_text(size=18)
  )




# plotting ----------------------------------------------------------------


#grid.arrange(lmaters1,lparasites1,lmaters2,lparasites2,
#             nrow=4,heights=c(1.3,1,1,1.5))
#grid.arrange(umaters1,uparasites1,umaters2,uparasites2,
#             nrow=4,heights=c(1.3,1,1,1.5))
data <- arrangeGrob(umaters1,lmaters1,
             uparasites1,lparasites1,
             umaters2,lmaters2,
             uparasites2,lparasites2,
             upop_size,lpop_size,
             nrow=5,heights=c(1.55,1,1,1,2.0))
pdf("/Users/user/Desktop/Basid.Sex.Sim/Fig4_competition/fig_comp.beta.May18.pdf",width=12,height=9.3)
grid.arrange(samples,data,nrow=2,heights=c(0.3,1))
dev.off()

library("ggplot2")
library("tidyr")
library(gridExtra)

# prep --------------------------------------------------------------------


setwd("/Users/user/Desktop/BMS/fig_competition")
#loads dataset
d1 <- read.csv("output.grouped.V5.3.third.csv",row.names = NULL)
d1$trt <- sub("temp_","",d1$trt)
d2 <- separate(d1,trt,c("linkage","role","prop"),sep="_")
d2$role[d2$role==0] <- "Dominant\nDMF"
d2$role[d2$role==1] <- "Co-Dominant\nDMF"
d2$role[d2$role==2] <- "Recessive\nDMF"
d2$role[d2$role==3] <- "No Dikaryon\nMale Function"
d2$role <- factor(d2$role,c("Dominant\nDMF","Co-Dominant\nDMF","Recessive\nDMF","No Dikaryon\nMale Function"))
#proprotion maters is 
d2$non_zero <- (d2$zero1+d2$lt151+d2$gt151)-d2$zero1
d2$p_maters <- (d2$gt151+d2$gt152)/(d2$lt151+d2$gt151+d2$lt152+d2$gt152)
d2$p_maters1 <- (d2$gt151)/(d2$lt151+d2$gt151)
d2$p_maters2 <- (d2$gt152)/(d2$lt152+d2$gt152)
d2$mat      <- (d2$mat1+d2$mat2)/(d2$tot1+d2$tot2)
d_link   <- d2[d2$linkage=="link",]
d_unlink <- d2[d2$linkage=="unlink",]

# linked ------------------------------------------------------------------
lmaters1 <- ggplot(data=d_link,aes(x=gen,y=p_maters1)) +
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  theme_classic() + theme(axis.title.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,colour="grey90"),
                     plot.title = element_text(size=25),
                     strip.background = element_blank(),
                     legend.position = "none") + 
  ggtitle("Linkage = 1.0 (complete linkage)") +
  ylab("")
  #ylab("Proportion of\nmale function\nmating types 1")
lmaters2 <- ggplot(data=d_link,aes(x=gen,y=p_maters2)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  theme_classic() + theme(axis.title.x = element_blank(),
                     axis.text = element_blank(),
                     strip.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("")
  #ylab("Proportion of\nmale function\nmating types 2")
lparasites1 <- ggplot(data=d_link,aes(x=gen,y=mat1/tot1)) + geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_classic() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("")
  #ylab(" >66% Mating\nFitness\nNucleus 1")
lparasites2 <- ggplot(data=d_link,aes(x=gen,y=mat2/tot2)) + geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_classic() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  labs(lty="Initial Proportion of non-maters")+
  xlab("Generations") + ylab("")

lpop_size <- ggplot(data=d_link,aes(x=gen,y=non_zero)) + geom_line(aes(group=prop,lty=prop)) + facet_grid(cols=vars(d_link$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-200,90200),breaks=c(0,30000,60000,90000))+
  theme_classic() + ylab("") + xlab("Generation") +
  theme(
    panel.grid.minor=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x = element_text(size=15),
    panel.grid.major=element_line(size=0.3,color="grey90"),
    strip.text.x=element_blank(),
    legend.position="bottom"
  )



# unlinked ----------------------------------------------------------------
umaters1 <- ggplot(data=d_unlink,aes(x=gen,y=p_maters1)) +
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  theme_classic() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.title.y = element_text(size=15),
                     panel.grid.minor = element_blank(),
                     plot.title = element_text(size=25),
                     panel.grid.major = element_line(size=0.3,colour="grey90"),
                     strip.background = element_blank(),
                     legend.position = "none") + 
  ggtitle("Linkage = 0.0 (no linkage)") +
  ylab("DMF Allele\nFreq.\nNucleus 1")
umaters2 <- ggplot(data=d_unlink,aes(x=gen,y=p_maters2)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  theme_classic() + theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     strip.text = element_blank(),
                     axis.title.y = element_text(size=15),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("DMF Alelle\nFreq.\nNucleus 2")

uparasites1 <- ggplot(data=d_unlink,aes(x=gen,y=mat1/tot1)) + geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_classic() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_text(size=15),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  ylab("% Mating\nSpecialists\nNucleus 1")
uparasites2 <- ggplot(data=d_unlink,aes(x=gen,y=mat2/tot2)) + geom_line(aes(group=prop,lty=prop),size=0.6) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-0.02,1.02))+
  theme_classic() + theme(axis.title.x = element_blank(),
                     strip.text.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.title.y = element_text(size=15),
                     axis.ticks.x = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_line(size=0.3,color="grey90"),
                     legend.position = "none") + 
  labs(lty="Initial Proportion of non-maters")+
  xlab("Generations") + ylab("% Mating\nSpecialists\nNucleus 2")

upop_size <- ggplot(data=d_unlink,aes(x=gen,y=non_zero)) + geom_line(aes(group=prop,lty=prop)) + facet_grid(cols=vars(d_unlink$role)) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,1000),breaks=c(0,250,500,750))+
  scale_y_continuous(expand=c(0,0),limits=c(-200,90200),breaks=c(0,30000,60000,90000))+
  theme_classic() + ylab("Population\nsize") + xlab("Generation") +
  theme(
    panel.grid.minor=element_blank(),
    axis.title.y = element_text(size=15),
    axis.title.x = element_text(size=15),
    panel.grid.major=element_line(size=0.3,color="grey90"),
    strip.text.x=element_blank(),
    legend.position="bottom"
  )




# plotting ----------------------------------------------------------------


#grid.arrange(lmaters1,lparasites1,lmaters2,lparasites2,
#             nrow=4,heights=c(1.3,1,1,1.5))
#grid.arrange(umaters1,uparasites1,umaters2,uparasites2,
#             nrow=4,heights=c(1.3,1,1,1.5))
pdf("/Users/user/Desktop/BMS/fig_competition/fig_comp.Apr30.pdf",width=12,height=8.3)
grid.arrange(umaters1,lmaters1,
             uparasites1,lparasites1,
             umaters2,lmaters2,
             uparasites2,lparasites2,
             upop_size,lpop_size,
             nrow=5,heights=c(1.55,1,1,1,1.8),widths=c(0.55,0.45))
dev.off()


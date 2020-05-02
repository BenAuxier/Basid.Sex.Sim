library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(cowplot)

linkage <- data.frame()
reps <- c("1","2","3","4")

for (i in reps){
  d1 <- read.csv(paste0("/Users/user/Desktop/BMS/fig_stubborn_norm_V5.2/linkage_vary_",i,".parasite_summary.txt"),header=T)
  d1$rep <- i
  linkage <- rbind(linkage,d1)}
rm(d1)

linkage <- separate(linkage,file,c("variable","value"),sep="_")
linkage$value <- as.numeric(gsub(".dat","",linkage$value))

#makes unique ids for grouping
linkage$id <- paste0(linkage$variable,linkage$value)

linkage <- linkage %>% group_by(id) %>% summarize(m0.1 = mean(gt0.1),m0.2 = mean(gt0.2),m0.3 = mean(gt0.3),
                                                  m0.4 = mean(gt0.4),m0.5 = mean(gt0.5),m0.6 = mean(gt0.6),
                                                  m0.7 = mean(gt0.7),m0.8 = mean(gt0.8),m0.9 = mean(gt0.9),
                                                  mzero = mean(non.zero),value=value[1],variable=variable[1])

p1 <- ggplot(linkage) + theme_bw() + ggtitle("A)") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=25),
        panel.spacing=unit(1.0,"lines"),
        axis.title=element_text(size=15),
        axis.text =element_text(size=15))+
  scale_x_continuous(limits=c(0,1),expand=c(0,0))+ scale_y_continuous(expand=c(0,0))+
  geom_area(aes(x=value,y=m0.1/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.2/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.3/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.4/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.5/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.6/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.7/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.8/mzero),fill="darkorchid4",alpha=0.1) +
  geom_area(aes(x=value,y=m0.9/mzero),fill="darkorchid4",alpha=0.1) +
  labs(x="Linkage",y="Proportion of nuclei")
#ggsave("C:\\Users\\auxie001\\Downloads\\fig_stubborn_norm\\test_linkage_plot.png")

subset<- linkage[linkage$value %in% c(0.95,0.99,1.00),]
subset$value <- as.factor(subset$value)
p2 <- ggplot(subset) + theme_bw() + ggtitle("B)")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(1.0,"lines"),
        plot.title=element_text(size=25),
        axis.title=element_text(size=15),
        axis.text =element_text(size=15))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="",x="")+
  geom_col(aes(x=value,y=m0.1/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.2/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.3/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.4/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.5/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.6/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.7/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.8/mzero),fill="darkorchid4",alpha=0.1) +
  geom_col(aes(x=value,y=m0.9/mzero),fill="darkorchid4",alpha=0.1)

comb <- data.frame()
reps <- c("1","2","3","4")

for (i in reps){
  d1 <- read.csv(paste0("/Users/user/Desktop/BMS/fig_stubborn_norm_V5.2/linkage_0.95_",i,".parasite_summary.txt"),header=T)
  d1$linkage <- "0.95"
  d1$rep <- i
  comb <- rbind(comb,d1)}
for (i in reps){
  d1 <- read.csv(paste0("/Users/user/Desktop/BMS/fig_stubborn_norm_V5.2/linkage_0.99_",i,".parasite_summary.txt"),header=T)
  d1$linkage <- "0.99"
  d1$rep <- i
  comb <- rbind(comb,d1)}
for (i in reps){
  d1 <- read.csv(paste0("/Users/user/Desktop/BMS/fig_stubborn_norm_V5.2/linkage_1.00_",i,".parasite_summary.txt"),header=T)
  d1$linkage <- "full"
  d1$rep <- i
  comb <- rbind(comb,d1)}
rm(d1)

comb <- separate(comb,file,c("variable","value"),sep="_")
comb$value <- as.numeric(gsub(".dat","",comb$value))

#makes unique ids for grouping
comb$id <- paste0(comb$variable,comb$value,comb$linkage)

#here comes the ggplot magic
summarized <- comb %>% group_by(id) %>% summarize(m0.1 = mean(gt0.1),m0.2 = mean(gt0.2),m0.3 = mean(gt0.3),
                                    m0.4 = mean(gt0.4),m0.5 = mean(gt0.5),m0.6 = mean(gt0.6),
                                    m0.7 = mean(gt0.7),m0.8 = mean(gt0.8),m0.9 = mean(gt0.9),
                                    mzero = mean(non.zero),linkage=linkage[1],value=value[1],variable=variable[1])

summarized$variable[summarized$variable=="mt"] <- "# of mating types"
summarized$variable[summarized$variable=="dominance"] <- "Phenotypic Dominance"
summarized$variable[summarized$variable=="spores"] <- "# of spores per cell"
summarized$variable[summarized$variable=="vgr"] <- "Environmental Growth Rate"
summarized$linkage[summarized$linkage=="0.95"] <- "Linkage = 0.95"
summarized$linkage[summarized$linkage=="0.99"] <- "Linkage = 0.99"
summarized$linkage[summarized$linkage=="full"] <- "Linkage = 1.00"

p3 <- ggplot(summarized) + theme_bw() + 
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing=unit(1.0,"lines"),
        plot.title=element_text(size=25),
        axis.title=element_text(size=15),
        axis.text = element_text(size=15),
        strip.text = element_text(size=15),
        strip.text.y = element_text(angle=0))+
  geom_area(aes(x=value,y=m0.1/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.2/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.3/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.4/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.5/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.6/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.7/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.8/mzero),fill="darkorchid4",alpha=0.1)  + 
  geom_area(aes(x=value,y=m0.9/mzero),fill="darkorchid4",alpha=0.1)  + 
  ylab("Proportion of nuclei")+
  facet_grid(cols=vars(summarized$variable),rows=vars(summarized$linkage),scales="free",
             labeller = label_wrap_gen(width=14)) + 
  scale_y_continuous(expand=c(0,0),limits=c(0,1))+ 
  scale_x_continuous(expand=c(0,0))+
  ggtitle("C)")

#now need to make a bit of a hacky title
legend_data <- data.frame(y = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
                          x = rep(1,10))

legend <- ggplot(legend_data) + 
  geom_rect(aes(xmin=x-0.01,xmax=x+0.005,ymin=1-y,ymax=1),alpha=0.1,fill="darkorchid4",color="black",linejoin="bevel") +
  geom_text(aes(x=1.03,y=y+0.05,label=y),size=5)+
  xlim(0.99,1.05)+
  labs(x="",y="",title="Per nucleus\npercent\nmating\nfitness") +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size=15)
  )

#ggsave("C:\\Users\\auxie001\\Downloads\\fig_stubborn_norm\\test_plot.png")
top_row <- plot_grid(p1,p2,legend,ncol=3,rel_widths=c(0.55,0.3,0.15))
pdf("/Users/user/Desktop/BMS/fig_stubborn_norm_V5.2/fig_stubborn_norm_Apr30.pdf",width=10,height=10)
plot_grid(top_row,p3,nrow=2,rel_heights=c(0.3,0.7))
dev.off()



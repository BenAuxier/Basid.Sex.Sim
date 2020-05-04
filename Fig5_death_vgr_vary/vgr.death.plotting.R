library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplotify)
library(cowplot)

standard   <- read.table("/Users/user/Desktop/Basid.Sex.Sim/Fig5_death_vgr_vary/my_fy_mating_summary.txt",sep=",",header=T)
normal <- read.table("/Users/user/Desktop/Basid.Sex.Sim/Fig5_death_vgr_vary/my_fn_mating_summary.txt",sep=",",header=T)
closed <- read.table("/Users/user/Desktop/Basid.Sex.Sim/Fig5_death_vgr_vary/mn_fn_mating_summary.txt",sep=",",header=T)

combined <- data.frame(rbind(standard,normal,closed))

#check if combined$both_empty and combined$firt_empty are the same
sum(combined$first_empty == combined$both_empty)
#should equal 300

#now we need to calculate the percentage empty
combined$p_survive <- 1-(combined$both_empty/combined$cells)

#now for the percentage homokaryotic cells
combined$p_homo  <- (combined$second_empty-combined$first_empty)/combined$cells

#calculate the percentage homokaryotic out of cells that are not empty
# if 4 cells out of 10 were empty and 1 was homokaryotic, 5 heterokaryotic
# then p_survive = 1-4/10
# and  p_homo  = (6-5)/10
# and rel_homo = 1/(10-4)
combined$rel_homo <- (combined$second_empty - combined$first_empty) / 
                     (combined$cells - combined$both_empty)
combined$p_dikaryon <- 1-combined$rel_homo
combined$file <- gsub("snap_","",combined$file)
combined$file <- gsub(".dat","",combined$file)
split <- separate(combined,file,c("male","female","death","vgr"),sep="_")
split2 <- unite(split,"role",c("male","female"))
split2$vgr <- as.factor(split2$vgr)
split2$death <- as.factor(split2$death)
split2[split2 == "mn_fn"] <- "Diploid"
split2[split2 == "my_fn"] <- "Standard"
split2[split2 == "my_fy"] <- "Open"


theme_heatmap <- function () { 
  theme_bw(base_size=12) %+replace% 
    theme(
      strip.text.x = element_text(size=18),
      axis.text = element_text(size=11),
      title = element_text(size=18),
      panel.background  = element_blank(),
      strip.background = element_blank(),
      #plot.background = element_rect(fill="grey70", color = NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}

pop <- ggplot(split2) + theme_heatmap() +
  geom_tile(aes(x=vgr,y=death,fill=p_survive,width=1.1,height=1.1)) + 
  scale_fill_gradient(low="white",high="black") + 
  scale_x_discrete(expand=c(0,0),breaks=c(0.1,0.3,0.5,0.7,0.9)) + 
  scale_y_discrete(expand=c(0,0),breaks=c(0.1,0.3,0.5,0.7,0.9)) +
  labs(x="Basal Vegetative Growth Rate (g)",y="Death Rate (d)",fill="Relative\nPopulation \nSize")+
  facet_grid(cols=vars(split2$role))+
  ggtitle("A) Population Size")

unmated <- ggplot(split2) + theme_heatmap() + 
  geom_tile(aes(x=vgr,y=death,fill=p_dikaryon,color=""),width=1.1,height=1.1) + 
  scale_fill_gradient(low="white",high="black",na.value="orange") +
  scale_color_manual(values=NA)+
  scale_x_discrete(expand=c(0,0),breaks=c(0.1,0.3,0.5,0.7,0.9)) + 
  scale_y_discrete(expand=c(0,0),breaks=c(0.1,0.3,0.5,0.7,0.9)) +
  labs(x="Basal Vegetative Growth Rate (g)",y="Death Rate (d)",fill="Proprotion \nDikaryotic")+
  facet_grid(cols=vars(split2$role)) +
  guides(colour=guide_legend("Dead", override.aes=list(colour="black",fill="orange")))+
  ggtitle("B) Proportion dikaryotic of occupied cells")

mean_survive <- c(mean(split2$p_survive[split2$role=="Diploid"]),
                  mean(split2$p_survive[split2$role=="Standard"]),
                  mean(split2$p_survive[split2$role=="Open"]))

mean_dikaryon <- c(mean(split2$p_dikaryon[split2$role=="Diploid"],na.rm=T),
                   mean(split2$p_dikaryon[split2$role=="Standard"],na.rm=T),
                   mean(split2$p_dikaryon[split2$role=="Open"],na.rm=T))

overview <- data.frame(mean_mating = mean_dikaryon,
                       mean_pop = mean_survive,
                       role = c("Diploid","Standard","Open"))

plot_overview <- ggplot(overview,aes(x=mean_dikaryon,y=mean_survive)) + 
  geom_point(size=3) + theme_bw() + ylim(0,0.8) + xlim(0,0.8) +
  theme(
    axis.text=element_text(size=9),
    title = element_text(size=18))+
  labs(x = "Mean Proportion Dikaryotic",y = "Mean\nPop. Size")+
  geom_text(aes(label=role,x=mean_mating-0.02,y=mean_survive),hjust="right",size=5) +
  ggtitle("C) Mean population size\n   and proportion dikaryotic")

overview_nested <- plot_grid(NULL,plot_overview,NULL,ncol=3,rel_widths=c(0.2,0.35,0.45))

pdf("/Users/user/Desktop/Basid.Sex.Sim/Fig5_death_vgr_vary/fig_death_vgr.May4.pdf",width=10,height=10)
plot_grid(pop,unmated,overview_nested,nrow=3)
dev.off()




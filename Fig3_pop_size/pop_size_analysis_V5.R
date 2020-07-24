library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(gridExtra)
pops <- data.frame("ngen" = seq(1,2000,5))

files <- c("mn_fn_0.00","mn_fn_0.50","mn_fn_0.75",
           "mn_fn_0.90","mn_fn_0.95","mn_fn_0.99",
           "mn_fn_1.00",
           "my_fn_0.00","my_fn_0.50","my_fn_0.75",
           "my_fn_0.90","my_fn_0.95","my_fn_0.99",
           "my_fn_1.00",
           "my_fy_0.00","my_fy_0.50","my_fy_0.75",
           "my_fy_0.90","my_fy_0.95","my_fy_0.99",
           "my_fy_1.00")

#now ignore 0.50 and 0.95 they only make the figure more complicated
files <- c("mn_fn_0.00","mn_fn_0.75",
           "mn_fn_0.90","mn_fn_0.99",
           "mn_fn_1.00",
           "my_fn_0.00","my_fn_0.75",
           "my_fn_0.90","my_fn_0.99",
           "my_fn_1.00",
           "my_fy_0.00","my_fy_0.75",
           "my_fy_0.90","my_fy_0.99",
           "my_fy_1.00")

for (i in files){
filename=paste0("/Users/user/Desktop/Basid.Sex.Sim/Fig3_pop_size/",i,"_output.txt")
d1 <- readChar(filename, file.info(filename)$size)
d2 <- gsub("\n+","\n",d1, perl=TRUE)
d3 <- gsub(" +"," ",d2,perl=TRUE)
d4 <- gsub(".*Gen", "Gen",d3)
d5 <- gsub("\n ","\n",d4)
temp_dat <- read.delim(text=d5,sep=" ",header=TRUE)
temp_dat <- temp_dat[2:nrow(temp_dat),]
pops[[i]] <- temp_dat$nind
}

data <- pops %>% gather(set,size,mn_fn_0.00:my_fy_1.00)

data <- separate(data,set,c("role","linkage"),sep=6)

data$role <- gsub("mn_fn_","Diploid",data$role)
data$role <- gsub("my_fy_","Open Dikaryon", data$role)
data$role <- gsub("my_fn_","Standard Dikaryon", data$role)

data$role <- factor(data$role,levels=c("Diploid","Standard Dikaryon","Open Dikaryon"))

#data = data[which(data$ngen %in% seq(1,2000,20)), ]

data.segm<-data.frame(
  x=c(rep(2000,18)),
  y=       c(83300,76834, 76349, 70647, 64839, 59474,
             56700,54257, 49511, 14190,     0,     0,
             15000,    0,     0,     0,     0,     0),
  role=c(rep("Diploid",6),rep("Standard Dikaryon",6),rep("Open Dikaryon",6)),
  text=rep(c("Linkage","1.00","0.99","0.90","0.75","0.00"),3)
)

#now I have the values for the di-mon fitness and I want to plot them underneath. This will happen by pulling out the right columns from the updated .txt files
#png("C:\\Users\\auxie001\\Downloads\\pop_size_download_V5\\population_size.Sep2.png",width=1500,height=800)
p1 <- ggplot(data, aes(x=ngen,group=linkage)) + theme_classic() +
  geom_segment(x=-10,xend=-10,y=-6000,yend=90000,lty=3,lwd=1)+
  geom_segment(x=500,xend=500,y=-6000,yend=90000,lty=3,lwd=1)+
  geom_line(aes(x=ngen,y=size),lwd=0.85) + 
  scale_y_continuous(limits=c(-4500,96000),breaks=c(0,15000,30000,45000,60000,75000,90000),labels=c("0","15000","30000","45000","60000","75000","90000"))+
  scale_x_continuous(limits=c(0,2550),breaks=c(0,500,1000,1500,2000),labels=c("0","500","1000","1500","2000")) +
  geom_text_repel(max.iter=10000,data=data.segm,mapping=aes(x=x,y=y,label=text),xlim=c(2000,2450),ylim=c(-7000,93000),inherit.aes=FALSE,size=10) +
  labs(x="Generation",y="Population Size")+
  theme(axis.title.y=element_text(angle=90,vjust=0.5),
        panel.grid.minor= element_blank(),
        panel.grid.major= element_line(size=0.5,color="grey50"),
        legend.position="right",
        axis.title = element_text(size=25),
        plot.title = element_text(size=35),
        strip.text = element_text(size=30),
        legend.title = element_text(size=30),
        legend.text  = element_text(size=25),
        axis.text = element_text(size=20),
        panel.spacing.x = unit(2.5,"lines")) +
  facet_grid(cols=vars(role))
#p1
#dev.off()


fitness <- data.frame()

for (i in files){
  filename=paste0("/Users/user/Desktop/Basid.Sex.Sim/Fig3_pop_size/",i,"_output.txt")
  d1 <- readChar(filename, file.info(filename)$size)
  d2 <- gsub("\n+","\n",d1, perl=TRUE)
  d3 <- gsub(" +"," ",d2,perl=TRUE)
  d4 <- gsub(".*Gen", "Gen",d3)
  d5 <- gsub("\n ","\n",d4)
  temp_dat <- read.delim(text=d5,sep=" ",header=T,stringsAsFactors=FALSE)
  temp_dat <- temp_dat[2:nrow(temp_dat),]
  temp_dat$veg_1 <- as.numeric(temp_dat$veg_1)
  temp_dat$spo_1 <- as.numeric(temp_dat$spo_1)
  temp_dat$mat_1 <- as.numeric(temp_dat$mat_1)
  #we need a spacer to move the bar up when the nucleus is empty in the open dikaryon
  temp_dat$spacer <- 0
  temp_dat$spacer[is.na(temp_dat$mat_1)] <- 1
  #time to make the bar separating the male/female nuclei
  temp_dat$bar <- 0.1
  #temp_dat$sum <- temp_dat$avg_cell1 + temp_dat$avg_cell2 + temp_dat$avg_cell3
  temp_dat$veg_2 <- as.numeric(temp_dat$veg_2)
  temp_dat$spo_2 <- as.numeric(temp_dat$spo_2)
  temp_dat$mat_2 <- as.numeric(temp_dat$mat_2)
  temp_dat <- temp_dat[2:nrow(temp_dat),]
  temp_dat <- temp_dat[c("Gen","veg_1","spo_1","mat_1","spacer","bar","veg_2","spo_2","mat_2")]
  temp_gather <- temp_dat %>% gather(component,proprortion,veg_1:mat_2)
  temp_gather$set <- i
  fitness <- rbind(fitness,temp_gather)
}
fitness$component <- gsub("avg_","",fitness$component)
fitness <- separate(fitness,set,c("role","linkage"),sep=6)

fitness$role <- gsub("mn_fn_","Diploid", fitness$role)
fitness$role <- gsub("my_fy_","Open Dikaryon", fitness$role)
fitness$role <- gsub("my_fn_","Standard Dikaryon", fitness$role)

fitness$role <- factor(fitness$role,levels=c("Diploid","Standard Dikaryon","Open Dikaryon"))

text.segm<-data.frame(
  x       =rep(50,3),
  y       =c(940,500,150),
  role    =rep("Diploid",3),
  linkage=rep("0.00",3),
  text    =c("Mating Fitness","Spore Production","Vegetative Growth"))

#now I have the values for the di-mon fitness and I want to plot them underneath. This will happen by pulling out the right columns from the updated .txt files

fitness[is.na(fitness)] <- 0
fitness[fitness == "NaN"] <- 0

fitness$linkage <- factor(fitness$linkage,levels=c("1.00","0.99","0.90","0.75","0.00"))

#png("C:\\Users\\auxie001\\Downloads\\pop_size_download_V5\\fitness_components.Sep2.png",width=2000,height=700)
p2 <- ggplot(fitness, aes(x=Gen,group=linkage)) + theme_classic() +
  geom_col(aes(y=proprortion,fill=component),width=5)+
  scale_x_continuous(limits=c(0,500),breaks=c(0,100,200,300,400,500),labels=c("0","100","200","300","400","500")) +
  #geom_text(data=text.segm,mapping=aes(x=x,y=y,label=text),inherit.aes=FALSE,size=9,hjust="left") +
  labs(x="Generation",y="\nProportion of Fitness\n")+
  scale_fill_manual(name="component",values=c("spo_1"="yellow2","veg_1"="green2","mat_1"="firebrick1",
                                              "bar"="black","spacer"="white",
                                              "spo_2"="yellow2","mat_2"="firebrick1","veg_2"="green2")) +
  scale_y_continuous(breaks=c(0.1,0.92),labels=c("0","1"))+
  theme(axis.title.y=element_text(angle=90,vjust=0.5),
        panel.grid.minor= element_blank(),
        legend.position="none",
        axis.title = element_text(size=25),
        plot.title = element_text(size=35),
        strip.text = element_text(size=30),
        legend.title = element_text(size=30),
        legend.text  = element_text(size=25),
        axis.text.x = element_text(size=25),
        axis.text.y = element_text(size=25),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(2.5,"lines"),
        panel.spacing.y = unit(1,"lines"),
        strip.text.x=element_blank()) + 
  facet_grid(cols=vars(role),rows=vars(linkage))
p2

png("/Users/user/Desktop/Basid.Sex.Sim/Fig3_pop_size/fig_pop_size.May27.png",width=1000,height=800)
grid.arrange(p1,p2,nrow=2,heights=c(0.5,0.5))
dev.off()


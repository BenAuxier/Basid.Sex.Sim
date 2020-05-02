library(ggplot2)
library(gridExtra)
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

pdf("/Users/user/Downloads/grid.betas.pdf",width=10,height=1.5)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=1,widths=c(1.7,1,1,1,1,1,1))
dev.off()


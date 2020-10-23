

library(ggplot2)
library(ggpubr)

QPA_Feb17 <- read.csv("Biplot/QPA_Feb17.csv")
head(QPA_Feb17)

QPA_Feb17$taxa <- factor(QPA_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "Phylloicus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "Machrobrachium", "A. evermani"))
levels(QPA_Feb17$taxa) 

  ggplot(QPA_Feb17, aes(x=C, y=N)) +
  geom_point(aes(colour=taxa), size=4) +
  labs(x= expression(delta^{13}*"C (???)"), y = expression(delta^{15}*"N (???)"))+
  
  geom_segment(aes(x=-31.91,xend=-32.25,yend=0.56,y=0.56), size=2, color="#d95f0e") + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=0.878914827,y=0.241085173), size=2, color="#d95f0e") + # N leaflitter
  
  geom_segment(aes(x=-31.48,xend=-31.90,yend=6.06,y=6.06), size=2, color="#2c7fb8")+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.389520784,y=5.730479216), size=2, color="#2c7fb8")+ # N biofilm
  
  geom_segment(aes(x=-30.79,xend=-31.81,yend=11.37138181,y=11.37138181), size=2, color="#31a354") +# C algae
  geom_segment(aes(x=-31.29893075,xend=-31.29893075,yend=17.153228921,y=5.589534699), size=2, color="#31a354") +# algae
  xlim(-40,-20) +
  ylim(-5,20) +

  theme(axis.title.y = element_text(size = 12, angle = 90)) + 
  theme(axis.title.x = element_text(size = 12, angle = 00)) +
  
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

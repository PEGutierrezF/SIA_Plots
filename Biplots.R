

library(ggplot2)
library(ggpubr)

QPA_Feb17 <- read.csv("Biplot/QPA_Feb17.csv")
head(QPA_Feb17)

QPA_Feb17$taxa <- factor(QPA_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                  "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                  "M. crenulatum", "A. evermani"))
levels(QPA_Feb17$taxa) 

  ggplot(QPA_Feb17, aes(x=C, y=N, group=taxa, shape=taxa)) +
  geom_point(aes(colour=taxa), size=5,stroke = 1.5) +
  labs(x= expression(delta^{13}*"C (\211)"), y = expression(delta^{15}*"N (\211)")) +
#color
      scale_colour_manual("Taxa",
                        values = c("#276419", "#4d9221", "#b35806", "#fdb863", "#2166ac", 
                                   "#D55E00", "#fdae61", "#f46d43", "#d73027", "#003c30"),
                        labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                   expression(italic("P. pulchrus")), "Libellulidae", 
                                   expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                   expression(italic("M. crenulatum")), expression(italic("A. evermani")))) +
# shape
    scale_shape_manual("Taxa",
                       values=c(0,1,2,3,4,5,6,7,8,9,10),
                       labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                  expression(italic("P. pulchrus")), "Libellulidae", 
                                  expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                  expression(italic("M. crenulatum")), expression(italic("A. evermani"))))  +
# Segments  
  geom_segment(aes(x=-31.91,xend=-32.25,yend=0.56,y=0.56), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=0.878914827,y=0.241085173), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter

  geom_segment(aes(x=-31.48,xend=-31.90,yend=6.06,y=6.06), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.389520784,y=5.730479216), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  
  geom_segment(aes(x=-30.69, xend=-31.88,yend=11.37138181,y=11.37138181), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-31.28781318,xend=-31.28781318,yend=17.441,y=5.1166), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae

# Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
    
# Axis
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 12, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
    
# Legend    
    theme(legend.title = element_text(color = "red", size = 14),
      legend.text = element_text(color = "black", size = 12)) +
    theme_set(theme_gray() + theme(legend.key=element_blank()))  + # gray background
    theme(legend.text.align = 0) + # text align left legend
    theme(legend.key.size = unit(2, 'lines')) +
    theme(legend.position = c(0.9, 0.7)) +# Position in a plot. Their values should be between 0 and 1. 
    
# Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

  

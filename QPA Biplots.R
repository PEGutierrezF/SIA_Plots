

library(ggplot2)
library(ggpubr)
library(patchwork)


# QPA February 17 ---------------------------------------------------------


QPA_Feb17 <- read.csv("Biplot/QPA_Feb17.csv")
head(QPA_Feb17)

QPA_Feb17$taxa <- factor(QPA_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                  "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                  "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPA_Feb17$taxa) 

QPAFeb17 <-  ggplot(QPA_Feb17, aes(x=C, y=N, group=taxa, shape=taxa)) +
  geom_point(aes(colour=taxa), size=3 ,stroke = 1.2) +
  labs(x= "", y = expression(delta^{15}*"N (\211)")) +
#color
      scale_colour_manual("Taxa",
                        values = c("#276419", "#4d9221", "#b35806", "#fdb863", "#2166ac", 
                                   "#D55E00", "#fdae61", "#f46d43", "#d73027", "#003c30", "#003c30"),
                        labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                   expression(italic("P. pulchrus")), "Libellulidae", 
                                   expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                   expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                   expression(italic("L. regnyi")))) +
# shape
    scale_shape_manual("Taxa",
                       values=c(0,1,2,3,4,5,6,7,8,9,10,11),
                       labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                  expression(italic("P. pulchrus")), "Libellulidae", 
                                  expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                  expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                  expression(italic("L. regnyi"))))  +
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
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
    
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
# Legend  
                    # =element_text(color = "white", size=14)
  theme(legend.title=element_blank(),
        legend.text =element_text(color = "black", size=8),
        legend.position=c(0.18, 0.72),# Position in a plot. Their values should be between 0 and 1. X y Y
        legend.key=element_blank(), # gray background
        legend.key.size = unit(1.1, 'lines'),
        legend.text.align = 0) +  # text align left legend
  
# Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +


QPAFeb17





# QPA November 2017 -------------------------------------------------------


QPA_Nov17 <- read.csv("Biplot/QPA_Nov17.csv")
head(QPA_Nov17)

QPA_Nov17$taxa <- factor(QPA_Nov17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPA_Nov17$taxa) 

QPANov17 <-  ggplot(QPA_Nov17, aes(x=C, y=N, group=taxa, shape=taxa)) +
  geom_point(aes(colour=taxa), size=3,stroke = 1.2) +
  labs(x= "", y = expression(delta^{15}*"N (\211)")) +
  #color
  scale_colour_manual("Taxa",
                      values = c("#276419", "#4d9221", "#b35806", "#fdb863", "#2166ac", 
                                 "#D55E00", "#fdae61", "#f46d43", "#d73027", "#003c30", "#003c30"),
                      labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                 expression(italic("P. pulchrus")), "Libellulidae", 
                                 expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                 expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                 expression(italic("L. regnyi")))) +
  # shape
  scale_shape_manual("Taxa",
                     values=c(0,1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                expression(italic("P. pulchrus")), "Libellulidae", 
                                expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                expression(italic("L. regnyi"))))  +
  # Segments  
  geom_segment(aes(x=-31.3557,xend=-31.77720,yend=0.9295,y=0.9295), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-31.5665,xend=-31.5665,yend=1.1411,y=0.7178), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  
  geom_segment(aes(x=-26.270,xend=-26.3690,yend=2.413,y=2.413), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-26.319,xend=-26.319,yend=3.03212,y=1.7938), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  
  geom_segment(aes(x=-18.232, xend=-26.769,yend=3.5359,y=3.5359), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-22.5013,xend=-22.50128,yend=4.864,y=2.2073), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  
  # Axis Limits 
  xlim(-43,-17) +
  ylim(-5,20) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
 # theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
 #  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
QPANov17





# QPA February 2019 -------------------------------------------------------


QPA_Feb19 <- read.csv("Biplot/QPA_Feb19.csv")
head(QPA_Feb19)

QPA_Feb19$taxa <- factor(QPA_Feb19$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPA_Feb19$taxa) 

QPAFeb19 <-  ggplot(QPA_Feb19, aes(x=C, y=N, group=taxa, shape=taxa)) +
  geom_point(aes(colour=taxa), size=3,stroke = 1.2) +
  labs(x= expression(delta^{13}*"C (\211)"), y = expression(delta^{15}*"N (\211)")) +
  #color
  scale_colour_manual("Taxa",
                      values = c("#276419", "#4d9221", "#b35806", "#fdb863", "#2166ac", 
                                 "#D55E00", "#fdae61", "#f46d43", "#d73027", "#003c30", "#003c30"),
                      labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                 expression(italic("P. pulchrus")), "Libellulidae", 
                                 expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                 expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                 expression(italic("L. regnyi")))) +
  # shape
  scale_shape_manual("Taxa",
                     values=c(0,1,2,3,4,5,6,7,8,9,10,11),
                     labels = c("Glossosomatidae", "Baetidae", "Chironomidae",expression(italic( "N. julio")),
                                expression(italic("P. pulchrus")), "Libellulidae", 
                                expression(italic("X. elongata")), expression(italic("A. lanipes")),
                                expression(italic("M. crenulatum")), expression(italic("A. evermani")),
                                expression(italic("L. regnyi"))))  +
  # Segments  
  geom_segment(aes(x=-28.297,xend=-28.596,yend=1.153,y=1.153), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-28.447,xend=-28.447,yend=1.4256,y=0.881), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  
  geom_segment(aes(x=-27.758,xend=-27.981,yend=4.18,y=4.18), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.87,xend=-27.87,yend=4.439,y=3.9213), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  
  geom_segment(aes(x=-25.7792, xend=-27.909,yend=9.433,y=9.433), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-26.844,xend=-26.844,yend=14.361,y=4.505), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Legend    
  theme(legend.position = "none") +

  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

QPAFeb19



Figure_2 <- (QPAFeb17 | QPBFeb17) / (QPANov17 | QPBNov17) / 
( plot_spacer()| plot_spacer()) / (QPAFeb19 | plot_spacer()) 
Figure_2

Figure_2 + plot_annotation(tag_levels = 'A')
Figure_2 + ggsave("Figure_2.pdf", width = 210, height = 297, units = "mm")

+ plot_layout(guides="collect")

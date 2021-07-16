



# ---------------------------------------------
# Biplots for Quebrada Prieta A
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# QPA February 17 ---------------------------------------------------------


QPA_Feb17 <- read.csv("Biplot/QPA_Feb17.csv")
head(QPA_Feb17)

QPA_Feb17$taxa <- factor(QPA_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                  "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                  "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPA_Feb17$taxa) 

QPAFeb17 <-  ggplot(QPA_Feb17, aes(x=C, y=N, group=taxa, shape=taxa)) +
  geom_point(aes(colour=taxa), size=3 ,stroke = 1.2) +
  labs(x="",y = expression(delta^{15}*"N (\211)"),fill = "Taxa") +
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
  geom_segment(aes(x=-31.90410042,xend=-31.77720437,yend=0.56,y=0.56), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=0.878914827,y=0.241085173), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.08, y = 0.56), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-31.48,xend=-31.90,yend=6.06,y=6.06), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.389520784,y=5.730479216), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -31.69, y = 6.06), shape=17,color = "turquoise3", size=5)+ 
  
  geom_segment(aes(x=-30.69, xend=-31.88,yend=11.278808,y=11.278808), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-31.28781318,xend=-31.28781318,yend=17.441,y=5.1166), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -31.28781318, y = 11.278808), shape=19,color = "greenyellow", size=5)+ 

  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
    
# Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
    
#  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -27, y = 12, label = "Algae") +
  annotate("text", x = -36, y = 7, label = "Biofilm") +
  annotate("text", x = -24, y = 5, label = "Leaf litter") +
  
  geom_segment(aes(x =-26, xend=-30.5,y = 4, yend = 1.75), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
# Legend  
        theme(legend.title= element_text(colour="black", size=16, face="bold"),
        legend.text =element_text(color = "black", size=15),
        legend.position=c(0.18, 0.72),# Position in a plot. Their values should be between 0 and 1. X y Y
        legend.key=element_blank(), # gray background
        legend.key.size = unit(1.1, 'lines'),
        legend.text.align = 0) +  # text align left legend
  
# Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 


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
  labs(x="", y = "") +
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
  geom_segment(aes(x=-31.3557,xend=-31.77720,yend=0.9295,y=0.9295), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-31.5665,xend=-31.5665,yend=1.1411,y=0.7178), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -31.5665, y = 0.9295), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-26.270,xend=-26.3690,yend=2.413,y=2.413), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-26.319,xend=-26.319,yend=3.03212,y=1.7938), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -26.319, y = 2.413), shape=17,color = "turquoise3", size=5)+ 
  
  geom_segment(aes(x=-18.232, xend=-26.769,yend=3.5359,y=3.5359), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-22.5013,xend=-22.50128,yend=4.864,y=2.2073), size=1,linetype='solid', color="black",arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -22.5013, y = 3.5359), shape=19,color = "greenyellow", size=5)+ 
  
  # Axis Limits 
  xlim(-43,-17) +
  ylim(-5,20) +
  
  #text
  annotate("text", x = -26, y = 7, label = "Algae") +
  annotate("text", x = -35, y = 5, label = "Biofilm") +
  annotate("text", x = -38, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-32, xend=-28,y = 4, yend = 3), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  geom_segment(aes(x =-36, xend=-33,y = -3, yend = 0), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
 # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
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
  labs(x="", y = "") +
  
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
  geom_segment(aes(x=-28.297,xend=-28.596,yend=1.153,y=1.153), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-28.447,xend=-28.447,yend=1.4256,y=0.881), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -28.447, y = 1.153), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.758,xend=-27.981,yend=4.18,y=4.18), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.87,xend=-27.87,yend=4.439,y=3.9213), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.87, y = 4.18), shape=17,color = "turquoise3", size=5)+
  
  geom_segment(aes(x=-25.7792, xend=-27.909,yend=9.433,y=9.433), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-26.844,xend=-26.844,yend=14.361,y=4.505), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -26.844, y = 9.433), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  #text
  annotate("text", x = -22, y = 11, label = "Algae") +
  annotate("text", x = -35, y = 9, label = "Biofilm") +
  annotate("text", x = -38, y = -3, label = "Leaf litter") +
  
  geom_segment(aes(x =-32, xend=-29,y = 8, yend = 5), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  geom_segment(aes(x =-35, xend=-30,y = -2, yend = 0), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
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




# ggarrange -------------------------------------------------------------

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p2_legend <- get_legend(QPAFeb17)

p1 <- ggarrange(QPAFeb17, QPANov17, QPBJune18, QPAFeb19,
                QPBFeb17, QPBNov17, QPBJune18, QPBFeb19,
                ncol=4,nrow=2, labels = c("A", "B", "C","D","E","F","G","H"),
                widths = c(0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75),
                common.legend = TRUE, legend="bottom")


p1 + ggsave("Figure_2B.pdf", width=11, height=6.5)




square <- "???"
triangle <- "???"
circle<- "???"

p2<- annotate_figure(p1,bottom = text_grob(" \n \n \n", color = "blue",
                                      hjust = 1, x = 1, face = "italic", size = 10))

p3 <- ggdraw(p2) + draw_label("Sources", x = 0.2, y = 0.09, size = 16, fontface = "bold",
                              fontfamily = "Helvetica") +
  
  draw_label("Leaf litter", x = 0.324, y = 0.09, size = 15 ,fontface = "plain",fontfamily = "Helvetica") +
  draw_label(square, x = 0.263, y = 0.092, size = 50,fontface = "bold", color = "coral4",
             fontfamily = "Tahoma") +

    draw_label("Biofilm", x = 0.437, y = 0.09, size = 15,fontface = "plain") +
  draw_label(triangle, x = 0.39, y = 0.092, size = 18,fontface = "bold", color = "turquoise3",
             fontfamily = "Tahoma") +
  
  draw_label("Algae", x = 0.54, y = 0.09, size = 15, fontface = "plain") +
    draw_label(circle, x = 0.50, y = 0.092, size = 28,fontface = "bold", color = "greenyellow",
               fontfamily = "Tahoma") 
p3

p3 + ggsave("Figure_2C.tiff", width=11, height=6.5,  dpi = 600)



#


# patchwork ---------------------------------------------------------------

Figure_2 <- QPAFeb17 + QPBFeb17 + QPANov17 + QPBNov17 + 
 plot_spacer() + QPBJune18 + QPAFeb19 +  QPBFeb19 +
  plot_spacer() +plot_spacer() +plot_spacer() +plot_spacer() +
  plot_layout(ncol = 4, widths = c(1, 1))

Figure_2

Figure_2 + plot_annotation(tag_levels = 'A')
Figure_2 + ggsave("Figure_2_new.pdf", width=11, height=8.5)










# ---------------------------------------------
# Biplots for Quebrada Prieta A
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# QPb February 17 ---------------------------------------------------------


QPB_Feb17 <- read.csv("Biplot/QPB_Feb17.csv")
head(QPB_Feb17)

QPB_Feb17$taxa <- factor(QPB_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPB_Feb17$taxa) 

QPBFeb17 <-  ggplot(QPB_Feb17, aes(x=C, y=N, group=taxa, shape=taxa)) +
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
  geom_segment(aes(x=-30.21171573,xend=-30.26828427,yend=0.65,y=0.65), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.24,xend=-30.24,yend=0.706568542,y=0.593431458), size=2, color="#8c2d04", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  
  geom_segment(aes(x=-27.07343146,xend=-27.18656854,yend=3.65,y=3.65), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.13,xend=-27.13,yend=4.116690476,y=3.183309524), size=2, color="#2c7fb8", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  
  geom_segment(aes(x=-27.19184263, xend=-34.15278073,yend=8.022895772,y=8.022895772), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-30.672311680065,xend=-30.672311680065,yend=12.21237758,y=3.833413964), size=2, color="#31a354", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  
  # Axis Limits 
  xlim(-43,-17) +
  ylim(-5,20) +
  
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
QPBFeb17


# QPB November 2017 -------------------------------------------------------


QPB_Nov17 <- read.csv("Biplot/QPB_Nov17.csv")
head(QPB_Nov17)

QPB_Nov17$taxa <- factor(QPB_Nov17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPB_Nov17$taxa) 

QPBNov17 <-  ggplot(QPB_Nov17, aes(x=C, y=N, group=taxa, shape=taxa)) +
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
  geom_segment(aes(x=-32.104,xend=-32.424,yend=-0.464,y=-0.464), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.264,xend=-32.264,yend=-0.307820616,y=-0.620179384), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.264, y = -0.464), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.40470835,xend=-27.49879165,yend=2.49425,y=2.49425), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.45175,xend=-27.45175,yend=2.719578464,y=2.268921536), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.45175, y = 2.49425), shape=17,color = "turquoise3", size=5) +

  geom_segment(aes(x=-4.591, xend=-24.4241,yend=10.3605,y=10.3605), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-14.508,xend=-14.508,yend=16.5156949,y=4.205494871), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -14.508, y = 10.3605), shape=19,color = "greenyellow", size=5)+
    
  # Axis Limits 
   xlim(-45,1) +
   ylim(-5,20) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
 # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -7, y = 13, label = "Algae") +
  annotate("text", x = -36, y = 5, label = "Biofilm") +
  annotate("text", x = -11, y = -3, label = "Leaf litter") +
  
  geom_segment(aes(x =-20, xend=-29,y = -3, yend = -1), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
QPBNov17


# QPB June 2018 -------------------------------------------------------


QPB_June18 <- read.csv("Biplot/QPB_June18.csv")
head(QPB_June18)

QPB_June18$taxa <- factor(QPB_June18$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPB_June18$taxa) 

QPBJune18 <-  ggplot(QPB_June18, aes(x=C, y=N, group=taxa, shape=taxa)) +
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
  geom_segment(aes(x=-30.22279365,xend=-30.66570635,yend=0.5765,y=0.5765), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.44425,xend=-30.44425,yend=0.717620516,y=0.435379484), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.444, y = 0.5765), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-28.0262638890269,xend=-29.42423611,yend=5.74375,y=5.74375), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-28.72525,xend=-28.72525,yend=6.920422816,y=4.567077184), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -28.72525, y = 5.74375), shape=17,color = "turquoise3", size=5) +
  
  
  
  
  geom_segment(aes(x=-19.2631071177543, xend=-27.1850047001927,yend=21.86152524,y=21.86152524), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-23.22405591,xend=-23.22405591,yend=33.10554446,y=10.61750602), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -23.22405591, y = 21.86152524), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-45,1) +
  ylim(-5,35) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
 # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -13, y = 25, label = "Algae") +
  annotate("text", x = -36, y = 10, label = "Biofilm") +
  annotate("text", x = -10, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-19, xend=-26,y = -3, yend = -1.25), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

QPBJune18



# QPB February 2019 -------------------------------------------------------


QPB_Feb19 <- read.csv("Biplot/QPB_Feb19.csv")
head(QPB_Feb19)

QPB_Feb19$taxa <- factor(QPB_Feb19$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                      "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                      "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPB_Feb19$taxa) 

QPBFeb19 <-  ggplot(QPB_Feb19, aes(x=C, y=N, group=taxa, shape=taxa)) +
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
  geom_segment(aes(x=-30.2117157287525,xend=-30.26828427,yend=0.65,y=0.65), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.24,xend=-30.24,yend=0.706568542,y=0.593431458), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.24, y = 0.65), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.07343146,xend=-27.18656854,yend=3.65,y=3.65), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.13,xend=-27.13,yend=4.116690476,y=3.183309524), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.13, y = 3.65), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-27.19184263, xend=-34.15278073,yend=8.022895772,y=8.022895772), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-30.67231168,xend=-30.67231168,yend=12.21237758,y=3.833413964), size=1,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -30.67231168, y = 8.022895772), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  #text
  annotate("text", x = -26, y = 11, label = "Algae") +
  annotate("text", x = -23, y = 6, label = "Biofilm") +
  annotate("text", x = -37, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-36, xend=-32,y = -3, yend = -1.25), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.2) +
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

QPBFeb19

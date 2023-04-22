



# ---------------------------------------------
# Biplots for Quebrada Prieta A
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# -------------------------------------------------------------------------
# QPB February 17 ---------------------------------------------------------
QPB_Feb17 <- read.csv("Biplot/QPB_Feb17.csv")
head(QPB_Feb17)

QPB_Feb17$taxa <- factor(QPB_Feb17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(QPB_Feb17$taxa) 

# Layman Polygon ----------------------------------------------------------
# Define hull area
# Computes the subset of points which lie on the convex hull of the set of points specified.
find_hull <- function(dataframe) {
  dataframe[chull(dataframe$C, dataframe$N), ]
}
qpbF17_hull <- find_hull(QPB_Feb17) 
qpbF17_hull1 <- qpbF17_hull[-6,] # Exclude spider
# -------------------------------------------------------------------------

# Food resources Polygon area -------------------------------------------
polygon_QPB_Feb <- data.frame(x = c(-41.0594,-47.8407,-41.0594,
                                    -41.0594,-34.2780,-41.0594, 
                                    -47.8407,-41.0594,-41.0594,
                                    
                                    -41.0594,-30.3005, -30.2554,
                                    -34.2780, -41.0594),
                              
                             y = c(14.13489,9.446942,9.446942,
                                   14.13489,9.446942,9.446942,
                                   9.446942,4.758994,9.446942,
                                   
                                   4.758994,1.537445838,2.13925,
                                   9.446942,9.446942),
                             
                             g=c('a','a','a','b','b','b',
                                 'c','c','c', 
                                 'd','d','d','d','d'))
polygon_QPB_Feb
# -------------------------------------------------------------------------


QPBFeb17 <-  ggplot(QPB_Feb17, aes(x=C, y=N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), 
             size=3,stroke = 1.2) +
#    geom_polygon(data=qpbF17_hull1, fill= "pink1", 
#                 colour = "pink2",size = 0.5, alpha=.5) +
  geom_polygon(data = polygon_QPB_Feb, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
  # Axis label
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
  geom_segment(aes(x=-30.2554,xend=-30.34556,yend=2.13925,y=2.13925), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.3005,xend=-30.3005,yend=2.741054,y=1.537445838), size=0.7, linetype='solid', color="black",  arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.3005, y = 2.13925), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-34.2122,xend=-34.5278,yend=4.9245,y=4.9245), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-34.37,xend=-34.37,yend=5.170192,y=4.678808), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -34.37, y = 4.9245), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-34.2780, xend=-47.8407,yend=9.446942,y=9.446942), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-41.0594,xend=-41.0594,yend=14.13489,y=4.758994), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -41.0594, y = 9.446942), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-49,-17) +
  ylim(-5,20) +
  
  # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -35, y = 12, label = "Algae") +
  annotate("text", x = -26, y = 8, label = "Biofilm") +
  annotate("text", x = -35, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-30.5, xend=-33.5,y = 8, yend = 6), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  geom_segment(aes(x =-35, xend=-32,y = -3, yend = 0), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_line(colour="gray95"), 
        panel.grid.minor = element_blank(),
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

# Define hull area
# Computes the subset of points which lie on the convex hull of the set of points specified.
find_hull <- function(dataframe) {
  dataframe[chull(dataframe$C, dataframe$N), ]
}
qpbN17_hull <- find_hull(QPB_Nov17) 


QPBNov17 <-  ggplot(QPB_Nov17, aes(x=C, y=N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), 
             size=3,stroke = 1.2) +
  geom_polygon(data=qpbN17_hull, fill= "pink1", 
               colour = "pink2",size = 0.5, alpha=.5) +
  
  # Axis label
  labs(x= expression(delta^{13}*"C (\211)"), y = "") +
  
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
  geom_segment(aes(x=-32.104,xend=-32.424,yend=-0.464,y=-0.464), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.264,xend=-32.264,yend=-0.307820616,y=-0.620179384), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.264, y = -0.464), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.40470835,xend=-27.49879165,yend=2.49425,y=2.49425), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.45175,xend=-27.45175,yend=2.719578464,y=2.268921536), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.45175, y = 2.49425), shape=17,color = "turquoise3", size=5) +

  geom_segment(aes(x=-4.591, xend=-24.4241,yend=10.3605,y=10.3605), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-14.508,xend=-14.508,yend=16.5156949,y=4.205494871), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -14.508, y = 10.3605), shape=19,color = "greenyellow", size=5)+
    
  # Axis Limits 
   xlim(-45,1) +
   ylim(-5,20) +

 # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -7, y = 13, label = "Algae") +
  annotate("text", x = -36, y = 5, label = "Biofilm") +
  annotate("text", x = -11, y = -3, label = "Leaf litter") +
  
  geom_segment(aes(x =-20, xend=-29,y = -3, yend = -1), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_line(colour="gray95"), 
        panel.grid.minor = element_blank(),
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

# Define hull area
# Computes the subset of points which lie on the convex hull of the set of points specified.
find_hull <- function(dataframe) {
  dataframe[chull(dataframe$C, dataframe$N), ]
}
qpbJ18_hull <- find_hull(QPB_June18) 


QPBJune18 <-  ggplot(QPB_June18, aes(x=C, y=N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), 
             size=3,stroke = 1.2) +
  geom_polygon(data=qpbJ18_hull, fill= "pink1", 
               colour = "pink2",size = 0.5, alpha=.5) +
  
  # Axis label
  labs(x= expression(delta^{13}*"C (\211)"), y = "") +
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
  geom_segment(aes(x=-30.22279365,xend=-30.66570635,yend=0.5765,y=0.5765), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.44425,xend=-30.44425,yend=0.717620516,y=0.435379484), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.444, y = 0.5765), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-28.0262638890269,xend=-29.42423611,yend=5.74375,y=5.74375), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-28.72525,xend=-28.72525,yend=6.920422816,y=4.567077184), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -28.72525, y = 5.74375), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-19.2631071177543, xend=-27.1850047001927,yend=21.86152524,y=21.86152524), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-23.22405591,xend=-23.22405591,yend=33.10554446,y=10.61750602), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -23.22405591, y = 21.86152524), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-45,1) +
  ylim(-5,35) +
  
 # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -13, y = 25, label = "Algae") +
  annotate("text", x = -36, y = 10, label = "Biofilm") +
  annotate("text", x = -10, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-19, xend=-26,y = -3, yend = -1.25), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_line(colour="gray95"), 
        panel.grid.minor = element_blank(),
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

# Define hull area
# Computes the subset of points which lie on the convex hull of the set of points specified.
find_hull <- function(dataframe) {
  dataframe[chull(dataframe$C, dataframe$N), ]
}
qpbF19_hull <- find_hull(QPB_Feb19) 

QPBFeb19 <-  ggplot(QPB_Feb19, aes(x=C, y=N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), 
             size=3,stroke = 1.2) +
  geom_polygon(data=qpbF19_hull, fill= "pink1", 
               colour = "pink2",size = 0.5, alpha=.5) +
  
  # Axis label
  labs(x= expression(delta^{13}*"C (\211)"), y = "") +
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
  geom_segment(aes(x=-30.2117157287525,xend=-30.26828427,yend=0.65,y=0.65), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.24,xend=-30.24,yend=0.706568542,y=0.593431458), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.24, y = 0.65), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.07343146,xend=-27.18656854,yend=3.65,y=3.65), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.13,xend=-27.13,yend=4.116690476,y=3.183309524), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.13, y = 3.65), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-27.19184263, xend=-34.15278073,yend=8.022895772,y=8.022895772), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-30.67231168,xend=-30.67231168,yend=12.21237758,y=3.833413964), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -30.67231168, y = 8.022895772), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  #text
  annotate("text", x = -26, y = 11, label = "Algae") +
  annotate("text", x = -23, y = 6, label = "Biofilm") +
  annotate("text", x = -37, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-36, xend=-32,y = -3, yend = -1.25), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  # Legend    
  theme(legend.position = "none") +
  
  # Panel
  theme(panel.grid.major = element_line(colour="gray95"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

QPBFeb19


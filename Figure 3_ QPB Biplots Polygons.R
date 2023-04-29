



# ---------------------------------------------
# Biplots for Quebrada Prieta A with Polygons
# 29 Apr 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



QPB_data <- "Biplot/QPB_cosumers_iso_signature.xlsx"
excel_sheets(path = QPB_data)




# QPA February 17  --------------------------------------------------------
qpb_Feb17 <- read_excel(path = QPB_data, sheet = "QPB_Feb17")  
qpb_Feb17 <- qpb_Feb17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpb_Feb17$taxa <- factor(qpb_Feb17$taxa, 
                         levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpb_Feb17$taxa) 


# Food resources Polygon area -------------------------------------------
polygon_QPB_Feb <- data.frame(x = c(-41.0594,-54.62206479,-41.0594,
                                    -41.0594,-27.49666901,-41.0594, 
                                    -54.62206479,-41.0594,-41.0594,
                                    
                                   -27.49666901,-30.21038156, -41.0594, -41.0594),
                              
                              y = c(18.82283924,9.446942,9.446942,
                                    18.82283924,9.446942,9.446942,
                                    9.446942,0.07104548,9.446942,
                                    
                                    9.446942,0.935641676,0.07104548,9.446942),
                              
                              g=c('a','a','a','b','b','b',
                                  'c','c','c', 
                                  'd','d','d', 'd'))
polygon_QPB_Feb
# -------------------------------------------------------------------------


QPBFeb17 <- ggplot(qpb_Feb17, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  
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
  geom_segment(aes(x=-30.21038156,xend=-30.39061844,yend=2.13925,y=2.13925), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.3005,xend=-30.3005,yend=3.342858324,y=0.935641676), size=0.7, linetype='solid', color="black",  arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.3005, y = 2.13925), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-34.05431239,xend=-34.68568761,yend=4.9245,y=4.9245), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-34.37,xend=-34.37,yend=5.415883082,y=4.433116918), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -34.37, y = 4.9245), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-27.49666901, xend=-54.62206479,yend=9.446942,y=9.446942), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-41.0594,xend=-41.0594,yend=18.82283924,y=0.07104548), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -41.0594, y = 9.446942), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  #xlim(-49,-17) +
  #ylim(-5,20) +
  scale_x_continuous(breaks = seq(-55, -20, 5), limits = c(-55, -20)) +
  scale_y_continuous(breaks = seq(-5, 20, 5), limits = c(-5, 20)) +
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



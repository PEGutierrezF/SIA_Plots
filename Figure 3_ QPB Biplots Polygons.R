



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



# -------------------------------------------------------------------------
# QPB November 2017 -------------------------------------------------------
qpb_Nov17 <- read_excel(path = QPB_data, sheet = "QPB_Nov17")  
qpb_Nov17 <- qpb_Nov17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpb_Nov17$taxa <- factor(qpb_Nov17$taxa, 
                         levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpb_Nov17$taxa) 

# Food resources Polygon area -------------------------------------------
polygon_QPB_Nov <- data.frame(x = c(-14.508,-34.34030504,-14.508,
                                    -14.508,5.32435306,-14.508,
                                    5.32435306,-14.508,-14.508,
                                    
                                    -14.508, -31.94351287, -34.34030504,
                                    -14.508),
                              
                              y = c(22.67079492, 10.3605,10.3605,
                                    22.67079492, 10.3605,10.3605,
                                    10.3605,-1.94960514,10.3605,
                                    
                                    -1.94960514,-0.776358768,10.3605,
                                    10.3605),
                              
                              g=c('a','a','a','b','b','b',
                                  'c','c','c', 
                                  'd','d','d','d'))
polygon_QPB_Nov
# -------------------------------------------------------------------------

QPBNov17 <- ggplot(qpb_Nov17, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  # Polygons 
  geom_polygon(data = polygon_QPB_Nov, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
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
  geom_segment(aes(x=-31.94351287,xend=-32.58448713,yend=-0.464,y=-0.464), size=0.7, linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.264,xend=-32.264,yend=-0.151641232,y=-0.776358768), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.264, y = -0.464), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.3576667,xend=-27.5458333,yend=2.49425,y=2.49425), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.45175,xend=-27.45175,yend=2.944906928,y=2.043593072), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.45175, y = 2.49425), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=5.32435306, xend=-34.34030504,yend=10.3605,y=10.3605), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-14.508,xend=-14.508,yend=22.67079492,y=-1.94960514), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -14.508, y = 10.3605), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  #xlim(-45,1) +
  #ylim(-5,20) +
  scale_x_continuous(breaks = seq(-40, 6, 10), limits = c(-43, 6)) +
  scale_y_continuous(breaks = seq(-5, 23, 5), limits = c(-5, 23)) +
  
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




# -------------------------------------------------------------------------
# QPB June 2018 -----------------------------------------------------------
QPB_data <- "Biplot/QPB_cosumers_iso_signature.xlsx"
excel_sheets(path = QPB_data)

qpb_Jun18 <- read_excel(path = QPB_data, sheet = "QPB_Jun18")  
qpb_Jun18 <- qpb_Jun18 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpb_Jun18$taxa <- factor(qpb_Jun18$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpb_Jun18$taxa) 


# Food resources Polygon area -------------------------------------------
polygon_QPB_Jun <- data.frame(x = c(-23.22405591, -31.14595349,-23.22405591,
                                    -23.22405591, -15.30215833,-23.22405591,
                                    -15.30215833,-23.22405591,-23.22405591,
                                    
                                    -23.22405591,-30.88716271,-31.14595349,
                                    -23.22405591),
                              
                              y = c(44.34956368,21.86152524,21.86152524,
                                    44.34956368,21.86152524,21.86152524,
                                    21.86152524,-0.6265132, 21.86152524,
                                    
                                    -0.6265132,0.294258968,
                                    21.86152524,21.86152524),
                              
                              g= c('a','a','a','b','b','b',
                                   'c','c','c', 
                                   'd','d','d','d'))
polygon_QPB_Jun
# -------------------------------------------------------------------------

QPBJune18 <-  ggplot(qpb_Jun18, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
    # Polygons
  geom_polygon(data = polygon_QPB_Jun, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
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
  geom_segment(aes(x=-30.00133729,xend=-30.88716271,yend=0.5765,y=0.5765), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.44425,xend=-30.44425,yend=0.858741032,y=0.294258968), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.444, y = 0.5765), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.32727778,xend=-30.12322222,yend=5.74375,y=5.74375), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-28.72525,xend=-28.72525,yend=8.097095632,y=3.390404368), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -28.72525, y = 5.74375), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-15.30215833, xend=-31.14595349,yend=21.86152524,y=21.86152524), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-23.22405591,xend=-23.22405591,yend=44.34956368,y=-0.6265132), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
  geom_point(aes(x = -23.22405591, y = 21.86152524), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  #xlim(-45,1) +
  #ylim(-5,35) +
  scale_x_continuous(breaks = seq(-40, -10, 5), limits = c(-43, -7)) +
  scale_y_continuous(breaks = seq(-5, 45, 10), limits = c(-5, 45)) +
  
  # theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -13, y = 25, label = "Algae") +
  annotate("text", x = -36, y = 10, label = "Biofilm") +
  annotate("text", x = -15, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-22, xend=-27,y = -3, yend = -1.25), # Leaf litter
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




# -------------------------------------------------------------------------
# QPB February 2019 -------------------------------------------------------
QPB_data <- "Biplot/QPB_cosumers_iso_signature.xlsx"
excel_sheets(path = QPB_data)

qpb_Feb19 <- read_excel(path = QPB_data, sheet = "QPB_Feb19")  
qpb_Feb19 <- qpb_Feb19 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpb_Feb19$taxa <- factor(qpb_Feb19$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpb_Feb19$taxa) 



# Food resources Polygon area -------------------------------------------
polygon_QPB_Feb19 <- data.frame(x = c(-30.67231168, -37.63324977,-30.67231168,
                                      -30.67231168, -23.71137359,-30.67231168,
                                      
                                      -23.71137359,-30.67231168,-30.67231168,
                                      -30.67231168,-37.63324977,-30.67231168),
                                
                                y = c(16.40185939,8.022895772,8.022895772,
                                      16.40185939,8.022895772,8.022895772,
                                      
                                      8.022895772,-0.356067844,8.022895772,
                                      -0.356067844,8.022895772,8.022895772),
                                
                                g= c('a','a','a','b','b','b',
                                     'c','c','c','d','d','d'))
polygon_QPB_Feb19
# -------------------------------------------------------------------------

QPBFeb19 <-  ggplot(qpb_Feb19, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  
  # Polygons
  geom_polygon(data = polygon_QPB_Feb19, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
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
  geom_segment(aes(x=-30.18343146,xend=-30.29656854,yend=0.65,y=0.65), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-30.24,xend=-30.24,yend=0.763137084,y=0.536862916), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -30.24, y = 0.65), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.01686292,xend=-27.24313708,yend=3.65,y=3.65), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.13,xend=-27.13,yend=4.583380952,y=2.716619048), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.13, y = 3.65), shape=17,color = "turquoise3", size=5) +
  
  geom_segment(aes(x=-23.71137359, xend=-37.63324977,yend=8.022895772,y=8.022895772), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-30.67231168,xend=-30.67231168,yend=16.40185939,y=-0.356067844), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# N algae
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

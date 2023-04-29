



# ---------------------------------------------
# Biplots for Quebrada Prieta A with Polygons
# 27 Apr 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



QPA_data <- "Biplot/QPA_cosumers_iso_signature.xlsx"
excel_sheets(path = QPA_data)



# QPA February 17  --------------------------------------------------------
qpa_Feb17 <- read_excel(path = QPA_data, sheet = "QPA_Feb17")  
qpa_Feb17 <- qpa_Feb17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpa_Feb17$taxa <- factor(qpa_Feb17$taxa, 
                         levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpa_Feb17$taxa) 


# Food resources Polygon area -------------------------------------------
polygon_QPAFeb <- data.frame(x = c(-31.2878,-32.47305561, -31.2878,
                                   -31.2878,-30.10257075,-31.2878,
                                   -30.10257075, -31.2878, -31.2878,
                                   
                                   -31.2878, -32.075,-32.41679916,-32.47305561,-31.2878), 
                             
                             y = c(23.60339213, 11.27881, 11.27881,
                                   23.60339213, 11.27881,11.27881,
                                   11.27881, -1.045775726, 11.27881,
                                   
                                   -1.045775726, -0.078829653, 0.559, 11.27881, 11.27881),
                             
                             g=c('a','a','a','b','b','b',
                                 'c','c','c',
                                 'd','d','d','d','d'))
polygon_QPAFeb
# -------------------------------------------------------------------------

QPAFeb17 <- ggplot(qpa_Feb17, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  # Polygons  
  geom_polygon(data = polygon_QPAFeb, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
  # Axis label
  labs(x="", y = expression(delta^{15}*"N (\211)"), fill = "Taxa") +
  
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
  geom_segment(aes(x=-31.73320084,xend=-32.41679916,yend=0.559,y=0.559), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=1.196829653,y=-0.078829653), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.075, y = 0.559), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-31.26717346,xend=-32.11782654,yend=6.06,y=6.06), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.719656916,y=5.402843084), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -31.69, y = 6.06), shape=17,color = "turquoise3", size=5)+ 
  
  geom_segment(aes(x=-30.10257075, xend=-32.47305561,yend=11.27881,y=11.27881), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-31.2878,xend=-31.2878,yend=23.60339213,y=-1.045775726), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -31.2878, y = 11.27881), shape=19,color = "greenyellow", size=5)+ 
  
  # Axis Limits 
  #  xlim(-40,-20) +
  #  ylim(-5,25) +
  scale_x_continuous(breaks = seq(-40, -20, 5), limits = c(-43, -20)) +
  scale_y_continuous(breaks = seq(-5, 25, 5), limits = c(-5, 25)) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  #  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  
  #text
  annotate("text", x = -27, y = 12, label = "Algae") +
  annotate("text", x = -36, y = 7, label = "Biofilm") +
  annotate("text", x = -24, y = 5, label = "Leaf litter") +
  
  geom_segment(aes(x =-26, xend=-30.5,y = 4, yend = 1.75), # Leaf litter
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  
  # Legend    
  theme(legend.position = "none") +
  theme(legend.key=element_blank()) +
  theme(legend.text.align = 0) +
  theme(legend.title=element_text(size=16, face="bold")) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  # Panel
  theme(panel.grid.major = element_line(colour="gray95"), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

QPAFeb17



# -------------------------------------------------------------------------
# QPA November 2017 -------------------------------------------------------
qpa_Nov17 <- read_excel(path = QPA_data, sheet = "QPA_Nov17")  
qpa_Nov17 <- qpa_Nov17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpa_Nov17$taxa <- factor(qpa_Nov17$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpa_Nov17$taxa) 


# Food resources Polygon area ------------------------------------------------------------
polygon_QPA_Nov <- data.frame(x = c(-22.5013, -31.03786873, -22.5013,   
                                    -22.5013,-13.96470099, -22.5013, 
                                    -13.96470099, -22.5013,  -22.5013,
                                    
                                    -22.5013, -31.5665,-31.98790875,
                                    -31.03786873,-22.5013),
                              
                              y = c(6.19315967, 3.5359, 3.5359,
                                    6.19315967, 3.5359, 3.5359,
                                    3.5359, 0.878692151, 3.5359,
                                    
                                    0.878692151, 0.506121525,0.9295,
                                    3.5359, 3.5359), 
                              
                              g=c('a','a','a', 'b','b','b',
                                  'c','c','c',
                                  'd','d','d','d','d'))
polygon_QPA_Nov
# -------------------------------------------------------------------------


QPANov17 <- ggplot(qpa_Nov17, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  # Polygons  
  geom_polygon(data = polygon_QPA_Nov, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
  # Axis label
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
  geom_segment(aes(x=-31.14509125,xend=-31.98790875,yend=0.9295,y=0.9295), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-31.5665,xend=-31.5665,yend=1.352878475,y=0.506121525), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -31.5665, y = 0.9295), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-26.22114135,xend=-26.41835865,yend=2.413,y=2.413), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-26.3198,xend=-26.3198,yend=3.651249302,y=1.174750698), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -26.3198, y = 2.413), shape=17,color = "turquoise3", size=5)+ 
  
  geom_segment(aes(x=-13.96470099, xend=-31.03786873,yend=3.5359,y=3.5359), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-22.5013,xend=-22.50128,yend=6.193159675,y=0.878692151), size=0.7,linetype='solid', color="black",arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -22.5013, y = 3.5359), shape=19,color = "greenyellow", size=5)+ 
  
  # Axis Limits 
  #  xlim(-43,-12) +
  #  ylim(-5,20) +
  scale_x_continuous(breaks = seq(-40, -13, 5), limits = c(-41, -13)) +
  scale_y_continuous(breaks = seq(-5, 20, 5), limits = c(-5, 20)) +
  
  #text
  annotate("text", x = -26, y = 8, label = "Algae") +
  annotate("text", x = -35, y = 5, label = "Biofilm") +
  annotate("text", x = -36, y = -4, label = "Leaf litter") +
  
  geom_segment(aes(x =-32, xend=-28,y = 4, yend = 3), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  geom_segment(aes(x =-36, xend=-33,y = -3, yend = 0), # Leaf litter
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

QPANov17



# -------------------------------------------------------------------------
# QPA June 2018 -----------------------------------------------------------
QPA_data <- "Biplot/QPA_cosumers_iso_signature.xlsx"
excel_sheets(path = QPA_data)

qpa_Jun18 <- read_excel(path = QPA_data, sheet = "QPA_Jun18")  
qpa_Jun18 <- qpa_Jun18 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpa_Jun18$taxa <- factor(qpa_Jun18$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpa_Jun18$taxa) 


# Food resources Polygon area ------------------------------------------------------------
polygon_QPA_Jun <- data.frame(x = c(-25.9788,-30.38987658,-25.9788,
                                    -25.9788,-21.56762567,-25.9788,
                                    -21.56762567,-25.9788, -25.9788,
                                    
                                    -25.9788,-29.99086489,-30.38987658,
                                    -25.9788),
                              
                              y = c(17.56498781,10.13571, 10.13571,
                                    17.56498781,10.13571, 10.13571, 
                                    10.13571,2.706437562,10.13571,
                                    
                                    2.706437562, 3.50408,
                                    10.13571, 10.13571),
                              
                              g=c('a','a','a', 'b','b','b',
                                  'c','c','c', 'd','d','d',
                                  'd'))
polygon_QPA_Jun
# -------------------------------------------------------------------------

QPAJune18 <-  ggplot(qpa_Jun18, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +

# Polygons
geom_polygon(data = polygon_QPA_Jun, aes(x = x, y = y, group=g), fill = "gray80", 
             colour = "gray80", size = 0.5, alpha = 0.5) +
  
  # Axis label
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
  geom_segment(aes(x=-29.56913511,xend=-29.99086489,yend=3.676,y=3.676), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-29.78,xend=-29.78,yend=3.847914708,y=3.504085292), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -29.78, y = 3.676), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.42816948,xend=-28.61633052,yend=6.694,y=6.694), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-28.0223,xend=-28.0223,yend=7.349762152,y=6.038237848), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -28.0223, y = 6.694), shape=17,color = "turquoise3", size=5)+
  
  geom_segment(aes(x=-21.56762567, xend=-30.38987658,yend=10.13571,y=10.13571), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-25.9788,xend=-25.9788,yend=17.56498781,y=2.706437562), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -25.9788, y = 10.13571), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  #text
  annotate("text", x = -22, y = 13, label = "Algae") +
  annotate("text", x = -37, y = 11, label = "Biofilm") +
  annotate("text", x = -38, y = -3, label = "Leaf litter") +
  
  geom_segment(aes(x =-34, xend=-29.5,y = 10.5, yend = 7.7), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  geom_segment(aes(x =-35, xend=-32,y = -2, yend = 1.5), # Leaf litter
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

QPAJune18



# -------------------------------------------------------------------------
# QPA February 2019 -------------------------------------------------------
QPA_data <- "Biplot/QPA.xlsx"
excel_sheets(path = QPA_data)

qpa_Feb18 <- read_excel(path = QPA_data, sheet = "QPA_Feb19")  
qpa_Feb18 <- qpa_Feb18 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpa_Feb18$taxa <- factor(qpa_Feb18$taxa, levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpa_Feb18$taxa) 


# Food resources Polygon area ------------------------------------------------------------
polygon_QPA_Feb18 <- data.frame(x = c(-26.844,-28.97460148,-26.844,
                                      -26.844,-24.71418889,-26.844,
                                      -24.71418889,-26.844,-26.844,
                                      
                                      -26.844,-28.74622189,
                                      -28.97460148,-26.844),
                                
                                y = c(19.2895266,9.433,9.433,
                                      19.2895266,9.433,9.433,
                                      9.433, -0.422554315,9.433,
                                      
                                      -0.422554315,0.60878459,
                                      9.433, 9.433),
                                
                                g=c('a','a','a', 'b','b','b',
                                    'c','c','c', 'd','d','d','d'))
polygon_QPA_Feb18

# -------------------------------------------------------------------------
QPAFeb19 <-  ggplot(qpa_Feb18, aes(x=mean_C, y=mean_N)) +
  geom_point(aes(group=taxa, shape=taxa, colour=taxa), size=3 ,stroke = 1.2)+ 
  
  # Error bars Taxa
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2, size=0.5)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2, size=0.5) +
  
  # Polygons
  geom_polygon(data = polygon_QPA_Feb18, aes(x = x, y = y, group=g), fill = "gray80", 
               colour = "gray80", size = 0.5, alpha = 0.5) +
  
  # Axis label
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
  geom_segment(aes(x=-28.14711144,xend=-28.74622189,yend=1.153,y=1.153), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-28.447,xend=-28.447,yend=1.697882076,y=0.60878459), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -28.447, y = 1.153), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-27.64728943,xend=-28.09271057,yend=4.18,y=4.18), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-27.87,xend=-27.87,yend=4.697300686,y=3.662699314), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -27.87, y = 4.18), shape=17,color = "turquoise3", size=5)+
  
  geom_segment(aes(x=-24.71418889, xend=-28.97460148,yend=9.433,y=9.433), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-26.844,xend=-26.844,yend=19.2895266,y=-0.422554315), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -26.844, y = 9.433), shape=19,color = "greenyellow", size=5)+
  
  # Axis Limits 
  xlim(-43,-20) +
  ylim(-5,20) +
  
  #text
  annotate("text", x = -22, y = 11, label = "Algae") +
  annotate("text", x = -35, y = 9, label = "Biofilm") +
  annotate("text", x = -38, y = -3, label = "Leaf litter") +
  
  geom_segment(aes(x =-32, xend=-29.1,y = 8, yend = 5), # Biofilm
               arrow = arrow(length = unit(0.3, "cm")), size = 0.3) +
  geom_segment(aes(x =-35, xend=-30,y = -2, yend = 0), # Leaf litter
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

QPAFeb19



# ggarrange -------------------------------------------------------------

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p2_legend <- get_legend(QPAFeb17)

p1 <- ggarrange(QPAFeb17, QPANov17, QPAJune18, QPAFeb19,
                QPBFeb17, QPBNov17, QPBJune18, QPBFeb19,
                ncol=4,nrow=2, # labels = c("A", "B", "C","D","E","F","G","H"),
                widths = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
                common.legend = TRUE, legend="bottom") +
  theme(plot.margin = margin(1,0.1,0.1,0.1, "cm")) # Increase area (margin) of the graph
p1
# p1 + ggsave("Figure 3b.jpg", width=11, height=6.5)


# Include strips
p2 <- ggdraw(p1) + 
  geom_rect(aes(xmin=0.066, xmax=0.244, ymin=0.933, ymax=0.986), color = "black", alpha=0.2) +
  draw_label("6mo pre-", x = 0.16, y = 0.960, size = 14 ,fontface = "plain") +
  
  geom_rect(aes(xmin=0.304, xmax=0.492, ymin=0.933, ymax=0.986), color = "black", alpha=0.2) +
  draw_label("2mo post-", x = 0.40, y = 0.960, size = 14, fontface = "plain") +
  
  geom_rect(aes(xmin=0.551, xmax=0.739, ymin=0.933, ymax=0.986), color = "black", alpha=0.2) +
  draw_label("9mo post-", x = 0.65, y = 0.960, size = 14,fontface = "plain")+
  
  geom_rect(aes(xmin=0.799, xmax=0.987, ymin=0.933, ymax=0.986), color = "black", alpha=0.2) +
  draw_label("18mo post-", x = 0.9, y = 0.960, size = 14, fontface = "plain") +
  
  # Prieta A
  geom_rect(aes(xmin=0.994, xmax= 1.025, ymin=0.622, ymax=0.924), color = "black", alpha=0.2) +
  draw_label("Prieta A", x = 1.010, y = 0.8, size = 14, fontface = "plain", angle=270) +
  
  # Prieta B
  geom_rect(aes(xmin=0.994, xmax= 1.025, ymin=0.241, ymax=0.529), color = "black", alpha=0.2) +
  draw_label("Prieta B", x = 1.010, y = 0.4, size = 14, fontface = "plain", angle=270)


p3 <- p2 + theme(plot.margin = unit(c(0.5, 2, 0, 0), units = "cm")) # t=1, l=2, b=1, r=1
p3

p3 + ggsave("Figure 3 Polygons 2SD.jpg", width=11, height=6.5)
#

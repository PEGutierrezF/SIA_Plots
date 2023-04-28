








QPA_data <- "Biplot/QPA.xlsx"
excel_sheets(path = QPA_data)

qpa_Feb17 <- read_excel(path = QPA_data, sheet = "QPA_Feb17")  
qpa_Feb17 <- qpa_Feb17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% 
  na.omit()



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
  xlim(-50,-0) +
  ylim(-5,35) +
  
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


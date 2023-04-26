

libraries <- c("ggplot2","igraph", "dplyr", "ggraph","readxl", 
               "ggnetwork","ITNr", "intergraph","patchwork",
               "xts","emmeans", 'quantmod', "ggpubr","janitor",
               'scales', 'ggalluvial','writexl')

lapply(libraries, require, character.only = TRUE)



QPA_data <- "Biplot/QPA.xlsx"
excel_sheets(path = QPA_data)

Feb17_qpa <- read_excel(path = QPA_data, sheet = "QPA_Feb17")  

Feb17_qpa1 <- Feb17_qpa %>% 
              select(mean_C,sd_C,mean_N,sd_N)



p <- ggplot(Feb17_qpa1, aes(x=mean_C, y=mean_N)) +
  geom_point(shape=1) + 
  geom_errorbarh(aes(xmin=mean_C - sd_C,
                     xmax=mean_C + sd_C),
                 height=0.2)+
  geom_errorbar(aes(ymin=mean_N - sd_N,
                    ymax=mean_N + sd_N),
                width=0.2) +
  
  # Segments  
  geom_segment(aes(x=-31.9041,xend=-32.2459,yend=0.559,y=0.559), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both", angle = 90)) + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=0.8789,y=0.2401), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) + # N leaflitter
  geom_point(aes(x = -32.075, y = 0.559), shape=15,color = "coral4", size=5)+ 
  
  geom_segment(aes(x=-31.48,xend=-31.90,yend=6.06,y=6.06), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90))+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.3904,y=5.7320), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90))+ # N biofilm
  geom_point(aes(x = -31.69, y = 6.06), shape=17,color = "turquoise3", size=5)+ 
  
  geom_segment(aes(x=-30.6952, xend=-31.8804,yend=11.27881,y=11.27881), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"), ends = "both",angle = 90)) +# C algae
  geom_segment(aes(x=-31.2878,xend=-31.2878,yend=17.4411,y=5.1166), size=0.7,linetype='solid', color="black", arrow = arrow(length = unit(0.1, "cm"),ends = "both", angle = 90)) +# algae
  geom_point(aes(x = -31.2878, y = 11.27881), shape=19,color = "greenyellow", size=5)


p

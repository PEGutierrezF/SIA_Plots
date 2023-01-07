



# ---------------------------------------------
# Figure 4: Layman's community-wide metrics for the PA and PB 
# 06 Jan 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())


df.data <- "data/data_laymanCWM.xlsx"
excel_sheets(path = df.data)  
df <- read_excel(path = df.data, sheet = "laymansCWM")


# First, reorganize the sampling events
df$sampling = factor(df$sampling, 
              levels=c('6mopre','2mopost','9mopost','18mopost'))


Fig_4_ecology <- ggplot(df, aes(x = sampling, y = value, color= stream, group=stream)) +
  geom_line(size=1, linetype = "dashed") + 
  geom_point(shape=19, size = 4.5)+
  
  scale_color_manual(values = c('#ce1256','#0570b0'), 
                     labels = c("Prieta A", "Prieta B")) +
  
  # Label  
  labs(x="Sampling event", y = "Value", colour = "Stream") +
  scale_x_discrete(labels = c("6mo\npre-", "2mo\npost-", "9mo\npost-", "18mo\npost-")) +  
  
  # line in discrete variable
  geom_vline(xintercept=seq(1.5, length(unique(df$stream))-0.5, 1), 
             lwd=2.7, colour="#df65b0", linetype="solid", alpha=0.5) +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Mueve los margenes para que la legenda entre
  #  top (t), right (r), bottom (b), and left (l)
  theme(axis.title.x = element_text(margin = margin(t = 15))) +
  
  # Strip  
  theme(strip.placement = 'outside') +
  theme(strip.switch.pad.grid = unit('0.2', "cm")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(strip.text.y = element_text(size = 14)) +
  theme(strip.background = element_rect(colour="black", fill="gray90"),
        strip.text = element_text(margin = margin(5, 5, 5, 5))) +
  
  # Legend 
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA))+
  
  # Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_wrap(.~factor(layman, levels=c('NR', 'CR', 'TA', 'CD', 'MNND', 'SDNND' )),
             scale = "free_y", strip.position ="top")


Fig_4_ecology

Fig_4_ecology + ggsave("Figure 4.jpeg", width=11, height=6.5)

#Ecology format
Fig_4_ecology + tiff(filename="D:/OneDrive - University of Vermont/LTER/Manuscript 2019 Stable Isotopes/SIA_Plots 2017-2019/Figure 4.tiff",
                            height=5600,width=7200,units="px",res=800,compression="lzw")

                         
                         
                         
                         
                         
                         

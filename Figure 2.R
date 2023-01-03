



# ---------------------------------------------
# Figure 2
# 03 Dec 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



data <- read.csv("data/isotopes.csv")
head(data)

# First, rename variables
source_new <- c(
  'Algae' = "Algae",
  'Epilithon' = "Biofilm",
  'LeafLitter' = "Leaf litter")

# Second, reorganize
data$sampling = factor(data$sampling, 
                       levels=c('six','two','9mo','18mo'))
# Rename the isotopes
isotopes_new <- c(
  "Carbon" = "delta^{13}*C ~(`\211`)",
  "Nitrogen" = "delta^{15}*N ~(`\211`)")


# Graph 

Fig_isotopes_ecology <- ggplot(data = data, aes(x = sampling, y = value, group= stream, colour= stream)) + 
  geom_line(size=1, linetype = "dashed") + 
  geom_point(shape=19, size = 4.5)+
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width = 0) +

  scale_color_manual(values = c('#ce1256','#0570b0'), 
                     labels = c("Prieta A", "Prieta B")) +
                     
# Label  
  labs(x="Sampling event", y = "Isotopic signature", colour = "Stream") +
  scale_x_discrete(labels = c("6mo\npre-", "2mo\npost-", "9mo\npost-", "18mo\npost-")) +  
  
# line in discrete variable
  geom_vline(xintercept=seq(1.5, length(unique(data$stream))-0.5, 1), 
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
  theme(strip.switch.pad.grid = unit('0.25', "cm")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(strip.text.y = element_text(size = 14)) +
  theme(strip.background = element_rect(colour="black", fill="gray90"),
        strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  
# Legend 
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA))+
  
# Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
facet_grid(isotope ~ source, 
               scales= "free_y",
               labeller = labeller(isotope = as_labeller(isotopes_new,  label_parsed),
                                                    source = as_labeller(source_new)))


Fig_isotopes_ecology

#Ecology format
Fig_isotopes_ecology + tiff(filename="D:OneDrive - University of Vermont/LTER/Manuscript 2019 Stable Isotopes/SIA_Plots 2017-2019/Figure 12.tiff",
     height=5600,width=7200,units="px",res=800,compression="lzw")

Fig_isotopes_ecology + ggsave("Figure 2.jpeg", width=11, height=6.5)


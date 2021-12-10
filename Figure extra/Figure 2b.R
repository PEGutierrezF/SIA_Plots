

library(tidyverse)

data <- read.csv("data/isotopes.csv")
head(data)

# First, rename variables
sampling_new <- c(
  'six' = "6mo pre-",
  'two' = "2mo post-",
  '9mo' = "9mo post-",
  '18mo' = "18mo post-")

# Second, reorganize
data$sampling = factor(data$sampling, 
                           levels=c('six','two','9mo','18mo'))
# Rename the isotopes
isotopes_new <- c(
  "Carbon" = "delta^{13}*C",
  "Nitrogen" = "delta^{15}*N")



p <- ggplot(data = data, aes(x = stream, y = value)) +
  geom_point(aes(colour = source), position = position_dodge(width = 1), size= 5) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), labels = c("Algae", "Biofilm", "Leaf litter") ) +
  
  geom_errorbar(aes(colour = source, ymin = value - sd, ymax = value + sd), 
                width = 0.2, position = position_dodge(width = 1), size= 1) +
  
  labs(x="Streams",y = "Isotopic signature", colour = "Source") +
  scale_x_discrete(labels = c("Quebrada\nPrieta A", "Quebrada\nPrieta B")) +

# line in discrete variable
  geom_vline(xintercept=seq(1.5, length(unique(data$stream))-0.5, 1), 
             lwd=1, colour="black", linetype="dashed", alpha=0.5) +

# Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

Fig_isotopes <- p + facet_grid(isotope ~ sampling, 
               scale= "free_y", labeller = labeller(isotope = as_labeller(isotopes_new,  label_parsed),
                                                    sampling = as_labeller(sampling_new))) 
Fig_isotopes

Fig_isotopes + ggsave("Figure 2 extra.tiff", width=11, height=6.5)













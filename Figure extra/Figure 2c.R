



# ---------------------------------------------
# Figure 2 other option
# 14 Dec 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  




library(tidyverse)

data <- read.csv("data/isotopes.csv")
head(data)


# Prieta A ----------------------------------------------------------------

PA <- slice(data, (1:24))

# First, rename variables
sampling_new <- c(
  'six' = "6mo pre-",
  'two' = "2mo post-",
  '9mo' = "9mo post-",
  '18mo' = "18mo post-")

# Second, reorganize
PA$sampling = factor(PA$sampling, 
                       levels=c('six','two','9mo','18mo'))
# Rename the isotopes
isotopes_new <- c(
  "Carbon" = "delta^{13}*C ~(`\211`)",
  "Nitrogen" = "delta^{15}*N ~(`\211`)")

q <- ggplot(data = PA, aes(x = source, y = value)) +
  geom_point(aes(colour = source), position = position_dodge(width = 1), size= 5) +
  geom_errorbar(aes(colour = source, ymin = value - sd, ymax = value + sd), 
                width = 0.2, position = position_dodge(width = 1), size= 1) +
  
# Labels
  labs(x="",y = "Isotopic signature", colour = "Source") +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +

# Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_blank()) + # axis x
  theme(axis.text.x=element_blank()) + #subaxis x
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

# Ticks
  theme(axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +  

# Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 
  
Fig_isotopes <- q + facet_grid(isotope ~ sampling, 
                               scale= "free_y", labeller = labeller(isotope = as_labeller(isotopes_new,  label_parsed),
                                                                    sampling = as_labeller(sampling_new))) 
Fig_isotopes  

Fig_isotopes + ggsave("Figure 2c.tiff", width=11, height=6.5)  
  

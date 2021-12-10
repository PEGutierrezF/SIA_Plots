



# ---------------------------------------------
# credible interval
# 09 Dec 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list = ls())


library(ggplot2)
library(ggtext) # remotes::install_github("clauswilke/ggtext")

CI_PA <- read.csv("data/CI_sources_PA.csv")
head(CI_PA)


group <- c("one"= "6mo pre-", "two"= "2mo post-")
CI_PA$group <- factor(CI_PA$group, levels=c("two", "one"))


ggplot(data=CI_PA, aes(x = group,y = C, ymin = hdi_lower, ymax = hdi_upper))+
  
  geom_pointrange(aes(col=source), position=position_dodge(0.2), size=1)+
  geom_errorbar(aes(ymin=hdi_lower, ymax=hdi_upper, col= source), 
                position=position_dodge(0.2), width=0, size=0.9)+
  
  # Labels  
  labs(x="", colour = "Source") +
  ylab(expression(~delta^13*C~("\U2030"))) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Legend 
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA))+
  
  # Strip  
  theme(strip.placement = 'outside') +
  theme(strip.switch.pad.grid = unit('0.25', "cm")) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(strip.text.y = element_text(size = 14)) +
  theme(strip.background = element_rect(colour="black", fill="gray90"),
        strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  
  #
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  # Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_wrap(.~ group, strip.position="left", 
             nrow=15, scales = "free_y",
             labeller = as_labeller(group)) +
  coord_flip()

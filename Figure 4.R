



# ---------------------------------------------
# credible interval
# 09 Dec 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list = ls())




CrI_vs_CI <- read.csv("data/CI_sources_PA.csv")
head(CrI_vs_CI)


# Carbon ------------------------------------------------------------------

C_PA <- slice(CrI_vs_CI, (1:12))
head(C_PA)
tail(C_PA)

period <- c("one"= "6mo pre-", "two"= "2mo post-",
           "three"= "9mo post-", "four"="18mo post-")

C_PA$period <- factor(C_PA$period, levels=c("four","three","two", "one"))


Carbon_PA <- ggplot(data=C_PA, aes(x = period, y = value, 
                                 ymin = hdi_lower, ymax = hdi_upper))+
  
  geom_pointrange(aes(col=source), position=position_dodge(0.5), size=1)+
  geom_errorbar(aes(ymin=hdi_lower, ymax=hdi_upper, col= source), 
                position=position_dodge(0.5), width=0, size=0.9)+
  
  # Labels  
  labs(x="", colour = "Source") +
  ylab(expression(~delta^13*C~("\U2030"))) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  labs(tag = "A") +
  
# Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
# Legend 
  theme(legend.position = "none") +
#  theme(legend.key.size = unit(0.6, "cm"))+
#  theme(legend.title=element_text(size=16)) + # legend title size
# theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
#  theme(legend.key = element_rect(fill = NA, color = NA))+
  
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
  
  facet_wrap(.~ period, strip.position="left", 
             nrow=15, scales = "free_y",
             labeller = as_labeller(period)) +
  coord_flip()

Carbon_PA


# Nitrogen ----------------------------------------------------------------
N_PA <- slice(CrI_vs_CI, (13:24))
head(N_PA)
tail(N_PA)

period <- c("one"= "6mo pre-", "two"= "2mo post-",
            "three"= "9mo post-", "four"="18mo post-")

N_PA$period <- factor(N_PA$period, levels=c("four","three","two", "one"))


Nitrogen_PA <- ggplot(data=N_PA, aes(x = period, y = value, 
                                 ymin = hdi_lower, ymax = hdi_upper))+
  
  geom_pointrange(aes(col=source), position=position_dodge(0.5), size=1)+
  geom_errorbar(aes(ymin=hdi_lower, ymax=hdi_upper, col= source), 
                position=position_dodge(0.5), width=0, size=0.9)+
  
# Labels  
  labs(x="", colour = "Source") +
  ylab(expression(~delta^15*N~("\U2030"))) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  labs(tag = "B") +
  
# Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
# Legend 
 # theme(legend.position = "none") +
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA))+
  
# Strip
  theme(strip.text.y = element_blank()) +
#  theme(strip.placement = 'outside') +
#  theme(strip.switch.pad.grid = unit('0.25', "cm")) +
#  theme(strip.text.x = element_text(size = 14)) +
#  theme(strip.text.y = element_text(size = 14)) +
#  theme(strip.background = element_rect(colour="black", fill="gray90"),
#       strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  
#
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
# Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_wrap(.~ period, strip.position="left", 
             nrow=15, scales = "free_y",
             labeller = as_labeller(period)) +
  coord_flip()

Nitrogen_PA


# Carbon Prieta B ---------------------------------------------------------

period <- c("one"= "6mo pre-", "two"= "2mo post-",
            "three"= "9mo post-", "four"="18mo post-")

CI_PA$period <- factor(CI_PA$period, levels=c("four","three","two", "one"))


C_PB <- ggplot(data=CI_PA, aes(x = period, y = carbon, 
                               ymin = hdi_lower, ymax = hdi_upper))+
  
  geom_pointrange(aes(col=source), position=position_dodge(0.2), size=1)+
  geom_errorbar(aes(ymin=hdi_lower, ymax=hdi_upper, col= source), 
                position=position_dodge(0.2), width=0, size=0.9)+
  
  # Labels  
  labs(x="", colour = "Source") +
  ylab(expression(~delta^13*C~("\U2030"))) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  labs(tag = "A") +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  # Legend 
  theme(legend.position = "none") +
  #  theme(legend.key.size = unit(0.6, "cm"))+
  #  theme(legend.title=element_text(size=16)) + # legend title size
  # theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  #  theme(legend.key = element_rect(fill = NA, color = NA))+
  
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
  
  facet_wrap(.~ period, strip.position="left", 
             nrow=15, scales = "free_y",
             labeller = as_labeller(period)) +
  coord_flip()

C_PB


# Nitrogen Prieta B ----------------------------------------------------------------

period <- c("one"= "6mo pre-", "two"= "2mo post-",
            "three"= "9mo post-", "four"="18mo post-")

CI_PA$period <- factor(CI_PA$period, levels=c("four","three","two", "one"))


N_PB <- ggplot(data=CI_PA, aes(x = period, y = carbon, 
                               ymin = hdi_lower, ymax = hdi_upper))+
  
  geom_pointrange(aes(col=source), position=position_dodge(0.2), size=1)+
  geom_errorbar(aes(ymin=hdi_lower, ymax=hdi_upper, col= source), 
                position=position_dodge(0.2), width=0, size=0.9)+
  
  # Labels  
  labs(x="", colour = "Source") +
  ylab(expression(~delta^15*N~("\U2030"))) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), 
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  labs(tag = "D") +
  
  # Axis
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y 
  theme(axis.title.x = element_text(size = 14, angle = 00)) + # axis x
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
# Legend
 # theme(legend.position = "none") +
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA))+
  
  # Strip
  theme(strip.text.y = element_blank()) +
  #  theme(strip.placement = 'outside') +
  #  theme(strip.switch.pad.grid = unit('0.25', "cm")) +
  #  theme(strip.text.x = element_text(size = 14)) +
  #  theme(strip.text.y = element_text(size = 14)) +
  #  theme(strip.background = element_rect(colour="black", fill="gray90"),
  #       strip.text = element_text(margin = margin(10, 10, 10, 10))) +
  
  #
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  # Panel   
  theme(panel.grid.major = element_line(color = "gray95"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
  facet_wrap(.~ period, strip.position="left", 
             nrow=15, scales = "free_y",
             labeller = as_labeller(period)) +
  coord_flip()

N_PB

# Plot --------------------------------------------------------------------



#Ecology format
C_and_N + tiff(filename="D:/LTER/Manuscript 2019 Stable Isotopes/SIA_Plots 2017-2019/Figure 4.tiff",
              height=5600,width=7200,units="px",res=800,compression="lzw")

C_and_N <- grid.arrange(Carbon_PA, Nitrogen_PA,
                        nrow=1, widths = c(5/6, 1))

ggsave("Figure 4.jpeg",C_and_N, width=11, height=8)


# Extra plot --------------------------------------------------------------

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p2_legend <- get_legend(Nitrogen_PA)

p4 <- ggarrange(C_PA, N_PA, C_PB, N_PB,
                ncol = 2, nrow=2, widths = c(5/6, 1, 5/6, 1),
                heights = c(1, 1, 1, 1),
                common.legend = TRUE, legend="bottom") +
  theme(legend.key.size = unit(0.6, "cm"))+
  theme(legend.title=element_text(size=16)) + # legend title size
  theme(legend.text = element_text(color = "black", size = 14))+  #factor name 
  theme(legend.key = element_rect(fill = NA, color = NA)) +
  theme(plot.margin = margin(1,0.1,0.1,0.1, "cm")) # Increase area (margin) of the graph


p4 + ggsave("Figure 4.jpeg", width=11, height=8)



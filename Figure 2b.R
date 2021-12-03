



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
  "Carbon" = "delta^{13}*C",
  "Nitrogen" = "delta^{15}*N")


# Graph 

Fig_iso1 <- ggplot(data = data, aes(x = sampling, y = value, group= stream, colour= stream)) + 
  geom_line(size=1.2) + geom_point(size = 2.2)+
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width = 0) +

  scale_color_manual(values = c('#ce1256','#0570b0'), 
                     labels = c("PA", "PB")) +
                     
# Label  
  labs(x="Sampling event", y = "Isotopic signature", colour = "Stream") +
  scale_x_discrete(labels = c("6mo pre-", "2mo post-", "9mo post-", "18mo post-")) +  
  
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
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  
facet_grid(isotope ~ source, 
               scales= "free_y",
               labeller = labeller(isotope = as_labeller(isotopes_new,  label_parsed),
                                                    source = as_labeller(source_new)))


Fig_iso1

Fig_iso1 + ggsave("Figure 2b.tiff", width=11, height=6.5)



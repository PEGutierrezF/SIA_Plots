


QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)


ggplot(QPA_G_Nov17, aes(x = density, color = source, linetype =source,
                        fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                    labels = c("Algae", "Biofilm", "Leaf litter"),
                    guide = "legend", na.value = "transparent") +
  
  scale_linetype_cyclical(values = c("solid", "dotted", "longdash")) +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "legend") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position=c(.85,.75),
  legend.text = element_text(size=18), # item legend text font size
  legend.title=element_text(size=20), # title font size
  legend.key.height= unit(1, 'cm'),# box height
  legend.key.width= unit(1, 'cm')) + # box width
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#


unique(QPA_G_Nov17$source)




# Final Plot  -------------------------------------------------------------

ggplot(QPA_G_Nov17, aes(x = density, color = source, linetype =source,
                        fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  

  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "legend") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        legend.background = element_rect(fill = "transparent"),
        ) + 
  
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#



















QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)


ggplot(QPA_G_Nov17, aes(x = density, color = source,
                        fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  scale_fill_cyclical(name = "", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                    labels = c("Algae", "Biofilm", "Leaf litter"),
                    guide = "legend", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "legend") +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position=c(.85,.75),
  legend.text = element_text(size=20), #change legend text font size
  legend.key.height= unit(1, 'cm'),
  legend.key.width= unit(1, 'cm')) + 
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#


unique(QPA_G_Nov17$source)


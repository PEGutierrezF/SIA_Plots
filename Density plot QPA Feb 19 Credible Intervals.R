



# ---------------------------------------------
# Density Plots February 2019 Quebrada Prieta A
# Credible Intervals 
# 04 Jul 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())



# Glossosomatidae (1) ---------------------------------------------------------

QPA_G_Feb19 <- read.csv("DensityPlots/QPAFeb19/01 QPA_Glossosomatidae_Feb19.csv")

g1 <- ggplot(QPA_G_Feb19, aes(x = density, color = source, linetype = source,
                               fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), size=1.2, 
                               quantile_lines = TRUE, quantile_fun = hdi,
                               key_glyph = "path", scale= 1) +
  
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "legend") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=16), # item legend text font size
        legend.title=element_text(size=18), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm')) +  # box width
  
  #  guides(color = guide_legend(override.aes = list(fill = "white")))+
  #  theme(plot.margin=unit(c(0,3,0,0),"cm"))+
  #  theme(legend.position=c(1.8,0.5)) +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

g1

L_G_QPA <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Leaflitter")
B_G_QPA <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Biofilm")
A_G_QPA <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Algae")

hdi(L_G_QPA$density)
hdi(B_G_QPA$density)
hdi(A_G_QPA$density)


# Baetidae (2) ----------------------------------------------------------------

QPA_B_Feb19 <- read.csv("DensityPlots/QPAFeb19/02 QPA_Baetidae_Feb19.csv")

b1 <- ggplot(QPA_B_Feb19, aes(x = density, color = source, linetype = source,
                               fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), size=1.2, 
                               quantile_lines = TRUE, quantile_fun = hdi,
                               key_glyph = "path", scale= 1) +
  
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "legend") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Baetidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=16), # item legend text font size
        legend.title=element_text(size=18), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm')) +  # box width
  
  #  guides(color = guide_legend(override.aes = list(fill = "white")))+
  #  theme(plot.margin=unit(c(0,3,0,0),"cm"))+
  #  theme(legend.position=c(1.8,0.5)) +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

b1

L_B_QPA <- QPA_B_Feb19 %>% filter(QPA_B_Feb19$source == "Leaflitter")
B_B_QPA <- QPA_B_Feb19 %>% filter(QPA_B_Feb19$source == "Biofilm")
A_B_QPA <- QPA_B_Feb19 %>% filter(QPA_B_Feb19$source == "Algae")

hdi(L_B_QPA$density)
hdi(B_B_QPA$density)
hdi(A_B_QPA$density)


# Chironomidae (3) ----------------------------------------------------------------

QPA_C_Feb19 <- read.csv("DensityPlots/QPAFeb19/03 QPA_Chrironomidae_Feb19.csv")

c1 <- ggplot(QPA_C_Feb19, aes(x = density, color = source, linetype = source,
                               fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), size=1.2, 
                               quantile_lines = TRUE, quantile_fun = hdi,
                               key_glyph = "path", scale= 1) +
  
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "legend") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Chironomidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=16), # item legend text font size
        legend.title=element_text(size=18), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm')) +  # box width
  
  #  guides(color = guide_legend(override.aes = list(fill = "white")))+
  #  theme(plot.margin=unit(c(0,3,0,0),"cm"))+
  #  theme(legend.position=c(1.8,0.5)) +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

c1


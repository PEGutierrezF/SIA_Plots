



# ---------------------------------------------
# Density Plots November 2017 Quebrada Prieta A
# Credible Intervals
# 04 Jul 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())



# Glossosomatidae (1) ---------------------------------------------------------

QPB_G_Nov17 <- read.csv("DensityPlots/QPB_Nov17/01 QPB_Glossosomatidae_Nov17.csv")

g1 <- ggplot(QPB_G_Nov17, aes(x = density, color = source, linetype = source,
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

L_G_QPA <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Leaflitter")
B_G_QPA <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Biofilm")
A_G_QPA <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Algae")

hdi(L_G_QPA$density)
hdi(B_G_QPA$density)
hdi(A_G_QPA$density)


# Baetidae (2) ----------------------------------------------------------------

QPB_B_Nov17 <- read.csv("DensityPlots/QPB_Nov17/02 QPB_Baetidae_Nov17.csv")

b1 <- ggplot(QPB_B_Nov17, aes(x = density, color = source, linetype = source,
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

L_B_QPA <- QPB_B_Nov17 %>% filter(QPB_B_Nov17$source == "Leaflitter")
B_B_QPA <- QPB_B_Nov17 %>% filter(QPB_B_Nov17$source == "Biofilm")
A_B_QPA <- QPB_B_Nov17 %>% filter(QPB_B_Nov17$source == "Algae")

hdi(L_B_QPA$density)
hdi(B_B_QPA$density)
hdi(A_B_QPA$density)


# Chironomidae (3) ----------------------------------------------------------------

QPB_C_Nov17 <- read.csv("DensityPlots/QPB_Nov17/03 QPB_Chrironomidae_Nov17.csv")

c1 <- ggplot(QPB_C_Nov17, aes(x = density, color = source, linetype = source,
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

# Njulio (4) ----------------------------------------------------------------

QPB_Nj_Nov17 <- read.csv("DensityPlots/QPB_Nov17/04 QPB_Njulio_Nov17.csv")

n1 <- ggplot(QPB_Nj_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('N. julio') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

n1


# Phylloicus (5) ----------------------------------------------------------------

QPB_P_Nov17 <- read.csv("DensityPlots/QPB_Nov17/05 QPB_Phylloicus_Nov17.csv")

p1 <- ggplot(QPB_P_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('P. pulchrus') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

p1


# Libellulidae (6) ----------------------------------------------------------------

QPB_L_Nov17 <- read.csv("DensityPlots/QPB_Nov17/06 QPB_Libellulidae_Nov17.csv")

l1 <- ggplot(QPB_L_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('Libelullidae') +
  theme(plot.title = element_text(face="bold")) +
  
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

l1

# Xyphocarys (7) ----------------------------------------------------------------

QPB_X_Nov17 <- read.csv("DensityPlots/QPB_Nov17/07 QPB_Xyphocaris_Nov17.csv")

x1 <- ggplot(QPB_X_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('X. elongata') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

x1

# Atya (8) ----------------------------------------------------------------

QPB_A_Nov17 <- read.csv("DensityPlots/QPB_Nov17/08 QPB_Atya_Nov17.csv")

a1 <- ggplot(QPB_A_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('A. lanipes') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

a1


# Macrobrachium (9) ----------------------------------------------------------------

QPB_M_Nov17 <- read.csv("DensityPlots/QPB_Nov17/09 QPB_Macrobrachium_Nov17.csv")

m1 <- ggplot(QPB_M_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('M. crenulatum') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

m1

# Anolis (10) ------------------------------------------------------------------

QPB_An_Nov17 <- read.csv("DensityPlots/QPB_Nov17/10 QPB_Anolis_Nov17.csv")

an1 <- ggplot(QPB_An_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('A. evermanni') +
  theme(plot.title = element_text(face="bold.italic")) +
  
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

an1


# Luecauge (11) ------------------------------------------------------------------

QPB_Lr_Nov17 <- read.csv("DensityPlots/QPB_Nov17/11 QPB_Leucauge_Nov17.csv")

lr <- ggplot(QPB_Lr_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('L. regnyi') +
  theme(plot.title = element_text(face="bold.italic")) +
  
  theme(legend.position = "none") +
  theme(legend.text = element_text(size=16), # item legend text font size
        legend.title=element_text(size=18), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm')) +  # box width
  
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  theme(plot.margin=unit(c(0,0.5,0,0),"cm"))+
  theme(legend.position=c(1.4,0.5)) +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

lr


Fig6 <- (x1+a1+m1) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + lr + plot_spacer())
Fig6
Fig6 + ggsave("Figure X Density plot QPB Nov 17 Credible Intervals.jpg",width = 210, height = 297, units = "mm")



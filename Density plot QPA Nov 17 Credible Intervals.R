



# ---------------------------------------------
# Density Plots November 2017 Quebrada Prieta A
# Credible Intervals 
# 02 Jul 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())



QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)

g1 <- ggplot(QPA_G_Nov17, aes(x = density, color = source, linetype = source,
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

L_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Leaflitter")
B_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Biofilm")
A_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Algae")

round(hdi(L_G_QPA$density),2)
round(hdi(B_G_QPA$density),2)
round(hdi(A_G_QPA$density),2)

round(mean(L_G_QPA$density),2)
round(mean(B_G_QPA$density),2)
round(mean(A_G_QPA$density),2)


# Baetidae ----------------------------------------------------------------

QPA_B_Nov17 <- read.csv("DensityPlots/QPANov17/02 QPA_Baetidae_Nov17.csv")

b1 <- ggplot(QPA_B_Nov17, aes(x = density, color = source, linetype = source,
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

L_B_QPA <- QPA_B_Nov17 %>% filter(QPA_B_Nov17$source == "Leaflitter")
B_B_QPA <- QPA_B_Nov17 %>% filter(QPA_B_Nov17$source == "Biofilm")
A_B_QPA <- QPA_B_Nov17 %>% filter(QPA_B_Nov17$source == "Algae")

round(hdi(L_B_QPA$density),2)
round(hdi(B_B_QPA$density),2)
round(hdi(A_B_QPA$density),2)

round(mean(L_B_QPA$density),2)
round(mean(B_B_QPA$density),2)
round(mean(A_B_QPA$density),2)

# Chironomidae ----------------------------------------------------------------

QPA_C_Nov17 <- read.csv("DensityPlots/QPANov17/03 QPA_Chrironomidae_Nov17.csv")

c1 <- ggplot(QPA_C_Nov17, aes(x = density, color = source, linetype = source,
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

L_C_QPA <- QPA_C_Nov17 %>% filter(QPA_C_Nov17$source == "Leaflitter")
B_C_QPA <- QPA_C_Nov17 %>% filter(QPA_C_Nov17$source == "Biofilm")
A_C_QPA <- QPA_C_Nov17 %>% filter(QPA_C_Nov17$source == "Algae")

round(hdi(L_C_QPA$density),2)
round(hdi(B_C_QPA$density),2)
round(hdi(A_C_QPA$density),2)

round(mean(L_C_QPA$density),2)
round(mean(B_C_QPA$density),2)
round(mean(A_C_QPA$density),2)


# Njulio ----------------------------------------------------------------

QPA_Nj_Nov17 <- read.csv("DensityPlots/QPANov17/04 QPA_Njulio_Nov17.csv")

n1 <- ggplot(QPA_Nj_Nov17, aes(x = density, color = source, linetype = source,
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

L_Nj_QPA <- QPA_Nj_Nov17 %>% filter(QPA_Nj_Nov17$source == "Leaflitter")
B_Nj_QPA <- QPA_Nj_Nov17 %>% filter(QPA_Nj_Nov17$source == "Biofilm")
A_Nj_QPA <- QPA_Nj_Nov17 %>% filter(QPA_Nj_Nov17$source == "Algae")

round(hdi(L_Nj_QPA$density),2)
round(hdi(B_Nj_QPA$density),2)
round(hdi(A_Nj_QPA$density),2)

round(mean(L_Nj_QPA$density),2)
round(mean(B_Nj_QPA$density),2)
round(mean(A_Nj_QPA$density),2)


# Phylloicus ----------------------------------------------------------------

QPA_P_Nov17 <- read.csv("DensityPlots/QPANov17/05 QPA_Phylloicus_Nov17.csv")

p1 <- ggplot(QPA_P_Nov17, aes(x = density, color = source, linetype = source,
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

L_Ph_QPA <- QPA_P_Nov17 %>% filter(QPA_P_Nov17$source == "Leaflitter")
B_Ph_QPA <- QPA_P_Nov17 %>% filter(QPA_P_Nov17$source == "Biofilm")
A_Ph_QPA <- QPA_P_Nov17 %>% filter(QPA_P_Nov17$source == "Algae")

round(hdi(L_Ph_QPA$density),2)
round(hdi(B_Ph_QPA$density),2)
round(hdi(A_Ph_QPA$density),2)

round(mean(L_Ph_QPA$density),2)
round(mean(B_Ph_QPA$density),2)
round(mean(A_Ph_QPA$density),2)

# Libellulidae ----------------------------------------------------------------

QPA_L_Nov17 <- read.csv("DensityPlots/QPANov17/06 QPA_Libellulidae_Nov17.csv")

l1 <- ggplot(QPA_L_Nov17, aes(x = density, color = source, linetype = source,
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
  ggtitle('Libellulidae') +
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

L_L_QPA <- QPA_L_Nov17 %>% filter(QPA_L_Nov17$source == "Leaflitter")
B_L_QPA <- QPA_L_Nov17 %>% filter(QPA_L_Nov17$source == "Biofilm")
A_L_QPA <- QPA_L_Nov17 %>% filter(QPA_L_Nov17$source == "Algae")

round(hdi(L_L_QPA$density),2)
round(hdi(B_L_QPA$density),2)
round(hdi(A_L_QPA$density),2)

round(mean(L_L_QPA$density),2)
round(mean(B_L_QPA$density),2)
round(mean(A_L_QPA$density),2)

# Xyphocarys ----------------------------------------------------------------

QPA_X_Nov17 <- read.csv("DensityPlots/QPANov17/07 QPA_Xyphocaris_Nov17.csv")

x1 <- ggplot(QPA_X_Nov17, aes(x = density, color = source, linetype = source,
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

L_x_QPA <- QPA_X_Nov17 %>% filter(QPA_X_Nov17$source == "Leaflitter")
B_x_QPA <- QPA_X_Nov17 %>% filter(QPA_X_Nov17$source == "Biofilm")
A_x_QPA <- QPA_X_Nov17 %>% filter(QPA_X_Nov17$source == "Algae")

round(hdi(L_x_QPA$density),2)
round(hdi(B_x_QPA$density),2)
round(hdi(A_x_QPA$density),2)

round(mean(L_x_QPA$density),2)
round(mean(B_x_QPA$density),2)
round(mean(A_x_QPA$density),2)

# Atya (8) ----------------------------------------------------------------

QPA_A_Nov17 <- read.csv("DensityPlots/QPANov17/08 QPA_Atya_Nov17.csv")

a1 <- ggplot(QPA_A_Nov17, aes(x = density, color = source, linetype = source,
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

L_A_QPA <- QPA_A_Nov17 %>% filter(QPA_A_Nov17$source == "Leaflitter")
B_A_QPA <- QPA_A_Nov17 %>% filter(QPA_A_Nov17$source == "Biofilm")
A_A_QPA <- QPA_A_Nov17 %>% filter(QPA_A_Nov17$source == "Algae")

round(hdi(L_A_QPA$density),2)
round(hdi(B_A_QPA$density),2)
round(hdi(A_A_QPA$density),2)

round(mean(L_A_QPA$density),2)
round(mean(B_A_QPA$density),2)
round(mean(A_A_QPA$density),2)

# Macrobrachium (9) ----------------------------------------------------------------

QPA_M_Nov17 <- read.csv("DensityPlots/QPANov17/09 QPA_Macrobrachium_Nov17.csv")

m1 <- ggplot(QPA_M_Nov17, aes(x = density, color = source, linetype = source,
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

L_M_QPA <- QPA_M_Nov17 %>% filter(QPA_M_Nov17$source == "Leaflitter")
B_M_QPA <- QPA_M_Nov17 %>% filter(QPA_M_Nov17$source == "Biofilm")
A_M_QPA <- QPA_M_Nov17 %>% filter(QPA_M_Nov17$source == "Algae")

round(hdi(L_M_QPA$density),2)
round(hdi(B_M_QPA$density),2)
round(hdi(A_M_QPA$density),2)

round(mean(L_M_QPA$density),2)
round(mean(B_M_QPA$density),2)
round(mean(A_M_QPA$density),2)

# Anolis (10) ------------------------------------------------------------------

QPA_An_Nov17 <- read.csv("DensityPlots/QPANov17/10 QPA_Anolis_Nov17.csv")

an1 <- ggplot(QPA_An_Nov17, aes(x = density, color = source, linetype = source,
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

L_An_QPA <- QPA_An_Nov17 %>% filter(QPA_An_Nov17$source == "Leaflitter")
B_An_QPA <- QPA_An_Nov17 %>% filter(QPA_An_Nov17$source == "Biofilm")
A_An_QPA <- QPA_An_Nov17 %>% filter(QPA_An_Nov17$source == "Algae")

round(hdi(L_An_QPA$density),2)
round(hdi(B_An_QPA$density),2)
round(hdi(A_An_QPA$density),2)

round(mean(L_An_QPA$density),2)
round(mean(B_An_QPA$density),2)
round(mean(A_An_QPA$density),2)

# Luecauge (11) ------------------------------------------------------------------

QPA_Lm_Nov17 <- read.csv("DensityPlots/QPANov17/11 QPA_Leucauge_Nov17.csv")

lr <- ggplot(QPA_Lm_Nov17, aes(x = density, color = source, linetype = source,
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

L_Lm_QPA <- QPA_Lm_Nov17 %>% filter(QPA_Lm_Nov17$source == "Leaflitter")
B_Lm_QPA <- QPA_Lm_Nov17 %>% filter(QPA_Lm_Nov17$source == "Biofilm")
A_Lm_QPA <- QPA_Lm_Nov17 %>% filter(QPA_Lm_Nov17$source == "Algae")

round(hdi(L_Lm_QPA$density),2)
round(hdi(B_Lm_QPA$density),2)
round(hdi(A_Lm_QPA$density),2)

round(mean(L_Lm_QPA$density),2)
round(mean(B_Lm_QPA$density),2)
round(mean(A_Lm_QPA$density),2)



Fig_QP_Nov17 <- (x1+a1+m1) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + lr + plot_spacer())
Fig_QP_Nov17
Fig_QP_Nov17 + ggsave("Figure S3 Density plot QPA Nov 17 Credible Intervals.tiff",width = 210, height = 297, units = "mm")


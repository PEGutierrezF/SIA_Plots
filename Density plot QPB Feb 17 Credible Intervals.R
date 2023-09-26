



# ---------------------------------------------
# Density Plots February 2017 Quebrada Prieta B
# Credible Intervals
# 04 Jul 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



# Glossosomatidae (1) ---------------------------------------------------------

QPB_G_Feb17 <- read.csv("DensityPlots/QPB_Feb17/01 QPB_Glossosomatidae_Feb17.csv")

g1 <- ggplot(QPB_G_Feb17, aes(x = density, color = source, linetype = source,
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


L_G_QPB <- QPB_G_Feb17 %>% filter(QPB_G_Feb17$source == "Leaflitter")
B_G_QPB <- QPB_G_Feb17 %>% filter(QPB_G_Feb17$source == "Biofilm")
A_G_QPB <- QPB_G_Feb17 %>% filter(QPB_G_Feb17$source == "Algae")

round(hdi(L_G_QPB$density),2)
round(hdi(B_G_QPB$density),2)
round(hdi(A_G_QPB$density),2)

round(mean(L_G_QPB$density),2)
round(mean(B_G_QPB$density),2)
round(mean(A_G_QPB$density),2)

# Baetidae (2) ----------------------------------------------------------------

QPB_B_Feb17 <- read.csv("DensityPlots/QPB_Feb17/02 QPB_Baetidae_Feb17.csv")

b1 <- ggplot(QPB_B_Feb17, aes(x = density, color = source, linetype = source,
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

L_B_QPB <- QPB_B_Feb17 %>% filter(QPB_B_Feb17$source == "Leaflitter")
B_B_QPB <- QPB_B_Feb17 %>% filter(QPB_B_Feb17$source == "Biofilm")
A_B_QPB <- QPB_B_Feb17 %>% filter(QPB_B_Feb17$source == "Algae")

round(hdi(L_B_QPB$density),2)
round(hdi(B_B_QPB$density),2)
round(hdi(A_B_QPB$density),2)

round(mean(L_B_QPB$density),2)
round(mean(B_B_QPB$density),2)
round(mean(A_B_QPB$density),2)


# Chironomidae (3) ----------------------------------------------------------------

QPB_C_Feb17 <- read.csv("DensityPlots/QPB_Feb17/03 QPB_Chironomidae_Feb17.csv")


c1 <- ggplot(QPB_C_Feb17, aes(x = density, color = source, linetype = source,
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

L_C_QPB <- QPB_C_Feb17 %>% filter(QPB_C_Feb17$source == "Leaflitter")
B_C_QPB <- QPB_C_Feb17 %>% filter(QPB_C_Feb17$source == "Biofilm")
A_C_QPB <- QPB_C_Feb17 %>% filter(QPB_C_Feb17$source == "Algae")

round(hdi(L_C_QPB$density),2)
round(hdi(B_C_QPB$density),2)
round(hdi(A_C_QPB$density),2)

round(mean(L_C_QPB$density),2)
round(mean(B_C_QPB$density),2)
round(mean(A_C_QPB$density),2)


# Njulio ----------------------------------------------------------------

QPB_Nj_Feb17 <- read.csv("DensityPlots/QPB_Feb17/04 QPB_Njulio_Feb17.csv")

n1 <- ggplot(QPB_Nj_Feb17, aes(x = density, color = source, linetype = source,
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

L_Nj_QPB <- QPB_Nj_Feb17 %>% filter(QPB_Nj_Feb17$source == "Leaflitter")
B_Nj_QPB <- QPB_Nj_Feb17 %>% filter(QPB_Nj_Feb17$source == "Biofilm")
A_Nj_QPB <- QPB_Nj_Feb17 %>% filter(QPB_Nj_Feb17$source == "Algae")

round(hdi(L_Nj_QPB$density),2)
round(hdi(B_Nj_QPB$density),2)
round(hdi(A_Nj_QPB$density),2)

round(mean(L_Nj_QPB$density),2)
round(mean(B_Nj_QPB$density),2)
round(mean(A_Nj_QPB$density),2)


# Phylloicus (5) ----------------------------------------------------------------

QPB_P_Feb17 <- read.csv("DensityPlots/QPB_Feb17/05 QPB_Phylloicus_Feb17.csv")

p1 <- ggplot(QPB_P_Feb17, aes(x = density, color = source, linetype = source,
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

L_Ph_QPB <- QPB_P_Feb17 %>% filter(QPB_P_Feb17$source == "Leaflitter")
B_Ph_QPB <- QPB_P_Feb17 %>% filter(QPB_P_Feb17$source == "Biofilm")
A_Ph_QPB <- QPB_P_Feb17 %>% filter(QPB_P_Feb17$source == "Algae")

round(hdi(L_Ph_QPB$density),2)
round(hdi(B_Ph_QPB$density),2)
round(hdi(A_Ph_QPB$density),2)

round(mean(L_Ph_QPB$density),2)
round(mean(B_Ph_QPB$density),2)
round(mean(A_Ph_QPB$density),2)


# Libellulidae ----------------------------------------------------------------

QPB_L_Feb17 <- read.csv("DensityPlots/QPB_Feb17/06 QPB_Libellulidae_Feb17.csv")

l1 <- ggplot(QPB_L_Feb17, aes(x = density, color = source, linetype = source,
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

L_L_QPB <- QPB_L_Feb17 %>% filter(QPB_L_Feb17$source == "Leaflitter")
B_L_QPB <- QPB_L_Feb17 %>% filter(QPB_L_Feb17$source == "Biofilm")
A_L_QPB <- QPB_L_Feb17 %>% filter(QPB_L_Feb17$source == "Algae")

round(hdi(L_L_QPB$density),2)
round(hdi(B_L_QPB$density),2)
round(hdi(A_L_QPB$density),2)

round(mean(L_L_QPB$density),2)
round(mean(B_L_QPB$density),2)
round(mean(A_L_QPB$density),2)


# Xyphocarys ----------------------------------------------------------------

QPB_X_Feb17 <- read.csv("DensityPlots/QPB_Feb17/07 QPB_Xyphocaris_Feb17.csv")

x1 <- ggplot(QPB_X_Feb17, aes(x = density, color = source, linetype = source,
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

L_x_QPB <- QPB_X_Feb17 %>% filter(QPB_X_Feb17$source == "Leaflitter")
B_x_QPB <- QPB_X_Feb17 %>% filter(QPB_X_Feb17$source == "Biofilm")
A_x_QPB <- QPB_X_Feb17 %>% filter(QPB_X_Feb17$source == "Algae")

round(hdi(L_x_QPB$density),2)
round(hdi(B_x_QPB$density),2)
round(hdi(A_x_QPB$density),2)

round(mean(L_x_QPB$density),2)
round(mean(B_x_QPB$density),2)
round(mean(A_x_QPB$density),2)

# Atya (8) ----------------------------------------------------------------

QPB_A_Feb17 <- read.csv("DensityPlots/QPB_Feb17/08 QPB_Atya_Feb17.csv")

a1 <- ggplot(QPB_A_Feb17, aes(x = density, color = source, linetype = source,
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

L_A_QPB <- QPB_A_Feb17 %>% filter(QPB_A_Feb17$source == "Leaflitter")
B_A_QPB <- QPB_A_Feb17 %>% filter(QPB_A_Feb17$source == "Biofilm")
A_A_QPB <- QPB_A_Feb17 %>% filter(QPB_A_Feb17$source == "Algae")

round(hdi(L_A_QPB$density),2)
round(hdi(B_A_QPB$density),2)
round(hdi(A_A_QPB$density),2)

round(mean(L_A_QPB$density),2)
round(mean(B_A_QPB$density),2)
round(mean(A_A_QPB$density),2)

# Macrobrachium (9) ----------------------------------------------------------------

QPB_M_Feb17 <- read.csv("DensityPlots/QPB_Feb17/09 QPB_Macrobrachium_Feb17.csv")

m1 <- ggplot(QPB_M_Feb17, aes(x = density, color = source, linetype = source,
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

L_M_QPB <- QPB_M_Feb17 %>% filter(QPB_M_Feb17$source == "Leaflitter")
B_M_QPB <- QPB_M_Feb17 %>% filter(QPB_M_Feb17$source == "Biofilm")
A_M_QPB <- QPB_M_Feb17 %>% filter(QPB_M_Feb17$source == "Algae")

round(hdi(L_M_QPB$density),2)
round(hdi(B_M_QPB$density),2)
round(hdi(A_M_QPB$density),2)

round(mean(L_M_QPB$density),2)
round(mean(B_M_QPB$density),2)
round(mean(A_M_QPB$density),2)

# Anolis (10) ------------------------------------------------------------------

QPB_An_Feb17 <- read.csv("DensityPlots/QPB_Feb17/10 QPB_Anolis_Feb17.csv")

an1 <- ggplot(QPB_An_Feb17, aes(x = density, color = source, linetype = source,
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
  
  guides(color = guide_legend(override.aes = list(fill = "white")))+
  theme(plot.margin=unit(c(0,3,0,0),"cm"))+
  theme(legend.position=c(1.8,0.5)) +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

an1

L_An_QPB <- QPB_An_Feb17 %>% filter(QPB_An_Feb17$source == "Leaflitter")
B_An_QPB <- QPB_An_Feb17 %>% filter(QPB_An_Feb17$source == "Biofilm")
A_An_QPB <- QPB_An_Feb17 %>% filter(QPB_An_Feb17$source == "Algae")

round(hdi(L_An_QPB$density),2)
round(hdi(B_An_QPB$density),2)
round(hdi(A_An_QPB$density),2)

round(mean(L_An_QPB$density),2)
round(mean(B_An_QPB$density),2)
round(mean(A_An_QPB$density),2)


Fig5 <- (x1+a1+m1) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + plot_spacer()+ plot_spacer())
Fig5
Fig5 + ggsave("Figure S2 Density plot QPB Feb 17 Credible Intervals.jpg",width = 210, height = 297, units = "mm")





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



QPA_G_Feb17 <- read.csv("DensityPlots/QPAFeb17/01 QPA_Glossosomatidae_Feb17.csv")
head(QPA_G_Feb17)

g1 <- ggplot(QPA_G_Feb17, aes(x = density, color = source, linetype =source,
                        fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
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
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

g1

L_G_QPA <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Leaflitter")
B_G_QPA <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Biofilm")
A_G_QPA <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Algae")

hdi(L_G_QPA$density)
hdi(B_G_QPA$density)
hdi(A_G_QPA$density)


# Baetidae ----------------------------------------------------------------

QPA_B_Feb17 <- read.csv("DensityPlots/QPAFeb17/02 QPA_Baetidae_Feb17.csv")

b1 <- ggplot(QPA_B_Feb17, aes(x = density, color = source, linetype =source,
                              fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Baetidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

b1


# Chironomidae ----------------------------------------------------------------

QPA_C_Feb17 <- read.csv("DensityPlots/QPAFeb17/03 QPA_Chrironomidae_Feb17.csv")


c1 <- ggplot(QPA_C_Feb17, aes(x = density, color = source, linetype =source,
                           fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Chironomidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

c1

# Njulio ----------------------------------------------------------------

QPA_Nj_Feb17 <- read.csv("DensityPlots/QPAFeb17/04 QPA_Njulio_Feb17.csv")

n1 <- ggplot(QPA_Nj_Feb17, aes(x = density, color = source, linetype =source,
                              fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('N. julio') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

n1


# Phylloicus ----------------------------------------------------------------

QPA_P_Feb17 <- read.csv("DensityPlots/QPAFeb17/05 QPA_Phylloicus_Feb17.csv")

p1 <- ggplot(QPA_P_Feb17, aes(x = density, color = source, linetype =source,
                               fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('P. pulchrus') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

p1

# Libellulidae ----------------------------------------------------------------

QPA_L_Feb17 <- read.csv("DensityPlots/QPAFeb17/06 QPA_Libellulidae_Feb17.csv")

l1 <- ggplot(QPA_L_Feb17, aes(x = density, color = source, linetype =source,
                              fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0, 
                               scale = 1) +
  labs(y = "Density", x = "Source contribution") + 
  
  # add legend:  guide = "legend",
  scale_linetype_cyclical(name = "Source", values = c("solid", "dotted", "longdash"),
                          labels = c("Algae", "Biofilm", "Leaf litter"),
                          guide = "none") +
  
  scale_fill_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +
  
  # add legend:  guide = "legend",
  scale_color_cyclical(name = "Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                       labels = c("Algae", "Biofilm", "Leaf litter"),
                       guide = "none") +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Libelullidae') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position=c(.85,.75),
        legend.text = element_text(size=18), # item legend text font size
        legend.title=element_text(size=20), # title font size
        legend.key.height= unit(1, 'cm'),# box height
        legend.key.width= unit(1, 'cm'), # box width
        # legend.background = element_rect(fill = "transparent"),
  ) + 
  
  # guides(color = guide_legend(override.aes = list(fill = "white")))+
  # annotate("text", x = .8, y = 4.5, label = "Some text") +
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

l1

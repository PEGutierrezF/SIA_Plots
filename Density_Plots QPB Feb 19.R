



# ---------------------------------------------
# Density Plots February 20019 Quebrada Prieta B
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



QPB_G_Feb19 <- read.csv("DensityPlots/QPB_Feb19/01 QPB_Glossosomatidae_Feb19.csv")
head(QPB_G_Feb19)


dens <- lapply(split(QPB_G_Feb19, QPB_G_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

g1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,
            position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,
            position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

L <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Leaflitter")
quantile(L$density, 0.025)
quantile(L$density, 0.975)

B <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Biofilm")
quantile(B$density, 0.025)
quantile(B$density, 0.975)

A <- QPB_G_Nov17 %>% filter(QPB_G_Nov17$source == "Algae")
quantile(A$density, 0.025)
quantile(A$density, 0.975)



# Baetidae ----------------------------------------------------------------

QPB_B_Feb19 <- read.csv("DensityPlots/QPB_Feb19/02 QPB_Baetidae_Feb19.csv")

dens <- lapply(split(QPB_B_Feb19, QPB_B_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

b1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8.5) +
  xlim(0, 1) +
  ggtitle('Baetidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# Chironomidae ----------------------------------------------------------------

QPB_C_Feb19 <- read.csv("DensityPlots/QPB_Feb19/03 QPB_Chrironomidae_Feb19.csv")

dens <- lapply(split(QPB_C_Feb19, QPB_C_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

c1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Chironomidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Njulio ----------------------------------------------------------------

QPB_Nj_Feb19 <- read.csv("DensityPlots/QPB_Feb19/04 QPB_Njulio_Feb19.csv")

dens <- lapply(split(QPB_Nj_Feb19, QPB_Nj_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

n1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('N. julio') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Phylloicus ----------------------------------------------------------------

QPB_P_Feb19 <- read.csv("DensityPlots/QPB_Feb19/05 QPB_Phylloicus_Feb19.csv")

dens <- lapply(split(QPB_P_Feb19, QPB_P_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

p1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('P. pulchrus') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Libellulidae ----------------------------------------------------------------

QPB_L_Feb19 <- read.csv("DensityPlots/QPB_Feb19/06 QPB_Libellulidae_Feb19.csv")

dens <- lapply(split(QPB_L_Feb19, QPB_L_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

l1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Libelullidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# Xyphocarys ----------------------------------------------------------------

QPB_X_Feb19 <- read.csv("DensityPlots/QPB_Feb19/07 QPB_Xyphocaris_Feb19.csv")

dens <- lapply(split(QPB_X_Feb19, QPB_X_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df_x <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

x1 <- ggplot(df_x, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('X. elongata') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Atya ----------------------------------------------------------------

QPB_A_Feb19 <- read.csv("DensityPlots/QPB_Feb19/08 QPB_Atya_Feb19.csv")

dens <- lapply(split(QPB_A_Feb19, QPB_A_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

a1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('A. lanipes') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# Macrobrachium ----------------------------------------------------------------

QPB_M_Feb19 <- read.csv("DensityPlots/QPB_Feb19/09 QPB_Macrobrachium_Feb19.csv")

dens <- lapply(split(QPB_M_Feb19, QPB_M_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

m1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('M. crenulatum') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))




# Anolis ------------------------------------------------------------------

QPB_An_Feb19 <- read.csv("DensityPlots/QPB_Feb19/10 QPB_Anolis_Feb19.csv")

dens <- lapply(split(QPB_An_Feb19, QPB_An_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

an1 <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_color_manual(values = c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('A. evermanni') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# Luecauge ------------------------------------------------------------------

QPB_Lm_Feb19 <- read.csv("DensityPlots/QPB_Feb19/11 QPB_Leucauge_Feb19.csv")

dens <- lapply(split(QPB_Lm_Feb19, QPB_Lm_Feb19$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975) 

Lm <- ggplot(df, aes(x, y, color = source)) + 
  geom_area(data = df[df$lower,], aes(fill = source), alpha = 0.5,position = "identity") +
  geom_area(data = df[df$upper,], aes(fill = source), alpha = 0.5,position = "identity") +
  labs(y = "Density", x = "Source contribution") +
  geom_line(aes(linetype = source), size = 1.2) +
  scale_fill_manual("Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                    labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  scale_color_manual("Source",values = c("#31a354", "#2c7fb8", "#d95f0e"),
                     labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  scale_linetype_manual("Source",values = c("solid", "dotted", "longdash"),
                        labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('L. regnyi') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  
  theme(legend.title = element_text(size = 18)) + #title
  theme(legend.text = element_text(size = 16))  + #
  guides(color=guide_legend(override.aes=list(fill=NA)))



Fig_QPB_Feb19 <- (x1+a1+m1) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + Lm+ plot_spacer())
Fig_QPB_Feb19
Fig_QPB_Feb19 + ggsave("Figure X Density plot QPB Feb 19.pdf",width = 210, height = 297, units = "mm")


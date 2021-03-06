



# ---------------------------------------------
# Density Plots February 2017 Quebrada Prieta A
# 09 Jun 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



QPA_G_Feb17 <- read.csv("DensityPlots/QPAFeb17/01 QPA_Glossosomatidae_Feb17.csv")
head(QPA_G_Feb17)


dens <- lapply(split(QPA_G_Feb17, QPA_G_Feb17$source), 
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

L <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Leaflitter")
quantile(L$density, 0.025)
quantile(L$density, 0.975)

B <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Biofilm")
quantile(B$density, 0.025)
quantile(B$density, 0.975)

A <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Algae")
quantile(A$density, 0.025)
quantile(A$density, 0.975)

# Baetidae ----------------------------------------------------------------

QPA_B_Feb17 <- read.csv("DensityPlots/QPAFeb17/02 QPA_Baetidae_Feb17.csv")

dens <- lapply(split(QPA_B_Feb17, QPA_B_Feb17$source), 
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
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Baetidae') +
  theme(plot.title = element_text(face="bold"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Chironomidae ----------------------------------------------------------------

QPA_C_Feb17 <- read.csv("DensityPlots/QPAFeb17/03 QPA_Chrironomidae_Feb17.csv")

dens <- lapply(split(QPA_C_Feb17, QPA_C_Feb17$source), 
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

QPA_Nj_Feb17 <- read.csv("DensityPlots/QPAFeb17/04 QPA_Njulio_Feb17.csv")

dens <- lapply(split(QPA_Nj_Feb17, QPA_Nj_Feb17$source), 
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

QPA_P_Feb17 <- read.csv("DensityPlots/QPAFeb17/05 QPA_Phylloicus_Feb17.csv")

dens <- lapply(split(QPA_P_Feb17, QPA_P_Feb17$source), 
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

QPA_L_Feb17 <- read.csv("DensityPlots/QPAFeb17/06 QPA_Libellulidae_Feb17.csv")

dens <- lapply(split(QPA_L_Feb17, QPA_L_Feb17$source), 
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
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Xiphocaris ----------------------------------------------------------------

QPA_X_Feb17 <- read.csv("DensityPlots/QPAFeb17/07 QPA_Xyphocaris_Feb17.csv")

dens <- lapply(split(QPA_X_Feb17, QPA_X_Feb17$source), 
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

QPA_A_Feb17 <- read.csv("DensityPlots/QPAFeb17/08 QPA_Atya_Feb17.csv")

dens <- lapply(split(QPA_A_Feb17, QPA_A_Feb17$source), 
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

QPA_M_Feb17 <- read.csv("DensityPlots/QPAFeb17/09 QPA_Macrobrachium_Feb17.csv")

dens <- lapply(split(QPA_M_Feb17, QPA_M_Feb17$source), 
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

QPA_An_Feb17 <- read.csv("DensityPlots/QPAFeb17/10 QPA_Anolis_Feb17.csv")

dens <- lapply(split(QPA_An_Feb17, QPA_An_Feb17$source), 
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
  scale_fill_manual("Source", values = c("#31a354", "#2c7fb8", "#d95f0e"),
          labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  scale_color_manual("Source",values = c("#31a354", "#2c7fb8", "#d95f0e"),
        labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  scale_linetype_manual("Source",values = c("solid", "dotted", "longdash"),
            labels = c("Algae", "Biofilm", "Leaf litter")) +
  
  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  
  ggtitle('A. evermanni') +
  theme(plot.title = element_text(face="bold.italic"))+
  
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  
  theme(legend.title = element_text(size = 18)) + #title
  theme(legend.text = element_text(size = 16))  + #
  guides(color=guide_legend(override.aes=list(fill=NA)))


Fig1 <- (x1+a1+m1) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + plot_spacer()+ plot_spacer())
Fig1
Fig1 + ggsave("Figure X Density plot QPA Feb 17.pdf",width = 210, height = 297, units = "mm")




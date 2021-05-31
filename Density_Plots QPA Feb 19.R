



# ---------------------------------------------
# Density Plots Quebrada Prieta A
# 06 May 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  




QPA_G_Feb19 <- read.csv("DensityPlots/QPAFeb19/01 QPA_Glossosomatidae_Feb19.csv")
head(QPA_G_Feb19)


dens <- lapply(split(QPA_G_Feb19, QPA_G_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

L <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Leaflitter")
quantile(L$density, 0.025)
quantile(L$density, 0.975)

B <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Biofilm")
quantile(B$density, 0.025)
quantile(B$density, 0.975)

A <- QPA_G_Feb19 %>% filter(QPA_G_Feb19$source == "Algae")
quantile(A$density, 0.025)
quantile(A$density, 0.975)




# Baetidae ----------------------------------------------------------------

QPA_B_Feb19 <- read.csv("DensityPlots/QPAFeb19/02 QPA_Baetidae_Feb19.csv")

dens <- lapply(split(QPA_B_Feb19, QPA_B_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Chironomidae ----------------------------------------------------------------

QPA_C_Feb19 <- read.csv("DensityPlots/QPAFeb19/03 QPA_Chrironomidae_Feb19.csv")

dens <- lapply(split(QPA_C_Feb19, QPA_C_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Njulio ----------------------------------------------------------------

QPA_Nj_Feb19 <- read.csv("DensityPlots/QPAFeb19/04 QPA_Njulio_Feb19.csv")

dens <- lapply(split(QPA_Nj_Feb19, QPA_Nj_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Phylloicus ----------------------------------------------------------------

QPA_P_Feb19 <- read.csv("DensityPlots/QPAFeb19/05 QPA_Phylloicus_Feb19.csv")

dens <- lapply(split(QPA_P_Feb19, QPA_P_Feb19$source), 
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
  ggtitle('Phylloicus') +
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Libellulidae ----------------------------------------------------------------

QPA_L_Feb19 <- read.csv("DensityPlots/QPAFeb19/06 QPA_Libellulidae_Feb19.csv")

dens <- lapply(split(QPA_L_Feb19, QPA_L_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Xyphocarys ----------------------------------------------------------------

QPA_X_Feb19 <- read.csv("DensityPlots/QPAFeb19/07 QPA_Xyphocaris_Feb19.csv")

dens <- lapply(split(QPA_X_Feb19, QPA_X_Feb19$source), 
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
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Atya ----------------------------------------------------------------

QPA_A_Feb19 <- read.csv("DensityPlots/QPAFeb19/08 QPA_Atya_Feb19.csv")

dens <- lapply(split(QPA_A_Feb19, QPA_A_Feb19$source), 
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
  ggtitle('M. crenulatun') +
  theme(legend.position = "none")+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))



# Anolis ------------------------------------------------------------------

QPA_An_Feb19 <- read.csv("DensityPlots/QPAFeb19/10 QPA_Anolis_Feb19.csv")

dens <- lapply(split(QPA_An_Feb19, QPA_An_Feb19$source), 
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
  ggtitle('A. evermani') +
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  
  theme(legend.title = element_text(size = 18)) + #title
  theme(legend.text = element_text(size = 16))  + #
  guides(color=guide_legend(override.aes=list(fill=NA)))


Fig2 <- (x1+a1+plot_spacer()) / (g1+b1+c1) /(n1+p1 +l1) / (an1 + plot_spacer()+ plot_spacer())
Fig2
Fig2 + ggsave("Figure 2 QPA Feb19.pdf",width = 210, height = 297, units = "mm")


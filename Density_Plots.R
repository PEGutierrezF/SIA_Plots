



# Density Plots -----------------------------
# 21 Oct 2020
#PEGF
#--------------------------------------------
#

library(ggplot2)
library(dplyr)
library(ggpubr)
QPA_G_Feb17 <- read.csv("DensityPlots/QPA_Glossosomatidae_Feb17.csv")
head(QPA_G_Feb17)


L <- filter(QPA_G_Feb17, source == "Leaflitter")
L <-as.data.frame(L)
L

Lq025  <- quantile(L$density, .025)
Lq975  <- quantile(L$density, .975)
Lq025
Lq975


gF17 <- ggplot(QPA_G_Feb17, aes(x=density, color=source)) + 
  labs(y="Density", x="Sorce contribution") +
  geom_density(aes(linetype = source), size=1.2) +
  scale_color_manual(values=c("#31a354", "#2c7fb8", "#d95f0e")) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  theme_classic()+
  ylim(0, 5)+
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  theme(axis.text.x =element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.title.x = element_text(color="black", size=14))+ # #Axis y
  theme(axis.title.y = element_text(color="black", size=14))  # #Axis x
 
gF17
gF17 + geom_area(data = subset(QPA_G_Feb17, density <= Lq025 & density >= Lq975), 
                   aes(x=density, color=source), fill = 'blue') 





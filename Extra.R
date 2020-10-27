


L <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Leaflitter")
L
quantile(L$density, 0.025)
quantile(L$density, 0.975)

B <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Biofilm")
quantile(B$density, 0.025)
quantile(B$density, 0.975)

A <- QPA_G_Feb17 %>% filter(QPA_G_Feb17$source == "Algae")
quantile(A$density, 0.025)
quantile(A$density, 0.975)



gF17 <- ggplot(QPA_G_Feb17, aes(x=density, color = source)) + 
  labs(y="Density", x="Source contribution") +
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

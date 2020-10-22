

library(ggplot2)
library(ggpubr)
QPA_Feb17 <- read.csv("Biplot/QPA_Feb17.csv")
head(QPA_Feb17)

ggplot(QPA_Feb17, aes(x=C, y=N, group=taxa)) +
  geom_point(aes(color=taxa), size=4) +
  geom_segment(aes(x=-31.91,xend=-32.25,yend=0.56,y=0.56)) + # C leaflitter
  geom_segment(aes(x=-32.08,xend=-32.08,yend=0.878914827,y=0.241085173)) + # N leaflitter
  geom_segment(aes(x=-31.48,xend=-31.90,yend=6.06,y=6.06))+ # C biofilm
  geom_segment(aes(x=-31.69,xend=-31.69,yend=6.389520784,y=5.730479216))+ # N biofilm
  geom_segment(aes(x=-30.79,xend=-31.81,yend=11.37138181,y=11.37138181)) +# C algae
  geom_segment(aes(x=-31.29893075,xend=-31.29893075,yend=17.153228921,y=5.589534699)) +# algae
  xlim(-40,-20)
               





# ---------------------------------------------
# AFDM Prieta A and B
# 28 Jan 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())




libraries <- c( "ggplot2", "plyr","dplyr", 'patchwork','tidyverse')
lapply(libraries, require, character.only = TRUE)

data <- read.csv("data/AFDMAll.csv")
head(data)

# First, rename variables
data$period <- factor(data$period, levels=c('six' ,
                                            'two', 'nine' ,
                                            'eighteen' ))

streams <- as_labeller(c(qpa="Prieta A", qpb="Prieta B"))

a <- ggplot(data, aes(x=period, y=ratio) )+
  geom_boxplot()+
  geom_jitter(width=.1, alpha=.5) +
  
  #Labels  
  labs(x="Sampling period", y= "AFDM value ("*g~m^-2~d^-1*")") +
  scale_x_discrete(labels=c('6mo pre-','2mo post-',
                            '9mo post-', 
                            '18mo post-')) +
  
  # Ticks interval
  scale_y_continuous(breaks = seq(0, 380, by=50), limits=c(0,380)) +
  
#Axis 
  theme_bw() +
  theme(legend.position="none")  +
  
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black",
                                 margin = margin(t = 0, r = 0, b = 10, l = 0))) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) +  #subaxis y
  
  facet_grid(~stream, labeller= streams) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold"))
a

#Ecology format
ggsave(file="Figure X.jpeg", a, height = 8, width = 11, dpi = 600)

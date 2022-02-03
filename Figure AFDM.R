



# ---------------------------------------------
# AFDM Prieta A
# 28 Jan 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())




libraries <- c( "ggplot2", "plyr","dplyr", 'patchwork','tidyverse')
lapply(libraries, require, character.only = TRUE)

data <- read.csv("data/AFDM.csv")
head(data)

# First, rename variables
data$period <- factor(data$period, levels=c('six' ,
                                            'two', 'nine' ,
                                            'eighteen' ))

streams <- as_labeller(c(qpa="Prieta A", qpb="Prieta B"))

ggplot(data, aes(x=period, y=value) )+
  geom_violin()+
  geom_jitter(width=.1, alpha=.5) +
  
  labs(x="Sampling period", y= "AFDM value ("*g~m^-2~d^-1*")") +
  scale_x_discrete(labels=c('6mo pre-','2mo post-',
                            '9mo post-', 
                            "18mo post-")) +
#Axis 
  theme_bw() +
  theme(legend.position="none")  +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) +  #subaxis y

  facet_grid(.~stream, labeller= streams) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold"))



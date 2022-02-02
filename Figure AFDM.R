



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

'six' = "6mo pre-",
'two' = "2mo post-", 'nine' = "9mo post-",
'eighteen' = "18mo post-"

ggplot(data, aes(x=period, y=value) )+
  geom_violin()+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  
  scale_x_discrete(labels=c('6mo pre-','2mo post-','9mo post-', 
                            "18mo post-")) +
  facet_grid(.~stream) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold"))



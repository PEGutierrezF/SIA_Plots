



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

ggplot(data, aes(x=date, y=value) ) + 
  geom_jitter(position=position_jitter(width=0.3, height=0.2), 
              alpha=0.9) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  facet_grid(.~period) +
  theme(strip.text.x = element_text(size=9, color="black", face="bold"))



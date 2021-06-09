



# ---------------------------------------------
# Packages and libraries
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  




# cleans global environment
rm(list = ls())



libraries <- c("ggplot2", "ggpubr","dplyr", 'patchwork')
lapply(libraries, require, character.only = TRUE)

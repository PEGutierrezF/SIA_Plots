



# ---------------------------------------------
# Packages and libraries
# 09 Jun 2021
# Pablo E. Guti�rrez-Fonseca
# ---------------------------------------------
#  




# cleans global environment
rm(list = ls())



libraries <- c("ggplot2", "ggpubr","dplyr", 'patchwork',
               'gridExtra',"cowplot")
lapply(libraries, require, character.only = TRUE)



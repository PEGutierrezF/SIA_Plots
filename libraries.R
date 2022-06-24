



# ---------------------------------------------
# Packages and libraries
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  




# cleans global environment
rm(list = ls())



libraries <- c("ggplot2", "ggpubr","dplyr", 'patchwork',
               'gridExtra',"cowplot",'HDInterval')
lapply(libraries, require, character.only = TRUE)


library(tidyverse)
library(HDInterval)
install.packages('ggridges')
library(ggridges)


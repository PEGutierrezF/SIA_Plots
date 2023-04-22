



# ---------------------------------------------
# Packages and libraries
# 09 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  




# cleans global environment
rm(list = ls())



libraries <- c("ggplot2", "ggpubr","dplyr", 'patchwork',"readxl", 
               'gridExtra',"cowplot",'HDInterval','ggridges')
lapply(libraries, require, character.only = TRUE)


library(tidyverse)
library(HDInterval)
install.packages('extrafont')
library(ggridges)

library(ggthemes)
library(extrafont)
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()

loadfonts(quiet = T)
fonts()

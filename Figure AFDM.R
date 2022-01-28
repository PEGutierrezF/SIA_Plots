



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

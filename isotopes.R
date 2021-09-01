

library(tidyverse)

data <- read.csv("data/isotopes.csv")
head(data)

# First, rename variables
sampling_new <- c(
  'six' = "6mo pre-",
  'two' = "2mo post-",
  '9mo' = "9mo post-",
  '18mo' = "18mo post-")

# Second, reorganize
data$sampling = factor(data$sampling, 
                           levels=c('six','two','9mo','18mo'))
# Rename the isotopes
isotopes_new <- c(
  "Carbon" = "delta^{13}*C",
  "Nitrogen" = "delta^{15}*N")



p <- ggplot(data = data, aes(x = stream, y = value)) +
  geom_point(aes(colour = source), position = position_dodge(width = 1), size= 5) +
  scale_color_manual(values = c("#238b45", "turquoise3", "coral4"), labels = c("Algae", "Biofilm", "Leaf litter") ) +
  
  geom_errorbar(aes(colour = source, ymin = value - sd, ymax = value + sd), 
                width = 0.2, position = position_dodge(width = 1), size= 1) +
  
  labs(x="Streams",y = "Value", colour = "Source") +
  scale_x_discrete(labels = c("Quebrada\nPrieta A", "Quebrada\nPrieta B")) +
  
# Panel   
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

p + facet_grid(isotope ~ sampling, 
               scale= "free_y", labeller = labeller(isotope = as_labeller(isotopes_new,  label_parsed),
                                                    sampling = as_labeller(sampling_new))) 
  














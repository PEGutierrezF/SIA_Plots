



iso <- read.csv("data/isotopes.csv")
head(iso)


isotopes_new <- 
  
  
ggplot(data = iso, aes(x = stream  , y = value, colour = source   )) +
  geom_point(aes(color=source),position = position_dodge(width = 1)) +
  scale_color_manual(values = c(1, 16, 19), labels = c("Algae", "Biofilm", "Leaf litter") ) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd), 
                width = .2, position = position_dodge(width = 1))


+
    facet_grid(~samplin)

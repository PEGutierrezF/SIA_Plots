


QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)


ggplot(QPA_G_Nov17, aes(x = density, color = source, 
                        fill = after_stat(ifelse(quantile == 2, NA, color)))) +
  geom_density_ridges_gradient(aes(y = 0), quantile_lines = TRUE, size=1.5, 
                               quantile_fun = hdi, vline_linetype = 0,
                               bandwidth = 0.1) +
  labs(y = "Density", x = "Source contribution") +
  
#  scale_fill_discrete(guide = "none", na.value = "transparent") 
  
  scale_fill_cyclical(name = "", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                    labels = c("Algae", "Biofilm", "Leaf litter"),
                    guide = "none", na.value = "transparent") +
  
  scale_color_cyclical(name = "", values = c("#31a354", "#2c7fb8", "#d95f0e"),
                      labels = c("Algae", "Biofilm", "Leaf litter"),
                      guide = "none", na.value = "transparent") +

  theme_classic() +
  ylim(0, 8) +
  xlim(0, 1) +
  ggtitle('Glossosomatidae') +
  theme(plot.title = element_text(face="bold"))+
  
 # theme(legend.position=c(.9,.75))+
  theme(axis.text.y  = element_text(size = 12, vjust = 0.5),
        axis.text.x  = element_text(size = 12, vjust = 0.5),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#


unique(QPA_G_Nov17$source)










QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)

ggplot(QPA_G_Nov17, aes(x = density, color = source)) +
  geom_density(size=1.5, alpha = 0.4) +
  labs(y = "Density", x = "Source contribution")

dens <- lapply(split(QPA_G_Nov17, QPA_G_Nov17$source), 
               function(x) density(x$density, from = 0, to = 1))

df <- do.call(rbind, mapply(function(x, y) {
  data.frame(x = x$x, y = x$y, source = y)
}, dens, names(dens), SIMPLIFY = FALSE))

df <- df %>% group_by(source) %>%
  mutate(cdf = cumsum(y * mean(diff(x))),
         lower = cdf < 0.025,
         upper = cdf > 0.975)

L_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Leaflitter")
B_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Biofilm")
A_G_QPA <- QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Algae")

data_lower <- tribble(~source, ~value,  ~hdi,
                      'L_G_QPA_l' , 0.025, hdi(L_G_QPA$density)["lower"],
                      'B_G_QPA_l' , 0.025, hdi(B_G_QPA$density)["lower"],
                      'A_G_QPA_l' , 0.025, hdi(A_G_QPA$density)["lower"])

data_upper <- tribble(~source, ~value, ~hdi,
                      'L_G_QPA_u', 0.975, hdi(L_G_QPA$density)["upper"],
                      'B_G_QPA_u', 0.975, hdi(B_G_QPA$density)["upper"],
                      'A_G_QPA_u', 0.975, hdi(A_G_QPA$density)["upper"])

value <- hdi(L_G_QPA$density)["upper"]
value. <- hdi(B_G_QPA$density)["upper"]


ggplot(df, aes(x = density, color = source)) +
  geom_density(alpha = 0.4) +
  geom_area(data=QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Biofilm"),
            stat = "function", fun = dnorm, 
            fill = "#00998a", xlim = c(0.5646074, 1))
  


set.seed(14)
df <- data.frame(density = c(rgamma(400, 2, 10), rgamma(400, 2.25, 9),rgamma(400, 5, 7)),
                 source = rep(c("source_1", "source_2", "source_3"), 
                              each = 400))

ggplot(df, aes(x = density, color = source)) +
  geom_density(size=1.5, alpha = 0.4) +
  labs(y = "Density", x = "Source contribution")


S1 <- df %>% filter(df$source == "source_1")
S2 <- df %>% filter(df$source == "source_2")
S3 <- df %>% filter(df$source == "source_3")

data_lower <- tribble(~source, ~value,  ~hdi,
                      'S1' , 0.025, hdi(S1$density)["lower"],
                      'S2' , 0.025, hdi(S2$density)["lower"],
                      'S3' , 0.025, hdi(S3$density)["lower"])

data_upper <- tribble(~source, ~value, ~hdi,
                      's1', 0.975, hdi(S1$density)["upper"],
                      's2', 0.975, hdi(S2$density)["upper"],
                      's3', 0.975, hdi(S3$density)["upper"])

hdi(S1$density, ci = 0.95)
hdi(S2$density, ci = 0.95)
hdi(S3$density, ci = 0.95)

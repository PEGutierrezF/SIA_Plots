


QPA_G_Nov17 <- read.csv("DensityPlots/QPANov17/01 QPA_Glossosomatidae_Nov17.csv")
head(QPA_G_Nov17)


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


ggplot(QPA_G_Nov17, aes(x = density, color = source)) +
  geom_density( alpha = 0.4) +
  geom_area(data=QPA_G_Nov17 %>% filter(QPA_G_Nov17$source == "Biofilm"),
            stat = "function", fun = dnorm, 
            fill = "#00998a", xlim = c(0.5646074, 1))
  









# ---------------------------------------------
# Figure 1
# 19 Aug 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

data <- read.csv("data/data.csv")
head(data)

data$date <-as.POSIXct(data$date,"%Y-%m-%d",tz = "UTC")

d <- ggplot(data, aes(x=date, y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  labs(x = "", y= "Water level (m)") +
  theme_classic()
  
d + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
           ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)

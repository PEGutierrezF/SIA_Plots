



# ---------------------------------------------
# Figure 1
# 19 Aug 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

libraries <- c( "ggplot2", "plyr","dplyr", 'patchwork','tidyverse')
lapply(libraries, require, character.only = TRUE)

data <- read.csv("data/data.csv")
head(data)

data$date <-as.POSIXct(data$date,"%Y-%m-%d",tz = "UTC")


# Discharge ---------------------------------------------------------------

discharge <- slice(data, (1:1763))
discharge$date <-as.POSIXct(discharge$date,"%Y-%m-%d",tz = "UTC")

 d +  ggplot(discharge, aes(x=date, y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  labs(x = "", y= "Water level (m)") +
  theme_classic() 
  
 d1 <- d + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
           ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)
 d1

# Canopy cover ------------------------------------------------------------

data <- read.csv("data/data.csv")
canopy <- slice(data, (1763:1814))
canopy$date <-as.POSIXct(canopy$date,"%Y-%m-%d",tz = "UTC")
head(canopy)
tail(canopy)

c <- ggplot(canopy, aes(x=date,y=value, colour=stream)) +
  labs(x= '', y= 'Canopy cover (%)') +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se), na.rm=TRUE, position="dodge",
                colour = "black") +
  theme_classic()
c  

c + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)


# Leaf litter -------------------------------------------------------------

data <- read.csv("data/data.csv")
leaf <- slice(data, (1815:1930))
leaf$date <-as.POSIXct(leaf$date,"%Y-%m-%d",tz = "UTC")
head(leaf)
tail(leaf)


 l <- ggplot(leaf, aes(x=date,y=value, colour=stream)) +
  labs(x= '', y= 'Canopy cover (%)') +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE, position="dodge",
                colour = "black") +
  theme_classic()
l
 l1 <- l + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)
l1

"Mean litter input rate ("*g~m^-2~d^-1*")"


 (d1 + plot_spacer()) / (c + c) /(c + c) 
''




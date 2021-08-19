



# ---------------------------------------------
# Figure 1
# 19 Aug 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

libraries <- c( "ggplot2", "plyr","dplyr", 'patchwork','tidyverse')
lapply(libraries, require, character.only = TRUE)


# Discharge ---------------------------------------------------------------

data <- read.csv("data/data.csv")
discharge <- slice(data, (1:1762))
discharge$date <-as.POSIXct(discharge$date,"%Y-%m-%d",tz = "UTC")
head(dischage)
tail(discharge)

 d <-  ggplot(discharge, aes(x=date, y=value, colour=stream)) +
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

canopy$se = as.numeric(canopy$se)

c <- ggplot(canopy, aes(x=date,y=value, colour=stream)) +
  labs(x= '', y= 'Canopy cover (%)') +
  geom_line(stat="identity",position='dodge',size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se), na.rm=TRUE, 
                width = 0.2, position = position_dodge(0.9),
                colour = "black") +
  theme_classic()

c1 <- c + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)


# Leaf litter -------------------------------------------------------------

data <- read.csv("data/data.csv")
leaf <- slice(data, (1815:1930))
leaf$date <-as.POSIXct(leaf$date,"%Y-%m-%d",tz = "UTC")
head(leaf)
tail(leaf)

leaf$se = as.numeric(leaf$se)

 l <- ggplot(leaf, aes(x=date,y=value, colour=stream)) +
  labs(x= '', y= "Mean litter input rate ("*g~m^-2~d^-1*")") +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE, position="dodge",
                colour = "black") +
  theme_classic()

 l1 <- l + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)
l1


# Chlorophyl -a  -------------------------------------------------------------

data <- read.csv("data/data.csv")
chla <- slice(data, (1931:1990))
chla$date <-as.POSIXct(chla$date,"%Y-%m-%d",tz = "UTC")
head(chla)
tail(chla)

chla$se = as.numeric(chla$se)

ch <- ggplot(chla, aes(x=date,y=value, colour=stream)) +
  labs(x= '', y= "Mean chlorophyll-a ("*g~m^-2*")") +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE, position="dodge",
                colour = "black") +
  theme_classic()

ch1 <- ch + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
                   ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)
ch1



Fig1 <- (d1 + plot_spacer()) / (c1 + l1) /(ch1 + c1) 
Fig1

Fig1 + ggsave("Figure X Density plot QPB Nov 17.tiff",width = 210, height = 250, units = "mm")





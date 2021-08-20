



# ---------------------------------------------
# Figure 1
# 19 Aug 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

rm(list=ls(all=TRUE)) #give R a blank slate

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
  labs(x = "", y= "Water level (m)", color='Stream') +
  theme_classic()  +
   
   theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
 d
 d1 <- d + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
           ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
   geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 0.45, xend = as.POSIXct(c("2017-02-01")), yend = 0.42), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +

   geom_segment(aes(x = as.POSIXct(c("2017-11-01")), y = 0.45, xend = as.POSIXct(c("2017-11-01")), yend = 0.42), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 0.45, xend = as.POSIXct(c("2018-06-01")), yend = 0.42), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
 
   geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 0.45, xend = as.POSIXct(c("2019-02-01")), yend = 0.42), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")
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
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se), na.rm=TRUE, 
                position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                colour = "black") +
  theme_classic()+
  theme(legend.position="none")  +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

c1 <- c + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
  
  geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 77, xend = as.POSIXct(c("2017-02-01")), yend = 69), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2017-11-30")), y = 77, xend = as.POSIXct(c("2017-11-30")), yend = 69), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 77, xend = as.POSIXct(c("2018-06-01")), yend = 69), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 77, xend = as.POSIXct(c("2019-02-01")), yend = 69), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")

c1

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
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE, 
                position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                colour = "black") +
  theme_classic() +
   theme(legend.position="none")  +
   
   theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

 l1 <- l + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
   
   geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 7.7, xend = as.POSIXct(c("2017-02-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2017-11-15")), y = 7.7, xend = as.POSIXct(c("2017-11-15")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 7.7, xend = as.POSIXct(c("2018-06-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 7.7, xend = as.POSIXct(c("2019-02-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")
 
l1


# Chlorophyl -a  -------------------------------------------------------------

data <- read.csv("data/data.csv")
chla <- slice(data, (1931:1990))
chla$date <-as.POSIXct(chla$date,"%Y-%m-%d",tz = "UTC")
head(chla)
tail(chla)

chla$se = as.numeric(chla$se)

ch <- ggplot(chla, aes(x=date,y=value, colour=stream)) +
  xlab('Year') + ylab(expression(paste("Chlorophyll-", italic("a") , ~"("*"\u03BC"~g~m^-2*")"))) +
           #("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE, 
                position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                colour = "black") +
  theme_classic() +
  theme(legend.position="none")  +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

ch1 <- ch + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
                   ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)  +
  
  geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 1125, xend = as.POSIXct(c("2017-02-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2017-11-15")), y = 1125, xend = as.POSIXct(c("2017-11-15")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 1125, xend = as.POSIXct(c("2018-06-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 1125, xend = as.POSIXct(c("2019-02-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")

ch1



# BOM ---------------------------------------------------------------------

data <- read.csv("data/data.csv")
BOM <- slice(data, (1991:2050))
BOM$date <-as.POSIXct(BOM$date,"%Y-%m-%d",tz = "UTC")
head(BOM)
tail(BOM)

BOM$se = as.numeric(BOM$se)

b <- ggplot(BOM, aes(x=date,y=value, colour=stream)) +
  xlab('Year') + ylab("Benthic organic matter ("*g~m^-2*")") +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+se, ymin=value-se),na.rm=TRUE,
                position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                colour = "black") + 
  theme_classic() +
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
  

b
b1 <- b + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
                     ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)  +
  
  geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 1125, xend = as.POSIXct(c("2017-02-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2017-11-15")), y = 1125, xend = as.POSIXct(c("2017-11-15")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 1125, xend = as.POSIXct(c("2018-06-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 1125, xend = as.POSIXct(c("2019-02-01")), yend = 975), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")

b1


Fig1 <- (d1 + plot_spacer()) / (c1 + l1) /(ch1 + b1) 
Fig1

Fig1 + ggsave("Figure X Density plot QPB Nov 17.tiff",width = 200, height = 220, units = "mm")





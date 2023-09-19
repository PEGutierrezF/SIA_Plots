



# ---------------------------------------------
# Figure 1
# 19 Aug 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())


# Read as excel .xlsx file
data <- read_excel("data/physicochemical_data_2017_2019.xlsx", 
                   sheet = "environmental_var", na = '')



# Discharge ---------------------------------------------------------------

data <- read.csv("data/physicochemical_data.csv")
discharge <- data[1:1762,]

# Convert the 'value' column to numeric
#data$value <- as.numeric(data$value)
discharge$date <-as.POSIXct(discharge$date,"%Y-%m-%d",tz = "UTC")
head(discharge)
tail(discharge)


 d <-  ggplot(discharge, aes(x=date, y=value, color = factor(stream, labels = c("Prieta A", "Prieta B")))) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
   
# Number of digits
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +

# Labels 
  labs(x = "", y= "Water level (m)", color='Stream') +
  labs(tag = "A") +
 
   theme_bw()  +
   theme(legend.position="none") +
#Quite la leyenda  
 # theme(legend.key.size = unit(0.6, "cm"))+
  # theme(legend.title=element_text(size=14)) + # legend title size
  # theme(legend.text = element_text(color = "black", size = 12))+  #factor name 
   
   theme(axis.title.x = element_blank()) + # axis x
   # theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.x=element_blank())  #subaxis x
   #theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
 
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

data <- read.csv("data/physicochemical_data.csv")
canopy <- slice(data, (1763:1814))
canopy$date <-as.POSIXct(canopy$date,"%Y-%m-%d",tz = "UTC")
head(canopy)
tail(canopy)

canopy$sd = as.numeric(canopy$sd)
class(canopy$sd)
c <- ggplot(canopy, aes(x=date,y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd), na.rm=TRUE, 
                #position = position_dodge(width = 0.9),stat = "identity", 
                width = 0, colour = "gray50") +
# Labels
  labs(x= '', y= 'Canopy openness (%)') +
  labs(tag = "B") +
  
  theme_bw()+
  theme(legend.position="none")  +
  ylim(0,100)+
#Axis  
  theme(axis.title.x = element_blank()) + # axis x
  # theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.x=element_blank())  #subaxis x
#theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y

c

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

data <- read.csv("data/physicochemical_data.csv")
leaf <- slice(data, (1815:1930))
leaf$date <-as.POSIXct(leaf$date,"%Y-%m-%d",tz = "UTC")
head(leaf)
tail(leaf)

leaf$sd = as.numeric(leaf$sd)

 l <- ggplot(leaf, aes(x=date,y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+

# Number of digits
   scale_y_continuous(labels = scales::number_format(accuracy = 1),
                      breaks = seq(0, 13, by=3), limits=c(0,13)) +

#Labels
   labs(x= '', y= "Litterfall input rate ("*g~m^-2~d^-1*")") + #  ("*g~m^-2~d^-1*")
   labs(tag = "D") + 
   
  geom_point() +
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd),na.rm=TRUE, 
               # position = position_dodge(width = 0.9),stat = "identity", 
                width = 0, colour = "gray50") +
   theme_bw() +
   theme(legend.position="none")  +
   
   theme(axis.title.x = element_blank()) + # axis x
   # theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
   theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
   theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
   theme(axis.text.x=element_blank())  #subaxis x
 #theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
 
 
 l
 l1 <- l + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
             ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5) +
   
   geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 8, xend = as.POSIXct(c("2017-02-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2017-11-15")), y = 8, xend = as.POSIXct(c("2017-11-15")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 8, xend = as.POSIXct(c("2018-06-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
   
   geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 8, xend = as.POSIXct(c("2019-02-01")), yend = 6.9), 
                arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")
 
l1


# Chlorophyl -a  -------------------------------------------------------------

chla <- slice(data, (1931:1990))
chla$date <-as.POSIXct(chla$date,"%Y-%m-%d",tz = "UTC")
head(chla)
tail(chla)

chla$sd = as.numeric(chla$sd)

ch <- ggplot(chla, aes(x=date,y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd),na.rm=TRUE, 
               # position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                width = 0, colour = "gray50") +
# Labels 
  xlab('Year') + ylab(expression(paste("Chlorophyll-", ~italic("a") , ~"("*mg~m^-2*")"))) +
  #("Chlorophyll-a ("*"\u03BC"~g~m^-2*")") +
  labs(tag = "C")+
  
  theme_bw() +
  theme(legend.position="none")  +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))  #subaxis y
ch 

ch1 <- ch + annotate("rect", xmin = as.POSIXct("2017-09-6"), xmax = as.POSIXct("2017-09-21"), 
                   ymin = -Inf, ymax = Inf,  fill = "#df65b0", alpha=.5)  +
  
  geom_segment(aes(x = as.POSIXct(c("2017-02-01")), y = 1275, xend = as.POSIXct(c("2017-02-01")), yend = 1150), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2017-11-15")), y = 1275, xend = as.POSIXct(c("2017-11-15")), yend = 1150), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2018-06-01")), y = 1275, xend = as.POSIXct(c("2018-06-01")), yend = 1150), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black") +
  
  geom_segment(aes(x = as.POSIXct(c("2019-02-01")), y = 1275, xend = as.POSIXct(c("2019-02-01")), yend = 1150), 
               arrow = arrow(length = unit(0.2, "cm")),size = 1, color = "black")

ch1



# BOM ---------------------------------------------------------------------

# data <- read.csv("data/physicochemical_data.csv")
BOM <- slice(data, (1991:2050))
BOM$date <-as.POSIXct(BOM$date,"%Y-%m-%d",tz = "UTC")
head(BOM)
tail(BOM)

BOM$sd = as.numeric(BOM$sd)

b <- ggplot(BOM, aes(x=date,y=value, colour=stream)) +
  geom_line(size=0.8) + 
  scale_color_manual(values=c('#ce1256','#0570b0'))+
  geom_point() +
  geom_errorbar(aes(ymax=value+sd, ymin=value-sd),na.rm=TRUE,
              #  position = position_dodge(width = 0.9),stat = "identity", #width = 0.2,
                width = 0, colour = "gray50") + 
#Labels
  xlab('Year') + ylab("Benthic organic matter ("*g~m^-2*")") +
  labs(tag = "E")+
  
  theme_bw() +
  theme(legend.position="none") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  

  
# Assuming 'data' is your full dataset
# Convert 'date' column to the Date class
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Filter data for the desired range (1991:2050)
BOM <- data %>%
  filter(year(date) >= 1991, year(date) <= 2050)

# Create a ggplot
b <- ggplot(BOM, aes(x = date, y = value, colour = stream)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c('#ce1256', '#0570b0')) +
  geom_point() +
  geom_errorbar(aes(ymax = value + sd, ymin = value - sd), na.rm = TRUE,
                width = 0, colour = "gray50") +
  # Labels
  xlab('Year') + ylab("Benthic organic matter (" * g ~ m^-2 * ")") +
  labs(tag = "E") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x = element_text(angle = 0, size = 10, vjust = 0.5, color = "black")) + # subaxis x
  theme(axis.text.y = element_text(angle = 0, size = 10, vjust = 0.5, color = "black"))  # subaxis y +

  

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



# Plot 1 ------------------------------------------------------------------

# Create your individual plots with tags
d1_tagged <- d1 + labs(title = "(a)") +
  theme(legend.position = c(0.25, 0.6)) +
  theme(legend.key.size = unit(0.6, "cm")) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(color = "black", size = 12))


# Extract the common legend
legend <- g_legend(d1_tagged)


Fig1 <- d1 + legend + c1 + l1 + ch1 + b1 
Fig1 <- Fig1 + plot_annotation(tag_levels = list(c('a)','', 'b)','c)','d)','e)')))
Fig1 <- Fig1 + plot_layout(nrow = 3)
Fig1

Fig1 + ggsave("Figure 1.tiff",width = 18, height = 22, units = "cm", dpi = 600)


# Plot 2 ------------------------------------------------------------------

## Function to extract legend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

library(gridExtra)
library(lemon)
legend <- g_legend(d1+theme(legend.position = c(0.25, 0.6)) +
                      theme(legend.key.size = unit(0.6, "cm"))+
                      theme(legend.title=element_text(size=14)) + # legend title size
                      theme(legend.text = element_text(color = "black", size = 12)))

Fig <-grid.arrange(d1 + theme(legend.position='hidden'), legend,
             c1 + theme(legend.position='hidden'), l1 + theme(legend.position='hidden'),
             ch1 + theme(legend.position='hidden'), b1 + theme(legend.position='hidden'),
             nrow=3)

#Ecology format
ggsave(file="Figure 1.tiff", Fig, width = 18, height = 24, units = "cm", dpi = 600)
 







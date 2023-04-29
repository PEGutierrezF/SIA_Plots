



# ---------------------------------------------
# Biplots for Quebrada Prieta A with Polygons
# 29 Apr 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



QPB_data <- "Biplot/QPB_cosumers_iso_signature.xlsx"
excel_sheets(path = QPB_data)




# QPA February 17  --------------------------------------------------------
qpb_Feb17 <- read_excel(path = QPB_data, sheet = "QPB_Feb17")  
qpb_Feb17 <- qpb_Feb17 %>% select(taxa,mean_C,sd_C,mean_N,sd_N) %>% na.omit()

qpb_Feb17$taxa <- factor(qpb_Feb17$taxa, 
                         levels = c("Glossosomatidae", "Baetidae", "Chironomidae", "N. julio",
                                    "P. pulchrus", "Libellulidae", "X. elongata", "A. lanipes",
                                    "M. crenulatum", "A. evermani","L. regnyi"))
levels(qpb_Feb17$taxa) 






install.packages("HDInterval")
library(HDInterval)
library(tidyr)

sources_cor <- rstan::extract(QPCA)
head(sources_cor) 

sources_QPA <- data.frame (
# February 2017 -----------------------------------------------------------
  February_2017 = c(
# Algae
    mean(sources_cor$d13C_A[,1]),
    sd(sources_cor$d13C_A[,1]),
    quantile(sources_cor$d13C_A[,1], 0.025),
    quantile(sources_cor$d13C_A[,1], 0.975),
    hdi(sources_cor$d13C_A[,1], credMass=0.8),
    
    mean(sources_cor$d15N_A[,1]),
    sd(sources_cor$d15N_A[,1]),
    quantile(sources_cor$d15N_A[,1], 0.025),
    quantile(sources_cor$d15N_A[,1], 0.975),
    hdi(sources_cor$d15N_A[,1], credMass=0.95),
    
    # Periphyton    
    mean(sources$delta13C_P[sources$Use=="1"]), 
    sd(sources$delta13C_P[sources$Use=="1"]),
    quantile(sources$delta13C_P[sources$Use=="1"], 0.025),
    quantile(sources$delta13C_P[sources$Use=="1"], 0.975),
    hdi(sources$delta13C_P[sources$Use=="1"], credMass=0.95),
    
    mean(sources$delta15N_P[sources$Use=="1"]), 
    sd(sources$delta15N_P[sources$Use=="1"]), 
    quantile(sources$delta15N_P[sources$Use=="1"], 0.025),
    quantile(sources$delta15N_P[sources$Use=="1"], 0.975),
    hdi(sources$delta15N_P[sources$Use=="1"], credMass=0.95),
    
    # Terrestrail    
    mean(sources$delta13C_T[sources$Use=="1"]), 
    sd(sources$delta13C_T[sources$Use=="1"]),
    quantile(sources$delta13C_T[sources$Use=="1"], 0.025),
    quantile(sources$delta13C_T[sources$Use=="1"], 0.975),
    hdi(sources$delta13C_T[sources$Use=="1"], credMass=0.95),
    
    mean(sources$delta15N_T[sources$Use=="1"]), 
    sd(sources$delta15N_T[sources$Use=="1"]),
    quantile(sources$delta15N_T[sources$Use=="1"], 0.025),
    quantile(sources$delta15N_T[sources$Use=="1"], 0.975),
    hdi(sources$delta15N_T[sources$Use=="1"], credMass=0.95)
),  


# November 2017 -----------------------------------------------------------
November_2017 = c(
  # Algae
  mean(sources_cor$d13C_A[,2]),
  sd(sources_cor$d13C_A[,2]),
  quantile(sources_cor$d13C_A[,2], 0.025),
  quantile(sources_cor$d13C_A[,2], 0.975),
  hdi(sources_cor$d13C_A[,2], credMass=0.8),
  
  mean(sources_cor$d15N_A[,2]),
  sd(sources_cor$d15N_A[,2]),
  quantile(sources_cor$d15N_A[,2], 0.025),
  quantile(sources_cor$d15N_A[,2], 0.975),
  hdi(sources_cor$d15N_A[,2], credMass=0.95),
  
  # Periphyton    
  mean(sources$delta13C_P[sources$Use=="2"]), 
  sd(sources$delta13C_P[sources$Use=="2"]),
  quantile(sources$delta13C_P[sources$Use=="2"], 0.025),
  quantile(sources$delta13C_P[sources$Use=="2"], 0.975),
  hdi(sources$delta13C_P[sources$Use=="2"], credMass=0.95),
  
  mean(sources$delta15N_P[sources$Use=="2"]), 
  sd(sources$delta15N_P[sources$Use=="2"]), 
  quantile(sources$delta15N_P[sources$Use=="2"], 0.025),
  quantile(sources$delta15N_P[sources$Use=="2"], 0.975),
  hdi(sources$delta15N_P[sources$Use=="2"], credMass=0.95),
  
  # Terrestrail    
  mean(sources$delta13C_T[sources$Use=="2"]), 
  sd(sources$delta13C_T[sources$Use=="2"]),
  quantile(sources$delta13C_T[sources$Use=="2"], 0.025),
  quantile(sources$delta13C_T[sources$Use=="2"], 0.975),
  hdi(sources$delta13C_T[sources$Use=="2"], credMass=0.95),
  
  mean(sources$delta15N_T[sources$Use=="2"]), 
  sd(sources$delta15N_T[sources$Use=="2"]),
  quantile(sources$delta15N_T[sources$Use=="2"], 0.025),
  quantile(sources$delta15N_T[sources$Use=="2"], 0.975),
  hdi(sources$delta15N_T[sources$Use=="2"], credMass=0.95)
  ),

# June 2018 ---------------------------------------------------------------
June_2018 = c(
  # Algae
  mean(sources_cor$d13C_A[,3]),
  sd(sources_cor$d13C_A[,3]),
  quantile(sources_cor$d13C_A[,3], 0.025),
  quantile(sources_cor$d13C_A[,3], 0.975),
  hdi(sources_cor$d13C_A[,3], credMass=0.8),
  
  mean(sources_cor$d15N_A[,3]),
  sd(sources_cor$d15N_A[,3]),
  quantile(sources_cor$d15N_A[,3], 0.025),
  quantile(sources_cor$d15N_A[,3], 0.975),
  hdi(sources_cor$d15N_A[,3], credMass=0.95),
  
  # Periphyton    
  mean(sources$delta13C_P[sources$Use=="3"]), 
  sd(sources$delta13C_P[sources$Use=="3"]),
  quantile(sources$delta13C_P[sources$Use=="3"], 0.025),
  quantile(sources$delta13C_P[sources$Use=="3"], 0.975),
  hdi(sources$delta13C_P[sources$Use=="3"], credMass=0.95),
  
  mean(sources$delta15N_P[sources$Use=="3"]), 
  sd(sources$delta15N_P[sources$Use=="3"]), 
  quantile(sources$delta15N_P[sources$Use=="3"], 0.025),
  quantile(sources$delta15N_P[sources$Use=="3"], 0.975),
  hdi(sources$delta15N_P[sources$Use=="3"], credMass=0.95),
  
  # Terrestrail    
  mean(sources$delta13C_T[sources$Use=="3"]), 
  sd(sources$delta13C_T[sources$Use=="3"]),
  quantile(sources$delta13C_T[sources$Use=="3"], 0.025),
  quantile(sources$delta13C_T[sources$Use=="3"], 0.975),
  hdi(sources$delta13C_T[sources$Use=="3"], credMass=0.95),
  
  mean(sources$delta15N_T[sources$Use=="3"]), 
  sd(sources$delta15N_T[sources$Use=="3"]),
  quantile(sources$delta15N_T[sources$Use=="3"], 0.025),
  quantile(sources$delta15N_T[sources$Use=="3"], 0.975),
  hdi(sources$delta15N_T[sources$Use=="3"], credMass=0.95)
  ),

# February 2019 -----------------------------------------------------------
February_2019= c(
  # Algae
  mean(sources_cor$d13C_A[,4]),
  sd(sources_cor$d13C_A[,4]),
  quantile(sources_cor$d13C_A[,4], 0.025),
  quantile(sources_cor$d13C_A[,4], 0.975),
  hdi(sources_cor$d13C_A[,4], credMass=0.8),
  
  mean(sources_cor$d15N_A[,4]),
  sd(sources_cor$d15N_A[,4]),
  quantile(sources_cor$d15N_A[,4], 0.025),
  quantile(sources_cor$d15N_A[,4], 0.975),
  hdi(sources_cor$d15N_A[,4], credMass=0.95),
  
  # Periphyton    
  mean(sources$delta13C_P[sources$Use=="4"]), 
  sd(sources$delta13C_P[sources$Use=="4"]),
  quantile(sources$delta13C_P[sources$Use=="4"], 0.025),
  quantile(sources$delta13C_P[sources$Use=="4"], 0.975),
  hdi(sources$delta13C_P[sources$Use=="4"], credMass=0.95),
  
  mean(sources$delta15N_P[sources$Use=="4"]), 
  sd(sources$delta15N_P[sources$Use=="4"]), 
  quantile(sources$delta15N_P[sources$Use=="4"], 0.025),
  quantile(sources$delta15N_P[sources$Use=="4"], 0.975),
  hdi(sources$delta15N_P[sources$Use=="4"], credMass=0.95),
  
  # Terrestrail    
  mean(sources$delta13C_T[sources$Use=="4"]), 
  sd(sources$delta13C_T[sources$Use=="4"]),
  quantile(sources$delta13C_T[sources$Use=="4"], 0.025),
  quantile(sources$delta13C_T[sources$Use=="4"], 0.975),
  hdi(sources$delta13C_T[sources$Use=="4"], credMass=0.95),
  
  mean(sources$delta15N_T[sources$Use=="4"]), 
  sd(sources$delta15N_T[sources$Use=="4"]),
  quantile(sources$delta15N_T[sources$Use=="4"], 0.025),
  quantile(sources$delta15N_T[sources$Use=="4"], 0.975),
  hdi(sources$delta15N_T[sources$Use=="4"], credMass=0.95)
)
)

sources_QPA

rownames(sources_QPA) <- c("d13C_A","sd_d13C_A","2.5%_d13C_A", "97.5%_d13C_A","hdi_d13C_A_lower", "hdi_d13C_A_upper", 
                           "d15N_A", "sd_d15N_A","2.5%_d15N_A","97.5%_d15N_A","hdi_d15N_A_lower","hdi_d15N_A_upper",
                           
                           "d13C_P","sd_d13C_P", "2.5%_d13C_P","97.5%_d13C_P", "hdi_d13C_P_lower", "hdi_d13C_P_upper", 
                           "d15N_P", "sd_d15N_P", "2.5%_d15N_P","97.5%_d15N_P", "hdi_d15N_P_lower","hdi_d15N_P_upper",
                           
                           "d13C_T", "sd_d13C_T", "2.5%_d13C_T", "97.5%_d13C_T", "hdi_d13C_T_lower", "hdi_d13C_T_upper",
                           "d15N_T", "sd_d15N_T", "2.5%_d15N_T", "97.5%_d15N_T", "hdi_d15N_T_lower", "hdi_d15N_T_upper")

sources_QPA <-as.data.frame(t(as.matrix(sources_QPA)))
print(sources_QPA, digits=4)

write.csv(sources_QPA, file= "D:/LTER/Manuscript 2019 Stable Isotopes/SIA_Plots 2017-2019/data/Sources_QPA_results.csv") ## export as csv for further results

getwd()

                           

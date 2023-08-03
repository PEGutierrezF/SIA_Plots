#--------------------------------------------
# Algae contribution from Biofilm -  Terrestrial sources
# PEGF
#--------------------------------------------

# Usage:
# Rscript --vanilla QPA_Algae.R Stan_QPA/QPA_Feb.csv Stan_QPA/sources_QPA_Feb.csv QPA_FoodWeb.stan

#Loading required packages
library(ggplot2)
library(plyr)
library(StanHeaders)
library(rstan)
rstan_options(auto_write = TRUE) ## To avoid recompilation of unchanged Stan programs
options(mc.cores = parallel::detectCores()) ## Le dice que use los nucleos disponibles para el analisis
Sys.setenv(LOCAL_CPPFLAGS = '-march=native') ## Rstan recommend this for improved execution time  

args <- commandArgs(trailingOnly=TRUE)

df <- read.csv(args[1], header = TRUE, encoding = "UTF-16")

srcs_QPA <- read.csv(args[2], header = TRUE, encoding = "UTF-16")

codes <- unique(df$Code)

for( code_i in codes){
	current_code <- df[df$Code==code_i, ]

	QPAlist <- list(d13C_ind=current_code$d13C_ind, d15N_ind=current_code$d15N_ind,
		N = length(current_code$d13C_ind), src_no = 3,
        src_C = srcs_QPA$meand13C, sd_src_C=srcs_QPA$SDd13C, 
        src_N = srcs_QPA$meand15N, sd_src_N = srcs_QPA$SDd15N
        )
	
	QPA_FW <- rstan::stan(file=args[3], data= QPAlist, warmup=198000,
        chains=4, iter=200000, cores=24, control=list(adapt_delta=0.999, max_treedepth=12)
        )    
    
	sink(file=paste0(code_i, "_summary.txt"))
		show(QPA_FW)
	sink()

	posterior_dist <- extract(QPA_FW)

	write.csv(posterior_dist$Theta[,1], file=paste0(code_i, "_theta_1_posterior.csv"), quote=F, row.names=F)
	write.csv(posterior_dist$Theta[,2], file=paste0(code_i, "_theta_2_posterior.csv"), quote=F, row.names=F)
	write.csv(posterior_dist$Theta[,3], file=paste0(code_i, "_theta_3_posterior.csv"), quote=F, row.names=F)
	
	pdf(file=paste0(code_i, "_Theta[1]_posterior.pdf"))
		plot(density(posterior_dist$Theta[,1]))
	dev.off()

	pdf(file=paste0(code_i, "_Theta[2]_posterior.pdf"))
		plot(density(posterior_dist$Theta[,2]))
	dev.off()

	pdf(file=paste0(code_i, "_Theta[3]_posterior.pdf"))
		plot(density(posterior_dist$Theta[,3]))
	dev.off()

	traceplot(QPA_FW)
	ggsave(paste0(code_i, "_traces.pdf"))
}
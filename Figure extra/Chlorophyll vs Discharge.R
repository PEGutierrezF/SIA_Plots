



# ---------------------------------------------
# Correlation between Discharge and Chlorophyll
# This correlation was made to address a comment from the Reviewer and Editor.
# 30 Jul 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  







# cleans global environment
rm(list = ls())


# Read as excel .xlsx file
data <- read_excel("data/physicochemical_data_2017_2019.xlsx", 
                   sheet = "environmental_var", na = '')



# Prieta A: Chlorophyll vs Discharge --------------------------------------
discharge <- data[1:1762,] # Select Discharge
discharge <- discharge %>% # Select Prieta A
  filter(stream=="QPA")

chla_QPA <- slice(data, (1931:1960)) # Select Chlorophyll

merged_df <- merge(chla_QPA, discharge, by = "date") # Merge both dataframe by date
head(merged_df)

# Correlation
merged_df$value.x <- as.numeric(as.character(merged_df$value.x))
shapiro.test(merged_df$value.x) # Normal

merged_df$value.y <- as.numeric(as.character(merged_df$value.y))
shapiro.test(merged_df$value.y) # Non-normal

merged_df <- na.omit(merged_df)
# Calculate Spearman's rank correlation
correlation_value_xy <- cor(merged_df$value.x, merged_df$value.y, method = "spearman")


print(paste("Correlation (rho):", correlation_value_xy))
print(paste("P-value:", correlation_value_xy))

library("ggpubr")
ggscatter(merged_df, x = "value.y", y = "value.x", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Discharge", ylab = "Chlorophyll-a")



# Prieta B:  Chlorophyll vs Discharge -------------------------------------

discharge <- data[1:1762,] # Select Discharge
discharge <- discharge %>% # Select Prieta B
  filter(stream=="QPB")

chla_QPB <- slice(data, (1961:1990)) # Select Chlorophyll
tail(chla_QPB)
merged_df <- merge(chla_QPB, discharge, by = "date") # Merge both dataframe by date


merged_df$value.x <- as.numeric(as.character(merged_df$value.x))
shapiro.test(merged_df$value.x)

merged_df <- na.omit(merged_df)
merged_df$value.y <- as.numeric(as.character(merged_df$value.y))
shapiro.test(merged_df$value.y)

merged_df <- na.omit(merged_df)
# Calculate Spearman's rank correlation
correlation_value_xy <- cor(merged_df$value.x, merged_df$value.y, method = "spearman")


print(paste("Correlation (rho):", correlation_value_xy))
print(paste("P-value:", correlation_value_xy))

library("ggpubr")
ggscatter(merged_df, x = "value.y", y = "value.x", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Discharge", ylab = "Chlorophyll-a")




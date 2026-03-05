rm(list = ls())
library(ggplot2)
library(tidyr)

## re-upload the trawl data
data <- read.csv("trawl_taxa_subset.csv", header=TRUE)
data$Survey <- as.factor(data$Survey)
data$Season <- as.factor(data$Season)
data$Region <- as.factor(data$Region)
data$Stratum <- as.factor(data$Stratum)
data$Grid <- as.factor(data$Grid)
data$TowID <- as.factor(data$TowID)
data$Common_Name <- as.factor(data$Common_Name)
data$Date <- as.Date(data$Date) 
data$Expanded_Catch[is.na(data$Expanded_Catch)] = 1

## Biomass Calculations
dat_kg <- data[,c("TowID", "Common_Name", "Expanded_Weight_kg")]
dat_kg <- na.omit(dat_kg)

dat_kg_tow <- aggregate(dat_kg$Expanded_Weight_kg, list(dat_kg$TowID), FUN=sum) 
colnames(dat_kg_tow) <- c("TowID", "Total_kg")

dat_kg_taxa <- aggregate(dat_kg$Expanded_Weight_kg, list(dat_kg$Common_Name), FUN=sum) 
colnames(dat_kg_taxa) <- c("Common_Name", "Total_kg")
dat_kg_taxa$Relative_kg <- dat_kg_taxa$Total_kg/max(dat_kg_taxa$Total_kg)
dat_kg_taxa <- dat_kg_taxa[order(dat_kg_taxa$Total_kg, decreasing=TRUE),]
barplot(dat_kg_taxa$Relative_kg, names.arg = dat_kg_taxa$Common_Name, las=2, cex.names= 0.65)

#eDNA Trawls
eDNA_trawls <- read.csv("eDNA_Trawls.csv")
eDNA_trawls$TowID <- as.factor(eDNA_trawls$TowID)

#Species Types
dat_spp <- read.csv("species_types.csv", header=TRUE)

### NOTES/PREVIOUS SCRIPTS
### with all the read Upload csv data file for fish species abundance/read count proportions 
### I had to adjust some Silver Hake numbers (there would be 2 entries per trawl, it was weird)
### Polished trawl data saved as "trawl_taxa_subset.csv"
#trawl_data <- read.csv("MaineDMR_Trawl_Survey_Catch_Data_2024-12-06.csv")
#head(trawl_data)
#summary(trawl_data)
#trawl_data$Survey <- as.factor(trawl_data$Survey)
#trawl_data$Season <- as.factor(trawl_data$Season)
#trawl_data$Region <- as.factor(trawl_data$Region)
#trawl_data$Stratum <- as.factor(trawl_data$Stratum)
#trawl_data$Grid <- as.factor(trawl_data$Grid)
#trawl_data$Common_Name <- as.factor(trawl_data$Common_Name)
#trawl_data$Date <- as.Date(trawl_data$Date)
#trawl_data$TowID <- as.factor(paste0(trawl_data$Survey, "0", trawl_data$Tow_Number))

#eDNA_trawls <- read.csv("eDNA_Trawls.csv")
#eDNA_trawls$TowID <- as.factor(eDNA_trawls$TowID)
#data <- droplevels(trawl_data[trawl_data$TowID %in% eDNA_trawls$TowID,])
#write.csv(data, "trawl_taxa_subset.csv", row.names = FALSE)
#levels(data$TowID)



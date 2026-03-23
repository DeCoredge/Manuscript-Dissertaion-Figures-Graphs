rm(list = ls())
library(readr)

data <- read_csv("DMR_Inshore_Trawl_eDNASample_metadata.csv")
View(data)

data <- data[, c("Sample_Type", "Replicate", "Trawl_number", "Qubit_mean")]
data <- droplevels(data[data$Sample_Type != "Blank",])
data <- droplevels(data[data$Replicate == "no",])
data$Sample_Type <-factor(data$Sample_Type, levels = c("bottom_metaprobe", "top_metaprobe", "slush"))
data_no_out <- subset(data, Qubit_mean <30)

boxplot(data$Qubit_mean~data$Trawl_number)
boxplot(data_no_out$Qubit_mean~data_no_out$Trawl_number)

boxplot(data$Qubit_mean~data$Sample_Type)
boxplot(data_no_out$Qubit_mean~data_no_out$Sample_Type, outline = FALSE,
        ylab="DNA ng_ul", xlab="Sample Type", main = "DNA concentration by Sample Type")
stripchart(Qubit_mean ~ Sample_Type, data = data, 
      method = "jitter", add = TRUE, vertical = TRUE, pch = 20, col = "black")


# Analysis of Variance
mod_1 <- lm(Qubit_mean ~ Sample_Type, data=data) # fit linear model
anova_1 <- aov(mod_1) # analysis of variance
TukeyHSD(anova_1)

mod_1_no_out <- lm(Qubit_mean ~ Sample_Type, data=data_no_out) # fit linear model
anova_1_no_out <- aov(mod_1_no_out) # analysis of variance
TukeyHSD(anova_1_no_out)

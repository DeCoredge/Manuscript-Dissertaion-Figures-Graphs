rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_fish_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count) 
edna_data$MiFish <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_model <- glm(Abundance ~ 1, data = edna_data,
      family = "poisson", na.action = na.omit)
edna_MiFish_model_02 <- glm(Abundance ~ MiFish, 
      data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_MiFish_model)
summary(edna_MiFish_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$MiFish, xlab = "MiFish12S Read Count",
     ylab = "Total Biomass", 
     main = "Fish biomass across all trawls ~ MiFish12S linear regression model")
abline(edna_MiFish_model, col="green3", lwd=2)
abline(edna_MiFish_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$MiFish, na.rm = TRUE), max(edna_data$MiFish, 
      na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_MiFish_model, newdata = data.frame(MiFish = preds_x), type = "response")
preds_y_m2 <- predict(edna_MiFish_model_02, newdata = data.frame(MiFish = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$MiFish + 0.001),
     xlab = "log10(MiFish12S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Fish biomass across all trawls ~ MiFish12S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts by station in csv.
edna_data<- read.csv("relative_reads_fish_trawl_species_by_station.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$trawl_count) 
edna_data$MiFish <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_model <- glm(Abundance ~ 1, data = edna_data,
                         family = "poisson", na.action = na.omit)
edna_MiFish_model_02 <- glm(Abundance ~ MiFish, 
                            data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_MiFish_model)
summary(edna_MiFish_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$MiFish, xlab = "MiFish12S Read Count",
     ylab = "Total Biomass", 
     main = "Fish biomass by Station ~ MiFish12S linear regression model")
abline(edna_MiFish_model, col="green3", lwd=2)
abline(edna_MiFish_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$MiFish, na.rm = TRUE), max(edna_data$MiFish, 
                                                        na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_MiFish_model, newdata = data.frame(MiFish = preds_x), type = "response")
preds_y_m2 <- predict(edna_MiFish_model_02, newdata = data.frame(MiFish = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$MiFish + 0.001),
     xlab = "log10(MiFish12S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Fish biomass by Station ~ MiFish12S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count) 
edna_data$LerayCOI <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Leray_model <- glm(Abundance ~ 1, data = edna_data,
                         family = "poisson", na.action = na.omit)
edna_Leray_model_02 <- glm(Abundance ~ LerayCOI, 
                            data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$LerayCOI, xlab = "Leray COI Read Counts",
     ylab = "Total Biomass", 
     main = "Invertebrate biomass across all trawls ~ Leray COI linear-regression model")
abline(edna_Leray_model, col="green3", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$LerayCOI, na.rm = TRUE), max(edna_data$LerayCOI, 
                                                        na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Leray_model, newdata = data.frame(LerayCOI = preds_x), type = "response")
preds_y_m2 <- predict(edna_Leray_model_02, newdata = data.frame(LerayCOI = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$LerayCOI + 0.001),
     xlab = "log10(Leray COI read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Invertebrate biomass across all trawls ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16, col = "black")





#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts by sampling station in csv.
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_by_station.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$trawl_count) 
edna_data$LerayCOI <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Leray_model <- glm(Abundance ~ 1, data = edna_data,
                        family = "poisson", na.action = na.omit)
edna_Leray_model_02 <- glm(Abundance ~ LerayCOI, 
                           data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$LerayCOI, xlab = "Leray COI Read Counts",
     ylab = "Total Biomass", 
     main = "Invertebrate biomass by Station ~ Leray COI linear-regression model")
abline(edna_Leray_model, col="green3", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$LerayCOI, na.rm = TRUE), max(edna_data$LerayCOI, 
                                          na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Leray_model, newdata = data.frame(LerayCOI = preds_x), type = "response")
preds_y_m2 <- predict(edna_Leray_model_02, newdata = data.frame(LerayCOI = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$LerayCOI + 0.001),
     xlab = "log10(Leray COI read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Invertebrate biomass by Station ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16, col = "black")







#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count) 
edna_data$Ceph18S <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Ceph18S_model <- glm(Abundance ~ 1, data = edna_data,
                         family = "poisson", na.action = na.omit)
edna_Ceph18S_model_02 <- glm(Abundance ~ Ceph18S, 
                   data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_Ceph18S_model)
summary(edna_Ceph18S_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Ceph18S, xlab = "Ceph18S Read Counts",
     ylab = "Total Biomass", 
     main = "Cephalopod biomass across all trawls ~ Ceph18S linear regression model")
abline(edna_Ceph18S_model, col="green3", lwd=2)
abline(edna_Ceph18S_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Ceph18S, na.rm = TRUE), max(edna_data$Ceph18S, 
                          na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Ceph18S_model, newdata = data.frame(Ceph18S = preds_x), type = "response")
preds_y_m2 <- predict(edna_Ceph18S_model_02, newdata = data.frame(Ceph18S = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Ceph18S + 0.001),
     xlab = "log10(Ceph18S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Cephalopod biomass across all trawls ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")





#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts by sampling station in csv.
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_by_station.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$trawl_count) 
edna_data$Ceph18S <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Ceph18S_model <- glm(Abundance ~ 1, data = edna_data,
                         family = "poisson", na.action = na.omit)
edna_Ceph18S_model_02 <- glm(Abundance ~ Ceph18S, 
                data = edna_data, family = "poisson", na.action = na.omit)

# View GLM summaries
summary(edna_Ceph18S_model)
summary(edna_Ceph18S_model_02)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Ceph18S, xlab = "Ceph18S Read Count",
     ylab = "Total Biomass", 
     main = "Cephalopod biomass by Station ~ Ceph18S linear regression model")
abline(edna_Ceph18S_model, col="green3", lwd=2)
abline(edna_Ceph18S_model_02, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Ceph18S, na.rm = TRUE), max(edna_data$Ceph18S, 
                                    na.rm = TRUE), length.out = 100)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Ceph18S_model, newdata = data.frame(Ceph18S = preds_x), type = "response")
preds_y_m2 <- predict(edna_Ceph18S_model_02, newdata = data.frame(Ceph18S = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)
lines(preds_x, preds_y_m2, col = "darkcyan", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Ceph18S + 0.001),
     xlab = "log10(Ceph18S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Cephalopod biomass by Station ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")

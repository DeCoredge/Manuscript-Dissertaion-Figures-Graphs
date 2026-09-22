rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_fish_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count)
edna_data$MiFish <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)

# Correctly extract columns containing special characters
edna_data$Starting_Depth <- round(edna_data$Starting_Depth.m.)
edna_data$End_Depth      <- round(edna_data$End_Depth.m.)
edna_data$Temperature    <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_full_model <- glm(Abundance ~ MiFish, data = edna_data, family = "poisson", na.action = na.omit)
edna_MiFish_backward_model <- step(edna_MiFish_full_model, direction = "backward")
summary(edna_MiFish_backward_model)

#View GLM model summary
summary(edna_MiFish_full_model)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$MiFish, 
  xlab = "MiFish12S Read Count", ylab = "Total Biomass", 
     main = "Fish biomass across all trawls ~ MiFish12S linear regression model")
abline(edna_MiFish_full_model, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$MiFish, na.rm = TRUE), max(edna_data$MiFish, 
      na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(edna_MiFish_full_model, data.frame(MiFish = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

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


# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_full_model <- glm(Abundance ~ MiFish + Temperature + Mean_Depth,
                data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_MiFish_backward_model <- step(edna_MiFish_full_model, direction = "backward")

summary(edna_MiFish_backward_model)

#View GLM model summary
summary(edna_MiFish_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$MiFish, xlab = "MiFish12S Read Count",
     ylab = "Total Biomass", 
     main = "Fish biomass by Station ~ MiFish12S linear regression model")
abline(edna_MiFish_full_model, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$MiFish, na.rm = TRUE), max(edna_data$MiFish, 
                                   na.rm = TRUE), length.out = 100)

# Create evaluation data frames matching model variables (holding covariates at their mean value)
new_data_frame <- data.frame(
  MiFish = preds_x,
  Temperature = mean(edna_data$Temperature, na.rm = TRUE),
  Mean_Depth = mean(edna_data$Mean_Depth, na.rm = TRUE))

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_MiFish_full_model, newdata = new_data_frame, type = "response")

#Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)

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
































#Perform a Multiplicative factor log-Poisson Regression GLM using the abundance proportion of all Invert. species to their read counts, and to their body type in csv.
edna_data_02<- read.csv("Leray_Abundance_Poisson.csv", header = TRUE)
edna_data_02$Abundance <- edna_data_02$Abundance
edna_data_02$Leray <- edna_data_02$Leray
edna_data_02$Body_Type <-edna_data_02$Body_Type

#Replace N/A in csv dataset table.
edna_data_02<- replace(edna_data_02, is.na(edna_data_02), "")

#Fix any dataframe formating issues
edna_data_02$Species <- as.factor(edna_data_02$Species)
edna_data_02$Body_Type <- as.factor(edna_data_02$Body_Type)

#Fit data into Poisson Regression GLMs.
edna_Leray_model <- glm(Abundance ~ 1, data= edna_data_02, family = "poisson")
edna_Leray_model_02 <- glm(Abundance ~ Leray*Body_Type, data= edna_data_02,
                           family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Add the model prediction to the plot
plot(edna_data_02$Leray ~ edna_data_02$Abundance, xlab = "Total Abundance",
     ylab = "Multiplicative Leray Read Count",  
     main = "Invertebrate abundance ~ Leray read counts multiplicative linear regression model")
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance), xlab = "Total Abundance",
     ylab = "Multiplicative Leray Read Count", 
     main = "Invertebrate abundance ~ Leray read counts multiplicative log-linear regression model")
abline(a=0, b=1) # one to one line

#Perform a Additive factor log-Poisson Regression GLM using the abundance proportion of all Invert. species to their read counts, and to their body type in csv.
edna_data_02<- read.csv("Leray_Abundance_Poisson.csv", header = TRUE)
edna_data_02$Abundance <- edna_data_02$Abundance
edna_data_02$Leray <- edna_data_02$Leray
edna_data_02$Body_Type <-edna_data_02$Body_Type

#Replace N/A in csv dataset table.
edna_data_02<- replace(edna_data_02, is.na(edna_data_02), "")

#Fix any dataframe formating issues
edna_data_02$Species <- as.factor(edna_data_02$Species)
edna_data_02$Body_Type <- as.factor(edna_data_02$Body_Type)

#Fit data into Poisson Regression GLMs.
edna_Leray_model <- glm(Abundance ~ 1, data= edna_data_02, family = "poisson")
edna_Leray_model_02 <- glm(Abundance ~ Leray+Body_Type, data= edna_data_02,
                           family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Leray_model)
summary(edna_Leray_model_02)

# Add the model prediction to the plot
plot(edna_data_02$Leray ~ edna_data_02$Abundance, xlab = "Total Abundance",
     ylab = "Additive Leray Read Count", 
     main = "Invertebrate abundance ~ Leray read counts additive linear regression model")
abline(edna_Leray_model, col="green", lwd=2)
abline(edna_Leray_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_02$Leray) ~ log10(edna_data_02$Abundance),  xlab = "Total Abundance",
     ylab = "Additive Leray Read Count", 
     main = "Invertebrate abundance ~ Leray read counts additive log-linear regression model")
abline(a=0, b=1) # one to one line


#Perform a Poisson Regression GLM using the abundance proportion of cephalopod species to their read counts in csv.
edna_data_03<- read.csv("Ceph18s_Abundance_Poisson.csv", header = TRUE)
edna_data_03$Abundance <- edna_data_03$Abundance
edna_data_03$Ceph18s <- edna_data_03$Ceph18s

#Replace N/A in csv dataset table.
edna_data_03<- replace(edna_data_03, is.na(edna_data_03), "")

#Fix any dataframe formating issues
edna_data_03$Species <- as.factor(edna_data_03$Species)

#Fit data into Poisson Regression GLMs.
edna_Ceph18s_model <- glm(Abundance ~ 1, data= edna_data_03, family = "poisson")
edna_Ceph18s_model_02 <- glm(Abundance ~ Ceph18s, data= edna_data_03,
                             family = "poisson")

#View GLMs & Explore model fits and independent variables signifance
summary(edna_Ceph18s_model)
summary(edna_Ceph18s_model_02)

# Add the model prediction to the plot
plot(edna_data_03$Ceph18s ~ edna_data_03$Abundance, xlab = "Total Abundance",
     ylab = "Ceph18S Read Count", 
     main = "Cephalopod abundance ~ Ceph18s read counts linear regression model")
abline(edna_Ceph18s_model, col="green", lwd=2)
abline(edna_Ceph18s_model_02, col="purple", lwd=2)

# Add the model prediction to the plot
plot(log10(edna_data_03$Ceph18s) ~ log10(edna_data_03$Abundance), xlab = "Total Abundance",
     ylab = "Ceph18S Read Count", 
     main = "Cephalopod abundance ~ Ceph18s read counts log-linear regression model")
abline(a=0, b=1) # one to one line

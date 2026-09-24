rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts across all trawls by each sample type.
edna_data<- read.csv("relative_reads_fish_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$MiFish <- edna_data$mean_edna_rel_read_count

#Keep ONLY rows from one specific sample type (e.g., Bottom_metaprobe)
# Makes each species only appears exactly once in the model dataset
edna_filtered <- subset(edna_data, sample_type == "Bottom_metaprobe")
edna_filtered$species <- as.factor(edna_filtered$species)

#Run the Poisson GLM on the single-entry dataset
full_model_filtered <- glm(Biomass ~ MiFish, data = edna_filtered, family = "poisson")
summary(full_model_filtered)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_filtered$Biomass ~ edna_filtered$MiFish,
     xlab = "MiFish12S Read Count", ylab = "Total Biomass", 
  main = "Fish biomass across all trawls by sample type ~ MiFish12S linear regression model", 
  ylim = c(0,25))
abline(full_model_filtered, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_filtered$MiFish, na.rm = TRUE), max(edna_filtered$MiFish, 
      na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(full_model_filtered, data.frame(MiFish = preds_x, type = "response"))

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_filtered$Biomass + 0.1) ~ log10(edna_filtered$MiFish + 0.001),
     xlab = "log10(MiFish12S read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Fish biomass across all trawls by sample type ~ MiFish12S Log-linear regression model Scale Exploration",
     pch = 16)


rm(list = ls())

#Perform a Pearson & Spearman Statistic test for each sample type
edna_data<- read.csv("relative_reads_fish_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$MiFish <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$sample_type <- as.factor(edna_data$sample_type)


# 1. Pearson Correlation for each sample_type

print("--- Pearson Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$MiFish, method = "pearson", na.action = na.omit)
})

# 2. Spearman Correlation for each sample_type
print("--- Spearman Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$MiFish, method = "spearman", na.action = na.omit)
})


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts by each station csv.
edna_data<- read.csv("", header = TRUE)
edna_data$Biomass <- round(edna_data$trawl_weight) 
edna_data$MiFish <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)

# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_full_model <- glm(Biomass ~ MiFish * Temperature * Mean_Depth,
                data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_MiFish_backward_model <- step(edna_MiFish_full_model, direction = "backward")

summary(edna_MiFish_backward_model)

#View GLM model summary
summary(edna_MiFish_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Biomass ~ edna_data$MiFish * edna_data$Temperature * edna_data$Mean_Depth,
     xlab = "MiFish12S Read Count", ylab = "Total Biomass", 
     main = "Fish biomass across all trawls ~ MiFish12S linear regression model", 
     ylim = c(0,25))
lines(edna_MiFish_full_model, col="purple", lwd=2)


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

plot(log10(edna_data$Biomass + 0.1) ~ log10(edna_data$MiFish + 0.001),
     xlab = "log10(MiFish12S read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Fish biomass by Station ~ MiFish12S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


# Correlation Testing (Biomass across all Trawls vs MiFish eDNA Read Counts)
# ---------------------------------------------------------

# 1. Pearson Correlation Test (Evaluates linear relationship strength)
pearson_result <- cor.test(edna_data$Biomass, edna_data$MiFish, 
                           method = "pearson")

print("--- PEARSON CORRELATION RESULTS ---")
print(pearson_result)


# 2. Spearman Rank Correlation Test (Evaluates non-linear/monotonic relationship)
# Recommended for skewed biomass counts and proportional eDNA reads
spearman_result <- cor.test(edna_data$Biomass, edna_data$MiFish, 
                            method = "spearman",
                            exact = FALSE) # ADD THIS LINE to silence the ties warning

print("--- SPEARMAN CORRELATION RESULTS ---")
print(spearman_result)


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts across all trawls by each sample type.
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$Leray <- edna_data$mean_edna_rel_read_count

#Keep ONLY rows from one specific sample type (e.g., Bottom_metaprobe)
# Makes each species only appears exactly once in the model dataset
edna_filtered <- subset(edna_data, sample_type == "Bottom_metaprobe")
edna_filtered$species <- as.factor(edna_filtered$species)

#Run the Poisson GLM on the single-entry dataset
full_model_filtered <- glm(Biomass ~ Leray, data = edna_filtered, family = "poisson")
summary(full_model_filtered)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_filtered$Biomass ~ edna_filtered$Leray,
     xlab = "Leray COI Read Counts", ylab = "Total Biomass", 
     main = "Invertebrate biomass across all trawls by sample type ~ Leray COI linear regression model", 
     ylim = c(0,25))
abline(full_model_filtered, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_filtered$Leray, na.rm = TRUE), max(edna_filtered$Leray, 
                                                            na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(full_model_filtered, data.frame(Leray = preds_x, type = "response"))

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_filtered$Biomass + 0.1) ~ log10(edna_filtered$Leray + 0.001),
     xlab = "log10(Leray COI read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Invertebrate biomass across all trawls ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16)


rm(list = ls())

#Perform a Pearson & Spearman Statistic test for each sample type
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$Leray <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$sample_type <- as.factor(edna_data$sample_type)


# 1. Pearson Correlation for each sample_type

print("--- Pearson Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$Leray, method = "pearson", na.action = na.omit)
})

# 2. Spearman Correlation for each sample_type
print("--- Spearman Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$Leray, method = "spearman", na.action = na.omit)
})


rm(list = ls())



#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts by each station in csv.
edna_data<- read.csv("", header = TRUE)
edna_data$Abundance <- round(edna_data$trawl_count) 
edna_data$Leray <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)
edna_data$Body_Type <- as.factor(edna_data$Body_Type)

# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Leray_full_model <- glm(Abundance ~ Leray * Body_Type + Temperature,
                              data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_Leray_backward_model <- step(edna_Leray_full_model, direction = "backward")

summary(edna_Leray_backward_model)

#View GLM model summary
summary(edna_Leray_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Leray, xlab = "Leray COI Read Count",
     ylab = "Total Biomass", 
     main = "Invertebrate biomass by Station ~ Leray COI linear regression model")
abline(edna_Leray_full_model, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Leray, na.rm = TRUE), max(edna_data$Leray, 
                                                        na.rm = TRUE), length.out = 100)

# Create evaluation data frames matching model variables (holding covariates at their mean value)
new_data_frame <- data.frame(
  Leray = preds_x,
  Temperature = mean(edna_data$Temperature, na.rm = TRUE),
  Body_Type   = unique(edna_data$Body_Type) # Generates a row for 'Crunchy' and a row for 'Slimy'
)

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Leray_full_model, newdata = new_data_frame, type = "response")

#Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Leray + 0.001),
     xlab = "log10(Leray COI read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Invertebrate biomass by Station ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts across all trawls by sample type in csv.
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_count)
edna_data$Ceph18s <- edna_data$mean_edna_rel_read_count

#Keep ONLY rows from one specific sample type (e.g., Bottom_metaprobe)
# Makes each species only appears exactly once in the model dataset
edna_filtered <- subset(edna_data, sample_type == "Bottom_metaprobe")
edna_filtered$species <- as.factor(edna_filtered$species)

#Run the Poisson GLM on the single-entry dataset
full_model_filtered <- glm(Biomass ~ Ceph18s, data = edna_filtered, family = "poisson")
summary(full_model_filtered)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_filtered$Biomass ~ edna_filtered$Ceph18s,
     xlab = "Ceph18s Read Count", ylab = "Total Biomass", 
     main = "Cephalopod biomass across all trawls by sample type ~ Ceph18s linear regression model", 
     ylim = c(0,25))
abline(full_model_filtered, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_filtered$Ceph18s, na.rm = TRUE), max(edna_filtered$Ceph18s, 
                                                            na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(full_model_filtered, data.frame(Ceph18s = preds_x, type = "response"))

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_filtered$Biomass + 0.1) ~ log10(edna_filtered$Ceph18s + 0.001),
     xlab = "log10(Ceph18s read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Cephalopod biomass across all trawls by sample type ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16)


rm(list = ls())

#Perform a Pearson & Spearman Statistic test for each sample type
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$Ceph18s <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$sample_type <- as.factor(edna_data$sample_type)


# 1. Pearson Correlation for each sample_type

print("--- Pearson Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$Ceph18s, method = "pearson", na.action = na.omit)
})

# 2. Spearman Correlation for each sample_type
print("--- Spearman Correlation Results ---")
by(edna_data, edna_data$sample_type, function(subsample) {
  cor.test(subsample$Biomass, subsample$Ceph18s, method = "spearman", na.action = na.omit)
})


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts by each station.
edna_data<- read.csv("", header = TRUE)
edna_data$Biomass <- round(edna_data$trawl_weight) 
edna_data$Ceph18s <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)

# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Ceph18s_full_model <- glm(Biomass ~ Ceph18s * Temperature * Mean_Depth,
                              data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_Ceph18s_backward_model <- step(edna_Ceph18s_full_model, direction = "backward")

summary(edna_Ceph18s_backward_model)

#View GLM model summary
summary(edna_Ceph18s_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Biomass ~ edna_data$Ceph18s * edna_data$Temperature * edna_data$Mean_Depth,
     xlab = "Ceph18s Read Counts", ylab = "Total Biomass", 
     main = "Fish biomass across all trawls ~ Ceph18s linear regression model", 
     ylim = c(0,25))
lines(edna_Ceph18s_full_model, col="purple", lwd=2)


# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Ceph18s, na.rm = TRUE), max(edna_data$Ceph18s, 
                                                        na.rm = TRUE), length.out = 100)

# Create evaluation data frames matching model variables (holding covariates at their mean value)
new_data_frame <- data.frame(
  Ceph18s = preds_x,
  Temperature = mean(edna_data$Temperature, na.rm = TRUE),
  Mean_Depth = mean(edna_data$Mean_Depth, na.rm = TRUE))

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Ceph18s_full_model, newdata = new_data_frame, type = "response")

#Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Biomass + 0.1) ~ log10(edna_data$Ceph18s + 0.001),
     xlab = "log10(Ceph18S read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Cephalopod biomass by Station ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


# Correlation Testing (Biomass across all Trawls vs MiFish eDNA Read Counts)
# ---------------------------------------------------------

# 1. Pearson Correlation Test (Evaluates linear relationship strength)
pearson_result <- cor.test(edna_data$Biomass, edna_data$Ceph18s, 
                           method = "pearson")

print("--- PEARSON CORRELATION RESULTS ---")
print(pearson_result)


# 2. Spearman Rank Correlation Test (Evaluates non-linear/monotonic relationship)
# Recommended for skewed biomass counts and proportional eDNA reads
spearman_result <- cor.test(edna_data$Biomass, edna_data$Ceph18s, 
                            method = "spearman",
                            exact = FALSE) # ADD THIS LINE to silence the ties warning

print("--- SPEARMAN CORRELATION RESULTS ---")
print(spearman_result)


rm(list = ls())

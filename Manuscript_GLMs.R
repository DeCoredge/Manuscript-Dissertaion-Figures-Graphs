rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_fish_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$mean_trawl_weight)
edna_data$MiFish <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)
edna_data$sample_type <- as.factor(edna_data$sample_type)

#Correctly extract columns containing special characters
edna_data$Starting_Depth <- round(edna_data$Starting_Depth.m.)
edna_data$End_Depth      <- round(edna_data$End_Depth.m.)
edna_data$Temperature    <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_MiFish_full_model <- glm(Biomass ~ MiFish * sample_type, data = edna_data, family = "poisson", na.action = na.omit)
edna_MiFish_backward_model <- step(edna_MiFish_full_model, direction = "backward")
summary(edna_MiFish_backward_model)

#View GLM model summary
summary(edna_MiFish_full_model)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Biomass ~ edna_data$MiFish * edna_data$sample_type,
     xlab = "MiFish12S Read Count", ylab = "Total Biomass", 
  main = "Fish biomass across all trawls ~ MiFish12S linear regression model", 
  ylim = c(0,25))
abline(edna_MiFish_full_model, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$MiFish, edna_data$sample_type, na.rm = TRUE), max(edna_data$MiFish, edna_data$sample_type, 
      na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(edna_MiFish_full_model, data.frame(MiFish = preds_x,
  sample_type = "Bottom_metaprobe", "Slush", "Top_metaprobe"), type = "response")

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 1. Define explicit colors matching your plot: Blue, Pink/Red, Green
legend_colors <- c("#2297E6", "#DF536B", "#61D04F")

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Biomass + 0.1) ~ log10(edna_data$MiFish + 0.001),
     xlab = "log10(MiFish12S read counts)", 
     ylab = "log10(Total Biomass)", 
     main = "Fish biomass across all trawls ~ MiFish12S Log-linear regression model Scale Exploration",
     pch = 16, col = edna_data$sample_type)

#Create a legend to label which sample types the colored data points are
# 3. Add the legend outside the plot, Start at the top-right corner of the plot
  
legend(x = "bottomright",                  # Start at the top-right corner of the plot
       inset = c(-0.03, 0),              # Move it 30% past the right edge (negative X pushes it out)
       legend = c("Bottom Metaprobe", "Slush", "Top Metaprobe"), 
       col = legend_colors, 
       pch = 16,
       xpd = TRUE)                      # CRITICAL: Allows drawing outside the plot box

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


#Perform a Poisson Regression GLM using the abundance proportion of Fish species to their read counts by each station in csv.
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
     main = "Fish biomass by Station ~ MiFish12S linear regression model", 
     ylim = c(0,25))
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


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count)
edna_data$Leray <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)

# Correctly extract columns containing special characters
edna_data$Starting_Depth <- round(edna_data$Starting_Depth.m.)
edna_data$End_Depth      <- round(edna_data$End_Depth.m.)
edna_data$Temperature    <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Leray_full_model <- glm(Abundance ~ Leray, data = edna_data, family = "poisson", na.action = na.omit)
edna_Leray_backward_model <- step(edna_Leray_full_model, direction = "backward")
summary(edna_Leray_backward_model)

#View GLM model summary
summary(edna_Leray_full_model)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Leray, 
     xlab = "Leray COI Read Counts", ylab = "Total Biomass", 
     main = "Invertebrate biomass across all trawls ~ Leray COI linear regression model")
abline(edna_Leray_full_model, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Leray, na.rm = TRUE), max(edna_data$Leray, 
                                                        na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(edna_Leray_full_model, data.frame(Leray = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Leray + 0.001),
     xlab = "log10(Leray read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Invertebrate biomass across all trawls ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Invertebrate species to their read counts by station in csv.
edna_data<- read.csv("relative_reads_invertebrate_trawl_species_by_station.csv", header = TRUE)
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


#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts across all trawls in csv.
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_trawl_wide.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$mean_trawl_count)
edna_data$Ceph18S <- edna_data$mean_edna_rel_read_count
edna_data$species <- as.factor(edna_data$species)
edna_data$Temperature <- as.factor(edna_data$Bottom_Water_Temperature.C..)

# Correctly extract columns containing special characters
edna_data$Starting_Depth <- round(edna_data$Starting_Depth.m.)
edna_data$End_Depth      <- round(edna_data$End_Depth.m.)
edna_data$Temperature    <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Ceph18S_full_model <- glm(Abundance ~ Ceph18S, data = edna_data, family = "poisson", na.action = na.omit)
edna_Ceph18S_backward_model <- step(edna_Ceph18S_full_model, direction = "backward")
summary(edna_Ceph18S_backward_model)

#View GLM model summary
summary(edna_Ceph18S_full_model)

# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Ceph18S, 
     xlab = "Ceph18S Read Count", ylab = "Total Biomass", 
     main = "Cephalopod biomass across all trawls ~ Ceph18S linear regression model")
abline(edna_Ceph18S_full_model, col="green3", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Ceph18S, na.rm = TRUE), max(edna_data$Ceph18S, 
                                                        na.rm = TRUE), length.out = 100)

# Calculate predictions from both models (using response scale for poisson count data)
pred_y_model01 <- predict(edna_Ceph18S_full_model, data.frame(Ceph18S = preds_x), type = "response")

# Draw curves for the graphs
lines(preds_x, pred_y_model01, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Ceph18S + 0.001),
     xlab = "log10(Ceph18S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Cephalopod biomass across all trawls ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


rm(list = ls())


#Perform a Poisson Regression GLM using the abundance proportion of Cephalopod species to their read counts by station in csv.
edna_data<- read.csv("relative_reads_cephalopod_trawl_species_by_station.csv", header = TRUE)
edna_data$Abundance <- round(edna_data$trawl_count) 
edna_data$Ceph18S <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)


# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Ceph18S_full_model <- glm(Abundance ~ Ceph18S + Temperature + Mean_Depth,
                              data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_Ceph18S_backward_model <- step(edna_Ceph18S_full_model, direction = "backward")

summary(edna_Ceph18S_backward_model)

#View GLM model summary
summary(edna_Ceph18S_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Abundance ~ edna_data$Ceph18S, xlab = "Ceph18S Read Counts",
     ylab = "Total Biomass", 
     main = "Cephalopod biomass by Station ~ Ceph18S linear regression model")
abline(edna_Ceph18S_full_model, col="purple", lwd=2)

# Generate smooth sequence for predictable curves
preds_x <- seq(min(edna_data$Ceph18S, na.rm = TRUE), max(edna_data$Ceph18S, 
                                                        na.rm = TRUE), length.out = 100)

# Create evaluation data frames matching model variables (holding covariates at their mean value)
new_data_frame <- data.frame(
  Ceph18S = preds_x,
  Temperature = mean(edna_data$Temperature, na.rm = TRUE),
  Mean_Depth = mean(edna_data$Mean_Depth, na.rm = TRUE))

# Predict values back onto response scale (type = "response")
preds_y_m1 <- predict(edna_Ceph18S_full_model, newdata = new_data_frame, type = "response")

#Draw curves for the graphs
lines(preds_x, preds_y_m1, col = "red", lwd = 2)

# 5. Log-log linear regression model creation & visualization
# Adding small constant (e.g., 0.001) protects against log(0) mathematically undefined errors

plot(log10(edna_data$Abundance + 0.1) ~ log10(edna_data$Ceph18S + 0.001),
     xlab = "log10(Ceph18S read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Cephalopod biomass by Station ~ Ceph18S Log-linear regression model Scale Exploration",
     pch = 16, col = "black")

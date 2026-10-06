rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

library(ggplot2)
library(readr)
library(cowplot) # A reliable alternative to avoid the layout "+" operator error

# ==============================================================================
# 2. LOAD AND PREPARE DATASETS
# ==============================================================================
ceph_data   <- read_csv("relative_reads_cephalopod_trawl_species_trawl_wide.csv")
fish_data   <- read_csv("relative_reads_fish_trawl_species_trawl_wide.csv")
invert_data <- read_csv("relative_reads_invertebrate_trawl_species_trawl_wide.csv")

# Filter out 0 values to protect log10 scaling transformations
ceph_data   <- subset(ceph_data, mean_trawl_weight > 0)
fish_data   <- subset(fish_data, mean_trawl_weight > 0)
invert_data <- subset(invert_data, mean_trawl_weight > 0)

# ==============================================================================
# 3. GENERATE THE INDIVIDUAL PLOTS
# ==============================================================================

# Plot 1: Cephalopods
p1 <- ggplot(ceph_data, aes(x = mean_edna_rel_read_count, y = mean_trawl_weight)) +
  geom_point(aes(color = sample_type), size = 2.5) +
  geom_smooth(aes(color = sample_type, fill = sample_type), 
              method = "lm", linetype = "dashed", alpha = 0.15) +
  scale_y_log10(labels = scales::label_log()) +
  labs(
    title = "Cephalopod Biomass ~ Ceph18s Mean Relative Read counts\nLog-Linear Relationship",
    x = "Ceph18s Mean Relative Read Counts",
    y = "Mean Cephalopod Biomass (Log10 Scale)",
    color = "Collection Method", fill = "Collection Method"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Handled globally by cowplot below

# Plot 2: Invertebrates
p2 <- ggplot(invert_data, aes(x = mean_edna_rel_read_count, y = mean_trawl_weight)) +
  geom_point(aes(color = sample_type), size = 2.5) +
  geom_smooth(aes(color = sample_type, fill = sample_type), 
              method = "lm", linetype = "dashed", alpha = 0.15) +
  scale_y_log10(labels = scales::label_log()) +
  labs(
    title = "Invertebrate Biomass ~ Leray COI Mean Relative Read counts\nLog-Linear Relationship",
    x = "Leray COI Mean Relative Read Counts",
    y = "Mean Invertebrate Biomass (Log10 Scale)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 3: Fish (Keep the legend on this one to extract it clean)
p3 <- ggplot(fish_data, aes(x = mean_edna_rel_read_count, y = mean_trawl_weight)) +
  geom_point(aes(color = sample_type), size = 2.5) +
  geom_smooth(aes(color = sample_type, fill = sample_type), 
              method = "lm", linetype = "dashed", alpha = 0.15) +
  scale_y_log10(labels = scales::label_log()) +
  scale_color_manual(
    name = "Collection Method",
    values = c("Bottom_metaprobe" = "#F8766D", "Slush" = "#00BA38", "Top_metaprobe" = "#619CFF"),
    labels = c("Bottom_metaprobe" = "Bottom Metaprobe", "Slush" = "Slush", "Top_metaprobe" = "Top Metaprobe")) +
  scale_fill_manual(name = "Collection Method", values = c("Bottom_metaprobe" = "#F8766D", "Slush" = "#00BA38", "Top_metaprobe" = "#619CFF"),
    labels = c("Bottom_metaprobe" = "Bottom Metaprobe", "Slush" = "Slush", "Top_metaprobe" = "Top Metaprobe")) +
  labs(title = "Fish Biomass ~ MiFish12s Mean Relative Read counts\nLog-Linear Relationship",
    x = "MiFish12s Mean Relative Read Counts",
    y = "Mean Fish Biomass (Log10 Scale)",
    color = "Collection Method", fill = "Collection Method") +
  theme_minimal() +
  theme(legend.position = "right")

# ==============================================================================
# 4. EXTRACT LEGEND & ASSEMBLE SIDE-BY-SIDE WITHOUT THE "+" ERROR
# ==============================================================================

# Extract the shared legend structure from plot 3
shared_legend <- get_legend(p3)

# Remove the legend from plot 3 so it matches the other two plots visually
p3 <- p3 + theme(legend.position = "none")

# Arrange the three clean charts side-by-side cleanly
plots_row <- plot_grid(p1, p2, p3, ncol = 3, align = 'h', axis = 'b')

# Append the single clean legend block back to the right margin side
final_composite_plot <- plot_grid(plots_row, shared_legend, rel_widths = c(3, 0.4))

# Output display and save operations
print(final_composite_plot)
ggsave("biomass_read_counts_side_by_side.png", final_composite_plot, width = 22, height = 6, dpi = 300)


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
edna_data$Temperature <- round(edna_data$Bottom_Water_Temperature.C..)
edna_data$Depth <- round(edna_data$Mean_Depth.m.)

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
edna_data<- read.csv("SP23096_relative_reads_invertebrate_trawl_species_by_station.csv", header = TRUE)
edna_data$Biomass <- round(edna_data$trawl_weight) 
edna_data$Leray <- edna_data$best_rel_reads
edna_data$species <- as.factor(edna_data$species)
edna_data$Body_Type <- as.factor(edna_data$Body_Type)
edna_data$Temperature <- round(edna_data$Bottom_Water_Temperature.C..)
edna_data$Depth <- round(edna_data$Mean_Depth.m.)

# Correctly extract columns containing special characters
edna_data$Mean_Depth <- round(edna_data$Mean_Depth.m.)
edna_data$Temperature <- edna_data$Bottom_Water_Temperature.C..

#Fit data into Poisson Regression GLMs (dropping NA values automatically)
edna_Leray_full_model <- glm(Biomass ~ Leray * Body_Type + Temperature,
                             data = edna_data, family = "poisson", na.action = na.omit)

#Perform a backwards selction GLM to eliminate unnecessary cofactors
edna_Leray_backward_model <- step(edna_Leray_full_model, direction = "backward")

summary(edna_Leray_backward_model)

#View GLM model summary
summary(edna_Leray_full_model)


# Generate the linear scale plot with dynamic model prediction curves
plot(edna_data$Biomass ~ edna_data$Leray, xlab = "Leray COI Read Count",
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

plot(log10(edna_data$Biomass + 0.1) ~ log10(edna_data$Leray + 0.001),
     xlab = "log10(Leray COI read counts)", 
     ylab = "log10(Total Abundance)", 
     main = "Invertebrate biomass by Station ~ Leray COI Log-linear regression model Scale Exploration",
     pch = 16, col = "black")


rm(list = ls())

# Correlation Testing (Biomass across all Trawls vs MiFish eDNA Read Counts)
# ---------------------------------------------------------

# 1. Pearson Correlation Test (Evaluates linear relationship strength)
pearson_result <- cor.test(edna_data$Biomass, edna_data$Leray, 
                           method = "pearson")

print("--- PEARSON CORRELATION RESULTS ---")
print(pearson_result)


# 2. Spearman Rank Correlation Test (Evaluates non-linear/monotonic relationship)
# Recommended for skewed biomass counts and proportional eDNA reads
spearman_result <- cor.test(edna_data$Biomass, edna_data$Leray, 
                            method = "spearman",
                            exact = FALSE) # ADD THIS LINE to silence the ties warning

print("--- SPEARMAN CORRELATION RESULTS ---")
print(spearman_result)


rm(list = ls())

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
edna_data<- read.csv("FL23018_relative_reads_cephalopod_trawl_species_by_station.csv", header = TRUE)
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

rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

library(readr)

# 1. Load data into 'df' instead of 'data' to avoid naming conflicts
# Replace the filename string if yours is named differently
df <- read_csv("DMR_Inshore_Trawl_eDNA_concentration_metadata.csv")

# 2. Subset and filter
df <- df[, c("Sample_Type", "Replicate", "Trawl", "Qubit_mean")]
df <- droplevels(df[df$Sample_Type != "Blank", ])
df <- droplevels(df[df$Replicate == "no", ])
df$Sample_Type <- factor(df$Sample_Type, levels = c("bottom_metaprobe", "top_metaprobe", "slush"))

# Create dataset excluding the massive outlier (>30)
df_no_out <- subset(df, Qubit_mean < 30)

# 3. Fit linear model and extract exact p-value dynamically
mod_1_no_out <- lm(Qubit_mean ~ Sample_Type, data = df_no_out)
anova_summary <- summary(mod_1_no_out)
p_val <- pf(anova_summary$fstatistic[1], anova_summary$fstatistic[2], anova_summary$fstatistic[3], lower.tail = FALSE)
p_text <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 4)))

# 4. Generate the Boxplot (ylim expanded to make room for labels)
boxplot(df_no_out$Qubit_mean ~ df_no_out$Sample_Type, outline = FALSE,
        ylab = "DNA ng_ul", xlab = "Sample Type", 
        main = "Mean eDNA concentration by Sample Type",
        names = c("Bottom Metaprobe", "Top Metaprobe", "Slush"),
        ylim = c(0, max(df_no_out$Qubit_mean) + 4)) # Adds top margin space

# 5. Add individual data points (using the correct df_no_out data)
stripchart(Qubit_mean ~ Sample_Type, data = df_no_out, 
           method = "jitter", add = TRUE, vertical = TRUE, pch = 20, col = "black")

# 6. Overlay ANOVA statistics in the top margin
mtext(paste("ANOVA:", p_text), side = 3, adj = 0, line = 0.5, font = 3, cex = 0.9)

# 7. Overlay Tukey HSD significance letters above each box
# Coordinates: x-axis positions are 1, 2, and 3
tukey_letters <- c("a", "a", "b")
y_positions <- c(4, 4, 21) # Perfect padding heights above your data spreads

text(x = 1:3, y = y_positions, labels = tukey_letters, font = 2, col = "red", cex = 1.2)

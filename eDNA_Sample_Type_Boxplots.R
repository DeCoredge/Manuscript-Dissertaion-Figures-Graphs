rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

library(readr)

# 1. Load data into 'df' instead of 'data' to avoid naming conflicts
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

# Extract specific indices for F-statistic and degrees of freedom
f_stat  <- anova_summary$fstatistic[1]
df_num  <- anova_summary$fstatistic[2]
df_den  <- anova_summary$fstatistic[3]

p_val  <- pf(f_stat, df_num, df_den, lower.tail = FALSE)
p_text <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 4)))

# Format the legend string
legend_text <- paste0("one-way ANOVA: F(", df_num, ", ", df_den, ") = ", round(f_stat, 2), ", ", p_text)

# 4. Set plot margins to make plenty of room below the figure
# Increased the bottom margin (first number) to 9.0 to hold the elements safely
par(mar = c(9.0, 4, 4, 2) + 0.1)

# Generate the Boxplot (xlab set to empty string for custom positioning)
boxplot(df_no_out$Qubit_mean ~ df_no_out$Sample_Type, outline = FALSE,
        ylab = "DNA ng_ul", xlab = "", 
        main = "Mean eDNA concentration by Sample Type",
        names = c("Bottom Metaprobe", "Top Metaprobe", "Slush"),
        ylim = c(0, max(df_no_out$Qubit_mean) + 4))

# Manually add the "Sample Type" axis label further down (line = 4.5)
title(xlab = "Sample Type", line = 4.5)

# 5. Add individual data points
stripchart(Qubit_mean ~ Sample_Type, data = df_no_out, 
           method = "jitter", add = TRUE, vertical = TRUE, pch = 20, col = "black")

# 6. Add post-hoc letters
tukey_letters <- c("a", "a", "b")
y_positions <- c(4, 4, 21)
text(x = 1:3, y = y_positions, labels = tukey_letters, font = 2, col = "red", cex = 1.2)

# 7. Add the ANOVA legend text even further down below the label (line = 6.5)
mtext(legend_text, side = 1, line = 6.5, cex = 0.9)

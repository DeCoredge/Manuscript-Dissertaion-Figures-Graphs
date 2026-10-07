#Clear current working environment
rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Instal package that allows you to make tables
install.packages("gt")
install.packages("grid")
install.packages("gridExtra")

library(grid)
library(gridExtra)

# Use data from csv file to create tables
primers_table <- read.csv("General_Primers.csv", header = TRUE, sep = ",", check.names = FALSE)

#Replace N/A in table
primers_table<- replace(primers_table, is.na(primers_table), "")

# 2. Clean up underscores (this will now preserve your hyphens and parentheses)
colnames(primers_table) <- gsub("_", " ", colnames(primers_table))
primers_table[] <- lapply(primers_table, function(x) gsub("_", " ", x))

#Create Table, Customize, and View it
trawl_primers <- tableGrob(primers_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
 colhead = list(fg_params = list(col = "blue")))

grid.draw(trawl_primers)


#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
trawl_table <- read.csv("Trawl_Surveys.csv", header = TRUE, sep = ",", check.names = FALSE)

#Create Table, Customize, and View it
trawl_table <- tableGrob(trawl_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
                            colhead = list(fg_params = list(col = "blue")))

grid.draw(trawl_table)


#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
species_table <- read.csv("Trawl_Species_List.csv", header = TRUE, sep = ",", check.names = FALSE)

# Define a theme where the second column ("Scientific Name") is dynamically italicized
table_theme <- ttheme_default(
  core = list(fg_params = list(col = "grey",  # Matrix approach: Check if column index is 2, make it italic, otherwise plain
  fontface = matrix(ifelse(col(matrix(NA, nrow=nrow(species_table), 
    ncol=ncol(species_table))) == 2, 
       "italic", "plain"), nrow=nrow(species_table), 
    ncol=ncol(species_table)))), colhead = list(fg_params = list(col = "blue")))

#Create Table, Customize, and View it
species_table <- tableGrob(species_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
                            colhead = list(fg_params = list(col = "blue")))

grid.draw(species_table)


#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
statistic_ranking_table <- read.csv("Biomass_eDNA_Statistics_Station.csv", header = TRUE,  sep = ",", check.names = FALSE)

# Convert the raw symbol '⍴' to the word 'rho' so tableGrob's math engine can read it
colnames(statistic_ranking_table) <- gsub("⍴", "rho", colnames(statistic_ranking_table))


# Create Custom Theme and turn on 'parse = TRUE' to process mathematical expressions
table_grob <- ttheme_default(
  core = list(fg_params = list(col = "black")),
  colhead = list(fg_params = list(col = "blue2")),
  parse = TRUE #<-- IMPORTANT: Tells R to translate "rho" into the Greek letter 
)

# Pass the custom theme to your tableGrob
trawl_statistics <- tableGrob(statistic_ranking_table, theme = table_grob)

# --- ADD THESE LINES FOR THE LEGEND ---

# 1. Create a text box for the legend
legend_text <- textGrob(
  label = "*p < 0.05, **p < 0.005, ***p < 0.0005",
  x = unit(0.05, "npc"),              # Aligns text to the left side of the table
  just = "left",                       # Left-justifies the text alignment
  gp = gpar(fontface = "italic", cex = 0.8) # Optional: makes text small and italic
)

# 2. Use grid.arrange to stack the table and the legend vertically
grid.arrange(
  trawl_statistics, 
  legend_text, 
  ncol = 1,                            # Arrange items in a single column
  heights = unit.c(unit(1, "null"), unit(1, "line")) # Gives table main space, legend 1 line space
)

grid.draw(trawl_statistics)

#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
ANOVA_statistic_table <- read.csv("eDNA_ANOVA_Statistics.csv", header = TRUE,  sep = ",", check.names = FALSE)

# Convert the raw symbol '⍴' to the word 'rho' so tableGrob's math engine can read it
colnames(ANOVA_statistic_table) <- gsub("⍴", "rho", colnames(ANOVA_statistic_table))


# Create Custom Theme and turn on 'parse = TRUE' to process mathematical expressions
table_grob <- ttheme_default(
  core = list(fg_params = list(col = "black")),
  colhead = list(fg_params = list(col = "blue2")),
  parse = TRUE #<-- IMPORTANT: Tells R to translate "rho" into the Greek letter 
)

# Pass the custom theme to your tableGrob
trawl_statistics <- tableGrob(ANOVA_statistic_table, theme = table_grob)

# --- ADD THESE LINES FOR THE LEGEND ---

# 1. Create a text box for the legend
legend_text <- textGrob(
  label = "*p < 0.05, **p < 0.005, ***p < 0.0005",
  x = unit(0.05, "npc"),              # Aligns text to the left side of the table
  just = "left",                       # Left-justifies the text alignment
  gp = gpar(fontface = "italic", cex = 0.8) # Optional: makes text small and italic
)

# 2. Use grid.arrange to stack the table and the legend vertically
grid.arrange(
  trawl_statistics, 
  legend_text, 
  ncol = 1,                            # Arrange items in a single column
  heights = unit.c(unit(1, "null"), unit(1, "line")) # Gives table main space, legend 1 line space
)

grid.draw(trawl_statistics)

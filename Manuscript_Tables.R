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
statistic_ranking_table <- read.csv("Biomass_eDNA_Statistics_Station.csv", header = TRUE, sep = ",")

# Replace underscores with spaces in the column headers
colnames(statistic_ranking_table) <- gsub("_", " ", colnames(statistic_ranking_table))

#Create Table, Customize, and View it
trawl_statistics <- tableGrob(statistic_ranking_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
                            colhead = list(fg_params = list(col = "blue")))

grid.draw(trawl_statistics)

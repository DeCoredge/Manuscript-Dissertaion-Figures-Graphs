#Clear current working environment
rm(list = ls())

#Instal package that allows you to make tables
install.packages("gt")
install.packages("grid")
install.packages("gridExtra")

library(grid)
library(gridExtra)

# Use data from csv file to create tables
primers_table <- read.csv("General_Primers.csv", header = TRUE, sep = ",")

#Replace N/A in table
primers_table<- replace(primers_table, is.na(primers_table), "")

#Create Table, Customize, and View it
trawl_primers <- tableGrob(primers_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
 colhead = list(fg_params = list(col = "blue")))

grid.draw(trawl_primers)


#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
trawl_table <- read.csv("Trawl_Surveys.csv", header = TRUE, sep = ",")

#Create Table, Customize, and View it
trawl_table <- tableGrob(trawl_table)

table_grob<- ttheme_default(core = list(fg_params = list(col = "grey")),
                            colhead = list(fg_params = list(col = "blue")))

grid.draw(trawl_table)


#Clear current working environment
rm(list = ls())


# Use data from csv file to create tables
species_table <- read.csv("Trawl_Species_List.csv", header = TRUE, sep = ",")

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

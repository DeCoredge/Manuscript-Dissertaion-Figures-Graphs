rm(list = ls())

library(ggplot2)
library(tidyr)

# with all the read Upload csv dta file for species abundance/read count proportions
fish_data<- read.csv("Fish_abundance_read_counts.csv")

#Add Primer/Trawl column to graph
fish_data<- pivot_longer(fish_data, cols = c("Abundance", "MiFish", "Leray"), 
                         names_to = "Trawl_Proportion_Primers", 
                         values_to = "Proportion_Abundance_Reads")
#Create Bar chart of abundance/read count proportions
fish_barchart<- ggplot()
fish_barchart<-fish_barchart + geom_col(data = fish_data, aes(x = Species,
                                y = Proportion_Abundance_Reads, 
                                fill = Trawl_Proportion_Primers), 
                                position= "dodge")
fish_barchart

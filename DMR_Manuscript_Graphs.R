rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

library(ggplot2)
library(tidyr)

# with all the read Upload csv data file for fish species abundance/read count proportions
fish_data<- read.csv("Fish_abundance_read_counts.csv")
fish_data$Species <- as.factor(fish_data$Species)
fish_data$Species <- factor(fish_data$Species, levels=fish_data$Species[order(-fish_data$Abundance)], ordered=TRUE)


#Add Primer/Trawl column to graph
fish_data_long<- pivot_longer(fish_data, cols = c("Abundance", 
                          "MiFish"), 
                         names_to = "Trawl_Proportion_Primers", 
                         values_to = "Proportion_Abundance_Reads")

fish_data_long$Trawl_Proportion_Primers <- factor(fish_data_long$Trawl_Proportion_Primers,
  levels = c("Abundance", "MiFish"),
  labels = c("Trawl Biomass", "MiFish 12S read counts"))

#Create Bar chart of abundance/read count proportions
fish_barchart<- ggplot(data = fish_data_long) + 
  geom_col(data = fish_data_long, aes(x = Species, y = Proportion_Abundance_Reads, 
     fill = Trawl_Proportion_Primers), position= "dodge") +
  theme_minimal() + #Clean background theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs( title = "Fish Relative Abundance Across All Trawls",
        x = "Species", y = "Proportion of Total Abundance", 
        fill = "Survey Type")

fish_barchart
print(fish_barchart)

#Upload cephalopod csv data file for cephalopod species abundance/read count proportions
ceph_data<- read.csv("Ceph_abundance_read_counts.csv")
ceph_data$Species <- as.factor(ceph_data$Species)
ceph_data$Species <- factor(ceph_data$Species, levels=ceph_data$Species[order(-ceph_data$Abundance)], ordered=TRUE)


#Add Primer/Trawl column to graph
ceph_data_long<- pivot_longer(ceph_data, cols = c("Abundance", "Ceph_18s"), 
                         names_to = "Trawl_Proportion_Primers", 
                         values_to = "Proportion_Abundance_Reads")

ceph_data_long$Trawl_Proportion_Primers <- factor(ceph_data_long$Trawl_Proportion_Primers,
          levels = c("Abundance", "Ceph_18s"),
          labels = c("Trawl Biomass", "Ceph18S read counts"))

#Create Bar chart of abundance/read count proportions
  ceph_barchart<- ggplot(data = ceph_data_long) + 
  geom_col(data = ceph_data_long, aes(x = Species, y = Proportion_Abundance_Reads, 
  fill = Trawl_Proportion_Primers), position= "dodge") +
  theme_minimal() + #Clean background theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs( title = "Cephalopod Relative Abundances Across All Trawls", 
        x = "Species", y = "Proportion of Total Abundance", 
        fill = "Survey Type")

ceph_barchart
print(ceph_barchart)

#Upload invertebrate csv data file for invertebrate species abundance/read count proportions & to test the invertebrate primer's detection abilities
invert_data<- read.csv("Invertebrate_abundance_read_counts.csv")
invert_data$Species <- as.factor(invert_data$Species)
invert_data$Species <- factor(invert_data$Species, levels=invert_data$Species[order(-invert_data$Abundance)], ordered=TRUE)


#Add Primer/Trawl column to graph
invert_data_long <- pivot_longer(invert_data, cols = c("Abundance", "Leray"), 
names_to = "Trawl_Proportion_Primers", values_to = "Proportion_Abundance_Reads")

invert_data_long$Trawl_Proportion_Primers <- factor(invert_data_long$Trawl_Proportion_Primers,
            levels = c("Abundance", "Leray"),
            labels = c("Trawl Biomass", "Leray COI read counts"))

#Create Bar chart of abundance/read count proportions
invert_barchart<- ggplot(data = invert_data_long) + 
  geom_col(data = invert_data_long, aes(x = Species, y = Proportion_Abundance_Reads, 
  fill = Trawl_Proportion_Primers), position= "dodge") + theme_minimal() + #Clean background theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs( title = "Invertebrate Relative Abundances Across All Trawls", x = "Species",
        y = "Proportion of Total Abundance", fill = "Survey Type")

invert_barchart
print(invert_barchart)

rm(list = ls())

library(ggplot2)
library(tidyr)

# with all the read Upload csv dta file for fish species abundance/read count proportions
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

#Upload cephalopod csv data file for cephalopod species abundance/read count proportions
ceph_data<- read.csv("Ceph_abundance_read_counts.csv")

#Add Primer/Trawl column to graph
ceph_data<- pivot_longer(ceph_data, cols = c("Abundance", "Ceph_18s"), 
                         names_to = "Trawl_Proportion_Primers", 
                         values_to = "Proportion_Abundance_Reads")
#Create Bar chart of abundance/read count proportions
ceph_barchart<- ggplot()
ceph_barchart<-ceph_barchart + geom_col(data = ceph_data, aes(x = Species,
                                                              y = Proportion_Abundance_Reads, 
                                                              fill = Trawl_Proportion_Primers), 
                                        position= "dodge")
ceph_barchart

#Upload invertebrate csv data file for invertebrate species abundance/read count proportions & to test the invertebrate primer's detection abilities
invert_data<- read.csv("Invertebrate_abundance_read_counts.csv")

#Add Primer/Trawl column to graph
invert_data<- pivot_longer(invert_data, cols = c("Abundance", "Leray"), 
                         names_to = "Trawl_Proportion_Primers", 
                         values_to = "Proportion_Abundance_Reads")
#Create Bar chart of abundance/read count proportions
invert_barchart<- ggplot()
invert_barchart<-invert_barchart + geom_col(data = invert_data, aes(x = Species,
                                                              y = Proportion_Abundance_Reads, 
                                                              fill = Trawl_Proportion_Primers), 
                                        position= "dodge")
invert_barchart

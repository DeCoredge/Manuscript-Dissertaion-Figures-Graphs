#Sample Site Map making for Manuscript & Dissertation

#Clear current working environment
rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")

#Install Packages needed to make U.S. State Maps & Oceans
install.packages(c("sf", "ggplot2", "usmap"))
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggOceanMaps")
install.packages("ggmap")
install.packages("marmap")
install.packages("ggnewscale")
library(sf)
library(ggplot2)
library(usmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggOceanMaps)
library(ggmap)
library(marmap)
library(ggnewscale)
library(ggpubr)

# Use csv file to upload coordinates of sampling locations from trawls
trawl_data <- read.csv("DMR_Inshore_Sample_Sites_Map.csv")

# FIX: Invert longitude values to be negative so they plot in the Western Hemisphere
trawl_data$Start_Longitude <- trawl_data$Start_Longitude * -1

# Define the boundaries of the Gulf of Maine
lon1 <- -71.1 # Min. Longitude
lon2 <- -62.28 # Max. Longitude
lat1 <- 39.65 # Min. Latitude
lat2 <- 46.02 # Max. Latitude

# Get bathymetric data of Gulf of Maine 
gom_bathy <- getNOAA.bathy(lon1 = lon1, lon2 = lon2, lat1 = lat1, 
                           lat2 = lat2, resolution = 1, keep = TRUE)

# Convert bathymetric data to a dataframe to plot
gom_bathy_df <- fortify(gom_bathy)

# Get coastline data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Prepare data frame for plotting adjustments
trawl_data_plot <- trawl_data
r5_indices <- which(trawl_data_plot$Region == 5)

# Scale Region 5 points so the western limit is exactly -67.43 and the eastern limit is exactly -67.0
r5_points <- trawl_data_plot$Start_Longitude[r5_indices]
min_orig <- min(r5_points)
max_orig <- max(r5_points)

# Shifted baseline to -67.43
trawl_data_plot$Start_Longitude[r5_indices] <- -67.43 + ((r5_points - min_orig) / (max_orig - min_orig)) * (-67.00 - (-67.43))

# Plot the map
base_map <- ggplot() +
  geom_raster(data = gom_bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colors = c("darkblue", "blue", "skyblue"), limits = c(-1000, 0),
                       oob = scales::squish,
                       name = "Depth (m)") +
  geom_sf(data = world, fill = "#EADEC9", color = "black") + # Add coastline
  geom_contour(data = gom_bathy_df, aes(x = x, y = y, z = z), 
               breaks = c(0, -100, -200), color = "darkgray", linetype = "dashed") + 
  
  # Visual boundary lines partitioning the 5 regions over the ocean
  geom_vline(xintercept = c(-70.18, -69.25, -68.32, -67.45), 
             color = "black", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = c(-70.35, -69.7, -68.8, -67.9, -67.2), y = 43.3, 
           label = c("Reg 1", "Reg 2", "Reg 3", "Reg 4", "Reg 5"), 
           color = "white", fontface = "bold", size = 3.5) +
  
  new_scale_fill() + 
  
  # FIXED: Maximized horizontal and vertical jitter settings
  geom_jitter(data = trawl_data_plot, 
              aes(x = Start_Longitude, y = Start_Latitude, fill = Season, shape = factor(Region)),
              color = "black", size = 3.5, stroke = 1.2,
              position = position_jitter(width = 0.11, height = 0.06, seed = 123)) +
  
  # Added override.aes so legend dots are fillable circles matching the map points
  scale_fill_manual(values = c("Fall" = "orange", "Spring" = "lightgrey"), 
                    name = "Trawl Season",
                    guide = guide_legend(override.aes = list(shape = 21))) +
  
  # Region 2 is mapped to shape 21 (circle). The legend dynamically updates.
  scale_shape_manual(values = c("1" = 21, "2" = 21, "3" = 23, "4" = 24, "5" = 25),
                     name = "Gulf of Maine Region") +
  
  # Crops map precisely to your study area limits matching the image
  coord_sf(xlim = c(-70.5, -66.5), ylim = c(43.2, 44.8), expand = FALSE) +
  labs(title = "Paired eDNA ~ ME-NH Inshore Trawl Sample Sites",
       x = "Longitude", y = "Latitude") +
  theme_bw()

# View the map
print(base_map)

#Map making for Manuscript & Dissertation

#Install Packages needed to make U.S. State Maps & Oceans
install.packages(c("sf", "ggplot2", "usmap"))
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggOceanMaps")
install.packages("ggmap")
install.packages("marmap")
library(sf)
library(ggplot2)
library(usmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggOceanMaps)
library(ggmap)
library(marmap)

#Define the boundaries of the Gulf of Maine
lon1 <- -71.1 # Min. Longitude
lon2 <- -62.28 # Max. Longitude
lat1 <- 39.65 # Min. Latitude
lat2 <- 46.02 # Max. Latitude

#Get bathymetric data of Gulf of Maine 
gom_bathy <- getNOAA.bathy(lon1 = lon1, lon2 = lon2, lat1 = lat1, lat2 = lat2, resolution = 5)

# Convert bathymetric data to a dataframe to plot
gom_bathy_df <- fortify(gom_bathy)

# Get coastline data
world <- ne_countries(scale = "medium", returnclass = "sf")

#Plot the map
base_map <- ggplot() +
  geom_raster(data = gom_bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue", "lightgreen", "darkgreen"),
                       values = scales::rescale(c(min(gom_bathy_df$z), 0, max(gom_bathy_df$z))),
                       name = "Depth (m)") +
  geom_sf(data = world, fill = "gray", color = "black") + # Add coastline
  geom_contour(data = gom_bathy_df, aes(x = x, y = y, z = z), breaks = c(0, -100, -200), color = "darkgray", linetype = "dashed") + # Add bathymetric contours
  coord_sf(xlim = c(lon1, lon2), ylim = c(lat1, lat2), expand = FALSE) +
  labs(title = "Map of 2023 ME-NH Inshore Trawl Survey of the Gulf of Maine",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
base_map # check to make sure that the base map looks alright

#Use csv file to upload coordinates of sampling locations from trawls
trawl_data <- read.csv("Maine_DMR_ALL_Trawl_Catch_Data.csv")

# data upload check
summary(trawl_data) # check that R read in the dataframe right
trawl_factors <- c("Survey", "Season", "Tow_Number", "Region", "Common_Name") # the columns that R didn't read in as factors
trawl_data[trawl_factors] <- lapply(trawl_data[trawl_factors], factor)# make these columns factors now
summary(trawl_data) # check that R read in the dataframe right


#Plot the points on the map
base_map + 
  geom_point(data = trawl_data, aes(x = Start_Longitude, y = Start_Latitude))

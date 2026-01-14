rm(list = ls())

setwd("C:/Users/DeCorey Bolton Jr/Documents/GitHub/Manuscript-Dissertaion-Figures-Graphs")
install.packages("marmap")
install.packages("shape")
library(marmap)
library(shape)
library(ggplot2)

#Coordinates for Gulf of Maine (GOM) & Mid-Atlantic Bight (MAB)
lon_min <- -77 #Western edge (Cape Hatteras)
lon_max <- -50 #Eastern edge (Newfoundland)
lat_min <- 34 #Southern edge (N. Carolina)
lat_max <- 46 #Northern edge (Maine/Canada)

#Download bathymetry data
bathy_data <- getNOAA.bathy(lon1 = lon_min, lon2 = lon_max, lat1 = lat_min, lat2 = lat_max, resolution = 10)

# Create nice looking color palette
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
tans <- c("khaki")

# plot map with geographic features (shelf, isobaths, coastline, etc.)
plot(bathy_data, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bathy_data), tans), c(min(bathy_data), 0, blues)), main = "Gulf of Maine & Mid-Atlantic Bight")

# highlight coastline
plot(bathy_data, lwd = 0.8, deep = 0, shallow = 0, step = 0, add = TRUE)

#Highlight isobaths
plot(bathy_data, lwd = 2.5, deep = -200, shallow = -200, step = 0, col="purple", add = TRUE)

#Create a Legend for water depth
colorlegend(zlim=c(min(bathy_data),0), col=blues, main="depth (m)", posx=c(0.82,0.84), posy=c(0.1,0.92))

#Use csv file to upload coordinates of sampling locations from trawls
trawl_data <- read.csv("Maine_DMR_Trawl_Catch_Data.csv")

# data upload check
summary(trawl_data) # check that R read in the dataframe right
trawl_factors <- c("Survey", "Season", "Tow_Number", "Region", "Common_Name") # the columns that R didn't read in as factors
trawl_data[trawl_factors] <- lapply(trawl_data[trawl_factors], factor)# make these columns factors now
summary(trawl_data) # check that R read in the data frame right

# Create a data frame for coordinates.
trawl_df <- data.frame(id = c("A", "A", "B", "B", "C", "C", "D", "D", "E", "E",
                              "F", "F", "G", "G", "H", "H", "I", "I", "J", "J", 
                              "K", "K"),
                       type = c("start", "end", "start", "end", "start", "end",
                                "start", "end", "start", "end", "start", "end",
                                "start", "end", "start", "end", "start", "end", 
                                "start", "end", "start", "end"),
                       Latitude = c(43.461, 43.450, 43.454, 43.444, 43.488, 
                                    43.477, 44.321, 44.312, 44.486, 44.473,
                                    43.487, 43.477, 43.461, 43.449, 43.455,
                                    43.444, 44.220, 44.233, 44.421, 44.412,
                                    44.483, 44.470),
                       Longitude = c(-69.836, -69.848 , -69.898, -69.912,
                                     -69.924, -69.937 , -67.559, -67.573, 
                                     -67.506, -67.513, -69.925, -69.938,
                                     -69.837, -69.848, -69.897, -69.910,
                                     -67.743, -67.737, -67.445, -67.460, 
                                     -67.508, -67.516))

#Set Factors and Numerics on Trawl_df
trawldf_factors <- c("id", "type")
trawl_df[trawldf_factors] <- lapply(trawl_df[trawldf_factors], factor)
summary(trawl_df)

#Plot the points on the map w/ the scale of the map fixed to have the coordinate points more visible
plot(bathy_data +
  geom_point(data = trawl_df, aes(x = Longitude, y = Latitude, 
                                  color = type), size = 5) +
  scale_colour_manual(values = c("yellow","purple")) +
  coord_sf(xlim = c(-77,-50), ylim = c(34,46)))

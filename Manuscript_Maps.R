#Plot the map
base_map <- ggplot() +
  geom_raster(data = gom_bathy_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colors = c( "darkblue", "blue", "skyblue"), limits = c(-1000, 0),
                       oob = scales::squish,
                       name = "Depth (m)") +
  geom_sf(data = world, fill = "#EADEC9", color = "black") + # Add coastline
  geom_contour(data = gom_bathy_df, aes(x = x, y = y, z = z), breaks = c(0, -100, -200), color = "darkgray", linetype = "dashed") + # Add bathymetric contours
  new_scale_fill() + # CHANGED: Added shape = factor(Region) inside aes() and removed hardcoded shape = 21
  geom_jitter(data = trawl_data, 
  aes(x = Start_Longitude, y = Start_Latitude, fill = Season, shape = factor(Region)),
  color = "black", size = 3.5, stroke = 1.2, width = 0.099, height = 0) +
  scale_fill_manual(values = c("Fall" = "orange", "Spring" = "lightgrey"), # Colorblind-friendly options and 77 at the end = ~46% transparent
             name = "Trawl Season") +
  # NEW: Legend for Region (Shapes 21-25 allow both border outlines and fills)
  scale_shape_manual(values = c("2" = 21, "5" = 24), 
                     name = "Gulf of Maine Region") + guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) + #ADD THIS LINE TO FIX THE LEGEND COLORS
  coord_sf(xlim = c(lon1, lon2), ylim = c(lat1, lat2), expand = FALSE) +
  labs(title = "Paired eDNA ~ ME-NH Inshore Trawl Sample Sites",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

base_map # check to make sure that the base map looks alright


#Plot the points on the map w/ the scale of the map fixed to have the coordinate points more visible
base_map_zoomed <- base_map +
  coord_sf(xlim = c(-70.5, -67), ylim = c(43.2, 44.8), expand = FALSE)

#View New zoomed in Map
base_map_zoomed
print(base_map_zoomed)

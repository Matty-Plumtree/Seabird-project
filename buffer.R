# These are plots of my Kincardine buffer to put in my appendix
ggplot() +
  geom_sf(data = kinc_buffer_wgs, fill = "lightblue", alpha = 0.4, color = "blue", linetype = "dashed") +
  geom_sf(data = FowlsH, fill = 'blue', color = 'blue') +
  geom_sf(data = CoastL, color = 'blue') +
  geom_sf(data = kinc_wgs, fill = "red", color = "darkred", size = 1.2) +
  geom_sf(data = E3breed12, fill = NA, color = "black", size = 0.5) +
  theme_minimal() +
  labs(title = "15 km Buffer around Kincardine OWF",
       x = "Longitude", y = "Latitude")

ggplot() +
  geom_sf(data = birdsf)+
  geom_sf(data = FowlsH, fill = 'blue', color = 'blue') +
  geom_sf(data = kinc, fill = "red", color = "darkred", size = 1.2) +
  geom_sf(data = E3breed12, fill = NA, color = "black", size = 0.5) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")



# Aggregate bird counts per location and removes NA
data_plot <- birdsf_sf2022 %>%
  filter(!is.na(Count.GU))  

# Reproject kinc and kinc_buffer to match the birdsf_sf CRS
kinc_proj <- st_transform(kinc, crs = st_crs(data_plot))
kinc_buffer_proj <- st_transform(kinc_buffer, crs = st_crs(data_plot))


ggplot() +
  geom_sf(data = kinc_proj, fill = "red", alpha = 0.4, color = "black") +
  geom_sf(data = kinc_buffer_proj, fill = NA, color = "blue", linetype = "dashed") +
  geom_sf(data = data_plot, aes(color = Count.GU), size = 1) +
  scale_color_viridis_c(name = "Guillemot Count") +
  labs(
    title = "Guillemot Count per Segment with OWF Boundary and Buffer",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()




# this filtered by month, this was used for data exploration and plotting rather than the actual GLMM
guillemot_april2023 <- birdsf %>%
  filter(CommonName == "Guillemot" & month == '2023-04') %>%
  mutate(month = "April")

guillemot_may2023 <- birdsf %>%
  filter(CommonName == "Guillemot" & month == '2023-05') %>%
  mutate(month = "May")

guillemot_june2023 <- birdsf %>%
  filter(CommonName == "Guillemot" & month == '2023-06') %>%
  mutate(month = "June")

guillemot_july2023 <- birdsf %>%
  filter(CommonName == "Guillemot" & month == '2023-07') %>%
  mutate(month = "July")

guillemot_august2023 <- birdsf %>%
  filter(CommonName == "Guillemot" & month == '2023-08') %>%
  mutate(month = "August")

# Combine all months
combined_guillemots <- bind_rows(
  guillemot_april2023, 
  guillemot_may2023, 
  guillemot_june2023, 
  guillemot_july2023, 
  guillemot_august2023
) %>%
  mutate(
    distance_km = distance / 1000,
    distance_band = cut(
      distance_km,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, Inf),
      labels = c("0–5", "5–10", "10–15", "15–20", "20–25",
                 "25–30", "30–35", "35–40", "40–45", ">45"),
      include.lowest = TRUE,
      right = FALSE
    ),
    month = factor(month, levels = c("April", "May", "June", "July", "August"))
  )

# Count by month and distance band
counts <- combined_guillemots %>%
  count(month, distance_band)

# Plot
ggplot(counts, aes(x = interaction(distance_band, month), y = n, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  facet_grid(. ~ distance_band, scales = "free_x", space = "free_x", switch = "x") +
  labs(
    subtitle = "Comparison of Guillemot count per distance band between each month in 2023",
    x = "Distance from Wind Farm (km)",
    y = "Count",
    fill = "Month"
  ) +
  ylim(0,11000) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10),
    panel.spacing.x = unit(1, "lines"),
    panel.grid.major.x = element_line(color = 'grey'),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
  )


#trying the trinder plot for visual differences on the map. 

grid <- st_make_grid(combined, cellsize = 1000, square = TRUE)
grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
grid_cropped <- st_intersection(grid_sf, kinc_buffer)

grid_2022 <- st_join(grid_cropped, guillemay2022) %>%
  group_by(grid_id) %>%
  summarise(count_2022 = n())

grid_2023 <- st_join(grid_cropped, guillemay2023) %>%
  group_by(grid_id) %>%
  summarise(count_2023 = n())

grid_diff <- grid_cropped %>%
  left_join(st_drop_geometry(grid_2022), by = "grid_id") %>%
  left_join(st_drop_geometry(grid_2023), by = "grid_id") %>%
  mutate(
    count_2022 = replace_na(count_2022, 0),
    count_2023 = replace_na(count_2023, 0),
    diff = count_2023 - count_2022
  )


ggplot(grid_diff) +
  geom_sf(aes(fill = diff)) +
  geom_sf(data = st_transform(kinc, 4326), fill = NA, color = "black", size = 1) +
  geom_sf(data = st_transform(E3breed12, 4326), fill = NA, color = "black", size = 1) +
  scale_fill_gradient2(
    low = "blue4", mid = "white", high = "firebrick",
    midpoint = 0,
    name = "Density change"
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Change in Guillemot Densities Within 15 km of OWF August 2022 & 2023",
    x = "Easting", y = "Northing"
  )


# Combine all months
combined1 <- bind_rows(guille2022, guille2023) %>%
  mutate(
    monthyear = factor(month),  
    distance_km = distance / 1000
  )

grid_joined <- st_join(grid_sf, combined1) %>%
  group_by(grid_id, monthyear) %>%
  summarise(
    count = n(),
    distance = mean(distance_km, na.rm = TRUE),
    area = 1 
  ) %>%
  ungroup()

grid_joined <- grid_joined %>%
  mutate(distance_scaled = scale(distance)[,1])


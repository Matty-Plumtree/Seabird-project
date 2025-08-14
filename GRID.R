
# Createed 200m x 200m square grid covering the full extent
grid_200m <- st_make_grid(
  birdsf_sf2022,
  cellsize = 200,
  what = "polygons",
  square = TRUE
) %>%
  st_sf(grid_id = seq_along(.))  

# Joined each segment (point) to the grid cell it falls in
birdsf_sf2022_with_grid <- st_join(
  birdsf_sf2022,
  grid_200m,
  join = st_within,
  left = FALSE  
)

# this was exploration to see if i need to average the predictors
env_covariates_summary <- birdsf_sf2022_with_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id, year, month) %>%
  summarise(
    total_gu = sum(Count.GU, na.rm = TRUE),
    total_effort = sum(effort, na.rm = TRUE),
    distance_to_coast = mean(d2c, na.rm = TRUE),
    distance_to_colony = mean(distance_to_fowl, na.rm = TRUE),
    seadepth = mean(depth, na.rm = TRUE),
    distance_OWF = mean(distance_to_OWF, na.rm = TRUE),
    .groups = "drop"
  )

# Join covariate averages back to each segment
birdsf_sf2022_envgrid <- left_join(
  birdsf_sf2022_with_grid,
  env_covariates_summary,
  by = "grid_id"
)


ggplot(env_covariates_summary) +
  geom_sf(aes(fill = total_gu), color = NA) +
  scale_fill_viridis_c(option = "C", na.value = "lightgrey") +
  labs(
    title = "Total Guillemot Count per 200m Grid Cell",
    fill = "Count"
  ) +
  theme_minimal()

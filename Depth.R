
# Load and process depthgrid
depthgrid <- file.depthgrid %>%
  read_stars() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(UTM) %>%
  mutate(area = drop_units(st_area(.)) / 1e6) %>%
  st_centroid() %>%
  rename(depth = `E3_envgrid.tif`) %>%
  mutate(
    depth = units::drop_units(depth),
    depth = ifelse(depth < 0, depth, NA),
    x.pos = st_coordinates(.)[, 1],
    y.pos = st_coordinates(.)[, 2]
  ) %>%
  filter(!is.na(depth))

# Read MRSea dataset
birddat <- read.csv("//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Aerial Survey Data/MRSea/E3_mrseadat_RT.csv")

# Create formatted dataset
dat <- birddat %>%
  mutate(x.pos = Easting, y.pos = Northing, area = effort)

# Convert to sf object and reproject
birdsf <- st_as_sf(dat, coords = c("Easting", "Northing"), crs = 32630) %>%
  st_transform(4326)

# Add coordinates back in
birdsf <- st_coordinates(birdsf) %>%
  cbind(birdsf)

# Extract depth from raster and assign to points
rasterdepth <- raster(file.depthgrid)
birdsf$depth <- raster::extract(rasterdepth, cbind(birdsf$X, birdsf$Y))

# Reproject for spatial filter
birdsf_sf <- st_as_sf(birdsf, coords = c("X", "Y"), crs = 4326) %>%
  st_transform(st_crs(kinc_buffer))


#This bands the depths that holds and average count e.g. 140 - 130 will have an average count
birdsf_sf3 <- birdsf_sf %>%
  filter(!is.na(depth), year == 2023, depth <= 0) %>%
  mutate(
    depth_band = cut(
      depth,
      breaks = c(-140, seq(-130, 0, by = 10)),  
      right = FALSE,
      labels = c(
        "-140 to -130", "-130 to -120", "-120 to -110", "-110 to -100",
        "-100 to -90", "-90 to -80", "-80 to -70", "-70 to -60", "-60 to -50",
        "-50 to -40", "-40 to -30", "-30 to -20", "-20 to -10", "-10 to 0"
      )
    )
  )

# Summarise mean guillemot count per depth band per month
depth_summary <- birdsf_sf3 %>%
  group_by(month, depth_band) %>%
  summarise(mean_count = mean(Count.KI, na.rm = TRUE), .groups = "drop")


depth_summary$depth_band <- factor(
  depth_summary$depth_band,
  levels = c(
    "-140 to -130", "-130 to -120", "-120 to -110", "-110 to -100",
    "-100 to -90", "-90 to -80", "-80 to -70", "-70 to -60", "-60 to -50",
    "-50 to -40", "-40 to -30", "-30 to -20", "-20 to -10", "-10 to 0"
  )
)


ggplot(depth_summary, aes(x = interaction(depth_band, month), y = mean_count, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  facet_grid(. ~ depth_band, scales = "free_x", space = "free_x", switch = "x") +
  facet_wrap( ~ depth_band, nrow = 2, strip.position = 'bottom')+
  labs(
    subtitle = "Mean Kittiwake Count per Sea Depth Band (by Month) for 2023",
    x = "Sea Depth Band (m)",
    y = "Mean Count per Segment",
    fill = "Month"
  ) +
  ylim(0, 200) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10),
    panel.spacing.x = unit(1.5, "lines"),
    panel.grid.major.x = element_line(color = 'grey'),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
  )




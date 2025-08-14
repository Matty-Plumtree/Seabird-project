# This applys a minimum distance to the closest point of the coastline to every bird sitting
birdsf_sf$dist_to_coast <- as.numeric(st_distance(birdsf_sf, CoastL)%>%
                                        apply(1, min))

birdsf_sf$d2c <- birdsf_sf$dist_to_coast / 1000


# Filterthe NA's
birdsf_sf_6 <- birdsf_sf %>%
  filter(
    year == 2023,
    !is.na(d2c),
    !is.na(Count.GX),
    d2c >= 0
  ) %>%
  mutate(
    d2c_km = floor(d2c)  
  )

# Summarise mean guillemot count per 1 km band and month
d2c_summary <- birdsf_sf_6 %>%
  group_by(month, d2c_km) %>%
  summarise(mean_count = mean(Count.GX, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(mean_count))



ggplot(d2c_summary, aes(x = d2c_km, y = mean_count, fill = month)) +
  geom_col(color = "black") +
  facet_wrap(~ month, nrow = 2, scales = "free_y") +
  labs(
    title = "Mean Gannet Count per 1 km Distance to Coast (2023)",
    x = "Distance to Coast (km)",
    y = "Mean Count per Segment",
    fill = "Month"
  ) +
  ylim(0, 5) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 10),
    panel.grid.major.x = element_line(color = 'grey90'),
    panel.grid.minor.x = element_blank()
  )





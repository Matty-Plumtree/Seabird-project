# this is the exact same as filterring by month, here i just plot how sea depth affects distribution across month
razorbill_april2022 <- birdsf_sf %>%
  filter(CommonName == "Gannet" & month == '2023-04') %>%
  mutate(month = "April")
razorbill_may2022 <- birdsf_sf %>%
  filter(CommonName == "Gannet" & month == '2023-05') %>%
  mutate(month = "May")
razorbill_june2022 <- birdsf_sf %>%
  filter(CommonName == "Gannet" & month == '2023-06') %>%
  mutate(month = "June")
razorbill_july2022 <- birdsf_sf %>%
  filter(CommonName == "Gannet" & month == '2023-07') %>%
  mutate(month = "July")
razorbill_august2022 <- birdsf_sf %>%
  filter(CommonName == "Gannet" & month == '2023-08') %>%
  mutate(month = "August")


combined_razorbill <- bind_rows(
  razorbill_april2022,
  razorbill_may2022,
  razorbill_june2022,
  razorbill_july2022,
  razorbill_august2022
) %>%
  mutate(
    month = factor(month, levels = c("April", "May", "June", "July", "August"))
  )

# Define breaks
min_depth <- floor(min(combined_razorbill$depth, na.rm = TRUE) / 10) * 10
breaks <- seq(0, min_depth, by = -10)
labels <- paste0(head(breaks, -1), " to ", tail(breaks, -1), " m")

combined_razorbill <- combined_razorbill %>%
  mutate(
    depth_band = cut(
      depth,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )

#Remove NAs
counts_depth <- combined_razorbill %>%
  filter(!is.na(depth_band)) %>%
  count(month, depth_band)


counts_depth$depth_band <- factor(counts_depth$depth_band, levels = labels)


ggplot(counts_depth, aes(x = depth_band, y = n, fill = month)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  labs(
    subtitle = "Comparison of Gannet count per sea depth band between each month in 2023",
    x = "Sea Depth Band (m)",
    y = "Count",
    fill = "Month"
  ) +
  ylim(0, 1000) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 10),
    panel.spacing.x = unit(1, "lines"),
    panel.grid.major.x = element_line(color = 'grey'),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
  )


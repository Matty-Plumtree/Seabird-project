
# this is where i explored sitting vs flying seabirds however, nothing was significant

guillefly2022<-birdsf%>%
  filter(CommonName=="Guillemot" & month %in% c('2022-03', '2022-04', '2022-05', '2022-06', '2022-07', '2022-08', '2022-09', '2022-10', '2022-11', '2022-12'))

guillefly2023<-birdsf%>%
  filter(CommonName=="Guillemot" & month %in% c('2023-03', '2023-04', '2023-05', '2023-06', '2023-07', '2023-08', '2023-09', '2023-10', '2023-11', '2023-12'))

combined_behaviour <- bind_rows(
  guillefly2022 %>% mutate(year = "2022"),
  guillefly2023 %>% mutate(year = "2023")
) %>%
  mutate(
    distance_km = distance / 1000,
    distance_band = cut(
      distance_km,
      breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", "20–25 km", "25–30 km", ">30 km"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

behaviour_summary <- combined_behaviour %>%
  group_by(year, distance_band, Behaviour) %>%
  summarise(n = n()) %>%
  ungroup()

behaviour_prop <- behaviour_summary %>%
  group_by(year, distance_band) %>%
  mutate(prop = n / sum(n))

ggplot(behaviour_prop, aes(x = distance_band, y = prop, fill = Behaviour)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~year) +
  labs(
    title = "Proportion of Guillemot Behaviour by Distance (Mar–Dec)",
    x = "Distance to Wind Farm (km)",
    y = "Proportion of Behaviour",
    fill = "Behaviour"
  ) +
  theme_minimal()


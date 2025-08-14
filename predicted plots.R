# Define months to include and keep in order
months <- c("April", "May", "June", "July", "August")

# Build prediction grid for both years
prediction_input <- expand.grid(
  distance_to_OWF = seq(0, 20, length.out = 100),
  month = months,
  year = c("2022", "2023")
) %>%
  mutate(
    scale_depth = 0,
    scale_depth2 = 0,
    scale_d2c = 0,
    scale_d2c2 = 0,
    scale_fowl = 0,
    effort = 1,
    grid_id = NA,
    month = factor(month, levels = months),
    year = as.factor(year)
  )

# Predict from glmmTMB with SE
preds <- predict(
  model_tmb,
  newdata = prediction_input,
  type = "response",
  se.fit = TRUE
)

# Add fitted values and confidence intervals
prediction_input$fit <- preds$fit
prediction_input$se <- preds$se.fit
prediction_input$lower <- prediction_input$fit - 1.96 * prediction_input$se
prediction_input$upper <- prediction_input$fit + 1.96 * prediction_input$se

# Prepare observed data
observed_data <- birdsf_sf2022_envgrid %>%
  filter(month %in% months, year %in% c("2022", "2023")) %>%
  mutate(
    year = as.factor(year),
    month = factor(month, levels = months)
  )


ggplot() +
  geom_point(data = observed_data,
             aes(x = distance_to_OWF, y = Count.GU, color = year),
             alpha = 0.1, shape = 16, size = 1) +
  geom_ribbon(data = prediction_input,
              aes(x = distance_to_OWF, ymin = lower, ymax = upper, fill = year, group = interaction(year, month)),
              alpha = 0.5, color = NA) +
  geom_line(data = prediction_input,
            aes(x = distance_to_OWF, y = fit, color = year, group = interaction(year, month)),
            size = 1.2) +
  scale_color_manual(values = c("2022" = "black", "2023" = "#d73027")) +
  scale_fill_manual(values = c("2022" = "black", "2023" = "#d73027")) +
  facet_wrap(~month, ncol = 2) +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 150)) +
  labs(
    title = "Guillemot Abundance by Distance to OWF",
    x = "Distance to OWF (km)",
    y = "Abundance",
    color = "Year",
    fill = "Year"
  ) +
  theme_minimal(base_size = 14)

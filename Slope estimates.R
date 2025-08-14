coefs <- summary(model_tmb)$coefficients$cond
months <- c("April", "May", "June", "July", "August")


slope_data_2022 <- lapply(months, function(m) {
  base_est <- coefs["distance_to_OWF", "Estimate"]
  base_se  <- coefs["distance_to_OWF", "Std. Error"]
  base_p   <- coefs["distance_to_OWF", "Pr(>|z|)"]
  
  if (m == "April") {
    slope <- base_est
    se <- base_se
    pval <- base_p
  } else {
    term <- paste0("distance_to_OWF:month", m)
    slope <- base_est + coefs[term, "Estimate"]
    se <- sqrt(base_se^2 + coefs[term, "Std. Error"]^2)
    pval <- coefs[term, "Pr(>|z|)"]
  }
  
  data.frame(Month = m, Slope = slope, SE = se, pval = pval)
}) %>% bind_rows()

slope_data_2022$Month <- factor(slope_data_2022$Month, levels = months)


slope_data_2023 <- lapply(months, function(m) {
  base <- coefs["distance_to_OWF", "Estimate"]
  base_se <- coefs["distance_to_OWF", "Std. Error"]
  year_eff <- coefs["distance_to_OWF:year2023", "Estimate"]
  year_se  <- coefs["distance_to_OWF:year2023", "Std. Error"]
  
  if (m == "April") {
    slope <- base + year_eff
    se <- sqrt(base_se^2 + year_se^2)
    pval <- coefs["distance_to_OWF:year2023", "Pr(>|z|)"]
  } else {
    month_term <- paste0("distance_to_OWF:month", m)
    month_year_term <- paste0("distance_to_OWF:month", m, ":year2023")
    
    month_eff <- coefs[month_term, "Estimate"]
    month_se  <- coefs[month_term, "Std. Error"]
    my_eff <- coefs[month_year_term, "Estimate"]
    my_se  <- coefs[month_year_term, "Std. Error"]
    
    slope <- base + year_eff + month_eff + my_eff
    se <- sqrt(base_se^2 + year_se^2 + month_se^2 + my_se^2)
    pval <- coefs[month_year_term, "Pr(>|z|)"]
  }
  
  data.frame(Month = m, Slope = slope, SE = se, pval = pval)
}) %>% bind_rows()

slope_data_2023$Month <- factor(slope_data_2023$Month, levels = months)


plot_2022 <- ggplot(slope_data_2022, aes(x = Month, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(color = "#1f78b4", size = 3) +
  geom_errorbar(aes(ymin = Slope - 1.96 * SE, ymax = Slope + 1.96 * SE),
                width = 0.2, color = "#1f78b4") +
  geom_text(aes(label = paste0("p = ", formatC(pval, digits = 3, format = "g"))),
            vjust = -1.2, size = 3.5) +
  labs(
    title = "2022",
    y = "Slope estimate (log-scale)"
  ) +
  ylim(-0.25, 0.25) +
  theme_minimal(base_size = 14)


plot_2023 <- ggplot(slope_data_2023, aes(x = Month, y = Slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = Slope - 1.96 * SE, ymax = Slope + 1.96 * SE),
                width = 0.2, color = "red") +
  geom_text(aes(label = paste0("p = ", formatC(pval, digits = 3, format = "g"))),
            vjust = -1.2, size = 3.5) +
  labs(
    title = "2023",
    y = "Slope estimate (log-scale)",
    x = "Month"
  ) +
  ylim(-0.25, 0.25) +
  theme_minimal(base_size = 14)


combined_plot <- plot_2022 / plot_2023


combined_plot

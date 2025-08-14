# adding in the scaled variables to match with the distance to OWF km
birdsf_sf2022_envgrid <- birdsf_sf2022_with_grid %>%
  mutate(
    scale_depth = scale(depth),
    scale_d2c = scale(d2c),
    scale_fowl = scale(distance_to_fowl)
  )

birdsf_sf2022_envgrid <- birdsf_sf2022_envgrid %>%
  mutate(
    scale_depth = scale(depth),
    scale_depth2 = scale_depth^2,
    scale_d2c = scale(d2c),
    scale_d2c2 = scale_d2c^2,
    scale_fowl = scale(distance_to_fowl)
  )            


# This is the main GLMM that i've used for my analysis
model_tmb <- glmmTMB(
  Count.GU ~
    distance_to_OWF * month * year +
    scale_depth * month * year + scale_depth2 * month * year +
    scale_d2c * month * year + scale_d2c2 * month * year +
    scale_fowl * month * year +
    offset(log(effort)) +
    (1 | grid_id),
  data = birdsf_sf2022_envgrid,
  family = poisson(),
  control = glmmTMBControl(
    optCtrl = list(iter.max = 500, eval.max = 500, trace = 1)
  )
)

summary(model_tmb)


?glmmTMB


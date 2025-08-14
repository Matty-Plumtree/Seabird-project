
# add the response and offset var since these are needed to be expressed in the model

wfdata <- birdsf_sf2022 %>%
  rename(
    response = Count.GU,        
    offsetvar = area            
  )

# the initial glm
initialModel <- glm(response ~ distance_to_OWF * month * year + offset(log(offsetvar)), 
                    family = "quasipoisson", 
                    data = wfdata)

# this is where you have to put the predictors in MRSEA
varlist <- c("depth", "d2c", 'distance_to_fowl')

# this is where you have to choose your knots based on the size of your data, here because of such a large dataset
# ive used the max knots
salsa1dlist <- list(fitnessMeasure = "QBIC", 
                    minKnots_1d = rep(3, length(varlist)),
                    maxKnots_1d = rep(6, length(varlist)), 
                    startKnots_1d = rep(3, length(varlist)), 
                    degree = rep(2, length(varlist)),
                    gaps = rep(0, length(varlist)))

# Running the SALSA 1D smoothing algorithm 
salsa1dOutput.multi <- runSALSA1D(initialModel = initialModel, 
                                  salsa1dlist = salsa1dlist,
                                  varlist = varlist, 
                                  datain = wfdata,
                                  suppress.printout = TRUE)
# this is the summary you get in which you get your estimates
summary(salsa1dOutput.multi$bestModel)
summary(salsa1dOutput.multi$modelFits1D)
salsa1dOutput$bestModel$splineParams[[2]]$knots

# this is the partial plots that help determine if the predictors do affect distribution of seabirds
runPartialPlots(model = salsa1dOutput.multi$bestModel, data = wfdata,
                varlist.in = varlist,
                showKnots = TRUE,
                type = 'link',
                includeB0 = TRUE)
?runPartialPlots
runPartialPlots(model = salsa1dOutput$bestModel, data = wfdata, 
                varlist = 'depth', 
                showKnots = TRUE, type='response', 
                includeB0 = TRUE)


wfdata <- wfdata %>%
  mutate(x_km = x.pos / 1000,
         y_km = y.pos / 1000)



ggplot(wfdata) +
  geom_tile(aes(x = x_km, y = y_km, fill = response,
                height = offsetvar, width = offsetvar)) +
  scale_fill_distiller(palette = "Spectral", name = "No. Birds") +
  labs(
    title = "Raw Guillemot survey counts by transect segment 2022",
    x = "Easting (km)", y = "Northing (km)"
  ) +
  theme_bw()


table(birdsf_sf2022$X)



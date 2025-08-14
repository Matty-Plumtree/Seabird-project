# I've basically used this section to initialise all my data since the pc that i was working on kept quitting

# transforms everything into the correct UTM
kinc_utm <- st_transform(kinc, crs = 32630) 


kinc_buffer <- st_buffer(kinc_utm, dist = 15000)

kinc_buffer_wgs <- st_transform(kinc_buffer, crs = 4326)
kinc_wgs <- st_transform(kinc_utm, crs = 4326)

#Initialise depth grid and transform birdsf
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


birddat <- read.csv("//EDIN-OP-03/Projects/Jobs/ECO03021 TWP E3 ornithology/Technical/Data/Aerial Survey Data/MRSea/E3_mrseadat_RT.csv")

# adds coordinates
dat <- birddat %>%
  mutate(x.pos = Easting, y.pos = Northing, area = effort)


birdsf <- st_as_sf(dat, coords = c("Easting", "Northing"), crs = 32630) %>%
  st_transform(4326)


birdsf <- st_coordinates(birdsf) %>%
  cbind(birdsf)

# prepping the depth grid
rasterdepth <- raster(file.depthgrid)
birdsf$depth <- raster::extract(rasterdepth, cbind(birdsf$X, birdsf$Y))


birdsf_sf <- st_as_sf(birdsf, coords = c("X", "Y"), crs = 4326) %>%
  st_transform(st_crs(kinc_buffer))

#initialise distance to coast
birdsf_sf$dist_to_coast <- as.numeric(st_distance(birdsf_sf, CoastL)%>%
                                        apply(1, min))

birdsf_sf$d2c <- birdsf_sf$dist_to_coast / 1000


#Initialise for Distance to OWF and convert all the rest into KM
centreFowl <- st_centroid(FowlsH)


birdsf_sf$dist_to_fowl <- as.numeric(st_distance(birdsf_sf, centreFowl))

birdsf_sf$distance_to_fowl <- birdsf_sf$dist_to_fowl / 1000


birdsf_sf$dist_to_OWF <- as.numeric(st_distance(birdsf_sf, kinc))

birdsf_sf$distance_to_OWF <- birdsf_sf$dist_to_OWF / 1000


birdsf_sf <- birdsf_sf %>%
  filter(distance_to_OWF < 15)


birdsf_sf <- birdsf_sf %>%
  filter(substr(month, 6, 7) %in% c("04", "05", "06", "07", "08")) %>%
  mutate(
    year = substr(month, 1, 4),
    month_num = substr(month, 6, 7),
    month = factor(month_num, levels = c("04", "05", "06", "07", "08"),
                   labels = c("April", "May", "June", "July", "August")),
    year = as.factor(year)
  )

birdsf_sf$month <- factor(birdsf_sf$month, levels = c('April', 'May', 'June', 'July', 'August'))                    

#make it so it only targets seadepth greater than 0
birdsf_sf2022 <- birdsf_sf %>%
  filter(depth <= 0)
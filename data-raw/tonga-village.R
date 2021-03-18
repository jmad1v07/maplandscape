library(sf)

tonga_village <- sf::st_read("data-raw/2016_PHC_Tonga_Village_4326.geojson") %>% 
  select(libgeo) %>%
  rename(
    zone = libgeo
  )

sf::st_crs(tonga_village) <- 4326

usethis::use_data(tonga_village, overwrite = TRUE)

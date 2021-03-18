library(sf)

tonga_district <- sf::st_read("data-raw/2016_PHC_Tonga_District_4326.geojson") %>% 
  select(libgeo) %>%
  rename(
    zone = libgeo
  )

sf::st_crs(tonga_district) <- 4326

usethis::use_data(tonga_district, overwrite = TRUE)
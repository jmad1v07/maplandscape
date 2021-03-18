library(sf)

tonga_block <- sf::st_read("data-raw/2016_PHC_Tonga_Block_4326.geojson") %>% 
  select(blkid) %>%
  rename(
    zone = blkid
  )

sf::st_crs(tonga_block) <- 4326

usethis::use_data(tonga_block, overwrite = TRUE)

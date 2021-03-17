library(sf)

tonga_block <- sf::st_read("data-raw/2016_PHC_Tonga_Block_4326.geojson") %>% 
  select(blkid) %>%
  rename(
    zone = blkid
  )

usethis::use_data(tonga_block, overwrite = TRUE)

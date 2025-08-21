drone_outlines = 
  list.files("imagery", full.names = T, pattern = "ndvi.tif") %>% 
  purrr::map(
    ~stars::read_stars(.x) %>% 
      st_as_sf(use_points = F) %>% 
      st_union() %>% 
      st_sf() %>% 
      mutate(fn = basename(.x)),
    .progress = T
  ) %>% 
  bind_rows()

drone_outlines = 
  drone_outlines %>% st_transform(4326) %>% 
  mutate(fn = stringr::str_extract(fn, "^[0-9]+_"))

write_sf(drone_outlines, "imagery/drone_outlines.geojson", delete_dsn = T)


fl_eco_all = st_read("fl_eco_l4/fl_eco_l4.shp") %>% 
  st_transform(4326)

as_tibble(fl_eco_all)
ctrs = tbl(craft_con, "expunitids_view") %>% 
  select(centroid) %>% 
  collect()

ctr_bufs = ctrs %>% 
  mutate(centroid = st_as_sfc(centroid)) %>% 
  filter(!st_is_empty(centroid)) %>% 
  st_sf(crs = 4326) %>% 
  st_transform(3857) %>% 
  st_buffer(units::set_units(500, "m")) %>% 
  st_union() %>% 
  st_transform(4326)

fl_eco_all %>% 
  st_filter(ctr_bufs) %>% 
  group_by(US_L4NAME) %>% 
  st_write("fl_eco.geojson")


st_read("fl_eco_l4/Florida_County_Boundaries_with_FDOT_Districts_6074369993038631266.geojson") %>% 
  st_filter(ctr_bufs)

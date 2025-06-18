library(httr)
library(DBI)
library(dplyr)
library(plotly)
library(sf)

source("secrets.R")
source("loginAPI.R")




craft_con <- dbConnect(
  RPostgres::Postgres(),
  user = dstadmin_creds$user,
  password = dstadmin_creds$password,
  host = dstadmin_creds$host,
  dbname = dstadmin_creds$dbname,
  sslmode = "require"
)


# forms_con <- dbConnect(
#   RPostgres::Postgres(),
#   user = dstadmin_creds$user,
#   password = dstadmin_creds$password,
#   host = dstadmin_creds$host,
#   dbname = "craft_forms",
#   sslmode = "require"
# )

user_base <- 
  tbl(craft_con, "access_view") %>% 
  select(user_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(password_dummy = "")

drone_ids <- 
  tbl(craft_con, "drone_rasters") %>% 
  collect()

# TODO: check these match
# drone_outlines <- st_read("imagery/drone_outlines.geojson")
ecoregions = st_read("fl_eco.geojson")


get_expunits <- function(contracts) {
  res <- tbl(craft_con, "expunitids_view") %>% 
    inner_join(
      tbl(craft_con, "contracts") %>% select(contract, year), 
      by = "contract"
      ) %>% 
    left_join(
      tbl(craft_con, "rootstocks") %>% 
        rename(rlabel = label) %>% 
        select(-uid),
      by = "rootstock"
    ) %>% 
    left_join(
      tbl(craft_con, "scions") %>% 
        rename(slabel = label) %>% 
        select(-uid),
      by = "scion"
    ) %>% 
    left_join(
      tbl(craft_con, "drone_rasters") %>% 
        distinct(contract) %>% 
        mutate(imagery = T),
      by = "contract"
    )
  
  if (length(contracts)) {
    res <- filter(res, contract %in% contracts) 
  }

  res = res %>% 
    collect() %>% 
    mutate(
      centroid = st_as_sfc(centroid),
      geometry = st_as_sfc(geometry)
    ) %>% 
    filter(!st_is_empty(centroid))%>% 
    # mutate(centroid = centroid + runif(nrow(.), min = 0.008, max = 0.008)) %>% 
    st_as_sf(crs = 4326) %>% 
    st_zm()
  
  res  
}



quietly_relevel_others <- function(.f) {
  if ("Others" %in% levels(.f)) {
    forcats::fct_relevel(.f, "Others", after = Inf)
  } else {
    .f
  }
}

make_map <- function(plots) {
  #browser()
  if (!nrow(plots)) {
    return( 
      leaflet(ecoregions) %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>% 
        addPolygons(opacity = 0, fillOpacity = 0)
        )
  }
  leaflet(plots) %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>% 
    addCircleMarkers(
      lat = ~st_coordinates(centroid)[,2],
      lng = ~st_coordinates(centroid)[,1],
      popup = ~contract,
      group = "centroids",
      layerId = ~contract,
      color = ~ifelse(!is.na(imagery), '#f1a340', '#998ec3'), #PuOr
      opacity = 0.7, fillOpacity = 0.3
    ) %>% 
    addPolygons(
      data = plots %>% select(geometry, contract, imagery), 
      group = "plots",
      layerId = ~contract,
      color = ~ifelse(!is.na(imagery), '#f1a340', '#998ec3'), #PuOr
      opacity = 0.7, fillOpacity = 0.3
    ) %>% 
    hideGroup("plots")
}

make_pie <- function(dat, col_id) {
  #browser()
  d = dat %>%
    group_by({{col_id}}) %>%
    summarize(acres = round(sum(area_acres)), .groups = "drop") %>%
    arrange(-acres) %>% 
    mutate(rn = row_number()) %>% 
    mutate(
      col = replace({{col_id}}, rn > 5, "Others"),
      rn = replace(rn, rn > 5, 6),
      col = forcats::fct_reorder(col, -acres),
      col = quietly_relevel_others(col) %>% 
        forcats::fct_rev(),
    ) %>% 
    group_by(col, rn) %>%
    summarize(acres = round(sum(acres)), .groups = "drop")

  d %>% 
    plot_ly(
      labels = ~col, values = ~acres, 
      type = "pie",
      text = ~col,
      # marker = list(
      #   colors = scales::brewer_pal(palette = "Set3")(nrow(d)) %>% 
      #     setNames(d$col)
      # ),
      hovertemplate = "%{value:,}ac<extra></extra>",
      showlegend = F
    ) %>%
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F)
  
}

make_bar <- function(dat, col, theme) {
  textcolor = if (theme == "dark") "white" else "black"
  d <- dat %>%
    group_by({{col}}) %>%
    summarize(acres = round(sum(area_acres)), .groups = "drop") %>%
    arrange(-acres) %>% 
    mutate(rn = row_number()) %>% 
    mutate(
      col = replace({{col}}, rn > 5, "Others"),
      rn = replace(rn, rn > 5, 6),
      col = forcats::fct_reorder(col, -acres),
      col = quietly_relevel_others(col) %>% 
        forcats::fct_rev()
    ) %>% 
    group_by(col, rn) %>%
    summarize(acres = round(sum(acres)), .groups = "drop") %>%
    arrange(rn)

  d %>% 
    plot_ly(hoverinfo = "text") %>% 
    add_bars(
      x = ~acres, y = ~col, color = ~col, 
      colors = scales::brewer_pal(palette = "Set3")(nrow(d)+1)[-2] %>% 
        setNames(d$col),
      orientation = 'h',
      showlegend = F
    ) %>%
    add_text(
      x = 0, y = ~col, 
      text = ~paste0(" <b>", col, "</b>: ", scales::label_comma()(acres), "ac"),
      textposition = "middle right"
    ) %>% 
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = NULL,
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = textcolor),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = F)
  
}
quietly_st_union = function(...) {
  ret = purrr::quietly(st_union)(...)
  ret$result
}

make_bbox_obj = function(...) {
  st_bbox(...) %>% st_as_sfc() %>% st_transform(4326)
}

make_raster_list <- function(ct, mt) {
  fn_df = drone_ids %>% 
    filter(contract == ct) %>% 
    filter(metric == "ndvi") %>%    # TODO: use `mt` here
    mutate(aero_date = lubridate::as_date(aero_date)) %>% 
    arrange(aero_date) %>% 
    group_by(aero_date)

  ht = ifelse(n_groups(fn_df) <= 2, "84vh", "42vh")
  
  dplyr::group_map(
    fn_df,
    ~{
      
      l = leaflet(
        options = leafletOptions(attributionControl = F), 
        height = ht 
        ) %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.DarkMatterOnlyLabels") 
      
      bb = st_point() %>% st_sfc(crs = 4326) 
      
      for (filename in .x$fn) {
        rs = stars::read_stars(file.path("imagery", filename))
        bb_ = rs %>% make_bbox_obj() 
        bb = quietly_st_union(bb, bb_) %>% make_bbox_obj()
        outline = drone_outlines %>% filter(fn == filename)
        eu = filter(.x, fn == filename) %>% pull(expunitid)
        
        l = leafem::addStarsImage(
          l, x = rs, 
          colors = viridis::inferno(256), 
          layerId = filename
          ) %>% 
          addPolygons(
            data = outline, layerId = eu, popup = eu, label = eu,
            fill = T, fillColor = "#00000000", color = "white"
            )
      }
      
      l %>% 
        addControl(tags$span(.y), position = "bottomleft") %>% 
        leafem::addHomeButton(
          ext = st_bbox(bb), group = "🏠",
          css = list(
            opacity = 1, 
            "font-size" = "125%", 
            "background-color" = "white"
            )
        )
    }
  )
}
# box_wkt = glue::glue_data(
#   input$map_bounds,
#   "BOX({west} {south}, {east} {north})"
# )
# SELECT * from expunitids where geometry && box_wkt
# 
# political map on main view, imagery when zoomed in
# when clicking on contract in the map, show all the exp metadata
#     even if not logged in
# even if not logged in, let them zoom in and see outlines
# no login at all, just do it publicly
# filter by county or ecoregion 
#   (the river, the ridge, the flatwoods (east, central, west/south)) 
#   or trialgroup
#   across the top (in the title bar?)
# they eventually want cost and harvest info
# look up a scion/rs combo, get stats on them, results etc
# look up a treatment combo, get stats, etc
# swap scion and rootstock positions
# compute trees per acre for sites with drone imagery
# "Can we add an explanation of the difference between the NDVI and NDRE also a color legend"
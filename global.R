library(httr)
library(DBI)
library(dplyr)
library(plotly)

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


forms_con <- dbConnect(
  RPostgres::Postgres(),
  user = dstadmin_creds$user,
  password = dstadmin_creds$password,
  host = dstadmin_creds$host,
  dbname = "craft_forms",
  sslmode = "require"
)

user_base <- 
  tbl(craft_con, "access_view") %>% 
  select(user_id) %>% 
  distinct() %>% 
  collect() %>% 
  mutate(password_dummy = "")

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
    )
  
  if (length(contracts)) {
    res <- filter(res, contract %in% contracts) 
  }

  res %>% 
    collect() %>% 
    mutate(
      centroid = st_as_sfc(centroid),
      geometry = st_as_sfc(geometry)
    ) %>% 
    filter(!st_is_empty(centroid)) %>% 
    # mutate(centroid = centroid + runif(nrow(.), min = 0.008, max = 0.008)) %>% 
    st_as_sf(crs = 4326) %>% 
    st_zm()
  
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
  leaflet(plots) %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>% 
    addCircleMarkers(
      lat = ~st_coordinates(centroid)[,2],
      lng = ~st_coordinates(centroid)[,1],
      popup = ~contract,
      group = "centroids"
    ) %>% 
    addPolygons(
      data = plots %>% select(geometry), 
      group = "plots"
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

get_images <- function(con, ct) {
  
}
# box_wkt = glue::glue_data(
#   input$map_bounds,
#   "BOX({west} {south}, {east} {north})"
# )
# SELECT * from expunitids where geometry && box_wkt
# 
# political map on main view, imagery when zoomed in
# Total groves planted -> "Total acres in CRAFT"
#     dupe with number of trees planted 
#       (154-172 trees/ac, closer to 170, estimate is fine)
# when clicking on contract in the map, show all the exp metadata
#     even if not logged in
# even if not logged in, let them zoom in and see outlines
# no login at all, just do it publicly
# filter by county or ecoregion 
#   (the river, the ridge, the flatwoods (east, central, west/south)) 
#   or trialgroup
#   across the top (in the title bar?)
# they eventually want cost and harvest info
# start with my idea for the drone imagery
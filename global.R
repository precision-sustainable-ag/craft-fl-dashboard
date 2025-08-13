library(httr)
library(DBI)
library(dplyr)
library(plotly)
library(sf)
library(leaflet)

source("secrets.R")
source("loginAPI.R")
source("leaflet_helpers.R")


bootswatch = "flatly"

# leaflet-providers.js
copy_text = 
  c('&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
    'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'#,
    #'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    ) %>% 
  paste0("<p>", ., "</p>") %>% 
  paste(collapse = "") %>% 
  HTML()

drone_imagery_explainer = 
  div(
    tags$p(
      tags$a(
        "NDVI is a measure of vegetation presence, where 0 is absent and 1 is total coverage.",
        bsicons::bs_icon("wikipedia"), bsicons::bs_icon("box-arrow-up-right"),
        href = "https://en.wikipedia.org/wiki/Normalized_difference_vegetation_index",
        target="_blank",
        class = "text-body"
      )
    ),
    tags$p(
      tags$a(
        "NDRE is a measure of chlorophyll within vegetation, where 0 is poor and 1 is very healthy.",
        bsicons::bs_icon("wikipedia"), bsicons::bs_icon("box-arrow-up-right"),
        href = "https://en.wikipedia.org/wiki/Normalized_Difference_Red_Edge_Index",
        target="_blank",
        class = "text-body"
      )
    ),
    tags$p("Tree canopy area and volume were estimated based on drone flyovers.")
  )

colorpills = function(cols, vals) {
  
  purrr::map2(
    cols, scales::label_number_auto()(vals), 
    # vals %>% {sprintf("%0.2f",. )},
    ~{
      hcl <- farver::decode_colour(.x, "rgb", "hcl")
      label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
      tags$button( 
        .y, 
        type = "button", 
        class = glue::glue("btn"),
        style = glue::glue("background-color: {.x}; color: {label_col}; pointer-events: none;")
      ) 
    }) %>% 
    span(class="btn-group", role="group")
}

exitButton <- function(id) {
  absolutePanel(
    actionButton(
      inputId = id,
      label = bsicons::bs_icon("x-circle-fill"),
      class = "btn-link btn-sm"
    ),
    top = 0, right = 0
  )
}

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

drone_outlines <- read_sf("imagery/drone_outlines.geojson")
ecoregions = read_sf("fl_eco.geojson")


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

# local_copy = get_expunits(NULL)
# get_expunits = function(...) { local_copy }

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
      leaflet(
        ecoregions,
        options = leafletOptions(attributionControl = F)
      ) %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>% 
        addPolygons(opacity = 0, fillOpacity = 0)
    )
  }
  
  leaflet(
    plots,
    options = leafletOptions(attributionControl = F)
  ) %>% 
    addProviderTiles("OpenStreetMap.Mapnik", group = "political") %>% 
    addProviderTiles("Esri.WorldImagery", group = "aerial") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels", group = "aerial") %>%
    addCircleMarkers(
      lat = ~st_coordinates(centroid)[,2],
      lng = ~st_coordinates(centroid)[,1],
      group = "centroids_aerial",
      layerId = ~paste(contract, " "),
      color = ~ifelse(!is.na(imagery), '#f1a340', '#ffffff'), #PuOr->white
      opacity = 0.9, fillOpacity = 0.7
    ) %>% 
    addCircleMarkers(
      lat = ~st_coordinates(centroid)[,2],
      lng = ~st_coordinates(centroid)[,1],
      group = "centroids_political",
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
    hideGroup("plots") %>% 
    hideGroup("aerial") %>% 
    hideGroup("centroids_aerial")
}

make_pie <- function(dat, col_id) {
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
    plotly::layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    plotly::config(displayModeBar = F)
  
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
    plotly::layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = NULL,
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = textcolor),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    plotly::config(displayModeBar = F)
  
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
    filter(metric == mt) %>%
    mutate(aero_date = lubridate::as_date(aero_date)) %>% 
    arrange(aero_date) %>% 
    group_by(aero_date)
  
  ht = ifelse(n_groups(fn_df) <= 2, "84vh", "42vh")
  
  cols = if (mt %in% c("ndvi", "ndre")) {
    clampedColorNumeric("inferno", domain = c(0,1), na.color = "#FFFFFF00")
  } else {
    colorNumeric("inferno", domain = NULL, na.color = "#FFFFFF00")
  }
  
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
          l, x = rs,    # TODO magic domain values should be dynamic for other metrics
          colors = cols, 
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

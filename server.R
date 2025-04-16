library(shiny)
library(DBI)
library(sf)
library(leaflet)
library(dplyr)


server <- function(input, output, session) {
  sf_use_s2(FALSE)
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- loginAPI(
    id = "login",
    data = user_base,
    user_col = user_id,
    pwd_col = password_dummy,
    log_out = reactive(logout_init())
  )
  # 
  # # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  output$user_info <- renderUI({
    if (credentials()$user_auth) {
      span( icon("user"), tags$strong(credentials()$info$user_id) )
    }
  })
  
  # credentials = reactive(
  #   list(
  #     user_auth = T,
  #     info = data.frame(user_id = "saseehav")
  #   )
  # )
  
  contracts_for_user =
    eventReactive(
      credentials()$info,
      {
        tbl(craft_con, "access_view") %>% 
          filter(user_id %in% local(credentials()$info$user_id)) %>% 
          collect() %>%
          pull(contract) %>% 
          unique()
      }
    )
  
  
  output$contract_list = 
    renderUI({
      req(credentials()$user_auth)
      selectInput(
        "contract_picker",
        "Choose a contract",
        choices = c("", sort(contracts_for_user()))
      )
    })
  
  plots_for_user = 
    eventReactive(
      1, #credentials()$user_auth,
      {
        
        tbl(craft_con, "expunitids_view") %>% 
          # filter(contract %in% local(contracts_for_user())) %>% 
          collect() %>% 
          mutate(
            centroid = st_as_sfc(centroid),
            geometry = st_as_sfc(geometry)
          ) %>% 
          # mutate(centroid = centroid + runif(nrow(.), min = 0.008, max = 0.008)) %>% 
          st_as_sf(crs = 4326) %>% 
          st_zm()
      }
    )
  
  
  
  output$map =
    renderLeaflet({
      leaflet(plots_for_user()) %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>% 
        addCircleMarkers(
          lat = ~st_coordinates(centroid)[,2],
          lng = ~st_coordinates(centroid)[,1],
          popup = ~contract,
          group = "centroids"
        ) %>% 
        addPolygons(
          data = plots_for_user() %>% select(geometry), 
          group = "plots"
        ) %>% 
        hideGroup("plots")
    })
  
  observeEvent(
    input$map_zoom, {
      message("zoom: ", input$map_zoom)
      if (input$map_zoom > 14 & !credentials()$user_auth) {
        leafletProxy("map") %>%
          setView(input$map_center[["lng"]], input$map_center[["lat"]], 14)
      }

      if (input$map_zoom > 12 & credentials()$user_auth) {
        leafletProxy("map") %>% 
          hideGroup("centroids") %>% 
          showGroup("plots")
      } else {
        leafletProxy("map") %>%
          hideGroup("plots") %>%
          showGroup("centroids")
      }
    }
  )
  
  plots_for_summary <- reactive({
    req(nrow(plots_for_user()))
    #req(input$map_bounds)
    req(is.list(input$map_bounds))
    # box_wkt = glue::glue_data(
    #   input$map_bounds,
    #   "BOX({west} {south}, {east} {north})"
    # )
    # SELECT * from expunitids where geometry && box_wkt
    ext = rev(unlist(input$map_bounds)) %>% 
      purrr::set_names(c("xmin", "ymin", "xmax", "ymax")) %>% 
      st_bbox() %>% 
      st_as_sfc() %>% 
      st_as_sf(crs = 4326)

    #message(jsonlite::toJSON(input$map_bounds))
    #message(jsonlite::toJSON(st_coordinates(ext)))
    res <- plots_for_user() %>% 
      st_filter(ext) %>% 
      st_drop_geometry() %>% 
      select(-matches("centroid"), -matches("geometry"))
    #message("nrow: ", nrow(res))
    res
  }) %>% 
    debounce(1000)
  
  output$table <- renderTable({
    req(plots_for_summary())
    req(is.data.frame(plots_for_summary()))

    tibble(n = nrow(plots_for_summary()))
    # plots_for_summary() %>% 
    #   group_by(rootstock) %>% 
    #   summarise(acres = sum(area_acres)) %>% 
    #   arrange(-acres) %>% 
    #   slice(1:5)
    })
  
  payloads = eventReactive(
    input$contract_picker,
    {
      req(input$contract_picker != "")
      tbl(forms_con, "user_forms") %>% 
        filter(str_detect(as.character(json), local(input$contract_picker))) %>% 
        collect()
    }
  )
  
  output$submissions = 
    renderUI({
      # query the forms for this contract
      # browser()
      req(payloads())
      payloads = payloads()
      wellPanel(
        purrr::map(
          sort(payloads$created_at),
          ~tags$li(.x)
        )
      )
    })
}


# TODO:
# add drone images
# finish auth
# get all plots but fuzz them if not logged in
#   don't show polys, but do show drone rasters separate as time series
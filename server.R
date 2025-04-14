library(shiny)
library(DBI)
library(sf)
library(leaflet)
library(dplyr)


server <- function(input, output, session) {
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  # credentials <- loginAPI(
  #   id = "login",
  #   data = user_base,
  #   user_col = user_id,
  #   pwd_col = password_dummy,
  #   log_out = reactive(logout_init())
  # )
  # 
  # # call the logout module with reactive trigger to hide/show
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  
  credentials = reactive(
    list(
      user_auth = T,
      info = data.frame(user_id = "saseehav")
    )
  )
  
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
      contracts_for_user(),
      {
        tbl(craft_con, "expunitids_view") %>% 
          filter(contract %in% local(contracts_for_user())) %>% 
          collect() %>% 
          mutate(
            centroid = st_as_sfc(centroid),
            geometry = st_as_sfc(geometry)
          ) %>% 
          st_as_sf(crs = 4326) %>% 
          st_zm()
      }
    )
  
  
  
  output$map =
    renderLeaflet({
      req(credentials()$user_auth)
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
      message(input$map_zoom)
      if (input$map_zoom > 12) {
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
    req(input$map_bounds)
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
    plots_for_user() %>% 
      filter(st_intersects(ext, sparse = F))
  })
  
  output$table <- renderTable(plots_for_summary())
  
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
# add auth
# fix zoom/polygon thing

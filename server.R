library(shiny)
library(DBI)
library(sf)
library(leaflet)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(promises)
library(shiny.telemetry)
future::plan("multisession")




server <- function(input, output, session) {
  sf_use_s2(FALSE)
  # my_id = reactive(
  #   paste(input$httpbin[["origin"]], uuid::UUIDgenerate(), sep = "__")
  #   )
  my_id = uuid::UUIDgenerate()
  telemetry$start_session(
    track_values = T,
    track_anonymous_user = F, 
    username = my_id
  )
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
  # 
  # output$user_info <- renderUI({
  #   if (credentials()$user_auth) {
  #     span( icon("user"), tags$strong(credentials()$info$user_id) )
  #   }
  # })
  
  # credentials = reactive(
  #   list(
  #     user_auth = T,
  #     info = data.frame(user_id = "saseehav")
  #   )
  # )
  
  # contracts_for_user =
  #   eventReactive(
  #     credentials()$info,
  #     {
  #       tbl(craft_con, "access_view") %>% 
  #         filter(user_id %in% local(credentials()$info$user_id)) %>% 
  #         collect() %>%
  #         pull(contract) %>% 
  #         unique()
  #     }
  #   )
  # 
  # 
  # output$contract_list = 
  #   renderUI({
  #     req(credentials()$user_auth)
  #     selectInput(
  #       "contract_picker",
  #       "Choose a contract",
  #       choices = c("", sort(contracts_for_user()))
  #     )
  #   })
  
  mirai::mirai(
    get_expunits(creds = dstadmin_creds),
    get_expunits = get_expunits,
    dstadmin_creds = dstadmin_creds,
    make_connection = make_connection
    ) %...>%
    (function(result) {})
  
  plots_for_user = 
    eventReactive(
      1, #credentials()$user_auth,
      {
        fetch_expunits()
      }
    )
  
  observeEvent(
    plots_for_user(), {
      req(nrow(plots_for_user()) > 0)
      updateSlimSelect(
        inputId = "rs",
        choices = sort(unique(plots_for_user() %>% pull(rlabel)))
      )
      updateSlimSelect(
        inputId = "sc",
        choices = sort(unique(plots_for_user() %>% pull(slabel)))
      )
    })
  
  plots_with_filters = 
    reactive({
      ret = plots_for_user()
      
      if (length(input$eco)) {
        ret = ret %>% 
          purrr::quietly(st_filter)(
            ecoregions %>% filter(US_L4NAME %in% input$eco)
          ) %>% 
          .[["result"]]
      }
      
      if (length(input$rs)) {
        ret = ret %>% filter(rlabel %in% input$rs)
      }
      
      if (length(input$sc)) {
        ret = ret %>% filter(slabel %in% input$sc)
      }
      
      if (input$contract_typed != "") {
        ret = ret %>% filter(
          contract %in% tolower(stringr::str_trim(input$contract_typed))
        )
      }
      
      
      ret
    })
  
  observe({
    filter_flags = c(length(input$eco), length(input$rs), 
                     length(input$sc), input$contract_typed != "")
    if (any(filter_flags)) {
      updateActionButton(inputId = "clear_filters", disabled = F, )
    } else  {
      updateActionButton(inputId = "clear_filters", disabled = T)
    }
  })
  
  observe({
    updateCheckboxGroupButtons(inputId = "eco", selected = character(0))
    updateSlimSelect(inputId = "sc", selected = character(0))
    updateSlimSelect(inputId = "rs", selected = character(0))
    updateSearchInput(
      session = getDefaultReactiveDomain(),
      inputId = "contract_typed", value = "", trigger = T
    )
  }) %>% 
    bindEvent(input$clear_filters)
  
  
  output$map =
    renderLeaflet({
      make_map(plots_with_filters())
    })
  
  observeEvent(
    input$map_zoom, {
      # if (input$map_zoom > 14 & !credentials()$user_auth) {
      #   leafletProxy("map") %>%
      #     setView(input$map_center[["lng"]], input$map_center[["lat"]], 14)
      # }
      
      # if (input$map_zoom > 12 & credentials()$user_auth) {
      #   leafletProxy("map") %>%
      #     hideGroup("centroids_political") %>%
      #     hideGroup("centroids_aerial") %>%
      #     showGroup("plots")
      # } else {
      #   leafletProxy("map") %>%
      #     hideGroup("plots") %>%
      #     showGroup("centroids_aerial") %>%
      #     showGroup("centroids_political")
      # }
      
      if (input$map_zoom > 10) {
        leafletProxy("map") %>%
          hideGroup("centroids_political") %>%
          hideGroup("political") %>%
          showGroup("centroids_aerial") %>%
          showGroup("aerial") %>% 
          showGroup("plots")
      } else {
        leafletProxy("map") %>%
          hideGroup("centroids_aerial") %>%
          hideGroup("aerial") %>% 
          hideGroup("plots") %>%
          showGroup("centroids_political") %>%
          showGroup("political")
      }
    }
  )
  
  observe({
    leafletProxy("map") %>% 
      setView(input$map_marker_click$lng, input$map_marker_click$lat, 16) 
  }) %>% 
    bindEvent(input$map_marker_click)
  # TODO: diagram this logic
  contract_clicked = 
    reactive({ 
      markerid = stringr::str_trim(input$map_marker_click$id) 
      # plotid = stringr::str_extract(input$map_shape_click$id, "^.+;") %>% 
      #   stringr::str_remove(";")
      # message(jsonlite::toJSON(list(markerid = markerid, plotid = plotid), pretty = T))
      # if (length(input$contract_typed) && nchar(input$contract_typed)) {
      #   tolower(stringr::str_trim(input$contract_typed))
      # } else if (length(plotid) && plotid != "" && input$map_zoom >= 15) { 
      #   plotid 
      # } else { 
      markerid 
      #}
    }) %>%
    bindEvent(input$map_marker_click, input$map_shape_click)
  
  output$drone_imagery = 
    renderUI({
      req(input$map_marker_click$id)
      make_raster_list(drone_ids(), contract_clicked(), input$drone_metric) %>% 
        leafsync::sync(ncol = 2)
    })
  
  output$imagery_legend = 
    renderUI({
      brks = if (input$drone_metric %in% c("ndvi", "ndre")) {
        (0:4)/4
      } else {
        c(0, 2.5, 5, 10, 20, 40)
      }
      tags$span(
        HTML("Legend:&nbsp;"), 
        colorpills(viridis::inferno(length(brks)), brks),
        style="white-space: nowrap;"
        )
    })
  
  drone_ids = reactive(fetch_drone_ids()) %>% 
    bindEvent(input$map_click, once = T)
  
  observe({
    imgs = drone_ids() %>% filter(contract == contract_clicked())
    req(nrow(imgs) > 0)
    
    # TODO: fetch quantiles or range from table given drone IDs to pass to colorpills
    showModal(
      modalDialog(
        uiOutput("drone_imagery"),
        title = div(
          tags$span("Contract: ", contract_clicked()),
          tags$span(
            uiOutput("imagery_legend"),
            pickerInput(
              "drone_metric", label = NULL,
              choices = c(
                "NDVI" = "ndvi", "NDRE" = "ndre"#,
                #"Canopy area" = "area",
                #"Canopy volume" = "volume"
              ),
              choicesOpt = list(
                content = c(
                  "NDVI", "NDRE"#,
                  # "Canopy area, m<sup>2</sup>", 
                  #"Canopy volume, m<sup>3</sup>"
                )
              ),
              selected = "ndvi",
              inline = T
            ),
            actionButton(
              "ndvi_popup", 
              label = bsicons::bs_icon("info-circle"),
              class = "btn-link btn-sm",
              "data-bs-trigger"="focus", # TODO: delete these two lines
              tabindex="0"
            ) %>% 
              popover(
                drone_imagery_explainer, 
                exitButton("x_metrics"), 
                id = "metrics_popover"
              ),
            style = "padding-left: 50px;"
          ),
          absolutePanel(
            div(modalButton(bsicons::bs_icon("x-circle-fill")), style = "float: right;"),
            top = 2, right = 2, width = 50, height = 50
          )
        ),
        easyClose = T,
        footer = NULL,
        size = "xl"
      )
    )
  }) %>% 
    bindEvent(input$show_imagery)
  
  plots_for_summary <- reactive({
    req(nrow(plots_with_filters()) > 0)
    req(is.list(input$map_bounds))
    
    ext = rev(unlist(input$map_bounds)) %>% 
      purrr::set_names(c("xmin", "ymin", "xmax", "ymax")) %>% 
      st_bbox() %>% 
      st_as_sfc() %>% 
      st_as_sf(crs = 4326)
    
    plots_with_filters() %>% 
      purrr::quietly(st_filter)(ext) %>% 
      .[["result"]] %>% 
      st_drop_geometry() %>% 
      select(-matches("centroid"), -matches("geometry"))
  }) %>% 
    debounce(1000)
  
  
  output$acres_total <- renderUI({
    req(plots_for_summary())
    
    val = plots_for_summary() %>%
      pull(area_acres) %>% 
      sum() %>% 
      round() 
    
    v_acres = scales::label_comma()(val)
    v_trees = scales::label_comma()(val*170)
    
    #glue::glue("{v_acres} acres<br>{v_trees} trees") %>% HTML()
    div(v_acres, "acres", tags$br(), v_trees, "trees")
  })
  
  output$trees_total <- reactive({
    req(nrow(plots_for_summary()) > 0)
    
    plots_for_summary() %>%
      pull(area_acres) %>% 
      sum() %>% .*170 %>% 
      round() %>% 
      scales::label_comma()(.) %>% 
      paste("trees")
  })
  
  output$acres_years_avg <- renderText({
    req(nrow(plots_for_summary()) > 0)
    
    res = plots_for_summary() %>%
      group_by(year) %>%
      summarize(acres = sum(area_acres), .groups = "drop") %>% 
      pull(acres) %>% 
      mean() %>% 
      round()
    #if (is.na(res)) res = 0
    res %>% 
      scales::label_comma()(.) %>% 
      paste(., "acres planted per year on average")
  })
  
  output$acres_years <- renderPlotly({
    req(plots_for_summary())
    
    plots_for_summary() %>%
      group_by(year) %>%
      summarize(acres = round(sum(area_acres)), .groups = "drop") %>%
      arrange(year) %>% 
      plot_ly() %>%
      add_lines(
        x = ~year, y = ~acres,
        color = I("white"), span = I(1),
        fill = 'tozeroy', alpha = 0.5,
        hovertemplate = "%{x}: <b>%{y:,}</b>ac<extra></extra>",
        name = ""
      ) %>%
      plotly::layout(
        xaxis = list(visible = F, showgrid = F, title = "", fixedrange = T),
        yaxis = list(visible = F, showgrid = F, title = "", fixedrange = T),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      plotly::config(displayModeBar = F)
  })
  
  output$rootstocks_pie <- renderPlotly({
    req(plots_for_summary())
    
    purrr::quietly(make_pie)(plots_for_summary(), rlabel)$result
  })
  
  output$scions_pie <- renderPlotly({
    req(plots_for_summary())
    
    purrr::quietly(make_bar)(plots_for_summary(), slabel, input$theme)$result
  })
  
  output$scions_contract = renderUI({
    has_img = st_drop_geometry(plots_for_user()) %>% 
      filter(contract == contract_clicked()) %>% 
      pull(imagery) %>% 
      any(na.rm = T)
    
    yr = st_drop_geometry(plots_for_user()) %>% 
      filter(contract == contract_clicked()) %>% 
      pull(year)
    
    btn = actionButton(
      "show_imagery", "Show drone imagery", 
      icon = icon("map"), class = "btn-success"
    )
    
    div(
      "Contract", contract_clicked(), "active since", yr[1],
      tags$hr(),
      if (has_img) { btn }
    ) %>% 
      as_fillable_container()
  })
  
  output$acres_contract = renderTable({
    st_drop_geometry(plots_for_user()) %>% 
      filter(contract == contract_clicked()) %>% 
      mutate(cofactor = replace(cofactor, is.na(cofactor) | cofactor == "NA", "")) %>% 
      select("Plot" = expunitid, "Acres" = area_acres, 
             "Scion" = slabel, "Rootstock" = rlabel,
             "Trial Group" = trial_group, "Cofactor" = cofactor)
  })
  
  output$rootstocks_contract = renderTable({
    
  })
  
  observe({
    id = contract_clicked()
    nav_insert(
      "navset_scions",
      nav = nav_panel_hidden(
        value = id,
        card(uiOutput("scions_contract"), exitButton("x"))
      ),
      select = T
    )
    nav_insert(
      "navset_acres",
      nav = nav_panel_hidden(
        value = id,
        card(tableOutput("acres_contract"), exitButton("x"))
      ),
      select = T
    )
    nav_insert(
      "navset_rootstocks",
      nav = nav_panel_hidden(
        value = id,
        card("Harvest data", tableOutput("rootstocks_contract"), exitButton("x"))
      ),
      select = T
    )
  }) %>% 
    bindEvent(input$map_marker_click)
  
  
  observe({
    nav_select("navset_scions", "main")
    nav_remove("navset_scions", contract_clicked())
    
    nav_select("navset_acres", "main")
    nav_remove("navset_acres", contract_clicked())
    
    nav_select("navset_rootstocks", "main")
    nav_remove("navset_rootstocks", contract_clicked())
  }) %>%
    bindEvent(input$x)
  
  observe({
    req(input$map_marker_click$lat)
    if (input$map_click$lat != input$map_marker_click$lat & input$map_zoom < 16) {
      nav_select("navset_scions", "main")
      nav_remove("navset_scions", contract_clicked())
      
      nav_select("navset_acres", "main")
      nav_remove("navset_acres", contract_clicked())
      
      nav_select("navset_rootstocks", "main")
      nav_remove("navset_rootstocks", contract_clicked())
    }
  }) %>%
    bindEvent(input$map_click)
  
  observe({
    toggle_popover("copyright_popover", show = F)
  }) %>% 
    bindEvent(input$x_copyright)
  
  observe({
    toggle_popover("metrics_popover", show = F)
  }) %>% 
    bindEvent(input$x_metrics)
  
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
# finish auth
library(shiny)
library(leaflet)
library(bslib)
library(plotly)
library(shinyWidgets)
library(sf)

fillable_navset_hidden <- function(..., id) {
  panel <- shiny::tabsetPanel(id = id, type = "hidden", ...)
  panel$children[[2]] <- bslib:::makeTabsFillable(panel$children[[2]])
  bslib::as_fill_carrier(panel)
}


vbs <- list(
  "acres_total" = value_box(
    "Total groves planted",
    value = uiOutput("acres_total"),
    theme = "primary",
    showcase = icon("seedling")
  ),
  "acres_years" = value_box(
    "Enrollment over time",
    value = textOutput("acres_years_avg"),
    theme = "info",
    showcase = plotlyOutput("acres_years"),
    showcase_layout = "bottom"
  ),
  "rootstocks" = card(
    card_header("Top rootstocks"),
    card_body(plotlyOutput("rootstocks_pie"))
  ),
  "scions" = card(
    card_header("Top scions"),
    card_body(plotlyOutput("scions_pie"))
  )
)


ui <- page_sidebar(
  window_title = "CRAFT Data Dashboard",
  title = div(
    span("CRAFT Data Dashboard"),
    span(input_dark_mode(id = "theme"), style = "padding-left: 50px;")
  ),
  theme = bs_theme(bootswatch = bootswatch) %>% 
    bslib::bs_add_rules(
      c(".modal-xl { width: 98%; margin: 1vh auto; }",
        ".modal-body { max-height: 90vh; overflow-y: auto; }",
        ".modal-dialog { max-width: 98%; }",
        ":root { 
          .ss-content .ss-search input {
            color: var(--ss-font-color);
          } 
        }",
        ".dropdown-menu { --bs-dropdown-zindex: 10000; }",
        "#drone_metric-label { display: none !important; }",
        ".form-group { margin-bottom: 0rem; }",
        ".popover {
           --bs-popover-max-width: 350;
           --bs-popover-font-size: 1rem;
        }",
        "button.btn-close { display: none; }"
      )
    ),  
  sidebar = sidebar(
    # shinyauthr::loginUI(
    #   id = "login",
    #   title = NULL,
    #   user_title = "Username",
    #   pass_title = "Password"
    # ),    
    
    # shinyauthr::logoutUI(id = "logout"),
    # uiOutput("user_info"),
    
    checkboxGroupButtons(
      "eco", "Filter by ecoregion", 
      choices = sort(unique(ecoregions$US_L4NAME)),
      status = "outline-info",
      direction = "vertical",
      width = "100%"
    ),
    
    slimSelectInput(
      "sc", "Filter by scion",
      choices = c(),
      multiple = T,
      width = "100%",
      closeOnSelect = T,
      placeholder = ""
    ),
    
    slimSelectInput(
      "rs", "Filter by rootstock",
      choices = c(),
      multiple = T,
      width = "100%",
      closeOnSelect = T,
      placeholder = ""
    ),
    
    uiOutput("contract_list"),
    open = F,
    width = 350
  ),
  layout_columns(
    col_widths = c(8, 4),
    layout_column_wrap(
      fillable_navset_hidden(
        id = "navset_acres",
        nav_panel_hidden(
          layout_column_wrap(
            vbs[["acres_total"]], 
            vbs[["acres_years"]]
          ), 
          value = "main"
        )
      ),
      card(
        card_body(
          leafletOutput("map"),
          absolutePanel(
            actionButton(
              "copyright", label = NULL, 
              icon = icon("copyright"),
              class = "btn-primary btn-sm",
              tabindex = "0",
              "data-bs-trigger"="focus"
            ) %>% 
              bslib::popover(
                copy_text, 
                exitButton("x_copyright"),
                title = "Map layer attributions", 
                id = "copyright_popover"
                ),
            bottom = 0, left = 0
          ),
          class = "p-0"
        )
      ),
      heights_equal = "row",
      width = 1
    ),
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      fillable_navset_hidden(
        id = "navset_scions",
        nav_panel_hidden(vbs[["scions"]], value = "main")
      ), 
      vbs[["rootstocks"]]
    )
  )
)
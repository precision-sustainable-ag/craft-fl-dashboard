library(shiny)
library(leaflet)
library(bslib)
library(plotly)

# ui <- fluidPage(
#   titlePanel("CRaFT Grower Dashboard"),
#   # add logout button UI
#   div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
#   # add login panel UI function
#   shinyauthr::loginUI(
#     id = "login",
#     title = "Log in with your Kobo credentials",
#     user_title = "Username"
#     ),
#   sidebarLayout(
#     sidebarPanel = sidebarPanel(
#       uiOutput("contract_list")
#     ),
#     mainPanel = mainPanel(
#       leafletOutput("map"),
#       fluidRow(
#         column(6, uiOutput("submissions")),
#         column(6, tableOutput("table"))
#       )
#     )
#   )
# )

vbs <- list(
  "acres_total" = value_box(
    "Total groves planted",
    value = textOutput("acres_total"),
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
  theme = bs_theme(bootswatch = "flatly") %>% 
    bslib::bs_add_rules(
      c(".modal-xl { width: 98%; margin: 1vh auto; }",
        ".modal-body { max-height: 90vh; overflow-y: auto; }",
        ".modal-dialog { max-width: 98%; }")
    ),  
  sidebar = sidebar(
    shinyauthr::loginUI(
      id = "login",
      title = NULL,
      user_title = "Username",
      pass_title = "Password"
    ),    
    
    shinyauthr::logoutUI(id = "logout"),
    uiOutput("user_info"),
    
    uiOutput("contract_list"),
    open = F,
    width = 350
  ),
  layout_columns(
    col_widths = c(8, 4),
    layout_column_wrap(
      layout_column_wrap(vbs[["acres_total"]], vbs[["acres_years"]]),
      leafletOutput("map"),
      heights_equal = "row",
      width = 1
    ),
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      vbs[["rootstocks"]], vbs[["scions"]]
    )
  )
  # layout_column_wrap(
  #   !!!vbs
  # ),
  # leafletOutput("map")
  # fluidRow(
  #   column(6, uiOutput("submissions")),
  # )
)